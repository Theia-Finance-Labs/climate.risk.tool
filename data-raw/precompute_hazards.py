"""
Precompute hazard statistics aggregated over administrative regions.

This script processes NetCDF (.nc) hazard files, extracting statistical summaries
(min, max, mean, percentiles) for each administrative region (ADM1 and ADM2 levels).

The output is a single CSV file combining all hazard types and regions.

Hazard-specific dimensions (such as 'season' for drought hazards) are automatically
detected and preserved in the output. Hazards without these dimensions will have
NaN values for those columns, allowing flexible processing of mixed dimension sets.
"""

import os
import glob
import gc
import itertools
import time
import numpy as np
import pandas as pd
import geopandas as gpd
import xarray as xr
import dask
from dask.diagnostics import ProgressBar
from dask.distributed import Client, LocalCluster
from unidecode import unidecode
from tqdm import tqdm


import warnings

# Silence Dask's "Sending large graph" warning
# We know we have many chunks (necessary for 2.5B points), so the graph is large.
# This warning is just advisory and we accept the overhead.
warnings.filterwarnings("ignore", message="Sending large graph")
warnings.filterwarnings(
    "ignore", message="The specified chunks separate the stored chunks"
)

# ============================================================================
# HELPER FUNCTIONS
# ============================================================================


def parse_hazard_from_path(path):
    """
    Extract hazard_type and hazard_indicator from directory structure.

    Expected path format: .../hazards/HazardType/HazardIndicator/...

    Args:
        path: File path containing hazard directory structure

    Returns:
        tuple: (hazard_type, hazard_indicator)

    Raises:
        ValueError: If path structure is invalid
    """
    path_parts = path.split(os.sep)

    if "hazards" not in path_parts:
        raise ValueError(
            f"Invalid path structure - 'hazards' directory not found: {path}"
        )

    hazards_idx = path_parts.index("hazards")

    if hazards_idx + 2 >= len(path_parts):
        raise ValueError(
            f"Invalid path structure - missing hazard type/indicator: {path}"
        )

    hazard_type = path_parts[hazards_idx + 1]
    hazard_indicator = path_parts[hazards_idx + 2]

    return hazard_type, hazard_indicator


def fix_text(text):
    """
    Normalize text by removing accents and converting to ASCII.

    This is equivalent to R's stri_trans_general("Latin-ASCII") and ensures
    region names from shapefiles are consistently normalized (no accents).

    Args:
        text: Input string (typically region names from shapefiles)

    Returns:
        str: Cleaned text with accents converted to ASCII
    """
    if not isinstance(text, str):
        return text

    # Simply unidecode - this removes accents and converts to ASCII
    # This matches what R does with stri_trans_general("Latin-ASCII")
    return unidecode(text)


def load_adm_shapefile(adm_path):
    """
    Load administrative boundary shapefile with strict validation.

    Args:
        adm_path: Path to shapefile

    Returns:
        gpd.GeoDataFrame: Loaded and validated geodataframe with 'region' column

    Raises:
        FileNotFoundError: If shapefile doesn't exist
        ValueError: If CRS is missing or region name column not found
    """
    if not os.path.exists(adm_path):
        raise FileNotFoundError(f"Shapefile not found: {adm_path}")

    # Try reading with UTF-8 encoding first, then latin1 if that fails
    try:
        gdf = gpd.read_file(adm_path, encoding="utf-8")
    except (UnicodeDecodeError, Exception):
        print(f"  ⚠️  UTF-8 encoding failed, trying latin1 for {adm_path}")
        try:
            gdf = gpd.read_file(adm_path, encoding="latin1")
        except:
            # Fall back to default encoding
            gdf = gpd.read_file(adm_path)

    # Strict CRS validation - fail if not defined
    if gdf.crs is None:
        raise ValueError(f"CRS not defined in shapefile: {adm_path}")

    # Transform to WGS84 if needed
    if gdf.crs != "EPSG:4326":
        gdf = gdf.to_crs("EPSG:4326")

    # Find region name column
    name_col = None
    for col in ["shapeName", "NAME_2", "NAME_1", "NAME", "name", "prov_name"]:
        if col in gdf.columns:
            name_col = col
            break

    if name_col is None:
        raise ValueError(f"No valid region name column found in shapefile: {adm_path}")

    # Apply unidecode to ALL region names to remove accents
    # This ensures consistent ASCII-only names throughout
    print(f"  🔧 Applying unidecode to region names from column '{name_col}'...")

    # Show a few examples before/after
    sample_originals = gdf[name_col].head(5).tolist()
    gdf["region"] = gdf[name_col].apply(fix_text)
    sample_fixed = gdf["region"].head(5).tolist()

    print(f"  📝 Sample region name transformations:")
    for orig, fixed in zip(sample_originals, sample_fixed):
        if orig != fixed:
            print(f"     '{orig}' → '{fixed}'")
        else:
            print(f"     '{orig}' (unchanged)")

    return gdf


def compute_statistics(values):
    """
    Compute statistical summaries for a set of values.

    Args:
        values: numpy array of values

    Returns:
        dict: Dictionary with keys: min, max, mean, median, p2_5, p5, p10, p90, p95, p97_5

    Raises:
        ValueError: If values array is empty
    """
    if values.size == 0:
        raise ValueError("Cannot compute statistics on empty array")

    def percentile(arr, q):
        return float(np.percentile(arr, q))

    return {
        "min": float(np.min(values)),
        "max": float(np.max(values)),
        "mean": float(np.mean(values)),
        "median": percentile(values, 50),
        "p2_5": percentile(values, 2.5),
        "p5": percentile(values, 5),
        "p10": percentile(values, 10),
        "p90": percentile(values, 90),
        "p95": percentile(values, 95),
        "p97_5": percentile(values, 97.5),
    }


def build_coordinate_region_lookup(
    lats,
    lons,
    adm_gdf,
    max_points=5_000_000,
    target_batch_size=250_000,
):
    """
    Build a lookup table that maps every coordinate in the dataset grid to a region.

    This allows us to skip running a spatial join for every chunk and reuse the same mapping
    when processing every dimension combination.

    Includes a fallback mechanism: for regions that contain no grid point centers,
    the nearest grid point center is assigned to ensure every region has data.
    """
    total_points = len(lats) * len(lons)

    if total_points == 0:
        return pd.DataFrame(columns=["lat", "lon", "region"])

    if total_points > max_points:
        print(
            f"    ⚠️  Coordinate grid ({total_points:,} points) exceeds "
            f"{max_points:,} – falling back to chunk-level spatial joins."
        )
        return None

    print(
        f"    Building coordinate→region lookup ({total_points:,} grid points, "
        f"{len(lats)} lat × {len(lons)} lon)..."
    )

    per_lat_quota = max(1, target_batch_size // max(1, len(lons)))
    batch_lat_size = min(len(lats), per_lat_quota)

    coords_batches = []

    for lat_start in range(0, len(lats), batch_lat_size):
        lat_slice = lats[lat_start : lat_start + batch_lat_size]
        lat_repeat = np.repeat(lat_slice, len(lons))
        lon_tile = np.tile(lons, len(lat_slice))

        batch_df = pd.DataFrame({"lat": lat_repeat, "lon": lon_tile})
        gdf_batch = gpd.GeoDataFrame(
            batch_df,
            geometry=gpd.points_from_xy(batch_df["lon"], batch_df["lat"]),
            crs="EPSG:4326",
        )

        coords_with_region = gpd.sjoin(
            gdf_batch,
            adm_gdf[["region", "geometry"]],
            how="inner",
            predicate="within",
        )

        coords_batches.append(coords_with_region[["lat", "lon", "region"]])

        del batch_df, gdf_batch, coords_with_region
        gc.collect()

    if not coords_batches:
        lookup_df = pd.DataFrame(columns=["lat", "lon", "region"])
    else:
        lookup_df = pd.concat(coords_batches, ignore_index=True)
    lookup_df = (
        lookup_df.drop_duplicates(subset=["lat", "lon", "region"])
        .reset_index(drop=True)
        .astype({"lat": float, "lon": float})
    )

    # Fallback for missing regions: assign nearest point to centroid
    assigned_regions = set(lookup_df["region"].unique())
    all_regions = set(adm_gdf["region"].unique())
    missing_regions_names = all_regions - assigned_regions

    if missing_regions_names:
        print(
            f"    📍 {len(missing_regions_names)} regions missing grid points. "
            "Assigning nearest neighbor fallbacks..."
        )

        missing_regions_gdf = adm_gdf[
            adm_gdf["region"].isin(missing_regions_names)
        ].copy()

        # Create a GeoDataFrame of ALL unique grid points for distance calculation
        lat_repeat_all = np.repeat(lats, len(lons))
        lon_tile_all = np.tile(lons, len(lats))
        all_points_gdf = gpd.GeoDataFrame(
            {"lat": lat_repeat_all, "lon": lon_tile_all},
            geometry=gpd.points_from_xy(lon_tile_all, lat_repeat_all),
            crs="EPSG:4326",
        )

        # Project to a metric CRS for accurate distance (Web Mercator is usually fine for this scale)
        # Using a more local projection could be better but 3857 is generally acceptable for center-of-Brazil
        all_points_proj = all_points_gdf.to_crs(epsg=3857)
        missing_regions_proj = missing_regions_gdf.to_crs(epsg=3857)

        fallbacks = []
        for idx, region_row in missing_regions_proj.iterrows():
            region_name = region_row["region"]
            centroid = region_row.geometry.centroid

            # Find index of nearest point
            distances = all_points_proj.distance(centroid)
            nearest_idx = distances.idxmin()

            nearest_lat = all_points_gdf.loc[nearest_idx, "lat"]
            nearest_lon = all_points_gdf.loc[nearest_idx, "lon"]

            fallbacks.append(
                {"lat": nearest_lat, "lon": nearest_lon, "region": region_name}
            )

        if fallbacks:
            fallback_df = pd.DataFrame(fallbacks)
            lookup_df = pd.concat([lookup_df, fallback_df], ignore_index=True)
            print(f"    ✅ Added {len(fallbacks)} fallback assignments.")

    print(f"    Coordinate lookup cached for {len(lookup_df):,} point-region pairs.")

    return lookup_df


# ============================================================================
# FILE TYPE PROCESSORS
# ============================================================================


def process_nc_hazard(
    nc_path, adm_gdf, adm_level, ensemble_filter, lat_chunk_size=100, lon_chunk_size=100
):
    """
    Process NetCDF hazard file directly and aggregate over regions.

    Uses chunked processing to handle large files efficiently without loading
    entire dataset into memory at once.
    """
    if not os.path.exists(nc_path):
        raise FileNotFoundError(f"NetCDF file not found: {nc_path}")

    # Open with chunks={} to ensure we get Dask arrays (using native chunking)
    ds = xr.open_dataset(nc_path, chunks={})

    # Get the data variable
    var_names = list(ds.data_vars.keys())
    if not var_names:
        raise ValueError(f"No data variables found in NetCDF: {nc_path}")
    var_name = var_names[0]
    da = ds[var_name]

    # Filter to specified ensemble (if dimension exists and value is present)
    has_ensemble = False
    ensemble_value_used = None
    if "ensemble" in da.dims:
        available_ensembles = da.coords["ensemble"].values.tolist()
        available_ensembles_str = [str(e) for e in available_ensembles]

        if ensemble_filter in available_ensembles_str:
            idx = available_ensembles_str.index(ensemble_filter)
            da = da.sel(ensemble=available_ensembles[idx])
            has_ensemble = True
            ensemble_value_used = ensemble_filter
        else:
            print(
                f"    ⚠️  Ensemble '{ensemble_filter}' not found. Using: '{available_ensembles_str[0]}'"
            )
            da = da.sel(ensemble=available_ensembles[0])
            has_ensemble = True
            ensemble_value_used = str(available_ensembles[0])

    # Extract hazard info from path
    hazard_type, hazard_indicator = parse_hazard_from_path(nc_path)

    # Get dimension information
    dims = list(da.dims)
    if "lon" not in dims or "lat" not in dims:
        raise ValueError(f"Missing required coordinates (lon/lat) in {nc_path}")

    non_spatial_dims = [d for d in dims if d not in ["lon", "lat"]]

    # Build coordinate -> region lookup (with fallbacks for missing regions)
    lats = da.coords["lat"].values
    lons = da.coords["lon"].values
    coord_region_lookup = build_coordinate_region_lookup(lats, lons, adm_gdf)

    # Build list of all dimension combinations
    dim_combinations = []
    if non_spatial_dims:
        dim_values = {dim: da[dim].values for dim in non_spatial_dims}
        keys = list(dim_values.keys())
        for combo in itertools.product(*[dim_values[k] for k in keys]):
            dim_combinations.append(dict(zip(keys, combo)))
    else:
        dim_combinations = [{}]

    all_chunk_results = []

    print(
        f"    Processing {len(dim_combinations)} dimension combinations for {len(adm_gdf)} regions..."
    )
    for dim_combo in tqdm(
        dim_combinations, desc="    Processing", leave=False, ncols=80
    ):
        # Select this specific slice
        da_slice = da
        for dim_name, dim_val in dim_combo.items():
            da_slice = da_slice.sel({dim_name: dim_val})

        # --- Mistake Fix: Nodata Handling ---
        # Specific fix for Heat HI which has 76.02 and other low values as nodata
        # Real Heat Index values in Kelvin are typically > 300 (27°C)
        if hazard_type == "Heat":
            da_slice = da_slice.where(da_slice > 300)
        # ------------------------------------

        accumulated_points = []
        n_lat = len(lats)
        n_lon = len(lons)
        n_lat_chunks = (n_lat + lat_chunk_size - 1) // lat_chunk_size
        n_lon_chunks = (n_lon + lon_chunk_size - 1) // lon_chunk_size
        total_spatial_chunks = n_lat_chunks * n_lon_chunks

        spatial_pbar = tqdm(
            total=total_spatial_chunks,
            desc="      Spatial chunks",
            leave=False,
            ncols=80,
            position=1,
        )

        for lat_start in range(0, n_lat, lat_chunk_size):
            lat_end = min(lat_start + lat_chunk_size, n_lat)
            for lon_start in range(0, n_lon, lon_chunk_size):
                lon_end = min(lon_start + lon_chunk_size, n_lon)

                # Select spatial chunk (lazy)
                da_chunk = da_slice.isel(
                    lat=slice(lat_start, lat_end), lon=slice(lon_start, lon_end)
                )

                # Compute and convert to dataframe
                if isinstance(da_chunk.data, dask.array.Array):
                    da_chunk_loaded = da_chunk.compute()
                else:
                    da_chunk_loaded = da_chunk

                spatial_pbar.update(1)

                df_chunk = (
                    da_chunk_loaded.to_dataframe(name="value")
                    .reset_index()
                    .dropna(subset=["value"])
                )

                if len(df_chunk) == 0:
                    del da_chunk_loaded
                    continue

                # --- Mistake Fix: Handle fallback when lookup table is None ---
                if coord_region_lookup is not None:
                    joined = pd.merge(
                        df_chunk, coord_region_lookup, on=["lat", "lon"], how="inner"
                    )
                else:
                    # Fallback: spatial join for this specific chunk
                    gdf_chunk = gpd.GeoDataFrame(
                        df_chunk,
                        geometry=gpd.points_from_xy(df_chunk["lon"], df_chunk["lat"]),
                        crs="EPSG:4326",
                    )
                    joined = gpd.sjoin(
                        gdf_chunk,
                        adm_gdf[["region", "geometry"]],
                        how="inner",
                        predicate="within",
                    )
                    # Convert back to DataFrame and drop geometry for consistency
                    joined = pd.DataFrame(joined.drop(columns="geometry"))
                # -----------------------------------------------------------

                if len(joined) > 0:
                    # Accumulate only necessary columns
                    cols_to_keep = ["region", "value"]
                    for col in joined.columns:
                        if col in dim_combo and col not in cols_to_keep:
                            cols_to_keep.append(col)
                    accumulated_points.append(joined[cols_to_keep])

                del da_chunk_loaded, df_chunk, joined

        spatial_pbar.close()
        gc.collect()

        if len(accumulated_points) == 0:
            continue

        df_all_points = pd.concat(accumulated_points, ignore_index=True)
        del accumulated_points
        gc.collect()

        # Add metadata
        df_all_points["hazard_type"] = hazard_type
        df_all_points["hazard_indicator"] = hazard_indicator
        df_all_points["ensemble"] = ensemble_value_used if has_ensemble else None

        # Standardize column names
        if "GWL" in df_all_points.columns:
            df_all_points = df_all_points.rename(columns={"GWL": "scenario_name"})
        if "return_period" in df_all_points.columns:
            df_all_points = df_all_points.rename(
                columns={"return_period": "hazard_return_period"}
            )

        # Identify groupby columns
        metadata_cols = [
            "scenario_name",
            "hazard_return_period",
            "ensemble",
            "hazard_type",
            "hazard_indicator",
            "value",
            "region",
        ]
        group_cols = ["hazard_type", "hazard_indicator", "region", "ensemble"]
        if "scenario_name" in df_all_points.columns:
            group_cols.insert(0, "scenario_name")
        if "hazard_return_period" in df_all_points.columns:
            group_cols.insert(1, "hazard_return_period")

        # Add any extra dimensions (like season)
        extra_dims = [c for c in df_all_points.columns if c not in metadata_cols]
        group_cols.extend(extra_dims)

        def q(p):
            return lambda x: float(np.nanpercentile(x, p))

        agg_slice = (
            df_all_points.groupby(group_cols, dropna=False)
            .agg(
                min=("value", "min"),
                max=("value", "max"),
                mean=("value", "mean"),
                median=("value", "median"),
                p2_5=("value", q(2.5)),
                p5=("value", q(5)),
                p10=("value", q(10)),
                p90=("value", q(90)),
                p95=("value", q(95)),
                p97_5=("value", q(97.5)),
            )
            .reset_index()
        )

        all_chunk_results.append(agg_slice)
        del df_all_points, agg_slice
        gc.collect()

    if coord_region_lookup is not None:
        del coord_region_lookup
    ds.close()

    if len(all_chunk_results) == 0:
        raise ValueError(f"No valid data in NetCDF after filtering: {nc_path}")

    agg = pd.concat(all_chunk_results, ignore_index=True)

    # Fill missing columns for consistency
    if "scenario_name" not in agg.columns:
        agg["scenario_name"] = np.nan
    if "hazard_return_period" not in agg.columns:
        agg["hazard_return_period"] = np.nan

    agg["adm_level"] = adm_level

    # Final column ordering
    base_cols = [
        "region",
        "adm_level",
        "scenario_name",
        "hazard_return_period",
        "hazard_type",
        "hazard_indicator",
        "min",
        "max",
        "mean",
        "median",
        "p2_5",
        "p5",
        "p10",
        "p90",
        "p95",
        "p97_5",
        "ensemble",
    ]
    extra_cols = [c for c in agg.columns if c not in base_cols]

    return agg[base_cols + extra_cols]


# ============================================================================
# INCREMENTAL PROCESSING HELPERS
# ============================================================================


def load_existing_precomputed(output_path):
    """
    Load existing precomputed file if it exists.

    Args:
        output_path: Path to precomputed CSV file

    Returns:
        pd.DataFrame: Existing data, or empty DataFrame if file doesn't exist
    """
    if not os.path.exists(output_path):
        print(f"  📄 No existing precomputed file found at {output_path}")
        return pd.DataFrame()

    print(f"  📄 Loading existing precomputed file: {output_path}")
    try:
        df = pd.read_csv(output_path, encoding="utf-8-sig", low_memory=False)
        print(f"  ✅ Loaded {len(df)} existing records")
        return df
    except Exception as e:
        print(f"  ⚠️  Error loading existing file: {e}")
        print(f"  📝 Will create new file")
        return pd.DataFrame()


def get_file_signature(file_path, file_type, adm_levels, ensemble_filter):
    """
    Get the signature of what combinations a NetCDF file would produce.

    Args:
        file_path: Path to NetCDF file
        file_type: Type of file (must be 'nc')
        adm_levels: List of ADM level names that would be processed
        ensemble_filter: Ensemble filter value

    Returns:
        list: List of dictionaries, each representing a combination that would be produced
    """
    hazard_type, hazard_indicator = parse_hazard_from_path(file_path)
    signatures = []

    # Only NC files are supported now
    if file_type != "nc":
        return []

    try:
        ds = xr.open_dataset(file_path)
        var_names = list(ds.data_vars.keys())
        if not var_names:
            ds.close()
            return []

        var_name = var_names[0]
        da = ds[var_name]

        # Get scenario dimension
        scenario_dim = None
        for dim in ["scenario", "GWL", "gwl"]:
            if dim in da.dims:
                scenario_dim = dim
                break

        if scenario_dim is None or "return_period" not in da.dims:
            ds.close()
            return []

        # Get unique values
        scenarios = da[scenario_dim].values
        return_periods = da["return_period"].values

        # Check for extra dimensions
        known_extra_dimensions = ["season"]
        extra_dims_present = [dim for dim in known_extra_dimensions if dim in da.dims]

        for scenario in scenarios:
            for rp in return_periods:
                for adm_level in adm_levels:
                    sig = {
                        "hazard_type": hazard_type,
                        "hazard_indicator": hazard_indicator,
                        "scenario_name": str(scenario),
                        "hazard_return_period": int(rp),
                        "adm_level": adm_level,
                        "ensemble": ensemble_filter,
                    }
                    for dim in extra_dims_present:
                        sig[dim] = np.nan
                    signatures.append(sig)

        ds.close()

    except Exception as e:
        print(f"      ⚠️  Error peeking into NetCDF file: {e}")
        return []

    return signatures


def is_already_processed(
    file_path,
    file_type,
    existing_df,
    adm_levels,
    ensemble_filter,
):
    """
    Check if a NetCDF file has already been processed based on existing precomputed data.

    Args:
        file_path: Path to NetCDF file
        file_type: Type of file (must be 'nc')
        existing_df: DataFrame with existing precomputed data
        adm_levels: List of ADM level names
        ensemble_filter: Ensemble filter value

    Returns:
        bool: True if file appears to be already processed, False otherwise
    """
    if len(existing_df) == 0:
        return False

    # Get signatures this file would produce
    signatures = get_file_signature(file_path, file_type, adm_levels, ensemble_filter)

    if len(signatures) == 0:
        # Can't determine signature, assume not processed
        return False

    # Convert signatures to DataFrame for comparison
    sigs_df = pd.DataFrame(signatures)

    if sigs_df.empty:
        return False

    existing_df_copy = existing_df.copy()

    def _normalize_scenario(value):
        """Normalize scenario names for comparison."""
        if pd.isna(value):
            return "__NA__"
        value_str = str(value).strip().lower()
        return value_str

    if "scenario_name" in sigs_df.columns:
        sigs_df["scenario_key"] = sigs_df["scenario_name"].apply(_normalize_scenario)
    else:
        sigs_df["scenario_key"] = "__NA__"

    if "scenario_name" in existing_df_copy.columns:
        existing_df_copy["scenario_key"] = existing_df_copy["scenario_name"].apply(
            _normalize_scenario
        )
    else:
        existing_df_copy["scenario_key"] = "__NA__"

    # Key columns to match on
    key_cols = [
        "hazard_type",
        "hazard_indicator",
        "scenario_name",
        "hazard_return_period",
        "adm_level",
    ]

    if "scenario_name" in key_cols:
        key_cols[key_cols.index("scenario_name")] = "scenario_key"

    # Check for extra dimensions that might exist in both
    extra_dims = ["season"]
    for dim in extra_dims:
        if dim in sigs_df.columns and dim in existing_df.columns:
            # Normalize the dimension to handle NaN values
            sigs_df[dim] = sigs_df[dim].fillna("__NA__").astype(str)
            existing_df_copy[dim] = existing_df_copy[dim].fillna("__NA__").astype(str)
            key_cols.append(dim)

    # Handle ensemble column separately (it might be NaN in existing data)
    sigs_df_copy = sigs_df.copy()
    if "ensemble" in sigs_df.columns:
        # For ensemble, we need to handle NaN values specially
        sigs_df_copy["ensemble"] = sigs_df_copy["ensemble"].fillna("__NA__").astype(str)
        if "ensemble" in existing_df.columns:
            existing_df_copy["ensemble"] = (
                existing_df_copy["ensemble"].fillna("__NA__").astype(str)
            )
            key_cols.append("ensemble")
        else:
            # Existing data doesn't have ensemble, but we're looking for it
            # This is a mismatch, so assume not processed
            return False
    else:
        existing_df_copy = existing_df.copy()
        # If existing has ensemble but sigs doesn't, that's OK - just don't include it in key_cols

    # Check if all signatures exist in existing data
    # Use merge to find matches
    merged = pd.merge(
        sigs_df_copy[key_cols],
        existing_df_copy[key_cols],
        on=key_cols,
        how="left",
        indicator=True,
    )

    # Check if all signatures were found
    all_found = (merged["_merge"] == "both").all()

    return all_found


# ============================================================================
# MAIN EXECUTION
# ============================================================================


def main():
    """Main execution function."""

    # ========================================================================
    # HARDCODED PARAMETERS
    # ========================================================================

    # Input paths
    HAZARDS_DIR = "workspace/demo_inputs_fullnc copy small/hazards"
    ADM1_PATH = "tests/tests_data/areas/state/geoBoundaries-BRA-ADM1.shp"
    ADM2_PATH = "tests/tests_data/areas/municipality/geoBoundaries-BRA-ADM2.shp"

    # Output path
    OUTPUT_PATH = (
        "workspace/Climate Data/Precomputed Regional Data/precomputed_adm_hazards.csv"
    )

    # Processing parameters
    ENSEMBLE_FILTER = "median"

    # Chunk size for spatial processing (larger = faster but more memory)
    # 200x200 reduces graph size and task overhead significantly
    # (~40,000 cells per chunk)
    CHUNK_SIZE_LAT = 5000
    CHUNK_SIZE_LON = 5000

    # ========================================================================
    # DASK CLUSTER & DASHBOARD
    # ========================================================================

    print("\n" + "=" * 60)
    print("STARTING DASK CLUSTER & DASHBOARD")
    print("=" * 60)

    # Detect number of CPUs
    num_cpus = os.cpu_count() or 4  # Fallback to 4 if detection fails
    print(f"  🔍 Detected {num_cpus} CPU(s)")

    # Start local Dask cluster with dashboard
    # Bind to 0.0.0.0 to allow external access (e.g., from Google Cloud VM)
    cluster = LocalCluster(
        n_workers=num_cpus,
        threads_per_worker=1,
        memory_limit="2GB",  # Limit per worker to prevent OOM
        dashboard_address="0.0.0.0:8787",  # Bind to all interfaces for cloud access
    )
    client = Client(cluster)

    print(f"  ✅ Dask cluster started with {len(cluster.workers)} workers")
    print(f"  📊 Dashboard available at: {client.dashboard_link}")
    print(f"  🌐 Local access: http://localhost:8787")
    print(f"  ☁️  Cloud VM access: http://<EXTERNAL_IP>:8787")
    print(f"     (Replace <EXTERNAL_IP> with your VM's external IP address)")
    print(f"  ⚠️  Note: Ensure firewall rule allows TCP port 8787")
    print(f"  Dask version: {dask.__version__}")
    print("\n  💡 TIP: Open the dashboard in your browser to see:")
    print("     - Real-time task progress")
    print("     - Memory usage per worker")
    print("     - Task graphs and computation flow")
    print("     - Worker CPU utilization")

    # ========================================================================
    # LOAD EXISTING PRECOMPUTED DATA
    # ========================================================================

    print("\n" + "=" * 60)
    print("CHECKING FOR EXISTING PRECOMPUTED DATA")
    print("=" * 60)
    existing_df = load_existing_precomputed(OUTPUT_PATH)

    # ========================================================================
    # LOAD ADMINISTRATIVE BOUNDARIES
    # ========================================================================

    print("\nLoading administrative boundaries...")
    adm_levels = [
        ("ADM1", ADM1_PATH),
        ("ADM2", ADM2_PATH),
    ]

    adm_gdfs = {}
    adm_level_names = []
    for adm_level, adm_path in adm_levels:
        print(f"  Loading {adm_level}: {adm_path}")
        adm_gdfs[adm_level] = load_adm_shapefile(adm_path)
        adm_level_names.append(adm_level)

    # ========================================================================
    # FIND ALL HAZARD FILES
    # ========================================================================

    print("\nSearching for hazard files...")

    # Find NetCDF files with ensemble_return_period in name
    nc_pattern = os.path.join(HAZARDS_DIR, "**", "*ensemble_return_period*.nc")
    nc_files = sorted(glob.glob(nc_pattern, recursive=True))
    print(f"  Found {len(nc_files)} NetCDF file(s)")

    if not nc_files:
        raise FileNotFoundError(f"No hazard files found in {HAZARDS_DIR}")

    # ========================================================================
    # FILTER FILES - SKIP ALREADY PROCESSED
    # ========================================================================

    print("\n" + "=" * 60)
    print("CHECKING WHICH FILES NEED PROCESSING")
    print("=" * 60)

    nc_files_to_process = []

    print(f"\n📁 Checking {len(nc_files)} NetCDF file(s)...")
    for nc_path in nc_files:
        try:
            hazard_type, hazard_indicator = parse_hazard_from_path(nc_path)
            file_label = (
                f"{hazard_type}/{hazard_indicator} - {os.path.basename(nc_path)}"
            )
        except:
            file_label = os.path.basename(nc_path)

        if is_already_processed(
            nc_path,
            "nc",
            existing_df,
            adm_level_names,
            ENSEMBLE_FILTER,
        ):
            print(f"  ⏭️  Skipping (already processed): {file_label}")
        else:
            print(f"  ✅ Will process: {file_label}")
            nc_files_to_process.append(nc_path)

    total_files_to_process = len(nc_files_to_process)
    if total_files_to_process == 0:
        print("\n" + "=" * 60)
        print("✅ ALL FILES ALREADY PROCESSED - NO WORK TO DO")
        print("=" * 60)
        if len(existing_df) > 0:
            print(f"\nExisting precomputed file has {len(existing_df)} records.")
            print(f"Output file: {OUTPUT_PATH}")
        return existing_df

    # ========================================================================
    # PROCESS NEW FILES
    # ========================================================================

    print("\n" + "=" * 60)
    print("STARTING FILE PROCESSING")
    print("=" * 60)
    print(f"Processing {total_files_to_process} new file(s)...")

    all_results = []

    print(f"\n📁 Processing {len(nc_files_to_process)} NetCDF file(s)...")
    for i, nc_path in enumerate(nc_files_to_process, 1):
        try:
            hazard_type, hazard_indicator = parse_hazard_from_path(nc_path)
            file_label = (
                f"{hazard_type}/{hazard_indicator} - {os.path.basename(nc_path)}"
            )
        except:
            file_label = os.path.basename(nc_path)

        print(f"\n[{i}/{len(nc_files_to_process)}] Processing NetCDF: {file_label}")
        file_start_time = time.time()

        for adm_level, adm_gdf in adm_gdfs.items():
            print(
                f"  📊 Aggregating {hazard_type}/{hazard_indicator} over {adm_level} ({len(adm_gdf)} regions)..."
            )
            adm_start_time = time.time()
            result = process_nc_hazard(
                nc_path,
                adm_gdf,
                adm_level,
                ENSEMBLE_FILTER,
                lat_chunk_size=CHUNK_SIZE_LAT,
                lon_chunk_size=CHUNK_SIZE_LON,
            )
            all_results.append(result)
            adm_elapsed = time.time() - adm_start_time
            print(
                f"    ✅ {adm_level}: {len(result)} records created in {adm_elapsed:.1f}s"
            )

            # Force garbage collection after each ADM level to free memory
            gc.collect()

        file_elapsed = time.time() - file_start_time
        print(f"  ⏱️  Total time for this file: {file_elapsed:.1f}s")

    # ========================================================================
    # COMBINE AND SAVE RESULTS
    # ========================================================================

    print("\n" + "=" * 60)
    print("COMBINING AND SAVING RESULTS")
    print("=" * 60)

    if len(all_results) == 0:
        print("\n⚠️  No new results to add")
        if len(existing_df) > 0:
            print(f"Existing precomputed file has {len(existing_df)} records.")
            print(f"Output file: {OUTPUT_PATH}")
        return existing_df

    print("\nCombining new results...")
    new_df = pd.concat(all_results, ignore_index=True)
    print(f"New records (before deduplication): {len(new_df)}")

    # Deduplicate new results (in case same file was processed multiple times)
    # Key columns that uniquely identify a record
    dedup_key_cols = [
        "region",
        "adm_level",
        "scenario_name",
        "hazard_return_period",
        "hazard_type",
        "hazard_indicator",
    ]
    # Add optional columns if they exist
    if "ensemble" in new_df.columns:
        dedup_key_cols.append("ensemble")
    if "season" in new_df.columns:
        dedup_key_cols.append("season")

    # Check for duplicates
    duplicates_before = new_df.duplicated(subset=dedup_key_cols, keep="first").sum()
    if duplicates_before > 0:
        print(f"  ⚠️  Found {duplicates_before} duplicate records in new results")
        print(f"  🧹 Removing duplicates (keeping first occurrence)...")
        new_df = new_df.drop_duplicates(subset=dedup_key_cols, keep="first")
        print(f"  ✅ After deduplication: {len(new_df)} records")
    else:
        print(f"  ✅ No duplicates found in new results")

    print(f"New records (after deduplication): {len(new_df)}")

    # Append to existing data if it exists
    if len(existing_df) > 0:
        print(f"Appending to existing {len(existing_df)} records...")
        # Ensure column order matches
        # Get all columns from both dataframes
        all_cols = list(set(existing_df.columns) | set(new_df.columns))

        # Reorder to match existing order if possible, then add any new columns
        existing_cols = list(existing_df.columns)
        new_cols = [col for col in all_cols if col not in existing_cols]
        ordered_cols = existing_cols + new_cols

        # Ensure both dataframes have the same columns (fill missing with NaN)
        for col in ordered_cols:
            if col not in existing_df.columns:
                existing_df[col] = np.nan
            if col not in new_df.columns:
                new_df[col] = np.nan

        # Remove records that are being recomputed to avoid duplicates
        key_columns = [
            col
            for col in [
                "region",
                "adm_level",
                "scenario_name",
                "hazard_return_period",
                "hazard_type",
                "hazard_indicator",
                "ensemble",
                "season",
            ]
            if col in existing_df.columns and col in new_df.columns
        ]

        if key_columns:
            print(
                f"  Filtering existing data using key columns: {', '.join(key_columns)}"
            )

            # Normalize ensemble column to string for merge compatibility
            for col in ["ensemble", "season"]:
                if col in key_columns:
                    if col in existing_df.columns:
                        existing_df[col] = existing_df[col].fillna("__NA__").astype(str)
                    if col in new_df.columns:
                        new_df[col] = new_df[col].fillna("__NA__").astype(str)

            new_keys = new_df[key_columns].drop_duplicates()
            marker_col = "_recomputed_match"
            existing_df = existing_df.merge(
                new_keys.assign(**{marker_col: 1}), on=key_columns, how="left"
            )
            replaced_rows = existing_df[marker_col].notna().sum()
            if replaced_rows > 0:
                print(f"  Removed {replaced_rows} existing record(s) being replaced")
            existing_df = existing_df[existing_df[marker_col].isna()].drop(
                columns=marker_col
            )
        else:
            print(
                "  ⚠️  No shared key columns found to filter existing data; appending all rows"
            )

        # Combine dataframes
        final_df = pd.concat(
            [existing_df[ordered_cols], new_df[ordered_cols]], ignore_index=True
        )
        print(
            f"Total records: {len(final_df)} (existing: {len(existing_df)}, new: {len(new_df)})"
        )
    else:
        final_df = new_df
        print(f"Total records: {len(final_df)} (all new)")

    # Final deduplication check before saving (safety net)
    print("\n🔍 Final deduplication check...")
    dedup_key_cols_final = [
        "region",
        "adm_level",
        "scenario_name",
        "hazard_return_period",
        "hazard_type",
        "hazard_indicator",
    ]
    # Add optional columns if they exist
    if "ensemble" in final_df.columns:
        dedup_key_cols_final.append("ensemble")
    if "season" in final_df.columns:
        dedup_key_cols_final.append("season")

    # Normalize ensemble and season for deduplication check
    final_df_check = final_df.copy()
    for col in ["ensemble", "season"]:
        if col in final_df_check.columns:
            final_df_check[col] = final_df_check[col].fillna("__NA__").astype(str)

    duplicates_final = final_df_check.duplicated(
        subset=dedup_key_cols_final, keep="first"
    ).sum()
    if duplicates_final > 0:
        print(f"  ⚠️  Found {duplicates_final} duplicate records in final dataset")
        print(f"  🧹 Removing duplicates (keeping first occurrence)...")

        # Use the normalized check dataframe to identify duplicates, then filter original
        keep_mask = ~final_df_check.duplicated(
            subset=dedup_key_cols_final, keep="first"
        )
        final_df = final_df[keep_mask].reset_index(drop=True)

        print(f"  ✅ After final deduplication: {len(final_df)} records")
    else:
        print(f"  ✅ No duplicates in final dataset")

    # Ensure output directory exists
    os.makedirs(os.path.dirname(OUTPUT_PATH), exist_ok=True)

    # Clean up __NA__ strings before saving (convert back to actual NaN)
    for col in ["ensemble", "season"]:
        if col in final_df.columns:
            final_df[col] = final_df[col].replace("__NA__", np.nan)

    # Save with UTF-8 encoding (with BOM for Excel compatibility)
    final_df.to_csv(OUTPUT_PATH, index=False, encoding="utf-8-sig")

    print(f"\n✅ Successfully saved results to:\n{OUTPUT_PATH}")
    print("\nFirst few rows:")
    print(final_df.head())

    # ========================================================================
    # CLEANUP - CLOSE DASK CLIENT
    # ========================================================================

    print("\n" + "=" * 60)
    print("CLOSING DASK CLUSTER")
    print("=" * 60)
    client.close()
    cluster.close()
    print("  ✅ Dask cluster and dashboard closed")

    return final_df


if __name__ == "__main__":
    main()
