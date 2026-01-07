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
import rasterio
import rasterio.features
from rasterio.transform import from_bounds
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


def build_region_id_raster(lats, lons, adm_gdf):
    """
    Build a region ID raster that maps every grid cell to a region integer ID.

    This rasterizes polygons once, then all subsequent operations can use
    array operations instead of spatial joins.

    Args:
        lats: Array of latitude coordinates (from NetCDF)
        lons: Array of longitude coordinates (from NetCDF)
        adm_gdf: GeoDataFrame with administrative boundaries and 'region' column

    Returns:
        tuple: (region_id_array, region_id_to_name_dict)
            - region_id_array: 2D numpy array (lat, lon) with integer region IDs
            - region_id_to_name_dict: dict mapping integer ID to region name
    """
    print(
        f"    🎨 Rasterizing {len(adm_gdf)} regions onto {len(lats)} × {len(lons)} grid..."
    )

    # Create mapping from region name to integer ID
    region_name_to_id = {name: idx + 1 for idx, name in enumerate(adm_gdf["region"])}
    region_id_to_name = {idx + 1: name for idx, name in enumerate(adm_gdf["region"])}

    # Add integer ID column to geodataframe
    adm_gdf_indexed = adm_gdf.copy()
    adm_gdf_indexed["region_id"] = adm_gdf_indexed["region"].map(region_name_to_id)

    # Calculate transform from lat/lon bounds
    # Note: lats may be descending or ascending
    lat_min, lat_max = float(np.min(lats)), float(np.max(lats))
    lon_min, lon_max = float(np.min(lons)), float(np.max(lons))

    # Create transform (maps pixel coords to geographic coords)
    transform = from_bounds(lon_min, lat_min, lon_max, lat_max, len(lons), len(lats))

    # Rasterize all polygons at once
    # shapes format: [(geometry, value), ...]
    shapes = [
        (geom, region_id)
        for geom, region_id in zip(
            adm_gdf_indexed.geometry, adm_gdf_indexed["region_id"]
        )
    ]

    # Rasterize (0 = no region, >0 = region ID)
    region_id_raster = rasterio.features.rasterize(
        shapes,
        out_shape=(len(lats), len(lons)),
        transform=transform,
        fill=0,  # Background value for pixels not in any region
        dtype=np.uint16,  # Support up to 65535 regions
        all_touched=False,  # Only pixels whose center is in polygon
    )

    # Check for regions with no pixels
    assigned_ids = set(np.unique(region_id_raster))
    assigned_ids.discard(0)  # Remove background
    all_ids = set(region_id_to_name.keys())
    missing_ids = all_ids - assigned_ids

    if missing_ids:
        print(
            f"    ⚠️  {len(missing_ids)} regions have no grid cell centers within their boundaries"
        )
        print(f"    📍 Assigning nearest grid cell to these regions...")

        # For each missing region, find nearest grid cell
        for region_id in missing_ids:
            region_name = region_id_to_name[region_id]
            region_geom = adm_gdf[adm_gdf["region"] == region_name].iloc[0].geometry

            # Get centroid
            centroid = region_geom.centroid
            centroid_lon, centroid_lat = centroid.x, centroid.y

            # Find nearest grid cell
            lat_diffs = np.abs(lats - centroid_lat)
            lon_diffs = np.abs(lons - centroid_lon)

            nearest_lat_idx = np.argmin(lat_diffs)
            nearest_lon_idx = np.argmin(lon_diffs)

            # Assign this grid cell to the region
            region_id_raster[nearest_lat_idx, nearest_lon_idx] = region_id

    assigned_after = len(set(np.unique(region_id_raster)) - {0})
    print(f"    ✅ Rasterized {assigned_after} regions onto grid")

    return region_id_raster, region_id_to_name


# ============================================================================
# FILE TYPE PROCESSORS
# ============================================================================


def compute_region_statistics_from_arrays(
    values_array, region_id_array, region_id_to_name
):
    """
    Compute statistics for each region using array operations (no pandas/point geometry).

    Args:
        values_array: 2D numpy array (lat, lon) with hazard values
        region_id_array: 2D numpy array (lat, lon) with region IDs
        region_id_to_name: dict mapping region ID to region name

    Returns:
        dict: {region_name: {min, max, mean, median, p2_5, ...}}
    """
    results = {}

    # Get unique region IDs (excluding 0 = background)
    unique_region_ids = np.unique(region_id_array)
    unique_region_ids = unique_region_ids[unique_region_ids > 0]

    for region_id in unique_region_ids:
        # Get all values for this region (where region_id_array matches)
        mask = region_id_array == region_id
        region_values = values_array[mask]

        # Remove NaN values
        region_values = region_values[~np.isnan(region_values)]

        if len(region_values) == 0:
            # No valid values for this region
            continue

        region_name = region_id_to_name[region_id]

        # Compute statistics using numpy (fast array operations)
        results[region_name] = {
            "min": float(np.min(region_values)),
            "max": float(np.max(region_values)),
            "mean": float(np.mean(region_values)),
            "median": float(np.percentile(region_values, 50)),
            "p2_5": float(np.percentile(region_values, 2.5)),
            "p5": float(np.percentile(region_values, 5)),
            "p10": float(np.percentile(region_values, 10)),
            "p90": float(np.percentile(region_values, 90)),
            "p95": float(np.percentile(region_values, 95)),
            "p97_5": float(np.percentile(region_values, 97.5)),
        }

    return results


def process_nc_hazard(
    nc_path,
    adm_gdf,
    adm_level,
    ensemble_filter,
    lat_chunk_size=2000,
    lon_chunk_size=2000,
):
    """
    Process NetCDF hazard file using rasterized region IDs and array operations.

    This avoids converting to DataFrame and uses efficient array-based zonal statistics.

    Args:
        nc_path: Path to NetCDF file
        adm_gdf: GeoDataFrame with administrative boundaries
        adm_level: Administrative level name (e.g., 'ADM1', 'ADM2')
        ensemble_filter: Ensemble value to filter for (e.g., 'median')
        lat_chunk_size: Chunk size for latitude (default 2000)
        lon_chunk_size: Chunk size for longitude (default 2000)

    Returns:
        pd.DataFrame: Aggregated statistics per region
    """
    if not os.path.exists(nc_path):
        raise FileNotFoundError(f"NetCDF file not found: {nc_path}")

    # Open with chunks for lazy loading
    ds = xr.open_dataset(nc_path, chunks={}, engine="netcdf4")

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

    # Build region ID raster ONCE for this file (reused for all dimension combos)
    lats = da.coords["lat"].values
    lons = da.coords["lon"].values
    region_id_raster, region_id_to_name = build_region_id_raster(lats, lons, adm_gdf)

    # Build list of all dimension combinations
    dim_combinations = []
    if non_spatial_dims:
        dim_values = {dim: da[dim].values for dim in non_spatial_dims}
        keys = list(dim_values.keys())
        for combo in itertools.product(*[dim_values[k] for k in keys]):
            dim_combinations.append(dict(zip(keys, combo)))
    else:
        dim_combinations = [{}]

    all_results = []

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

        # Apply nodata filtering
        if hazard_type == "Heat":
            # Heat Index values in Kelvin should be > 300 (27°C)
            da_slice = da_slice.where(da_slice > 300)

        # Load the 2D array into memory (or process in chunks if too large)
        # Check size to decide if we can load all at once
        n_lat = len(lats)
        n_lon = len(lons)
        total_cells = n_lat * n_lon

        # If small enough, compute entire slice at once
        if total_cells <= 10_000_000:  # ~10M cells = ~40MB for float32
            # Compute entire array at once
            if isinstance(da_slice.data, dask.array.Array):
                values_array = da_slice.compute().values
            else:
                values_array = da_slice.values

            # Compute statistics for all regions using array operations
            region_stats = compute_region_statistics_from_arrays(
                values_array, region_id_raster, region_id_to_name
            )

        else:
            # For very large arrays, process in spatial chunks and accumulate per-region values
            print(
                f"      Large array ({n_lat}×{n_lon}={total_cells:,} cells), processing in chunks..."
            )

            # Accumulate values per region across chunks
            region_values_accum = {
                region_name: [] for region_name in region_id_to_name.values()
            }

            n_lat_chunks = (n_lat + lat_chunk_size - 1) // lat_chunk_size
            n_lon_chunks = (n_lon + lon_chunk_size - 1) // lon_chunk_size
            total_chunks = n_lat_chunks * n_lon_chunks

            chunk_pbar = tqdm(
                total=total_chunks,
                desc="      Spatial chunks",
                leave=False,
                ncols=80,
                position=1,
            )

            for lat_start in range(0, n_lat, lat_chunk_size):
                lat_end = min(lat_start + lat_chunk_size, n_lat)
                for lon_start in range(0, n_lon, lon_chunk_size):
                    lon_end = min(lon_start + lon_chunk_size, n_lon)

                    # Select spatial chunk
                    da_chunk = da_slice.isel(
                        lat=slice(lat_start, lat_end), lon=slice(lon_start, lon_end)
                    )

                    # Compute this chunk
                    if isinstance(da_chunk.data, dask.array.Array):
                        values_chunk = da_chunk.compute().values
                    else:
                        values_chunk = da_chunk.values

                    # Get corresponding region ID chunk
                    region_id_chunk = region_id_raster[
                        lat_start:lat_end, lon_start:lon_end
                    ]

                    # Accumulate values per region
                    unique_ids = np.unique(region_id_chunk)
                    unique_ids = unique_ids[unique_ids > 0]

                    for region_id in unique_ids:
                        mask = region_id_chunk == region_id
                        region_vals = values_chunk[mask]
                        region_vals = region_vals[~np.isnan(region_vals)]

                        if len(region_vals) > 0:
                            region_name = region_id_to_name[region_id]
                            region_values_accum[region_name].append(region_vals)

                    chunk_pbar.update(1)
                    del values_chunk, region_id_chunk

            chunk_pbar.close()

            # Compute statistics from accumulated values
            region_stats = {}
            for region_name, vals_list in region_values_accum.items():
                if len(vals_list) == 0:
                    continue

                # Concatenate all chunks for this region
                all_vals = np.concatenate(vals_list)

                if len(all_vals) == 0:
                    continue

                region_stats[region_name] = {
                    "min": float(np.min(all_vals)),
                    "max": float(np.max(all_vals)),
                    "mean": float(np.mean(all_vals)),
                    "median": float(np.percentile(all_vals, 50)),
                    "p2_5": float(np.percentile(all_vals, 2.5)),
                    "p5": float(np.percentile(all_vals, 5)),
                    "p10": float(np.percentile(all_vals, 10)),
                    "p90": float(np.percentile(all_vals, 90)),
                    "p95": float(np.percentile(all_vals, 95)),
                    "p97_5": float(np.percentile(all_vals, 97.5)),
                }

            del region_values_accum

        # Convert to DataFrame rows
        for region_name, stats in region_stats.items():
            row = {
                "region": region_name,
                "adm_level": adm_level,
                "hazard_type": hazard_type,
                "hazard_indicator": hazard_indicator,
                "ensemble": ensemble_value_used if has_ensemble else None,
                **stats,
            }

            # Add dimension combo values
            for dim_name, dim_val in dim_combo.items():
                # Standardize dimension names
                if dim_name == "GWL":
                    row["scenario_name"] = str(dim_val)
                elif dim_name == "return_period":
                    row["hazard_return_period"] = int(dim_val)
                else:
                    row[dim_name] = dim_val

            all_results.append(row)

        gc.collect()

    ds.close()

    if len(all_results) == 0:
        raise ValueError(f"No valid data in NetCDF after filtering: {nc_path}")

    # Convert to DataFrame
    agg = pd.DataFrame(all_results)

    # Fill missing columns for consistency
    if "scenario_name" not in agg.columns:
        agg["scenario_name"] = np.nan
    if "hazard_return_period" not in agg.columns:
        agg["hazard_return_period"] = np.nan

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
        ds = xr.open_dataset(file_path, engine="netcdf4")
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
    HAZARDS_DIR = "workspace/demo_inputs_fullnc/hazards"
    ADM1_PATH = "workspace/demo_inputs_fullnc/areas/state/geoBoundaries-BRA-ADM1.shp"
    ADM2_PATH = (
        "workspace/demo_inputs_fullnc/areas/municipality/geoBoundaries-BRA-ADM2.shp"
    )

    # Output path
    OUTPUT_PATH = (
        "workspace/Climate Data/Precomputed Regional Data/precomputed_adm_hazards.csv"
    )

    # Processing parameters
    ENSEMBLE_FILTER = "median"

    # Chunk size for spatial processing
    # With the new rasterization approach, larger chunks are more efficient
    # 2000x2000 = ~4M cells per chunk (~16MB for float32)
    # Only used for very large arrays (>10M total cells)
    CHUNK_SIZE_LAT = 2000
    CHUNK_SIZE_LON = 2000

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
