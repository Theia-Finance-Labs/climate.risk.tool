"""
Precompute hazard statistics aggregated over administrative regions.

This script processes NetCDF (.nc), CSV (pre-converted NC), and GeoTIFF (.tif)
hazard files, extracting statistical summaries (min, max, mean, percentiles)
for each administrative region (ADM1 and ADM2 levels).

The output is a single CSV file combining all hazard types and regions.

Hazard-specific dimensions (such as 'season' for drought hazards) are automatically
detected and preserved in the output. Hazards without these dimensions will have
NaN values for those columns, allowing flexible processing of mixed dimension sets.
"""

import os
import glob
import re
import numpy as np
import pandas as pd
import geopandas as gpd
import xarray as xr
import rasterio
import rasterio.mask
import rioxarray as rxr  # make sure: pip/conda install rioxarray
from shapely.geometry import Point
from unidecode import unidecode
from dask.distributed import Client, LocalCluster, progress
from dask.diagnostics import ProgressBar
import dask


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


def is_nc_cube_format(nc_path):
    """
    Detect if a NetCDF file is in cube format.

    Cube format files have multiple dimensions representing scenario and return_period
    directly in the structure (not as layer names like terra would parse).

    Args:
        nc_path: Path to NetCDF file

    Returns:
        bool: True if cube format, False otherwise
    """
    try:
        ds = xr.open_dataset(nc_path)

        # Get the data variable
        var_names = list(ds.data_vars.keys())
        if not var_names:
            print(f"      -> No data variables found")
            ds.close()
            return False

        var_name = var_names[0]
        da = ds[var_name]

        # Cube format check: has scenario AND return_period dimensions BUT NO ensemble dimension
        # Regular format: has ensemble, scenario/GWL, and return_period dimensions
        # Cube format: has scenario and return_period dimensions but NO ensemble dimension
        dims = list(da.dims)
        print(f"      -> Dimensions: {dims}")

        has_scenario = any(dim in dims for dim in ["scenario", "GWL", "gwl"])
        has_return_period = "return_period" in dims
        has_ensemble = "ensemble" in dims

        print(f"      -> Has scenario dim: {has_scenario}")
        print(f"      -> Has return_period dim: {has_return_period}")
        print(f"      -> Has ensemble dim: {has_ensemble}")

        # Cube format: has scenario AND return_period but NO ensemble
        # Regular format: has ensemble AND scenario/GWL AND return_period
        is_cube = has_scenario and has_return_period and not has_ensemble

        ds.close()
        return is_cube

    except Exception as e:
        print(f"      -> Error reading file: {e}")
        return False


# ============================================================================
# FILE TYPE PROCESSORS
# ============================================================================


def process_csv_hazard(csv_path, adm_gdf, adm_level, ensemble_filter):
    """
    Process CSV hazard file (pre-converted from NC) and aggregate over regions.

    Args:
        csv_path: Path to CSV file
        adm_gdf: GeoDataFrame with administrative boundaries
        adm_level: Administrative level name (e.g., 'ADM1', 'ADM2')
        ensemble_filter: Ensemble value to filter for (e.g., 'mean')

    Returns:
        pd.DataFrame: Aggregated statistics per region

    Raises:
        FileNotFoundError: If CSV file doesn't exist
        ValueError: If required columns are missing
        RuntimeError: If spatial join fails
    """
    if not os.path.exists(csv_path):
        raise FileNotFoundError(f"CSV file not found: {csv_path}")

    df = pd.read_csv(csv_path, encoding="utf-8", low_memory=False)

    # Filter to specified ensemble
    if "ensemble" in df.columns:
        df = df[df["ensemble"] == ensemble_filter]
        if len(df) == 0:
            raise ValueError(
                f"No data found for ensemble '{ensemble_filter}' in {csv_path}"
            )

    # Check for required columns
    required_cols = ["lon", "lat", "GWL", "return_period"]
    missing_cols = [col for col in required_cols if col not in df.columns]
    if missing_cols:
        raise ValueError(f"Missing required columns {missing_cols} in {csv_path}")

    # Extract hazard info from path
    hazard_type, hazard_indicator = parse_hazard_from_path(csv_path)
    df["hazard_type"] = hazard_type
    df["hazard_indicator"] = hazard_indicator

    # Find the value column - hardcoded known names
    # Standard column names: "value", "hazard_intensity", or the hazard_indicator name
    if "value" not in df.columns:
        if "hazard_intensity" in df.columns:
            df = df.rename(columns={"hazard_intensity": "value"})
        elif hazard_indicator in df.columns:
            df = df.rename(columns={hazard_indicator: "value"})
        else:
            raise ValueError(
                f"Could not find value column in {csv_path}. "
                f"Expected 'value', 'hazard_intensity', or '{hazard_indicator}'"
            )

    # Known extra grouping dimensions (hardcoded)
    # These are categorical dimensions that some hazards have but others don't
    known_extra_dimensions = ["season"]

    # Detect which extra dimensions are present in this file
    extra_groupby_cols = [col for col in known_extra_dimensions if col in df.columns]

    # Create point geometries
    gdf_pts = gpd.GeoDataFrame(
        df,
        geometry=gpd.points_from_xy(df["lon"], df["lat"]),
        crs="EPSG:4326",
    )

    # For CSV files, use nearest-neighbor matching for ALL regions
    # (CSV data is typically sparse, so we assign the closest grid point to each region's centroid)
    print(f"  📍 Using nearest-neighbor matching for all {len(adm_gdf)} regions...")

    # Project to a metric CRS (EPSG:3857 - Web Mercator) for accurate distance calculations
    gdf_pts_projected = gdf_pts.to_crs(epsg=3857)
    adm_gdf_projected = adm_gdf.to_crs(epsg=3857)

    # Get unique lat/lon combinations to find nearest point
    unique_coords = df[["lon", "lat"]].drop_duplicates()
    unique_coords_gdf = gpd.GeoDataFrame(
        unique_coords,
        geometry=gpd.points_from_xy(unique_coords["lon"], unique_coords["lat"]),
        crs="EPSG:4326",
    ).to_crs(epsg=3857)

    all_matches = []
    for idx, region_row in adm_gdf.iterrows():
        region_name = region_row["region"]
        region_geom_proj = adm_gdf_projected.loc[idx, "geometry"]
        region_centroid_proj = region_geom_proj.centroid

        # Calculate distance from region centroid to all unique grid points (in meters)
        distances = unique_coords_gdf.geometry.distance(region_centroid_proj)

        # Find nearest coordinate
        nearest_coord_idx = distances.idxmin()
        nearest_lon = unique_coords.loc[nearest_coord_idx, "lon"]
        nearest_lat = unique_coords.loc[nearest_coord_idx, "lat"]

        # Get ALL data rows for this nearest coordinate (all seasons, return periods, etc.)
        nearest_point_data = df[
            (df["lon"] == nearest_lon) & (df["lat"] == nearest_lat)
        ].copy()

        # Assign this region to all those rows
        nearest_point_data["region"] = region_name

        all_matches.append(nearest_point_data)

    # Convert all matches to dataframe
    joined = pd.concat(all_matches, ignore_index=True)
    print(f"  ✅ Assigned nearest grid point to all {len(joined)} region entries")

    # Aggregate statistics per region
    def q(p):
        return lambda x: float(np.nanpercentile(x, p))

    # Build groupby list including any extra categorical columns
    groupby_cols = [
        "GWL",
        "return_period",
        "ensemble",
        "hazard_type",
        "hazard_indicator",
        "region",
    ]
    # Add extra groupby columns if they exist in joined dataframe
    for col in extra_groupby_cols:
        if col in joined.columns:
            groupby_cols.append(col)

    # Select ONLY the columns we need (groupby cols + value) to avoid aggregating geometry/lat/lon
    cols_to_keep = groupby_cols + ["value"]
    joined_subset = joined[cols_to_keep]

    agg = (
        joined_subset.groupby(groupby_cols, dropna=False)
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

    # Format output
    agg = agg.rename(
        columns={
            "GWL": "scenario_name",
            "return_period": "hazard_return_period",
        }
    )
    agg["adm_level"] = adm_level

    # Build output columns list, including extra groupby columns
    cols = [
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
    # Add extra groupby columns to output if they exist
    for col in extra_groupby_cols:
        if col in agg.columns and col not in cols:
            cols.append(col)

    return agg[cols]


# def process_nc_cube_hazard(nc_path, adm_gdf, adm_level, ensemble_filter):
#     """
#     Process NetCDF cube format hazard file and aggregate over regions using Dask.

#     This function uses chunked, lazy loading with Dask to handle massive files
#     (e.g., 245GB+) without loading everything into memory. It processes each
#     region in parallel using multiple cores.

#     Args:
#         nc_path: Path to NetCDF cube file
#         adm_gdf: GeoDataFrame with administrative boundaries
#         adm_level: Administrative level name (e.g., 'ADM1', 'ADM2')
#         ensemble_filter: Ensemble value to filter for (e.g., 'mean')

#     Returns:
#         pd.DataFrame: Aggregated statistics per region

#     Raises:
#         FileNotFoundError: If NC file doesn't exist
#         ValueError: If required dimensions/variables are missing
#         RuntimeError: If spatial operations fail
#     """
#     if not os.path.exists(nc_path):
#         raise FileNotFoundError(f"NetCDF cube file not found: {nc_path}")

#     print(f"    🚀 Starting Dask cluster for parallel processing...")

#     # Set up Dask distributed processing
#     cluster = LocalCluster(
#         n_workers=8,  # Use 8 workers for parallel processing
#         threads_per_worker=2,  # 2 threads per worker
#         memory_limit="4GB",  # Limit memory per worker
#         processes=True,  # Use processes for true parallelism
#         silence_logs=True,  # Reduce log noise
#     )
#     client = Client(cluster)
#     print(f"    📊 Dask dashboard: {client.dashboard_link}")
#     print(f"    💻 Using {len(cluster.workers)} workers with {4} GB memory each")

#     try:
#         # Open with chunking - LAZY LOADING, doesn't load data yet
#         print(f"    📂 Opening NetCDF with chunked lazy loading...")
#         ds = xr.open_dataset(
#             nc_path, chunks={"lat": 1000, "lon": 1000}  # Adjust based on your RAM
#         )

#         # Get the data variable
#         var_names = list(ds.data_vars.keys())
#         if not var_names:
#             raise ValueError(f"No data variables found in NetCDF cube: {nc_path}")
#         var_name = var_names[0]
#         da = ds[var_name]

#         print(f"    📐 Data array dimensions: {da.dims}")
#         print(f"    📏 Data array shape: {da.shape}")
#         print(f"    🧩 Chunk sizes: {da.chunks}")

#         # Extract hazard info from path
#         hazard_type, hazard_indicator = parse_hazard_from_path(nc_path)

#         # Filter to specified ensemble if ensemble dimension exists
#         if "ensemble" in da.dims:
#             da = da.sel(ensemble=ensemble_filter)

#         # Get dimension names for scenarios and return periods
#         scenario_dim = None
#         for dim in ["scenario", "GWL", "gwl"]:
#             if dim in da.dims:
#                 scenario_dim = dim
#                 break

#         if scenario_dim is None:
#             raise ValueError(f"No scenario dimension found in {nc_path}")

#         # Get the actual dimension values
#         scenarios = da[scenario_dim].values
#         return_periods = da["return_period"].values

#         print(
#             f"    🌍 Processing {len(scenarios)} scenarios × {len(return_periods)} return periods × {len(adm_gdf)} regions"
#         )
#         print(f"    ⏱️  This will be processed in parallel using Dask...")

#         # Create delayed tasks for parallel processing
#         from dask import delayed

#         def process_region_scenario(region_row, scenario, rp):
#             """Process a single region-scenario-return_period combination"""
#             region_name = region_row["region"]
#             geom = region_row.geometry

#             # Get bounding box of geometry to reduce data
#             minx, miny, maxx, maxy = geom.bounds

#             # Clip to bounding box (still lazy)
#             da_slice = da.sel({scenario_dim: scenario, "return_period": rp})
#             da_bbox = da_slice.sel(
#                 lat=slice(maxy, miny),  # lat is usually descending
#                 lon=slice(minx, maxx),
#             )

#             # Check if any data in bbox
#             if da_bbox.sizes["lat"] == 0 or da_bbox.sizes["lon"] == 0:
#                 return None

#             # Create mask for this geometry
#             # Get lat/lon coordinates
#             lats = da_bbox.lat.values
#             lons = da_bbox.lon.values

#             # Create meshgrid
#             lon_grid, lat_grid = np.meshgrid(lons, lats)

#             # Flatten for point-in-polygon test
#             points = np.column_stack([lon_grid.ravel(), lat_grid.ravel()])

#             # Create GeoDataFrame of points for spatial join
#             from shapely.geometry import Point

#             point_geoms = [Point(x, y) for x, y in points]
#             points_gdf = gpd.GeoDataFrame(geometry=point_geoms, crs="EPSG:4326")

#             # Spatial join to find points within region
#             within_mask = points_gdf.within(geom)

#             if not within_mask.any():
#                 return None

#             # Reshape mask to match data shape
#             mask_2d = within_mask.values.reshape(len(lats), len(lons))

#             # Apply mask and compute statistics (THIS is where Dask loads only needed chunks)
#             da_masked = da_bbox.where(mask_2d)

#             # Compute statistics - this triggers Dask computation
#             values = da_masked.values.ravel()
#             values = values[~np.isnan(values)]

#             if len(values) == 0:
#                 return None

#             # Calculate statistics
#             stats = {
#                 "region": region_name,
#                 "adm_level": adm_level,
#                 "scenario_name": str(scenario),
#                 "hazard_return_period": int(rp),
#                 "hazard_type": hazard_type,
#                 "hazard_indicator": hazard_indicator,
#                 "min": float(np.min(values)),
#                 "max": float(np.max(values)),
#                 "mean": float(np.mean(values)),
#                 "median": float(np.percentile(values, 50)),
#                 "p2_5": float(np.percentile(values, 2.5)),
#                 "p5": float(np.percentile(values, 5)),
#                 "p10": float(np.percentile(values, 10)),
#                 "p90": float(np.percentile(values, 90)),
#                 "p95": float(np.percentile(values, 95)),
#                 "p97_5": float(np.percentile(values, 97.5)),
#                 "ensemble": ensemble_filter,
#             }

#             return stats

#         # Create all delayed tasks
#         print(
#             f"    🔄 Creating {len(scenarios) * len(return_periods) * len(adm_gdf)} parallel tasks..."
#         )
#         delayed_tasks = []

#         for scenario in scenarios:
#             for rp in return_periods:
#                 for idx, region_row in adm_gdf.iterrows():
#                     task = delayed(process_region_scenario)(region_row, scenario, rp)
#                     delayed_tasks.append(task)

#         # Execute all tasks in parallel with progress bar
#         print(f"    ⚡ Executing tasks in parallel...")
#         with ProgressBar():
#             results = dask.compute(*delayed_tasks)

#         # Filter out None results
#         all_results = [r for r in results if r is not None]

#         # Close dataset
#         ds.close()

#         if len(all_results) == 0:
#             raise RuntimeError(f"No valid data found for any region in {nc_path}")

#         # Convert to DataFrame
#         result_df = pd.DataFrame(all_results)

#         print(f"    ✅ Processed {len(result_df)} region records")

#         return result_df

#     finally:
#         # Always clean up Dask resources
#         print(f"    🧹 Cleaning up Dask cluster...")
#         client.close()
#         cluster.close()


def process_nc_hazard(nc_path, adm_gdf, adm_level, ensemble_filter):
    """
    Process NetCDF hazard file directly and aggregate over regions.

    Args:
        nc_path: Path to NetCDF file
        adm_gdf: GeoDataFrame with administrative boundaries
        adm_level: Administrative level name (e.g., 'ADM1', 'ADM2')
        ensemble_filter: Ensemble value to filter for (e.g., 'mean')

    Returns:
        pd.DataFrame: Aggregated statistics per region

    Raises:
        FileNotFoundError: If NC file doesn't exist
        ValueError: If required dimensions/variables are missing
        RuntimeError: If spatial operations fail
    """
    if not os.path.exists(nc_path):
        raise FileNotFoundError(f"NetCDF file not found: {nc_path}")

    ds = xr.open_dataset(nc_path)

    # Get the data variable
    var_names = list(ds.data_vars.keys())
    if not var_names:
        raise ValueError(f"No data variables found in NetCDF: {nc_path}")
    var_name = var_names[0]
    da = ds[var_name]

    # Filter to specified ensemble
    if "ensemble" in da.dims:
        da = da.sel(ensemble=ensemble_filter)

    # Convert to dataframe
    df = da.to_dataframe(name="value").reset_index().dropna(subset=["value"])

    if len(df) == 0:
        raise ValueError(f"No valid data in NetCDF after filtering: {nc_path}")

    # Check for required columns
    required_cols = ["lon", "lat"]
    missing_cols = [col for col in required_cols if col not in df.columns]
    if missing_cols:
        raise ValueError(f"Missing required coordinates {missing_cols} in {nc_path}")

    # Extract hazard info from path
    hazard_type, hazard_indicator = parse_hazard_from_path(nc_path)
    df["hazard_type"] = hazard_type
    df["hazard_indicator"] = hazard_indicator
    df["ensemble"] = ensemble_filter

    # Rename GWL and return_period columns if they exist
    if "GWL" in df.columns:
        df = df.rename(columns={"GWL": "scenario_name"})
    if "return_period" in df.columns:
        df = df.rename(columns={"return_period": "hazard_return_period"})

    # Detect additional categorical columns to include in groupby (e.g., "season")
    # Exclude standard columns, geometry, and spatial join artifacts
    metadata_cols = [
        "scenario_name",
        "lon",
        "lat",
        "hazard_return_period",
        "ensemble",
        "hazard_type",
        "hazard_indicator",
        "value",
        "geometry",
        "index_right",  # Added by spatial join
    ]
    extra_groupby_cols = [c for c in df.columns if c not in metadata_cols]

    # Create point geometries
    gdf_pts = gpd.GeoDataFrame(
        df,
        geometry=gpd.points_from_xy(df["lon"], df["lat"]),
        crs="EPSG:4326",
    )

    # Spatial join
    joined = gpd.sjoin(
        gdf_pts, adm_gdf[["region", "geometry"]], how="inner", predicate="within"
    )

    if len(joined) == 0:
        raise RuntimeError(
            f"Spatial join failed - no points within regions for {nc_path}"
        )

    # Aggregate statistics per region
    group_cols = ["hazard_type", "hazard_indicator", "region", "ensemble"]
    if "scenario_name" in joined.columns:
        group_cols.insert(0, "scenario_name")
    if "hazard_return_period" in joined.columns:
        group_cols.insert(1, "hazard_return_period")
    # Add extra groupby columns if they exist in joined dataframe
    for col in extra_groupby_cols:
        if col in joined.columns:
            group_cols.append(col)

    def q(p):
        return lambda x: float(np.nanpercentile(x, p))

    agg = (
        joined.groupby(group_cols, dropna=False)
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

    # Format output
    if "scenario_name" in agg.columns:
        pass  # scenario_name already exists
    else:
        agg["scenario_name"] = np.nan

    if "hazard_return_period" not in agg.columns:
        agg["hazard_return_period"] = np.nan

    agg["adm_level"] = adm_level

    # Build output columns list, including extra groupby columns
    cols = [
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
    # Add extra groupby columns to output if they exist
    for col in extra_groupby_cols:
        if col in agg.columns and col not in cols:
            cols.append(col)

    return agg[cols]


def process_tif_hazard(tif_path, adm_gdfs_dict, hazards_metadata):
    """
    Process GeoTIFF hazard file and aggregate over multiple ADM levels.

    Args:
        tif_path: Path to GeoTIFF file
        adm_gdfs_dict: Dictionary mapping ADM level names to GeoDataFrames
        hazards_metadata: DataFrame with hazard metadata (must include this file)

    Returns:
        pd.DataFrame: Aggregated statistics per region for all ADM levels

    Raises:
        FileNotFoundError: If TIF file doesn't exist
        ValueError: If filename parsing fails or CRS issues
        RuntimeError: If raster masking fails
    """
    if not os.path.exists(tif_path):
        raise FileNotFoundError(f"GeoTIFF file not found: {tif_path}")

    basename = os.path.basename(tif_path)
    metadata_row = hazards_metadata[hazards_metadata["hazard_file"] == basename]

    if metadata_row.empty:
        print(f"  ⏭️  Skipping GeoTIFF with no metadata entry: {basename}")
        return None

    metadata_row = metadata_row.iloc[0]
    hazard_type = str(metadata_row["hazard_type"]).strip()
    hazard_indicator = str(metadata_row["hazard_indicator"]).strip()
    scenario_name = str(metadata_row["scenario_name"]).strip()
    try:
        hazard_return_period = int(float(metadata_row["hazard_return_period"]))
    except (ValueError, TypeError):
        raise ValueError(
            f"Invalid hazard_return_period in metadata for {basename}: "
            f"{metadata_row['hazard_return_period']}"
        )

    all_results = []

    # Open TIF file once and process all ADM levels
    with rasterio.open(tif_path) as src:
        nodata = src.nodata

        if src.crs is None:
            raise ValueError(f"CRS not defined in GeoTIFF: {tif_path}")

        # Process each ADM level
        for adm_level, adm_gdf in adm_gdfs_dict.items():
            # Reproject ADM to raster CRS if needed
            if adm_gdf.crs != src.crs:
                gdf_proj = adm_gdf.to_crs(src.crs)
            else:
                gdf_proj = adm_gdf

            for idx, row in gdf_proj.iterrows():
                geom = [row.geometry]

                # Extract raster values within geometry
                try:
                    out_image, out_transform = rasterio.mask.mask(src, geom, crop=True)
                except Exception as e:
                    raise RuntimeError(
                        f"Failed to extract raster values for region '{row['region']}' "
                        f"from {tif_path}: {e}"
                    )

                data = out_image[0]

                # Mask nodata values (typically 0.0 for flood maps)
                if nodata is not None:
                    valid_mask = data != nodata
                else:
                    valid_mask = np.ones(data.shape, dtype=bool)

                valid = data[valid_mask]

                # Handle masked arrays
                if np.ma.isMaskedArray(valid):
                    valid = valid.compressed()

                # Compute statistics
                if valid.size == 0:
                    # No valid flood data - region has zero flood risk, return 0 for all stats
                    stats = {
                        "min": 0.0,
                        "max": 0.0,
                        "mean": 0.0,
                        "median": 0.0,
                        "p2_5": 0.0,
                        "p5": 0.0,
                        "p10": 0.0,
                        "p90": 0.0,
                        "p95": 0.0,
                        "p97_5": 0.0,
                    }
                else:
                    stats = compute_statistics(valid)

                # Get original region name from original CRS dataframe
                region_name = adm_gdf.loc[idx, "region"]

                result_row = {
                    "region": region_name,
                    "adm_level": adm_level,
                    "scenario_name": scenario_name,
                    "hazard_return_period": hazard_return_period,
                    "hazard_type": hazard_type,
                    "hazard_indicator": hazard_indicator,
                    "ensemble": np.nan,
                    **stats,
                }

                all_results.append(result_row)

    cols = [
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

    return pd.DataFrame(all_results, columns=cols)


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


def load_hazards_metadata(metadata_path):
    """
    Load hazard metadata describing scenario names and return periods.

    Args:
        metadata_path: Path to metadata CSV file

    Returns:
        pd.DataFrame: Metadata with normalized hazard_file column
    """
    if not os.path.exists(metadata_path):
        raise FileNotFoundError(f"Hazard metadata not found: {metadata_path}")

    metadata = pd.read_csv(metadata_path, encoding="utf-8")

    required_cols = {
        "hazard_file",
        "hazard_type",
        "hazard_indicator",
        "scenario_name",
        "hazard_return_period",
    }
    missing_cols = required_cols - set(metadata.columns)
    if missing_cols:
        raise ValueError(
            f"Hazard metadata is missing required columns: {', '.join(sorted(missing_cols))}"
        )

    metadata["hazard_file"] = metadata["hazard_file"].astype(str).str.strip()

    return metadata


def get_file_signature(
    file_path, file_type, adm_levels, ensemble_filter, hazards_metadata=None
):
    """
    Get the signature of what combinations a file would produce without processing it.

    This function peeks into the file to determine what combinations of
    (hazard_type, hazard_indicator, scenario_name, hazard_return_period, adm_level, ensemble, extra_dims)
    would be produced if the file were processed.

    Args:
        file_path: Path to hazard file
        file_type: Type of file ('csv', 'nc', 'tif')
        adm_levels: List of ADM level names that would be processed
        ensemble_filter: Ensemble filter value
        hazards_metadata: DataFrame with hazard metadata (required for TIF files)

    Returns:
        list: List of dictionaries, each representing a combination that would be produced
    """
    hazard_type, hazard_indicator = parse_hazard_from_path(file_path)
    signatures = []

    if file_type == "tif":
        basename = os.path.basename(file_path)
        metadata_row = None
        if hazards_metadata is not None:
            metadata_row = hazards_metadata[hazards_metadata["hazard_file"] == basename]

        if metadata_row is None or metadata_row.empty:
            print(f"  ⏭️  Skipping GeoTIFF with no metadata entry: {basename}")
            return []

        metadata_row = metadata_row.iloc[0]
        scenario_name = str(metadata_row["scenario_name"]).strip()
        hazard_type_from_meta = str(metadata_row["hazard_type"]).strip()
        hazard_indicator_from_meta = str(metadata_row["hazard_indicator"]).strip()
        try:
            return_period = int(float(metadata_row["hazard_return_period"]))
        except (ValueError, TypeError):
            raise ValueError(
                f"Invalid hazard_return_period in metadata for {basename}: "
                f"{metadata_row['hazard_return_period']}"
            )

        hazard_type = hazard_type_from_meta
        hazard_indicator = hazard_indicator_from_meta

        # TIF files produce one combination per ADM level
        for adm_level in adm_levels:
            signatures.append(
                {
                    "hazard_type": hazard_type,
                    "hazard_indicator": hazard_indicator,
                    "scenario_name": scenario_name,
                    "hazard_return_period": return_period,
                    "adm_level": adm_level,
                    "ensemble": np.nan,
                }
            )

        return signatures

    if file_type == "csv":
        # For CSV files, peek at unique combinations
        # Read only the key columns to be efficient
        try:
            # First, read just a sample to see what columns exist
            sample_df = pd.read_csv(
                file_path, encoding="utf-8", low_memory=False, nrows=10
            )

            # Check for required columns
            if (
                "GWL" not in sample_df.columns
                or "return_period" not in sample_df.columns
            ):
                return []

            # Determine which columns to read
            cols_to_read = ["GWL", "return_period"]
            if "ensemble" in sample_df.columns:
                cols_to_read.append("ensemble")

            # Check for extra dimensions
            known_extra_dimensions = ["season"]
            extra_dims_present = [
                col for col in known_extra_dimensions if col in sample_df.columns
            ]
            cols_to_read.extend(extra_dims_present)

            # Read only the columns we need (much faster than reading entire file)
            df = pd.read_csv(
                file_path, encoding="utf-8", low_memory=False, usecols=cols_to_read
            )

            # Filter to specified ensemble
            if "ensemble" in df.columns:
                df = df[df["ensemble"] == ensemble_filter]
                if len(df) == 0:
                    return []

            # Get unique combinations
            unique_combos = df[
                ["GWL", "return_period"] + extra_dims_present
            ].drop_duplicates()

            for _, row in unique_combos.iterrows():
                for adm_level in adm_levels:
                    sig = {
                        "hazard_type": hazard_type,
                        "hazard_indicator": hazard_indicator,
                        "scenario_name": str(row["GWL"]),
                        "hazard_return_period": int(row["return_period"]),
                        "adm_level": adm_level,
                        "ensemble": ensemble_filter,
                    }
                    # Add extra dimensions if present
                    for dim in extra_dims_present:
                        sig[dim] = row[dim] if pd.notna(row[dim]) else np.nan
                    signatures.append(sig)

        except Exception as e:
            print(f"      ⚠️  Error peeking into CSV file: {e}")
            return []

    elif file_type == "nc":
        # For NC files, peek at dimensions
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
            extra_dims_present = [
                dim for dim in known_extra_dimensions if dim in da.dims
            ]

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
                        # For extra dimensions, we'd need to iterate through all values
                        # For now, we'll mark them as potentially present
                        for dim in extra_dims_present:
                            sig[dim] = (
                                np.nan
                            )  # We'll check for any value in this dimension
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
    hazards_metadata=None,
):
    """
    Check if a file has already been processed based on existing precomputed data.

    Args:
        file_path: Path to hazard file
        file_type: Type of file ('csv', 'nc', 'tif')
        existing_df: DataFrame with existing precomputed data
        adm_levels: List of ADM level names
        ensemble_filter: Ensemble filter value
        hazards_metadata: DataFrame with hazard metadata (required for TIF files)

    Returns:
        bool: True if file appears to be already processed, False otherwise
    """
    if len(existing_df) == 0:
        return False

    # If GeoTIFF has no metadata, skip it entirely (treated as already processed)
    if (
        file_type == "tif"
        and hazards_metadata is not None
        and hazards_metadata[
            hazards_metadata["hazard_file"] == os.path.basename(file_path)
        ].empty
    ):
        print(
            f"  ⏭️  Skipping GeoTIFF with no metadata entry: {os.path.basename(file_path)}"
        )
        return True

    # Get signatures this file would produce
    signatures = get_file_signature(
        file_path, file_type, adm_levels, ensemble_filter, hazards_metadata
    )

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
    HAZARDS_DIR = "workspace/demo_inputs/hazards"
    ADM1_PATH = "tests/tests_data/areas/state/geoBoundaries-BRA-ADM1.shp"
    ADM2_PATH = "tests/tests_data/areas/municipality/geoBoundaries-BRA-ADM2.shp"

    # Output path
    OUTPUT_PATH = (
        "workspace/Climate Data/Precomputed Regional Data/precomputed_adm_hazards.csv"
    )

    # Processing parameters
    ENSEMBLE_FILTER = "mean"
    HAZARDS_METADATA_PATH = "workspace/demo_inputs/hazards_metadata.csv"

    print("\nLoading hazard metadata...")
    hazards_metadata = load_hazards_metadata(HAZARDS_METADATA_PATH)

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

    # Find NetCDF files (both regular and cube formats)
    nc_pattern = os.path.join(HAZARDS_DIR, "**", "*.nc")
    all_nc_files = sorted(glob.glob(nc_pattern, recursive=True))
    print(f"  Scanning {len(all_nc_files)} total NetCDF files for format detection...")

    # Separate NC files into cube format and regular format
    nc_cube_files = []
    nc_regular_files = []

    for nc_file in all_nc_files:
        print(f"    Checking: {os.path.basename(nc_file)}")
        print(f"      -> Full path: {nc_file}")

        # First check if it's a cube format by examining the file structure
        if is_nc_cube_format(nc_file):
            nc_cube_files.append(nc_file)
            print(f"      -> Detected as CUBE format")
        else:
            # For non-cube files, check naming pattern
            if "ensemble_return_period" in os.path.basename(nc_file):
                nc_regular_files.append(nc_file)
                print(f"      -> Detected as regular format")
            else:
                print(f"      -> Skipped (no ensemble_return_period in name)")

    print(f"  Found {len(nc_cube_files)} NetCDF cube file(s)")
    print(f"  Found {len(nc_regular_files)} regular NetCDF file(s)")

    # Find CSV files (pre-converted NC)
    csv_pattern = os.path.join(HAZARDS_DIR, "**", "*ensemble_return_period*.csv")
    csv_files = sorted(glob.glob(csv_pattern, recursive=True))
    print(f"  Found {len(csv_files)} CSV file(s)")

    # Find GeoTIFF files
    tif_pattern = os.path.join(HAZARDS_DIR, "**", "*.tif")
    tif_files = sorted(glob.glob(tif_pattern, recursive=True))
    # Filter to only flood hazard files matching naming pattern (exclude aggregated versions)
    tif_files = [
        f
        for f in tif_files
        if re.search(
            r"(?:global|flood)_\w+[_h]\d+[_h]?(?:_glob|glob)\.tif$", os.path.basename(f)
        )
    ]
    print(f"  Found {len(tif_files)} GeoTIFF file(s)")

    if not nc_cube_files and not nc_regular_files and not csv_files and not tif_files:
        raise FileNotFoundError(f"No hazard files found in {HAZARDS_DIR}")

    # ========================================================================
    # FILTER FILES - SKIP ALREADY PROCESSED
    # ========================================================================

    print("\n" + "=" * 60)
    print("CHECKING WHICH FILES NEED PROCESSING")
    print("=" * 60)

    # Filter files that are already processed
    tif_files_to_process = []
    csv_files_to_process = []
    nc_regular_files_to_process = []

    if tif_files:
        print(f"\n📁 Checking {len(tif_files)} GeoTIFF file(s)...")
        for tif_path in tif_files:
            if is_already_processed(
                tif_path,
                "tif",
                existing_df,
                adm_level_names,
                ENSEMBLE_FILTER,
                hazards_metadata,
            ):
                print(
                    f"  ⏭️  Skipping (already processed): {os.path.basename(tif_path)}"
                )
            else:
                print(f"  ✅ Will process: {os.path.basename(tif_path)}")
                tif_files_to_process.append(tif_path)

    if csv_files:
        print(f"\n📁 Checking {len(csv_files)} CSV file(s)...")
        for csv_path in csv_files:
            # Extract hazard info for better logging
            try:
                hazard_type, hazard_indicator = parse_hazard_from_path(csv_path)
                file_label = (
                    f"{hazard_type}/{hazard_indicator} - {os.path.basename(csv_path)}"
                )
            except:
                file_label = os.path.basename(csv_path)

            if is_already_processed(
                csv_path,
                "csv",
                existing_df,
                adm_level_names,
                ENSEMBLE_FILTER,
                hazards_metadata,
            ):
                print(f"  ⏭️  Skipping (already processed): {file_label}")
            else:
                print(f"  ✅ Will process: {file_label}")
                csv_files_to_process.append(csv_path)

    if nc_regular_files:
        print(f"\n📁 Checking {len(nc_regular_files)} regular NetCDF file(s)...")
        for nc_path in nc_regular_files:
            # Extract hazard info for better logging
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
                hazards_metadata,
            ):
                print(f"  ⏭️  Skipping (already processed): {file_label}")
            else:
                print(f"  ✅ Will process: {file_label}")
                nc_regular_files_to_process.append(nc_path)

    total_files_to_process = (
        len(tif_files_to_process)
        + len(csv_files_to_process)
        + len(nc_regular_files_to_process)
    )
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

    # Process GeoTIFF files (opens each file once for all ADM levels)
    if tif_files_to_process:
        print(f"\n📁 Processing {len(tif_files_to_process)} GeoTIFF file(s)...")
        for i, tif_path in enumerate(tif_files_to_process, 1):
            print(
                f"\n[{i}/{len(tif_files_to_process)}] Processing GeoTIFF: {os.path.basename(tif_path)}"
            )
            print("  📊 Aggregating over all ADM levels...")
            result = process_tif_hazard(tif_path, adm_gdfs, hazards_metadata)
            if result is None:
                print("    ⏭️  Skipped (missing metadata)")
                continue
            all_results.append(result)
            print(f"    ✅ Success: {len(result)} records")

    # Process CSV files
    if csv_files_to_process:
        print(f"\n📁 Processing {len(csv_files_to_process)} CSV file(s)...")
        for i, csv_path in enumerate(csv_files_to_process, 1):
            # Extract hazard info for better logging
            try:
                hazard_type, hazard_indicator = parse_hazard_from_path(csv_path)
                file_label = (
                    f"{hazard_type}/{hazard_indicator} - {os.path.basename(csv_path)}"
                )
            except:
                file_label = os.path.basename(csv_path)

            print(f"\n[{i}/{len(csv_files_to_process)}] Processing CSV: {file_label}")

            for adm_level, adm_gdf in adm_gdfs.items():
                print(
                    f"  📊 Aggregating {hazard_type}/{hazard_indicator} over {adm_level} ({len(adm_gdf)} regions)..."
                )

                result = process_csv_hazard(
                    csv_path, adm_gdf, adm_level, ENSEMBLE_FILTER
                )
                all_results.append(result)
                print(f"    ✅ {adm_level}: {len(result)} records created")

    # Process regular NetCDF files
    if nc_regular_files_to_process:
        print(
            f"\n📁 Processing {len(nc_regular_files_to_process)} regular NetCDF file(s)..."
        )
        for i, nc_path in enumerate(nc_regular_files_to_process, 1):
            # Extract hazard info for better logging
            try:
                hazard_type, hazard_indicator = parse_hazard_from_path(nc_path)
                file_label = (
                    f"{hazard_type}/{hazard_indicator} - {os.path.basename(nc_path)}"
                )
            except:
                file_label = os.path.basename(nc_path)

            print(
                f"\n[{i}/{len(nc_regular_files_to_process)}] Processing NetCDF: {file_label}"
            )

            for adm_level, adm_gdf in adm_gdfs.items():
                print(
                    f"  📊 Aggregating {hazard_type}/{hazard_indicator} over {adm_level} ({len(adm_gdf)} regions)..."
                )
                result = process_nc_hazard(nc_path, adm_gdf, adm_level, ENSEMBLE_FILTER)
                all_results.append(result)
                print(f"    ✅ {adm_level}: {len(result)} records created")

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
    print(f"New records: {len(new_df)}")

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

    return final_df


if __name__ == "__main__":
    main()
