"""
Precompute hazard statistics aggregated over administrative regions.

This script processes NetCDF (.nc), CSV (pre-converted NC), and GeoTIFF (.tif)
hazard files, extracting statistical summaries (min, max, mean, percentiles)
for each administrative region (ADM1 and ADM2 levels).

The output is a single CSV file combining all hazard types and regions.
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
from shapely.geometry import Point
from unidecode import unidecode


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
    Fix mojibake and encoding issues in text, converting accented characters.

    Args:
        text: Input string

    Returns:
        str: Cleaned text with accents converted to ASCII
    """
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

    gdf["region"] = gdf[name_col].apply(fix_text)

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

    df = pd.read_csv(csv_path, encoding="utf-8")

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

    # Find value column
    if "value" not in df.columns:
        possible_val = [
            c
            for c in df.columns
            if c
            not in [
                "GWL",
                "lon",
                "lat",
                "return_period",
                "ensemble",
                "hazard_type",
                "hazard_indicator",
            ]
        ]
        if not possible_val:
            raise ValueError(f"No value column found in {csv_path}")
        df = df.rename(columns={possible_val[0]: "value"})

    # Extract hazard info from path
    hazard_type, hazard_indicator = parse_hazard_from_path(csv_path)
    df["hazard_type"] = hazard_type
    df["hazard_indicator"] = hazard_indicator

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
            f"Spatial join failed - no points within regions for {csv_path}"
        )

    # Aggregate statistics per region
    def q(p):
        return lambda x: float(np.nanpercentile(x, p))

    agg = (
        joined.groupby(
            [
                "GWL",
                "return_period",
                "ensemble",
                "hazard_type",
                "hazard_indicator",
                "region",
            ],
            dropna=False,
        )
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

    return agg[cols]


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

    return agg[cols]


def process_tif_hazard(tif_path, adm_gdfs_dict, scenario_map):
    """
    Process GeoTIFF hazard file and aggregate over multiple ADM levels.

    Args:
        tif_path: Path to GeoTIFF file
        adm_gdfs_dict: Dictionary mapping ADM level names to GeoDataFrames
        scenario_map: Dictionary mapping scenario codes to names

    Returns:
        pd.DataFrame: Aggregated statistics per region for all ADM levels

    Raises:
        FileNotFoundError: If TIF file doesn't exist
        ValueError: If filename parsing fails or CRS issues
        RuntimeError: If raster masking fails
    """
    if not os.path.exists(tif_path):
        raise FileNotFoundError(f"GeoTIFF file not found: {tif_path}")

    # Parse filename: global_<scenario>_h<return_period>glob.tif
    basename = os.path.basename(tif_path)
    match = re.match(r"global_(\w+)_h(\d+)glob.*\.tif", basename)
    if not match:
        raise ValueError(
            f"Cannot parse scenario and return period from filename: {basename}"
        )

    scenario_code = match.group(1)
    return_period = int(match.group(2))

    # Map scenario code to name
    scenario_code_map = {"pc": "pc", "rcp26": "rcp26", "rcp85": "rcp85"}
    scenario_name = scenario_code_map.get(scenario_code, scenario_code)

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
                    "hazard_return_period": return_period,
                    "hazard_type": "FloodTIF",
                    "hazard_indicator": "Flood Height",
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
# MAIN EXECUTION
# ============================================================================


def main():
    """Main execution function."""

    # ========================================================================
    # HARDCODED PARAMETERS
    # ========================================================================

    # Input paths
    HAZARDS_DIR = "tests/tests_data/hazards"
    ADM1_PATH = "tests/tests_data/areas/province/geoBoundaries-BRA-ADM1.shp"
    ADM2_PATH = "tests/tests_data/areas/municipality/geoBoundaries-BRA-ADM2.shp"

    # Output path
    OUTPUT_PATH = (
        "workspace/Climate Data/Precomputed Regional Data/precomputed_adm_hazards.csv"
    )

    # Processing parameters
    ENSEMBLE_FILTER = "mean"
    FLOOD_SCENARIOS = {"pc": "CurrentClimate", "rcp26": "RCP2.6", "rcp85": "RCP8.5"}

    # ========================================================================
    # LOAD ADMINISTRATIVE BOUNDARIES
    # ========================================================================

    print("Loading administrative boundaries...")
    adm_levels = [
        ("ADM1", ADM1_PATH),
        ("ADM2", ADM2_PATH),
    ]

    adm_gdfs = {}
    for adm_level, adm_path in adm_levels:
        print(f"  Loading {adm_level}: {adm_path}")
        adm_gdfs[adm_level] = load_adm_shapefile(adm_path)

    # ========================================================================
    # FIND ALL HAZARD FILES
    # ========================================================================

    print("\nSearching for hazard files...")

    # Find NetCDF files
    nc_pattern = os.path.join(HAZARDS_DIR, "**", "*ensemble_return_period*.nc")
    nc_files = sorted(glob.glob(nc_pattern, recursive=True))
    print(f"  Found {len(nc_files)} NetCDF file(s)")

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
        if re.search(r"global_\w+_h\d+glob\.tif$", os.path.basename(f))
    ]
    print(f"  Found {len(tif_files)} GeoTIFF file(s)")

    if not nc_files and not csv_files and not tif_files:
        raise FileNotFoundError(f"No hazard files found in {HAZARDS_DIR}")

    # ========================================================================
    # PROCESS ALL FILES
    # ========================================================================

    all_results = []

    # Process CSV files
    for csv_path in csv_files:
        print(f"\nProcessing CSV: {os.path.basename(csv_path)}")
        for adm_level, adm_gdf in adm_gdfs.items():
            print(f"  Aggregating over {adm_level}...")
            result = process_csv_hazard(csv_path, adm_gdf, adm_level, ENSEMBLE_FILTER)
            all_results.append(result)

    # Process NetCDF files
    for nc_path in nc_files:
        print(f"\nProcessing NetCDF: {os.path.basename(nc_path)}")
        for adm_level, adm_gdf in adm_gdfs.items():
            print(f"  Aggregating over {adm_level}...")
            result = process_nc_hazard(nc_path, adm_gdf, adm_level, ENSEMBLE_FILTER)
            all_results.append(result)

    # Process GeoTIFF files (opens each file once for all ADM levels)
    for tif_path in tif_files:
        print(f"\nProcessing GeoTIFF: {os.path.basename(tif_path)}")
        print("  Aggregating over all ADM levels...")
        result = process_tif_hazard(tif_path, adm_gdfs, FLOOD_SCENARIOS)
        all_results.append(result)

    # ========================================================================
    # COMBINE AND SAVE RESULTS
    # ========================================================================

    print("\nCombining all results...")
    final_df = pd.concat(all_results, ignore_index=True)

    print(f"Total records: {len(final_df)}")

    # Ensure output directory exists
    os.makedirs(os.path.dirname(OUTPUT_PATH), exist_ok=True)

    # Save with UTF-8 encoding (with BOM for Excel compatibility)
    final_df.to_csv(OUTPUT_PATH, index=False, encoding="utf-8-sig")

    print(f"\n✅ Successfully saved results to:\n{OUTPUT_PATH}")
    print("\nFirst few rows:")
    print(final_df.head())

    return final_df


if __name__ == "__main__":
    main()
