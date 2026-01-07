"""
Precompute hazard statistics aggregated over administrative regions (ADM1/ADM2).

This version computes EXACT statistics (min/max/mean/median/percentiles) by:
  1) Using Dask workers to extract RAW values per region from each chunk (block).
  2) Returning those region cutouts to the driver.
  3) Concatenating per-region values on the driver and computing exact stats.

Notes:
- Mean is computed from raw values (but could be reduced via sum/count too).
- Quantiles are exact because they are computed from the full raw value set.
- Requires the NetCDF grid to be a regular lat/lon grid (1D lat, 1D lon).
"""

import os
import glob
import gc
import itertools
import time
import warnings
from typing import Dict, List, Tuple, Any

import numpy as np
import pandas as pd
import geopandas as gpd
import xarray as xr

import rasterio.features
from rasterio.transform import from_bounds

import dask
from dask import delayed
from dask.distributed import Client, LocalCluster
from unidecode import unidecode
from tqdm import tqdm


# ---------------------------------------------------------------------------
# WARNINGS
# ---------------------------------------------------------------------------

warnings.filterwarnings("ignore", message="Sending large graph")
warnings.filterwarnings(
    "ignore", message="The specified chunks separate the stored chunks"
)


# ---------------------------------------------------------------------------
# HELPERS
# ---------------------------------------------------------------------------


def parse_hazard_from_path(path: str) -> Tuple[str, str]:
    """
    Extract hazard_type and hazard_indicator from directory structure.

    Expected path format: .../hazards/HazardType/HazardIndicator/...

    Returns: (hazard_type, hazard_indicator)
    """
    parts = path.split(os.sep)
    if "hazards" not in parts:
        raise ValueError(
            f"Invalid path structure - 'hazards' directory not found: {path}"
        )
    i = parts.index("hazards")
    if i + 2 >= len(parts):
        raise ValueError(
            f"Invalid path structure - missing hazard type/indicator: {path}"
        )
    return parts[i + 1], parts[i + 2]


def fix_text(text):
    if not isinstance(text, str):
        return text
    return unidecode(text)


def load_adm_shapefile(adm_path: str) -> gpd.GeoDataFrame:
    if not os.path.exists(adm_path):
        raise FileNotFoundError(f"Shapefile not found: {adm_path}")

    try:
        gdf = gpd.read_file(adm_path, encoding="utf-8")
    except (UnicodeDecodeError, Exception):
        try:
            gdf = gpd.read_file(adm_path, encoding="latin1")
        except Exception:
            gdf = gpd.read_file(adm_path)

    if gdf.crs is None:
        raise ValueError(f"CRS not defined in shapefile: {adm_path}")

    if str(gdf.crs) != "EPSG:4326":
        gdf = gdf.to_crs("EPSG:4326")

    name_col = None
    for col in ["shapeName", "NAME_2", "NAME_1", "NAME", "name", "prov_name"]:
        if col in gdf.columns:
            name_col = col
            break
    if name_col is None:
        raise ValueError(f"No valid region name column found in shapefile: {adm_path}")

    gdf = gdf.copy()
    gdf["region"] = gdf[name_col].apply(fix_text)

    return gdf


def _is_regular_1d_grid(lat: np.ndarray, lon: np.ndarray, rtol=1e-5, atol=1e-8) -> bool:
    """
    Checks whether 1D lat and lon are regularly spaced.
    Accepts descending or ascending.
    """
    if lat.ndim != 1 or lon.ndim != 1:
        return False
    if len(lat) < 3 or len(lon) < 3:
        return True

    dlat = np.diff(lat)
    dlon = np.diff(lon)

    # allow descending too; compare absolute step sizes
    dlat_abs = np.abs(dlat)
    dlon_abs = np.abs(dlon)

    return np.allclose(dlat_abs, dlat_abs[0], rtol=rtol, atol=atol) and np.allclose(
        dlon_abs, dlon_abs[0], rtol=rtol, atol=atol
    )


def _grid_bounds(lat: np.ndarray, lon: np.ndarray) -> Tuple[float, float, float, float]:
    """
    Compute bounds for from_bounds.
    Works for ascending or descending lat/lon arrays (interpreted as cell centers).
    """
    lat_min, lat_max = float(np.min(lat)), float(np.max(lat))
    lon_min, lon_max = float(np.min(lon)), float(np.max(lon))
    return lon_min, lat_min, lon_max, lat_max


def prepare_region_shapes(adm_gdf: gpd.GeoDataFrame) -> Dict[str, Any]:
    """
    Prepare lightweight shapes package for rasterization. Uses integer region_id.
    """
    regions = adm_gdf["region"].tolist()
    region_name_to_id = {name: i + 1 for i, name in enumerate(regions)}
    region_id_to_name = {i + 1: name for i, name in enumerate(regions)}

    shapes = [
        (geom, region_name_to_id[name])
        for geom, name in zip(adm_gdf.geometry, adm_gdf["region"])
    ]

    bounds = adm_gdf.total_bounds  # (minx, miny, maxx, maxy)
    return {
        "shapes": shapes,  # list of (shapely geom, int id)
        "region_id_to_name": region_id_to_name,
        "bounds": bounds,
        "n_regions": len(regions),
    }


def compute_exact_stats(values: np.ndarray) -> Dict[str, float]:
    """
    Exact stats from raw values (NaNs already removed).
    """
    # Ensure float64 for stable stats
    v = values.astype(np.float64, copy=False)

    # exact via selection-based quantile implementation in numpy
    qs = np.quantile(v, [0.025, 0.05, 0.10, 0.50, 0.90, 0.95, 0.975], method="linear")

    return {
        "min": float(np.min(v)),
        "max": float(np.max(v)),
        "mean": float(np.mean(v)),
        "median": float(qs[3]),
        "p2_5": float(qs[0]),
        "p5": float(qs[1]),
        "p10": float(qs[2]),
        "p90": float(qs[4]),
        "p95": float(qs[5]),
        "p97_5": float(qs[6]),
    }


def build_missing_region_fallbacks_by_row_order(
    lats: np.ndarray,
    lons: np.ndarray,
    adm_gdf: gpd.GeoDataFrame,
) -> Dict[int, Tuple[int, int]]:
    """
    Fallback cell per region_id using the SAME region_id definition as rasterization:
      region_id = row order + 1
    This works even if region names are duplicated.
    """
    fallbacks: Dict[int, Tuple[int, int]] = {}

    for i, geom in enumerate(adm_gdf.geometry):
        rid = i + 1  # critical: consistent with prepare_region_shapes()
        c = geom.centroid
        ilat = int(np.argmin(np.abs(lats - c.y)))
        ilon = int(np.argmin(np.abs(lons - c.x)))
        fallbacks[rid] = (ilat, ilon)

    return fallbacks


# ---------------------------------------------------------------------------
# DASK BLOCK EXTRACTION TASK
# ---------------------------------------------------------------------------


def _block_extract_region_values(
    values_block: np.ndarray,
    lat_block: np.ndarray,
    lon_block: np.ndarray,
    shapes_pkg: Dict[str, Any],
    hazard_type: str,
) -> Dict[int, np.ndarray]:
    """
    Run on workers. Given a computed values block and its 1D lat/lon coordinate arrays,
    rasterize polygons onto the block grid and return raw values per region_id.

    Returns:
        dict {region_id: 1D np.ndarray of values (float32/float64), NaNs removed}
    """
    # values_block is 2D (lat, lon)
    if values_block.ndim != 2:
        raise ValueError(f"Expected 2D block, got shape {values_block.shape}")

    # nodata filtering example: Heat rule
    if hazard_type == "Heat":
        # Kelvin HI values should be > 300
        values_block = np.where(values_block > 300, values_block, np.nan)

    # fast skip if all nan
    if np.all(np.isnan(values_block)):
        return {}

    # Build transform for this block
    lon_min, lat_min, lon_max, lat_max = _grid_bounds(lat_block, lon_block)
    transform = from_bounds(
        lon_min, lat_min, lon_max, lat_max, len(lon_block), len(lat_block)
    )

    region_ids = rasterio.features.rasterize(
        shapes_pkg["shapes"],
        out_shape=(len(lat_block), len(lon_block)),
        transform=transform,
        fill=0,
        dtype=np.int32,
        all_touched=False,  # pixel center inside polygon
    )

    out: Dict[int, np.ndarray] = {}

    # extract per present region
    present = np.unique(region_ids)
    present = present[present > 0]
    if present.size == 0:
        return {}

    for rid in present:
        mask = region_ids == rid
        vals = values_block[mask]
        if vals.size == 0:
            continue
        vals = vals[~np.isnan(vals)]
        if vals.size == 0:
            continue
        out[int(rid)] = vals

    return out


# ---------------------------------------------------------------------------
# CORE PROCESSOR (EXACT)
# ---------------------------------------------------------------------------


def process_nc_hazard(
    nc_path: str,
    adm_gdf: gpd.GeoDataFrame,
    adm_level: str,
    ensemble_filter: str,
    client: Client,
    chunk_lat: int = 1024,
    chunk_lon: int = 1024,
) -> pd.DataFrame:
    """
    Exact zonal stats per region for one NetCDF file and one ADM level.
    """
    if not os.path.exists(nc_path):
        raise FileNotFoundError(f"NetCDF file not found: {nc_path}")

    hazard_type, hazard_indicator = parse_hazard_from_path(nc_path)

    # Open lazily. Let xarray choose chunking if present; otherwise we rechunk.
    ds = xr.open_dataset(nc_path, chunks={}, engine="netcdf4")
    try:
        var_names = list(ds.data_vars.keys())
        if not var_names:
            raise ValueError(f"No data variables found in NetCDF: {nc_path}")
        var_name = var_names[0]
        da = ds[var_name]

        if "lat" not in da.dims or "lon" not in da.dims:
            raise ValueError(f"Missing required coordinates (lat/lon) in {nc_path}")

        # Ensure 1D regular grid (required for rasterize-from-bounds logic)
        lats = da["lat"].values
        lons = da["lon"].values
        if not _is_regular_1d_grid(lats, lons):
            raise ValueError(
                f"Grid in {nc_path} is not a regular 1D lat/lon grid. "
                "Exact rasterization approach here assumes a regular grid."
            )

        # Ensemble selection
        has_ensemble = False
        ensemble_value_used = None
        if "ensemble" in da.dims:
            available = da["ensemble"].values.tolist()
            available_str = [str(x) for x in available]
            if ensemble_filter in available_str:
                da = da.sel(ensemble=available[available_str.index(ensemble_filter)])
                ensemble_value_used = ensemble_filter
            else:
                da = da.sel(ensemble=available[0])
                ensemble_value_used = str(available[0])
            has_ensemble = True

        # Rechunk to a reasonable block size for extraction tasks
        da = da.chunk({"lat": chunk_lat, "lon": chunk_lon})

        # Non-spatial dims
        non_spatial_dims = [d for d in da.dims if d not in ["lat", "lon"]]

        # Prepare shapes and scatter once
        shapes_pkg = prepare_region_shapes(adm_gdf)
        region_id_to_name = shapes_pkg["region_id_to_name"]
        region_name_to_id = {v: k for k, v in region_id_to_name.items()}

        shapes_future = client.scatter(shapes_pkg, broadcast=True)

        # Fallback indices for every region (in case of zero covered pixels)
        fallbacks = build_missing_region_fallbacks_by_row_order(
            lats, lons, adm_gdf, region_name_to_id
        )

        # Build list of all dimension combinations
        if non_spatial_dims:
            dim_values = {dim: da[dim].values for dim in non_spatial_dims}
            keys = list(dim_values.keys())
            dim_combinations = [
                dict(zip(keys, combo))
                for combo in itertools.product(*[dim_values[k] for k in keys])
            ]
        else:
            dim_combinations = [{}]

        print(
            f"    Processing {len(dim_combinations)} dimension combinations for {len(adm_gdf)} regions..."
        )

        all_rows: List[Dict[str, Any]] = []

        for combo_i, dim_combo in enumerate(
            tqdm(dim_combinations, desc="    Processing", leave=False, ncols=80), 1
        ):
            # Select slice lazily
            da_slice = da
            for dim_name, dim_val in dim_combo.items():
                da_slice = da_slice.sel({dim_name: dim_val})

            # Ensure 2D now
            da_slice = da_slice.transpose("lat", "lon")

            # Build delayed tasks per block
            # dask.array.Array has .to_delayed() blocks in same chunk grid.
            darr = da_slice.data
            if not isinstance(darr, dask.array.Array):
                # fallback: just compute the full array (small data)
                values_full = da_slice.values
                region_vals = _block_extract_region_values(
                    values_full, lats, lons, shapes_pkg, hazard_type
                )
                block_dicts = [region_vals]
            else:
                blocks = darr.to_delayed().ravel()

                # We need lat/lon coordinates for each block.
                # Because chunks are regular, we can compute block index -> lat/lon slices.
                lat_chunks = darr.chunks[0]
                lon_chunks = darr.chunks[1]

                # prefix sums for chunk boundaries
                lat_starts = np.cumsum((0,) + lat_chunks[:-1])
                lon_starts = np.cumsum((0,) + lon_chunks[:-1])

                n_lat_blocks = len(lat_chunks)
                n_lon_blocks = len(lon_chunks)

                tasks = []
                b = 0
                for bi in range(n_lat_blocks):
                    ls = int(lat_starts[bi])
                    le = ls + int(lat_chunks[bi])
                    lat_block = lats[ls:le]

                    for bj in range(n_lon_blocks):
                        cs = int(lon_starts[bj])
                        ce = cs + int(lon_chunks[bj])
                        lon_block = lons[cs:ce]

                        block_delayed = blocks[b]
                        b += 1

                        key_name = f"extract-{os.path.basename(nc_path)}-{adm_level}-{combo_i}-{bi}-{bj}"
                        t = delayed(
                            _block_extract_region_values,
                            name=key_name,
                            pure=False,
                        )(
                            block_delayed,
                            lat_block,
                            lon_block,
                            shapes_future,
                            hazard_type,
                        )
                        tasks.append(t)

                # Compute all block dicts in parallel
                block_dicts = list(dask.compute(*tasks))

            # Merge dicts into per-region lists of arrays on driver
            accum: Dict[int, List[np.ndarray]] = {}
            for dct in block_dicts:
                if not dct:
                    continue
                for rid, arr in dct.items():
                    accum.setdefault(rid, []).append(arr)

            # Ensure all regions appear (fallback if needed)
            for rid in region_id_to_name.keys():
                if rid in accum and sum(a.size for a in accum[rid]) > 0:
                    continue

                # fetch one fallback point from the slice
                ilat, ilon = fallbacks[rid]
                try:
                    v = da_slice.isel(lat=ilat, lon=ilon).compute().item()
                except Exception:
                    # if compute fails, skip
                    continue
                if v is None or (isinstance(v, float) and np.isnan(v)):
                    continue
                accum[rid] = [np.array([v], dtype=np.float64)]

            # Compute exact stats per region and emit rows
            for rid, arrays in accum.items():
                if not arrays:
                    continue
                vals = np.concatenate(arrays)
                vals = vals[~np.isnan(vals)]
                if vals.size == 0:
                    continue

                stats = compute_exact_stats(vals)

                row: Dict[str, Any] = {
                    "region": region_id_to_name[rid],
                    "adm_level": adm_level,
                    "hazard_type": hazard_type,
                    "hazard_indicator": hazard_indicator,
                    "ensemble": ensemble_value_used if has_ensemble else None,
                    **stats,
                }

                # Dimension combo into canonical names
                for dim_name, dim_val in dim_combo.items():
                    if dim_name == "GWL":
                        row["scenario_name"] = str(dim_val)
                    elif dim_name == "return_period":
                        row["hazard_return_period"] = int(dim_val)
                    else:
                        row[dim_name] = dim_val

                all_rows.append(row)

            # Reduce driver memory spikes
            del block_dicts, accum
            gc.collect()

        if not all_rows:
            raise ValueError(f"No valid data produced for {nc_path} ({adm_level})")

        df = pd.DataFrame(all_rows)

        # Ensure columns exist
        if "scenario_name" not in df.columns:
            df["scenario_name"] = np.nan
        if "hazard_return_period" not in df.columns:
            df["hazard_return_period"] = np.nan

        # Order columns
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
        extra_cols = [c for c in df.columns if c not in base_cols]
        return df[base_cols + extra_cols]

    finally:
        try:
            ds.close()
        except Exception:
            pass


# ---------------------------------------------------------------------------
# INCREMENTAL HELPERS (UNCHANGED LOGIC, MINIMAL)
# ---------------------------------------------------------------------------


def load_existing_precomputed(output_path: str) -> pd.DataFrame:
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


def get_file_signature(
    file_path: str, file_type: str, adm_levels: List[str], ensemble_filter: str
):
    hazard_type, hazard_indicator = parse_hazard_from_path(file_path)
    if file_type != "nc":
        return []

    try:
        ds = xr.open_dataset(file_path, engine="netcdf4")
        var_names = list(ds.data_vars.keys())
        if not var_names:
            ds.close()
            return []
        da = ds[var_names[0]]

        scenario_dim = None
        for dim in ["scenario", "GWL", "gwl"]:
            if dim in da.dims:
                scenario_dim = dim
                break

        if scenario_dim is None or "return_period" not in da.dims:
            ds.close()
            return []

        scenarios = da[scenario_dim].values
        rps = da["return_period"].values

        known_extra = ["season"]
        extra_present = [d for d in known_extra if d in da.dims]

        sigs = []
        for scn in scenarios:
            for rp in rps:
                for adm in adm_levels:
                    sig = {
                        "hazard_type": hazard_type,
                        "hazard_indicator": hazard_indicator,
                        "scenario_name": str(scn),
                        "hazard_return_period": int(rp),
                        "adm_level": adm,
                        "ensemble": ensemble_filter,
                    }
                    for d in extra_present:
                        sig[d] = np.nan
                    sigs.append(sig)

        ds.close()
        return sigs

    except Exception as e:
        print(f"      ⚠️  Error peeking into NetCDF file: {e}")
        return []


def is_already_processed(
    file_path: str,
    file_type: str,
    existing_df: pd.DataFrame,
    adm_levels: List[str],
    ensemble_filter: str,
) -> bool:
    if existing_df.empty:
        return False

    sigs = get_file_signature(file_path, file_type, adm_levels, ensemble_filter)
    if not sigs:
        return False

    sigs_df = pd.DataFrame(sigs)
    if sigs_df.empty:
        return False

    def _norm(x):
        if pd.isna(x):
            return "__NA__"
        return str(x).strip().lower()

    sigs_df["scenario_key"] = sigs_df["scenario_name"].apply(_norm)
    ex = existing_df.copy()
    ex["scenario_key"] = (
        ex["scenario_name"].apply(_norm) if "scenario_name" in ex.columns else "__NA__"
    )

    key_cols = [
        "hazard_type",
        "hazard_indicator",
        "scenario_key",
        "hazard_return_period",
        "adm_level",
    ]

    for dim in ["season"]:
        if dim in sigs_df.columns and dim in ex.columns:
            sigs_df[dim] = sigs_df[dim].fillna("__NA__").astype(str)
            ex[dim] = ex[dim].fillna("__NA__").astype(str)
            key_cols.append(dim)

    if "ensemble" in sigs_df.columns and "ensemble" in ex.columns:
        sigs_df["ensemble"] = sigs_df["ensemble"].fillna("__NA__").astype(str)
        ex["ensemble"] = ex["ensemble"].fillna("__NA__").astype(str)
        key_cols.append("ensemble")

    merged = pd.merge(
        sigs_df[key_cols],
        ex[key_cols],
        on=key_cols,
        how="left",
        indicator=True,
    )
    return (merged["_merge"] == "both").all()


# ---------------------------------------------------------------------------
# MAIN
# ---------------------------------------------------------------------------


def main():
    # Paths
    HAZARDS_DIR = "workspace/demo_inputs_fullnc/hazards"
    ADM1_PATH = "workspace/demo_inputs_fullnc/areas/state/geoBoundaries-BRA-ADM1.shp"
    ADM2_PATH = (
        "workspace/demo_inputs_fullnc/areas/municipality/geoBoundaries-BRA-ADM2.shp"
    )
    OUTPUT_PATH = (
        "workspace/Climate Data/Precomputed Regional Data/precomputed_adm_hazards.csv"
    )

    ENSEMBLE_FILTER = "median"

    # Chunk sizes for Dask extraction
    # Adjust depending on memory and region granularity
    CHUNK_LAT = 4096
    CHUNK_LON = 4096

    # -----------------------------------------------------------------------
    # DASK CLUSTER
    # -----------------------------------------------------------------------
    num_cpus = os.cpu_count() or 4
    print(f"  🔍 Detected {num_cpus} CPU(s)")

    cluster = LocalCluster(
        n_workers=num_cpus,
        threads_per_worker=1,
        memory_limit="2GB",
        dashboard_address="0.0.0.0:8787",
    )
    client = Client(cluster)

    try:
        print(f"  ✅ Dask cluster started with {len(cluster.workers)} workers")
        print(f"  📊 Dashboard: {client.dashboard_link}")

        # Existing data
        existing_df = load_existing_precomputed(OUTPUT_PATH)

        # Load boundaries
        adm_levels = [("ADM1", ADM1_PATH), ("ADM2", ADM2_PATH)]
        adm_gdfs = {}
        adm_level_names = []
        for level, path in adm_levels:
            print(f"  Loading {level}: {path}")
            adm_gdfs[level] = load_adm_shapefile(path)
            adm_level_names.append(level)

        # Find hazard files
        nc_pattern = os.path.join(HAZARDS_DIR, "**", "*ensemble_return_period*.nc")
        nc_files = sorted(glob.glob(nc_pattern, recursive=True))
        print(f"  Found {len(nc_files)} NetCDF file(s)")
        if not nc_files:
            raise FileNotFoundError(f"No hazard files found in {HAZARDS_DIR}")

        # Filter files
        nc_files_to_process = []
        for nc_path in nc_files:
            label = os.path.basename(nc_path)
            try:
                ht, hi = parse_hazard_from_path(nc_path)
                label = f"{ht}/{hi} - {label}"
            except Exception:
                pass

            if is_already_processed(
                nc_path, "nc", existing_df, adm_level_names, ENSEMBLE_FILTER
            ):
                print(f"  ⏭️  Skipping (already processed): {label}")
            else:
                print(f"  ✅ Will process: {label}")
                nc_files_to_process.append(nc_path)

        if not nc_files_to_process:
            print("✅ ALL FILES ALREADY PROCESSED")
            return existing_df

        all_results = []

        for i, nc_path in enumerate(nc_files_to_process, 1):
            try:
                ht, hi = parse_hazard_from_path(nc_path)
                label = f"{ht}/{hi} - {os.path.basename(nc_path)}"
            except Exception:
                label = os.path.basename(nc_path)

            print(f"\n[{i}/{len(nc_files_to_process)}] Processing NetCDF: {label}")
            file_start = time.time()

            for adm_level, adm_gdf in adm_gdfs.items():
                print(f"  📊 Aggregating over {adm_level} ({len(adm_gdf)} regions)...")
                t0 = time.time()

                df = process_nc_hazard(
                    nc_path=nc_path,
                    adm_gdf=adm_gdf,
                    adm_level=adm_level,
                    ensemble_filter=ENSEMBLE_FILTER,
                    client=client,
                    chunk_lat=CHUNK_LAT,
                    chunk_lon=CHUNK_LON,
                )

                all_results.append(df)
                print(
                    f"    ✅ {adm_level}: {len(df)} records created in {time.time() - t0:.1f}s"
                )
                gc.collect()

            print(f"  ⏱️  Total time for this file: {time.time() - file_start:.1f}s")

        # Combine new results
        new_df = pd.concat(all_results, ignore_index=True)

        # Deduplicate
        dedup_key = [
            "region",
            "adm_level",
            "scenario_name",
            "hazard_return_period",
            "hazard_type",
            "hazard_indicator",
        ]
        if "ensemble" in new_df.columns:
            dedup_key.append("ensemble")
        if "season" in new_df.columns:
            dedup_key.append("season")

        new_df = new_df.drop_duplicates(subset=dedup_key, keep="first")

        # Append / replace into existing
        if not existing_df.empty:
            # Align columns
            all_cols = list(set(existing_df.columns) | set(new_df.columns))
            for c in all_cols:
                if c not in existing_df.columns:
                    existing_df[c] = np.nan
                if c not in new_df.columns:
                    new_df[c] = np.nan

            key_cols = [
                c
                for c in [
                    "region",
                    "adm_level",
                    "scenario_name",
                    "hazard_return_period",
                    "hazard_type",
                    "hazard_indicator",
                    "ensemble",
                    "season",
                ]
                if c in existing_df.columns and c in new_df.columns
            ]

            if key_cols:
                ex = existing_df.copy()
                nd = new_df.copy()
                for c in ["ensemble", "season"]:
                    if c in key_cols:
                        ex[c] = ex[c].fillna("__NA__").astype(str)
                        nd[c] = nd[c].fillna("__NA__").astype(str)

                ex["_k"] = 1
                nd_keys = nd[key_cols].drop_duplicates()
                ex = ex.merge(nd_keys.assign(_repl=1), on=key_cols, how="left")
                ex = ex[ex["_repl"].isna()].drop(columns=["_repl"])
                ex = ex.drop(columns=["_k"], errors="ignore")

                final_df = pd.concat(
                    [ex[all_cols], new_df[all_cols]], ignore_index=True
                )
            else:
                final_df = pd.concat(
                    [existing_df[all_cols], new_df[all_cols]], ignore_index=True
                )
        else:
            final_df = new_df

        # Final dedup safety
        tmp = final_df.copy()
        for c in ["ensemble", "season"]:
            if c in tmp.columns:
                tmp[c] = tmp[c].fillna("__NA__").astype(str)
        final_df = final_df.loc[
            ~tmp.duplicated(subset=dedup_key, keep="first")
        ].reset_index(drop=True)

        os.makedirs(os.path.dirname(OUTPUT_PATH), exist_ok=True)
        final_df.to_csv(OUTPUT_PATH, index=False, encoding="utf-8-sig")
        print(f"\n✅ Saved results to: {OUTPUT_PATH}")
        print(final_df.head())

        return final_df

    finally:
        # Clean shutdown to avoid noisy worker-loss messages after exceptions
        try:
            client.close()
        except Exception:
            pass
        try:
            cluster.close()
        except Exception:
            pass


if __name__ == "__main__":
    main()
