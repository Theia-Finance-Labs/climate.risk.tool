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
import uuid
from typing import Dict, List, Tuple, Any

import numpy as np
import pandas as pd
import geopandas as gpd
import xarray as xr

import rasterio.features
from rasterio.transform import from_bounds

import dask.array as dask_da
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


def ensure_parent_dir(path: str) -> None:
    os.makedirs(os.path.dirname(path), exist_ok=True)


def append_df_to_csv(df: pd.DataFrame, path: str) -> None:
    ensure_parent_dir(path)
    header = not os.path.exists(path)
    df.to_csv(path, mode="a", header=header, index=False, encoding="utf-8-sig")


def make_tmp_path(base_dir: str, stem: str, suffix: str = ".csv") -> str:
    ensure_parent_dir(os.path.join(base_dir, "x"))
    return os.path.join(base_dir, f"{stem}.{uuid.uuid4().hex}{suffix}")


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


def rasterize_regions_full_grid(
    lats: np.ndarray, lons: np.ndarray, shapes_pkg: Dict[str, Any]
) -> np.ndarray:
    """
    One-time rasterization of all polygons onto the full NetCDF grid.
    """
    lon_min, lat_min, lon_max, lat_max = _grid_bounds(lats, lons)
    transform = from_bounds(lon_min, lat_min, lon_max, lat_max, len(lons), len(lats))

    region_ids = rasterio.features.rasterize(
        shapes_pkg["shapes"],
        out_shape=(len(lats), len(lons)),
        transform=transform,
        fill=0,
        dtype=np.int32,
        all_touched=True,
    )
    return region_ids


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


def choose_spatial_chunks(
    nlat: int,
    nlon: int,
    target_chunks_lat: int,
    target_chunks_lon: int,
    n_workers: int,
    min_blocks_per_worker: int = 4,
    min_chunk: int = 256,
) -> Tuple[int, int]:
    """
    Pick chunk_lat/chunk_lon <= target_* while ensuring enough total blocks.

    Goal: total_blocks >= n_workers * min_blocks_per_worker
    """
    # Start with preferred big chunks
    clat = min(target_chunks_lat, nlat)
    clon = min(target_chunks_lon, nlon)

    def nblocks(clat_, clon_):
        return int(np.ceil(nlat / clat_)) * int(np.ceil(nlon / clon_))

    needed = max(1, n_workers * min_blocks_per_worker)

    # If we don't have enough blocks, shrink chunks (down to min_chunk)
    while nblocks(clat, clon) < needed and (clat > min_chunk or clon > min_chunk):
        # shrink the dimension that gives the biggest increase in blocks
        if int(np.ceil(nlat / max(min_chunk, clat // 2))) * int(
            np.ceil(nlon / clon)
        ) >= int(np.ceil(nlat / clat)) * int(np.ceil(nlon / max(min_chunk, clon // 2))):
            clat = max(min_chunk, clat // 2)
        else:
            clon = max(min_chunk, clon // 2)

    return clat, clon


# ---------------------------------------------------------------------------
# DASK BLOCK EXTRACTION TASK
# ---------------------------------------------------------------------------


def _block_extract_region_values(
    values_block: np.ndarray,
    region_ids_block: np.ndarray,
    hazard_type: str,
) -> Dict[int, np.ndarray]:
    """
    Run on workers. Given a computed values block and a pre-rasterized block of region IDs,
    return raw values per region_id.

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

    # Vectorized group-by on region id (critical for many-polygons cases like ADM2)
    v = values_block.ravel()
    r = region_ids_block.ravel()

    m = (r > 0) & ~np.isnan(v)
    if not np.any(m):
        return {}

    v = v[m]
    r = r[m].astype(np.int32, copy=False)

    order = np.argsort(r, kind="mergesort")
    r = r[order]
    v = v[order]

    ids, idx, counts = np.unique(r, return_index=True, return_counts=True)

    out: Dict[int, np.ndarray] = {}
    for rid, start, cnt in zip(ids, idx, counts):
        # Copy so each region array is independent (avoids keeping one big base array alive)
        out[int(rid)] = v[start : start + cnt].copy()

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
) -> str:
    """
    Exact zonal stats per region for one NetCDF file and one ADM level.
    Returns path to a CSV containing results for this (nc_path, adm_level).
    """
    if not os.path.exists(nc_path):
        raise FileNotFoundError(f"NetCDF file not found: {nc_path}")

    hazard_type, hazard_indicator = parse_hazard_from_path(nc_path)

    # 1) Open metadata to get dimensions and worker count
    with xr.open_dataset(nc_path, engine="netcdf4") as ds_temp:
        nlat_full = ds_temp.dims["lat"]
        nlon_full = ds_temp.dims["lon"]

    n_workers = len(client.scheduler_info()["workers"])

    # 2) Choose effective chunks for this specific grid size
    chunk_lat_eff, chunk_lon_eff = choose_spatial_chunks(
        nlat=nlat_full,
        nlon=nlon_full,
        target_chunks_lat=chunk_lat,
        target_chunks_lon=chunk_lon,
        n_workers=n_workers,
    )

    # 3) Open lazily with explicit spatial chunking (reduces repeated rechunk graph construction).
    ds = xr.open_dataset(
        nc_path,
        chunks={"lat": chunk_lat_eff, "lon": chunk_lon_eff},
        engine="netcdf4",
    )
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

        # Ensure expected spatial chunking for extraction tasks
        da = da.chunk({"lat": chunk_lat_eff, "lon": chunk_lon_eff})

        # Non-spatial dims
        non_spatial_dims = [d for d in da.dims if d not in ["lat", "lon"]]

        # Prepare shapes
        shapes_pkg = prepare_region_shapes(adm_gdf)
        region_id_to_name = shapes_pkg["region_id_to_name"]

        # Pre-rasterize full grid once for this ADM level and this NetCDF grid
        print(
            f"    Pre-rasterizing regions for {adm_level} (chunks: {chunk_lat_eff}x{chunk_lon_eff})..."
        )
        region_id_map = rasterize_regions_full_grid(lats, lons, shapes_pkg)

        # Convert to dask array to easily get blocks matching the data chunking
        region_id_map_da = dask_da.from_array(
            region_id_map, chunks=(chunk_lat_eff, chunk_lon_eff)
        )
        region_id_blocks = region_id_map_da.to_delayed().ravel()

        # Fallback indices for every region_id (row-order IDs)
        fallbacks = build_missing_region_fallbacks_by_row_order(lats, lons, adm_gdf)
        # Build stable (rid -> fallback) arrays once (avoid per-rid compute storms)
        all_rids = np.array(sorted(region_id_to_name.keys()), dtype=np.int32)
        fb_lat = np.array([fallbacks[int(r)][0] for r in all_rids], dtype=np.int64)
        fb_lon = np.array([fallbacks[int(r)][1] for r in all_rids], dtype=np.int64)
        lat_idx = xr.DataArray(fb_lat, dims="rid")
        lon_idx = xr.DataArray(fb_lon, dims="rid")

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

        # Setup temp output path
        tmp_dir = os.path.join("workspace", "tmp_precompute")
        tmp_out = make_tmp_path(
            tmp_dir,
            stem=f"zonal_{adm_level}_{os.path.basename(nc_path)}".replace(".nc", ""),
        )

        for combo_i, dim_combo in enumerate(
            tqdm(dim_combinations, desc="    Processing", leave=False, ncols=80), 1
        ):
            combo_rows: List[Dict[str, Any]] = []

            # Select slice lazily
            da_slice = da
            for dim_name, dim_val in dim_combo.items():
                da_slice = da_slice.sel({dim_name: dim_val})

            # Ensure 2D now
            da_slice = da_slice.transpose("lat", "lon")

            # Build delayed tasks per block
            # dask.array.Array has .to_delayed() blocks in same chunk grid.
            darr = da_slice.data
            if not isinstance(darr, dask_da.Array):
                # fallback: just compute the full array (small data)
                values_full = da_slice.values
                region_vals = _block_extract_region_values(
                    values_full, region_id_map, hazard_type
                )
                block_dicts = [region_vals]
            else:
                blocks = darr.to_delayed().ravel()

                tasks = []
                for b in range(len(blocks)):
                    block_delayed = blocks[b]
                    rid_block_delayed = region_id_blocks[b]

                    key_name = (
                        f"extract-{os.path.basename(nc_path)}-{adm_level}-{combo_i}-{b}"
                    )
                    t = delayed(
                        _block_extract_region_values,
                        name=key_name,
                        pure=False,
                    )(
                        block_delayed,
                        rid_block_delayed,
                        hazard_type,
                    )
                    tasks.append(t)

                # Compute block dicts via the distributed scheduler
                futures = client.compute(tasks)

                # BATCHED GATHER to avoid driver spikes
                BATCH_SIZE = 32
                block_dicts = []
                for i in range(0, len(futures), BATCH_SIZE):
                    batch = client.gather(futures[i : i + BATCH_SIZE])
                    block_dicts.extend(batch)
                    del batch

            # Merge dicts into per-region lists of arrays on driver
            accum: Dict[int, List[np.ndarray]] = {}
            for dct in block_dicts:
                if not dct:
                    continue
                for rid, arr in dct.items():
                    accum.setdefault(rid, []).append(arr)

            # Ensure all regions appear (fallback if needed), but batch the sampling:
            # one vectorized point-sample compute per combo instead of thousands.
            try:
                fb_vals = da_slice.isel(lat=lat_idx, lon=lon_idx).compute().values
            except Exception:
                fb_vals = None

            if fb_vals is not None:
                for rid, v in zip(all_rids, fb_vals):
                    rid_int = int(rid)
                    if rid_int in accum and sum(a.size for a in accum[rid_int]) > 0:
                        continue
                    if v is None or (isinstance(v, float) and np.isnan(v)):
                        continue
                    accum[rid_int] = [np.array([v], dtype=np.float64)]

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

                combo_rows.append(row)

            # Flush this combo to disk and free memory
            if combo_rows:
                df_combo = pd.DataFrame(combo_rows)

                # Ensure these columns always exist (so append is consistent)
                if "scenario_name" not in df_combo.columns:
                    df_combo["scenario_name"] = np.nan
                if "hazard_return_period" not in df_combo.columns:
                    df_combo["hazard_return_period"] = np.nan

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
                for c in base_cols:
                    if c not in df_combo.columns:
                        df_combo[c] = np.nan
                extra_cols = [c for c in df_combo.columns if c not in base_cols]
                df_combo = df_combo[base_cols + extra_cols]

                append_df_to_csv(df_combo, tmp_out)
                del df_combo

            # Reduce driver memory spikes
            del combo_rows, block_dicts, accum
            gc.collect()

        if not os.path.exists(tmp_out):
            raise ValueError(f"No valid data produced for {nc_path} ({adm_level})")

        return tmp_out

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

                tmp_path = process_nc_hazard(
                    nc_path=nc_path,
                    adm_gdf=adm_gdf,
                    adm_level=adm_level,
                    ensemble_filter=ENSEMBLE_FILTER,
                    client=client,
                    chunk_lat=CHUNK_LAT,
                    chunk_lon=CHUNK_LON,
                )

                # load only this chunk of results
                new_df = pd.read_csv(tmp_path, encoding="utf-8-sig", low_memory=False)

                # (optional) delete tmp file to save disk
                try:
                    os.remove(tmp_path)
                except Exception:
                    pass

                # dedup within the chunk
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

                # merge with existing_df incrementally
                if existing_df.empty:
                    existing_df = new_df
                else:
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

                        nd_keys = nd[key_cols].drop_duplicates()
                        ex = ex.merge(nd_keys.assign(_repl=1), on=key_cols, how="left")
                        ex = ex[ex["_repl"].isna()].drop(
                            columns=["_repl"], errors="ignore"
                        )

                        existing_df = pd.concat(
                            [ex[all_cols], new_df[all_cols]], ignore_index=True
                        )
                    else:
                        existing_df = pd.concat(
                            [existing_df[all_cols], new_df[all_cols]], ignore_index=True
                        )

                # periodic write to disk so a crash doesn’t lose progress
                ensure_parent_dir(OUTPUT_PATH)
                existing_df.to_csv(OUTPUT_PATH, index=False, encoding="utf-8-sig")

                print(
                    f"    ✅ {adm_level}: {len(new_df)} records merged in {time.time() - t0:.1f}s"
                )
                del new_df
                gc.collect()

            print(f"  ⏱️  Total time for this file: {time.time() - file_start:.1f}s")

        print(f"\n✅ All processing complete. Final results saved to: {OUTPUT_PATH}")
        print(existing_df.head())

        return existing_df

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
