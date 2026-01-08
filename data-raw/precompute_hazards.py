#!/usr/bin/env python3
"""
Precompute hazard statistics aggregated over administrative regions (ADM1/ADM2).

Approximate-quantile version (bounded memory, scalable):
  - Workers do per-block reductions per region:
      count, sum, min, max  (exact)
      bottom-k hash sample of pixel values (bounded) -> quantiles (approx)
  - Driver merges block results per region:
      count/sum/min/max merged exactly
      bottom-k samples merged deterministically
  - Quantiles computed from the deterministic sample.

Reproducibility:
  - Sample membership is determined by a deterministic hash of the GLOBAL pixel index
    + a fixed SEED. This makes results stable across runs and independent of chunking.

Output:
  - Writes one PART csv per (nc file, adm level) into OUTPUT_PARTS_DIR.
  - Creates a ".done" marker when a part is fully written.
  - At the end concatenates all parts into OUTPUT_PATH (streaming, no big memory).
"""

import os
import glob
import gc
import itertools
import time
import warnings
from typing import Dict, List, Tuple, Any, Optional

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
# CONFIG
# ---------------------------------------------------------------------------

# Deterministic sampling seed (change to change the sample)
SEED = 123456789  # int

# Max samples kept per region (memory ~ regions * K_SAMPLES)
K_SAMPLES = 1024  # 512/1024/2048 are common choices

# Quantiles requested
Q_LIST = np.array([0.025, 0.05, 0.10, 0.50, 0.90, 0.95, 0.975], dtype=np.float64)


# ---------------------------------------------------------------------------
# HELPERS: IO / TEXT
# ---------------------------------------------------------------------------


def parse_hazard_from_path(path: str) -> Tuple[str, str]:
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


# ---------------------------------------------------------------------------
# HELPERS: GRID / RASTERIZE
# ---------------------------------------------------------------------------


def _is_regular_1d_grid(lat: np.ndarray, lon: np.ndarray, rtol=1e-5, atol=1e-8) -> bool:
    if lat.ndim != 1 or lon.ndim != 1:
        return False
    if len(lat) < 3 or len(lon) < 3:
        return True

    dlat = np.diff(lat)
    dlon = np.diff(lon)

    dlat_abs = np.abs(dlat)
    dlon_abs = np.abs(dlon)

    return np.allclose(dlat_abs, dlat_abs[0], rtol=rtol, atol=atol) and np.allclose(
        dlon_abs, dlon_abs[0], rtol=rtol, atol=atol
    )


def _grid_bounds(lat: np.ndarray, lon: np.ndarray) -> Tuple[float, float, float, float]:
    lat_min, lat_max = float(np.min(lat)), float(np.max(lat))
    lon_min, lon_max = float(np.min(lon)), float(np.max(lon))
    return lon_min, lat_min, lon_max, lat_max


def prepare_region_shapes(adm_gdf: gpd.GeoDataFrame) -> Dict[str, Any]:
    # IMPORTANT: row-order IDs (1..N). Do not key on name (names can repeat).
    regions = adm_gdf["region"].tolist()
    region_id_to_name = {i + 1: name for i, name in enumerate(regions)}

    shapes = [(geom, i + 1) for i, geom in enumerate(adm_gdf.geometry)]

    bounds = adm_gdf.total_bounds
    return {
        "shapes": shapes,
        "region_id_to_name": region_id_to_name,
        "bounds": bounds,
        "n_regions": len(regions),
    }


# ---------------------------------------------------------------------------
# HELPERS: RASTERIZE PER BLOCK (avoid materializing full-grid region_id_map)
# ---------------------------------------------------------------------------


def rasterize_regions_block(
    lats_block: np.ndarray,
    lons_block: np.ndarray,
    shapes: List[Tuple[Any, int]],
    all_touched: bool = True,
) -> np.ndarray:
    """
    Rasterize region IDs for a single (lat, lon) block only.
    This avoids allocating a full-grid region_id_map which can OOM on very large rasters.
    """
    lon_min, lat_min, lon_max, lat_max = _grid_bounds(lats_block, lons_block)
    transform = from_bounds(
        lon_min, lat_min, lon_max, lat_max, len(lons_block), len(lats_block)
    )

    return rasterio.features.rasterize(
        shapes,
        out_shape=(len(lats_block), len(lons_block)),
        transform=transform,
        fill=0,
        dtype=np.int32,
        all_touched=all_touched,
    )


def _block_rasterize_only(
    lats_block: np.ndarray,
    lons_block: np.ndarray,
    shapes: List[Tuple[Any, int]],
    all_touched: bool = True,
) -> np.ndarray:
    """
    Rasterize-only worker helper.

    Keeping rasterio/GDAL scoped to this call helps reduce unmanaged/native memory
    growth in long-running distributed workers.
    """
    from rasterio.env import Env

    # Limit GDAL cache (in MB) to reduce native allocator growth/fragmentation.
    with Env(GDAL_CACHEMAX=128):
        return rasterize_regions_block(
            lats_block=lats_block,
            lons_block=lons_block,
            shapes=shapes,
            all_touched=all_touched,
        )


# ---------------------------------------------------------------------------
# CHUNKING
# ---------------------------------------------------------------------------


def choose_spatial_chunks(
    nlat: int,
    nlon: int,
    target_chunks_lat: int,
    target_chunks_lon: int,
    n_workers: int,
) -> Tuple[int, int]:
    """
    HARD-CAPPED spatial chunking.
    Never exceeds target sizes. No auto growth/shrinking.
    """
    _ = n_workers  # intentionally unused; kept for call-site simplicity
    return min(target_chunks_lat, nlat), min(target_chunks_lon, nlon)


# ---------------------------------------------------------------------------
# DETERMINISTIC HASH (splitmix64) FOR BOTTOM-K
# ---------------------------------------------------------------------------


def _splitmix64(x: np.ndarray) -> np.ndarray:
    """
    Vectorized splitmix64 for uint64 arrays. Deterministic across runs.
    """
    x = (x + np.uint64(0x9E3779B97F4A7C15)) & np.uint64(0xFFFFFFFFFFFFFFFF)
    z = x
    z = (z ^ (z >> np.uint64(30))) * np.uint64(0xBF58476D1CE4E5B9) & np.uint64(
        0xFFFFFFFFFFFFFFFF
    )
    z = (z ^ (z >> np.uint64(27))) * np.uint64(0x94D049BB133111EB) & np.uint64(
        0xFFFFFFFFFFFFFFFF
    )
    z = z ^ (z >> np.uint64(31))
    return z


def merge_bottomk(
    h1: Optional[np.ndarray],
    v1: Optional[np.ndarray],
    h2: np.ndarray,
    v2: np.ndarray,
    k: int,
) -> Tuple[np.ndarray, np.ndarray]:
    """
    Merge two bottom-k sets that are already sorted by hash ascending.
    Deterministic merge independent of task order.
    """
    if h1 is None or v1 is None or h1.size == 0:
        return h2[:k].copy(), v2[:k].copy()
    if h2.size == 0:
        return h1[:k].copy(), v1[:k].copy()

    i = j = 0
    out_h = []
    out_v = []
    while len(out_h) < k and (i < h1.size or j < h2.size):
        if j >= h2.size or (i < h1.size and h1[i] <= h2[j]):
            out_h.append(h1[i])
            out_v.append(v1[i])
            i += 1
        else:
            out_h.append(h2[j])
            out_v.append(v2[j])
            j += 1

    return np.array(out_h, dtype=np.uint64), np.array(out_v, dtype=np.float64)


# ---------------------------------------------------------------------------
# WORKER TASK: PER-BLOCK AGGREGATION + BOTTOM-K SAMPLE
# ---------------------------------------------------------------------------


def _block_reduce_bottomk(
    values_block: np.ndarray,
    region_ids_block: np.ndarray,
    hazard_type: str,
    start_lat: int,
    start_lon: int,
    nlon_full: int,
    seed: int,
    k: int,
    nodata_value: Optional[float] = None,
) -> Dict[int, Tuple[int, float, float, float, np.ndarray, np.ndarray]]:
    """
    Returns dict:
      rid -> (count, sum, min, max, sample_hashes_sorted, sample_values_sorted)
    """
    if values_block.ndim != 2:
        raise ValueError(f"Expected 2D block, got shape {values_block.shape}")

    # Hazard-specific nodata filtering
    if hazard_type == "Heat":
        values_block = np.where(values_block > 300, values_block, np.nan)

    # Flatten
    h, w = values_block.shape
    v = values_block.ravel()
    r = region_ids_block.ravel()

    # Valid mask:
    # - floating: drop NaNs
    # - integer/categorical: keep all pixels unless a nodata_value is provided
    if np.issubdtype(v.dtype, np.floating):
        m = (r > 0) & ~np.isnan(v)
    else:
        if nodata_value is None:
            m = r > 0
        else:
            # compare in native dtype to avoid float casting edge cases
            m = (r > 0) & (v != np.asarray(nodata_value, dtype=v.dtype))

    if not np.any(m):
        return {}

    v = v[m].astype(np.float64, copy=False)
    r = r[m].astype(np.int32, copy=False)

    # global pixel index for deterministic hashing
    # local flat index -> (ilat, ilon)
    flat_idx = np.nonzero(m)[0].astype(
        np.int64, copy=False
    )  # indices in full ravel order
    ilat = flat_idx // w
    ilon = flat_idx - ilat * w
    glat = ilat + int(start_lat)
    glon = ilon + int(start_lon)
    gidx = glat.astype(np.uint64) * np.uint64(nlon_full) + glon.astype(np.uint64)

    seed_u = np.uint64(seed)
    hh = _splitmix64(gidx ^ seed_u)

    # Sort by (rid asc, hash asc) so first k per rid is bottom-k
    order = np.lexsort((hh, r))  # primary r, secondary hh
    r = r[order]
    v = v[order]
    hh = hh[order]

    # Group boundaries
    ids, idx, counts = np.unique(r, return_index=True, return_counts=True)

    out: Dict[int, Tuple[int, float, float, float, np.ndarray, np.ndarray]] = {}

    # Exact reductions per rid
    # Sum: reduceat
    sums = np.add.reduceat(v, idx)
    mins = np.minimum.reduceat(v, idx)
    maxs = np.maximum.reduceat(v, idx)

    for rid, start, cnt, s, mn, mx in zip(ids, idx, counts, sums, mins, maxs):
        rid_int = int(rid)
        take = int(min(k, cnt))
        sh = hh[start : start + take].copy()
        sv = v[start : start + take].copy()
        # Already sorted by hash within rid due to lexsort
        out[rid_int] = (int(cnt), float(s), float(mn), float(mx), sh, sv)

    return out


def _block_reduce_with_rasterize_bottomk(
    values_block: np.ndarray,
    lats_block: np.ndarray,
    lons_block: np.ndarray,
    shapes: List[Tuple[Any, int]],
    hazard_type: str,
    start_lat: int,
    start_lon: int,
    nlon_full: int,
    seed: int,
    k: int,
    nodata_value: Optional[float] = None,
) -> Dict[int, Tuple[int, float, float, float, np.ndarray, np.ndarray]]:
    """
    Worker helper: rasterize region IDs for *this block only* and then run reductions.
    """
    region_ids_block = rasterize_regions_block(
        lats_block=lats_block,
        lons_block=lons_block,
        shapes=shapes,
        all_touched=True,
    )
    return _block_reduce_bottomk(
        values_block=values_block,
        region_ids_block=region_ids_block,
        hazard_type=hazard_type,
        start_lat=start_lat,
        start_lon=start_lon,
        nlon_full=nlon_full,
        seed=seed,
        k=k,
        nodata_value=nodata_value,
    )


# ---------------------------------------------------------------------------
# PROCESS ONE (NetCDF, ADM_LEVEL) -> PART CSV
# ---------------------------------------------------------------------------


def process_nc_hazard_bottomk(
    nc_path: str,
    adm_gdf: gpd.GeoDataFrame,
    adm_level: str,
    ensemble_filter: str,
    client: Client,
    out_part_csv: str,
    target_chunk_lat: int,
    target_chunk_lon: int,
    seed: int,
    k_samples: int,
) -> None:
    """
    Writes results directly to out_part_csv (appending combo by combo).
    Creates the file from scratch (overwrites if exists).
    """
    hazard_type, hazard_indicator = parse_hazard_from_path(nc_path)

    # Open metadata for dims
    with xr.open_dataset(nc_path, engine="netcdf4") as ds_temp:
        nlat_full = int(ds_temp.sizes["lat"])
        nlon_full = int(ds_temp.sizes["lon"])

    n_workers = len(client.scheduler_info()["workers"])
    chunk_lat, chunk_lon = choose_spatial_chunks(
        nlat=nlat_full,
        nlon=nlon_full,
        target_chunks_lat=target_chunk_lat,
        target_chunks_lon=target_chunk_lon,
        n_workers=n_workers,
    )

    ds = xr.open_dataset(
        nc_path,
        chunks={"lat": chunk_lat, "lon": chunk_lon},
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

        lats = da["lat"].values
        lons = da["lon"].values
        if not _is_regular_1d_grid(lats, lons):
            raise ValueError(f"Grid in {nc_path} is not a regular 1D lat/lon grid.")

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

        # Ensure chunking
        da = da.chunk({"lat": chunk_lat, "lon": chunk_lon})

        # Non-spatial dims to iterate
        non_spatial_dims = [d for d in da.dims if d not in ["lat", "lon"]]
        if non_spatial_dims:
            dim_values = {dim: da[dim].values for dim in non_spatial_dims}
            keys = list(dim_values.keys())
            dim_combinations = [
                dict(zip(keys, combo))
                for combo in itertools.product(*[dim_values[k] for k in keys])
            ]
        else:
            dim_combinations = [{}]

        # Rasterize regions once
        shapes_pkg = prepare_region_shapes(adm_gdf)
        region_id_to_name = shapes_pkg["region_id_to_name"]
        shapes = shapes_pkg["shapes"]
        # Broadcast once; avoids repeatedly serializing geometries into every task
        shapes_fut = client.scatter(shapes, broadcast=True)

        print(
            f"    Rasterizing regions per block for {adm_level} (chunks: {chunk_lat}x{chunk_lon})..."
        )

        # Prepare output file fresh
        ensure_parent_dir(out_part_csv)
        if os.path.exists(out_part_csv):
            os.remove(out_part_csv)

        header_written = False

        print(
            f"    Processing {len(dim_combinations)} dimension combinations for {len(adm_gdf)} regions..."
        )

        # -------------------------------------------------------------------
        # Precompute region-id rasters ONCE per block for this ADM level
        # (reused for all dimension combinations; avoids rasterize in hot loop)
        # -------------------------------------------------------------------
        # Keep region-id blocks on workers as Futures (do NOT gather to driver).
        rid_futures: Dict[Tuple[int, int], Any] = {}

        # Build a cheap template Dask array so we know the block grid shape.
        # If non-spatial dims exist, take the first element so we end up 2D.
        template_da = da
        for dim in non_spatial_dims:
            template_da = template_da.isel({dim: 0})
        template_data = template_da.transpose("lat", "lon").data

        # Only precompute per-block region rasters if the hazard data is actually chunked
        # as a Dask array. (If it's already an in-memory NumPy array, we'll use the
        # single-block path below.)
        if isinstance(template_data, dask_da.Array):
            template_blocks = template_data.to_delayed()

            for bi in range(template_blocks.shape[0]):
                for bj in range(template_blocks.shape[1]):
                    start_lat = bi * chunk_lat
                    start_lon = bj * chunk_lon
                    end_lat = min(start_lat + chunk_lat, nlat_full)
                    end_lon = min(start_lon + chunk_lon, nlon_full)

                    lats_block = lats[start_lat:end_lat]
                    lons_block = lons[start_lon:end_lon]

                    # Submit directly so the scattered shapes are a true dependency
                    rid_futures[(bi, bj)] = client.submit(
                        _block_rasterize_only,
                        lats_block,
                        lons_block,
                        shapes_fut,
                        True,
                        pure=False,
                    )

        # Small-array fast path: one full-grid rasterization, reused for combos
        region_ids_full: Optional[np.ndarray] = None

        for combo_i, dim_combo in enumerate(
            tqdm(dim_combinations, desc="    Processing", leave=False, ncols=80), 1
        ):
            da_slice = da
            for dim_name, dim_val in dim_combo.items():
                da_slice = da_slice.sel({dim_name: dim_val})
            da_slice = da_slice.transpose("lat", "lon")

            darr = da_slice.data

            # best-effort nodata detection (useful for integer/categorical rasters)
            nodata_value = None
            try:
                if "_FillValue" in da_slice.attrs:
                    nodata_value = da_slice.attrs["_FillValue"]
                elif hasattr(da_slice, "encoding") and isinstance(
                    da_slice.encoding, dict
                ):
                    nodata_value = da_slice.encoding.get("_FillValue", None)
            except Exception:
                nodata_value = None

            if not isinstance(darr, dask_da.Array):
                # small array: compute in one go as a single block
                values_full = da_slice.values
                if region_ids_full is None:
                    region_ids_full = _block_rasterize_only(
                        lats_block=lats,
                        lons_block=lons,
                        shapes=shapes,
                        all_touched=True,
                    )

                block_res = _block_reduce_bottomk(
                    values_block=values_full,
                    region_ids_block=region_ids_full,
                    hazard_type=hazard_type,
                    start_lat=0,
                    start_lon=0,
                    nlon_full=nlon_full,
                    seed=seed,
                    k=k_samples,
                    nodata_value=nodata_value,
                )
                block_dicts = [block_res]
            else:
                blocks = darr.to_delayed()  # 2D grid

                # Defensive: if template-based precompute didn't run, precompute rid futures
                # from the actual block grid shape (still keep them on workers).
                if not rid_futures:
                    for bi in range(blocks.shape[0]):
                        for bj in range(blocks.shape[1]):
                            start_lat = bi * chunk_lat
                            start_lon = bj * chunk_lon
                            end_lat = min(start_lat + chunk_lat, nlat_full)
                            end_lon = min(start_lon + chunk_lon, nlon_full)

                            lats_block = lats[start_lat:end_lat]
                            lons_block = lons[start_lon:end_lon]

                            rid_futures[(bi, bj)] = client.submit(
                                _block_rasterize_only,
                                lats_block,
                                lons_block,
                                shapes_fut,
                                True,
                                pure=False,
                            )

                # Turn hazard blocks into Futures (data stays on workers)
                val_keys: List[Tuple[int, int]] = []
                val_delayed: List[Any] = []
                for bi in range(blocks.shape[0]):
                    for bj in range(blocks.shape[1]):
                        val_keys.append((bi, bj))
                        val_delayed.append(blocks[bi, bj])

                val_futures_list = client.compute(val_delayed)
                val_futures = {k: f for k, f in zip(val_keys, val_futures_list)}

                # Submit reductions: each task depends on (values_future, rid_future)
                reduce_futures = []
                for bi in range(blocks.shape[0]):
                    for bj in range(blocks.shape[1]):
                        start_lat = bi * chunk_lat
                        start_lon = bj * chunk_lon

                        reduce_futures.append(
                            client.submit(
                                _block_reduce_bottomk,
                                val_futures[(bi, bj)],
                                rid_futures[(bi, bj)],
                                hazard_type,
                                start_lat,
                                start_lon,
                                nlon_full,
                                seed,
                                k_samples,
                                nodata_value,
                                pure=False,
                            )
                        )

                block_dicts = client.gather(reduce_futures)

                # Release per-combo value/reduce futures promptly to avoid buildup.
                try:
                    client.cancel(list(val_futures.values()))
                    client.cancel(reduce_futures)
                except Exception:
                    pass

            # Driver merge per rid with bounded bottom-k
            # accum: rid -> (count,sum,min,max, hashes, values)
            accum_count: Dict[int, int] = {}
            accum_sum: Dict[int, float] = {}
            accum_min: Dict[int, float] = {}
            accum_max: Dict[int, float] = {}
            accum_h: Dict[int, np.ndarray] = {}
            accum_v: Dict[int, np.ndarray] = {}

            for dct in block_dicts:
                if not dct:
                    continue
                for rid, (cnt, s, mn, mx, sh, sv) in dct.items():
                    if rid not in accum_count:
                        accum_count[rid] = cnt
                        accum_sum[rid] = s
                        accum_min[rid] = mn
                        accum_max[rid] = mx
                        accum_h[rid] = sh
                        accum_v[rid] = sv
                    else:
                        accum_count[rid] += cnt
                        accum_sum[rid] += s
                        accum_min[rid] = min(accum_min[rid], mn)
                        accum_max[rid] = max(accum_max[rid], mx)
                        mh, mv = merge_bottomk(
                            accum_h[rid], accum_v[rid], sh, sv, k_samples
                        )
                        accum_h[rid] = mh
                        accum_v[rid] = mv

            # Emit rows: include all regions (even if zero pixels) so shapes are complete
            rows: List[Dict[str, Any]] = []
            for rid, name in region_id_to_name.items():
                cnt = accum_count.get(rid, 0)
                if cnt == 0:
                    row = {
                        "region": name,
                        "adm_level": adm_level,
                        "scenario_name": np.nan,
                        "hazard_return_period": np.nan,
                        "hazard_type": hazard_type,
                        "hazard_indicator": hazard_indicator,
                        "count": 0,
                        "min": np.nan,
                        "max": np.nan,
                        "mean": np.nan,
                        "median": np.nan,
                        "p2_5": np.nan,
                        "p5": np.nan,
                        "p10": np.nan,
                        "p90": np.nan,
                        "p95": np.nan,
                        "p97_5": np.nan,
                        "ensemble": ensemble_value_used if has_ensemble else None,
                    }
                else:
                    mean = accum_sum[rid] / float(cnt)
                    mn = accum_min[rid]
                    mx = accum_max[rid]
                    sample_vals = accum_v[rid]
                    # approximate quantiles from deterministic sample
                    qs = np.quantile(sample_vals, Q_LIST, method="linear")
                    row = {
                        "region": name,
                        "adm_level": adm_level,
                        "scenario_name": np.nan,
                        "hazard_return_period": np.nan,
                        "hazard_type": hazard_type,
                        "hazard_indicator": hazard_indicator,
                        "count": int(cnt),
                        "min": float(mn),
                        "max": float(mx),
                        "mean": float(mean),
                        "median": float(qs[3]),
                        "p2_5": float(qs[0]),
                        "p5": float(qs[1]),
                        "p10": float(qs[2]),
                        "p90": float(qs[4]),
                        "p95": float(qs[5]),
                        "p97_5": float(qs[6]),
                        "ensemble": ensemble_value_used if has_ensemble else None,
                    }

                # map non-spatial dims into canonical columns
                for dim_name, dim_val in dim_combo.items():
                    if dim_name == "GWL":
                        row["scenario_name"] = str(dim_val)
                    elif dim_name == "return_period":
                        row["hazard_return_period"] = int(dim_val)
                    else:
                        row[dim_name] = dim_val

                rows.append(row)

            df = pd.DataFrame(rows)

            # stable column order
            base_cols = [
                "region",
                "adm_level",
                "scenario_name",
                "hazard_return_period",
                "hazard_type",
                "hazard_indicator",
                "count",
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
                if c not in df.columns:
                    df[c] = np.nan
            extra_cols = [c for c in df.columns if c not in base_cols]
            df = df[base_cols + extra_cols]

            # append to part csv
            df.to_csv(
                out_part_csv,
                mode="a",
                header=(not header_written),
                index=False,
                encoding="utf-8-sig",
            )
            header_written = True

            # cleanup
            del df, rows, block_dicts
            gc.collect()

    finally:
        try:
            ds.close()
        except Exception:
            pass


# ---------------------------------------------------------------------------
# CONCAT PARTS -> FINAL CSV (STREAMING)
# ---------------------------------------------------------------------------


def concat_parts_to_final(parts_dir: str, final_csv: str) -> None:
    part_files = sorted(glob.glob(os.path.join(parts_dir, "*.csv")))
    if not part_files:
        raise FileNotFoundError(f"No part CSVs found in {parts_dir}")

    ensure_parent_dir(final_csv)
    tmp_final = final_csv + ".tmp"

    with open(tmp_final, "w", encoding="utf-8-sig") as out_f:
        wrote_header = False
        for p in part_files:
            with open(p, "r", encoding="utf-8-sig") as in_f:
                for line_i, line in enumerate(in_f):
                    if line_i == 0:
                        if not wrote_header:
                            out_f.write(line)
                            wrote_header = True
                        # else skip header
                    else:
                        out_f.write(line)

    os.replace(tmp_final, final_csv)


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
    OUTPUT_PARTS_DIR = "workspace/Climate Data/Precomputed Regional Data/parts"

    ENSEMBLE_FILTER = "median"

    # HARD CAP chunk sizes (never exceeded) for stable memory use across hazards.
    TARGET_CHUNK_LAT = 2048
    TARGET_CHUNK_LON = 2048

    # Dask cluster
    num_cpus = os.cpu_count() or 4
    print(f"  🔍 Detected {num_cpus} CPU(s)")

    cluster = LocalCluster(
        n_workers=num_cpus,
        threads_per_worker=1,
        # IMPORTANT: if you have small RAM, lower this or lower n_workers
        memory_limit="4GB",
        dashboard_address="0.0.0.0:8787",
    )
    client = Client(cluster)

    try:
        print(f"  ✅ Dask cluster started with {len(cluster.workers)} workers")
        print(f"  📊 Dashboard: {client.dashboard_link}")

        # Load boundaries
        adm_levels = [("ADM1", ADM1_PATH), ("ADM2", ADM2_PATH)]
        adm_gdfs = {}
        for level, path in adm_levels:
            print(f"  Loading {level}: {path}")
            adm_gdfs[level] = load_adm_shapefile(path)

        # Find hazard files
        nc_pattern = os.path.join(HAZARDS_DIR, "**", "*ensemble_return_period*.nc")
        nc_files = sorted(glob.glob(nc_pattern, recursive=True))
        print(f"  Found {len(nc_files)} NetCDF file(s)")
        if not nc_files:
            raise FileNotFoundError(f"No hazard files found in {HAZARDS_DIR}")

        ensure_parent_dir(os.path.join(OUTPUT_PARTS_DIR, "x"))

        for i, nc_path in enumerate(nc_files, 1):
            ht, hi = parse_hazard_from_path(nc_path)
            label = f"{ht}/{hi} - {os.path.basename(nc_path)}"
            print(f"\n[{i}/{len(nc_files)}] Processing NetCDF: {label}")
            file_start = time.time()

            for adm_level, adm_gdf in adm_gdfs.items():
                part_name = f"{ht}__{hi}__{adm_level}__{os.path.basename(nc_path).replace('.nc','')}"
                part_csv = os.path.join(OUTPUT_PARTS_DIR, part_name + ".csv")
                done_flag = part_csv + ".done"

                if os.path.exists(done_flag):
                    print(f"  ⏭️  Skipping {adm_level} (done): {part_name}")
                    continue

                print(f"  📊 Aggregating over {adm_level} ({len(adm_gdf)} regions)...")
                t0 = time.time()

                process_nc_hazard_bottomk(
                    nc_path=nc_path,
                    adm_gdf=adm_gdf,
                    adm_level=adm_level,
                    ensemble_filter=ENSEMBLE_FILTER,
                    client=client,
                    out_part_csv=part_csv,
                    target_chunk_lat=TARGET_CHUNK_LAT,
                    target_chunk_lon=TARGET_CHUNK_LON,
                    seed=SEED,
                    k_samples=K_SAMPLES,
                )

                # mark done only after success
                with open(done_flag, "w") as f:
                    f.write("ok\n")

                print(f"    ✅ {adm_level}: wrote part in {time.time() - t0:.1f}s")
                gc.collect()

            print(f"  ⏱️  Total time for this file: {time.time() - file_start:.1f}s")

        print("\n🧩 Concatenating parts into final CSV...")
        concat_parts_to_final(OUTPUT_PARTS_DIR, OUTPUT_PATH)
        print(f"✅ Saved final results to: {OUTPUT_PATH}")

    finally:
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
