#!/usr/bin/env python3
"""
Precompute hazard statistics aggregated over administrative regions (ADM1/ADM2),
using a REGION-BY-REGION strategy (no global region-id raster).

Why this version:
- Avoids building/touching a full (nlat x nlon) region-id raster memmap (can be ~100GB+).
- Reads only the raster window that overlaps each region's bbox.
- Rasterizes the region polygon only on that window to create a mask.
- Computes exact stats per region. Uses exact quantiles by materializing values for the region.

Checkpoint/cleanup:
- Writes ONE part CSV per (hazard file, adm_level): _parts/<hazard_type>__<haz_ind>__<ADM>.csv
- Writes a ".done" marker once part completed
- On rerun, skips parts already done

Notes:
- This is single-process (same as your exact version).
- Exact quantiles are computed from ALL pixels of a region window that are inside the polygon and valid.
- If a region intersects raster bounds but has no valid pixels => "all_nodata_or_no_overlap".
- If it doesn't intersect raster bounds => "outside_raster_bounds".

Performance expectations:
- For very large grids, this is typically much faster/safer than global region-id raster + full streaming.
- For many small regions, IO pattern is "many windows". Still usually fine with NetCDF4, but if it's slow,
  converting the specific hazard to a tiled GeoTIFF per hazard (optional) is a good next optimization.

"""

import os
import glob
import gc
import itertools
import warnings
import hashlib
import shutil
import signal
import sys
import faulthandler
from typing import Dict, List, Tuple, Any, Optional

import numpy as np
import pandas as pd
import geopandas as gpd
import xarray as xr

import rasterio.features
from rasterio.transform import from_bounds
from affine import Affine
from shapely.geometry import box
from unidecode import unidecode
from tqdm import tqdm

warnings.filterwarnings(
    "ignore", message="The specified chunks separate the stored chunks"
)

# -----------------------
# CONFIG
# -----------------------
Q_LIST = np.array([0.025, 0.05, 0.10, 0.50, 0.90, 0.95, 0.975], dtype=np.float64)

BASE_COLS = [
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
    "intersects_raster_bounds",
    "empty_reason",
]

# Window block size when reading a region window in chunks (within bbox window)
# (keeps per-region processing bounded even for large regions)
WIN_BLOCK = 2048

# If a single region would exceed this many values, we switch to per-region spill buckets
# for exact quantiles (continuous hazards). This prevents RAM blowups on huge regions.
MAX_REGION_VALUES_IN_RAM = 25_000_000  # 25M float64 ~ 200MB; tune

# Spill buckets for per-region exact quantiles (continuous hazards only)
N_BUCKETS_PER_REGION = 256

# Safety: require some free space before writing spill chunks (bytes)
MIN_FREE_BYTES_BEFORE_SPILL = 2 * 1024**3  # 2 GiB

# Hazards where missing data (NaN/NoData) should NOT be filled with zero.
# For these hazards, we keep only valid pixels for statistics.
# For others (e.g. Flood, Fire), NaN usually means "no hazard", so we fill with 0.
HAZARDS_THAT_DONT_FILL_ZERO = {"Heat", "Drought"}
INDICATORS_THAT_DONT_FILL_ZERO = {"land_cover"}

# GDAL cache and thread caps to avoid native memory growth
GDAL_CACHEMAX = "128"


# -----------------------
# Non-silent failure aids
# -----------------------
faulthandler.enable(all_threads=True)


def _handle_term(signum, frame):
    print(f"\n[signal] got {signum}, dumping traceback:", flush=True)
    faulthandler.dump_traceback(file=sys.stderr, all_threads=True)
    sys.exit(128 + signum)


for _sig in (signal.SIGTERM, signal.SIGINT):
    signal.signal(_sig, _handle_term)


# -----------------------
# Helpers: text + paths
# -----------------------
def fix_text(text):
    return unidecode(text) if isinstance(text, str) else text


def parse_hazard_from_path(path: str) -> Tuple[str, str]:
    parts = path.split(os.sep)
    if "hazards" not in parts:
        raise ValueError(f"Invalid path structure - 'hazards' not found: {path}")
    i = parts.index("hazards")
    if i + 2 >= len(parts):
        raise ValueError(
            f"Invalid path structure - missing hazard type/indicator: {path}"
        )
    return parts[i + 1], parts[i + 2]


def safe_slug(s: str) -> str:
    s = fix_text(s)
    s = "".join(ch if (ch.isalnum() or ch in "-_") else "_" for ch in str(s))
    return s[:200]


def load_adm_shapefile(adm_path: str) -> gpd.GeoDataFrame:
    if not os.path.exists(adm_path):
        raise FileNotFoundError(adm_path)
    try:
        gdf = gpd.read_file(adm_path, encoding="utf-8")
    except Exception:
        try:
            gdf = gpd.read_file(adm_path, encoding="latin1")
        except Exception:
            gdf = gpd.read_file(adm_path)

    if gdf.crs is None:
        raise ValueError(f"CRS missing: {adm_path}")
    if str(gdf.crs) != "EPSG:4326":
        gdf = gdf.to_crs("EPSG:4326")

    name_col = None
    for col in ["shapeName", "NAME_2", "NAME_1", "NAME", "name", "prov_name"]:
        if col in gdf.columns:
            name_col = col
            break
    if name_col is None:
        raise ValueError(f"No region name column found in: {adm_path}")

    gdf = gdf.copy()
    gdf["region"] = gdf[name_col].apply(fix_text)
    return gdf


# -----------------------
# Grid / transform
# -----------------------
def ensure_lat_descending(da: xr.DataArray) -> xr.DataArray:
    lat = da["lat"].values
    if lat[0] < lat[-1]:
        return da.isel(lat=slice(None, None, -1))
    return da


def grid_transform_from_latlon(lats: np.ndarray, lons: np.ndarray) -> Affine:
    lon_min, lon_max = float(lons.min()), float(lons.max())
    lat_min, lat_max = float(lats.min()), float(lats.max())
    return from_bounds(lon_min, lat_min, lon_max, lat_max, len(lons), len(lats))


def raster_bounds_polygon(lats: np.ndarray, lons: np.ndarray) -> Any:
    lon_min, lon_max = float(lons.min()), float(lons.max())
    lat_min, lat_max = float(lats.min()), float(lats.max())
    return box(lon_min, lat_min, lon_max, lat_max)


# -----------------------
# Spill utils for per-region exact quantiles (continuous hazards)
# -----------------------
import struct


def cleanup_dir(path: str) -> None:
    if not path or not os.path.exists(path):
        return
    try:
        shutil.rmtree(path, ignore_errors=True)
    except Exception:
        pass


def _spill_write_chunk(path: str, v: np.ndarray) -> None:
    if v.size == 0:
        return

    base_dir = os.path.dirname(path) or "."
    try:
        free = shutil.disk_usage(base_dir).free
        if free < MIN_FREE_BYTES_BEFORE_SPILL:
            raise OSError(
                f"Not enough free space to continue spilling. "
                f"Free={free} bytes, required~={MIN_FREE_BYTES_BEFORE_SPILL} bytes. "
                f"Spill dir: {base_dir}"
            )
    except FileNotFoundError:
        pass

    # store float32 on disk (enough for hazards)
    if v.dtype != np.float32:
        v = v.astype(np.float32, copy=False)

    n = v.size
    with open(path, "ab") as f:
        f.write(struct.pack("<I", n))
        v.tofile(f)


def _spill_iter_chunks(path: str):
    with open(path, "rb") as f:
        while True:
            header = f.read(4)
            if not header:
                break
            (n,) = struct.unpack("<I", header)
            v = np.fromfile(f, dtype=np.float32, count=n)
            if v.size != n:
                raise IOError(f"Corrupt spill file (unexpected EOF): {path}")
            yield v


def exact_quantiles_from_spill_dir(spill_dir: str, q_list: np.ndarray) -> np.ndarray:
    chunks = []
    for p in sorted(glob.glob(os.path.join(spill_dir, "bucket_*.bin"))):
        # concatenate all chunks in this bucket; (we'll just read all buckets into one big vector)
        for v in _spill_iter_chunks(p):
            chunks.append(v)
    if not chunks:
        return np.full(len(q_list), np.nan, dtype=np.float64)
    vals = np.concatenate(chunks).astype(np.float64, copy=False)
    return np.quantile(vals, q_list, method="linear").astype(np.float64, copy=False)


# -----------------------
# Region-by-region computation for ONE slice (one dim_combo)
# -----------------------
def compute_region_stats_for_slice(
    *,
    da_slice: xr.DataArray,  # 2D (lat, lon)
    lats: np.ndarray,
    lons: np.ndarray,
    transform: Affine,
    region_geom,
    region_name: str,
    raster_bounds_poly,
    hazard_type: str,
    hazard_indicator: str,
    nodata_value: Optional[float],
    spill_root: str,
) -> Dict[str, Any]:
    # bounds intersection check
    intersects_bounds = bool(region_geom.intersects(raster_bounds_poly))
    if not intersects_bounds:
        return {
            "region": region_name,
            "count": 0,
            "min": np.nan,
            "max": np.nan,
            "mean": np.nan,
            "qs": np.full(len(Q_LIST), np.nan, dtype=np.float64),
            "intersects_raster_bounds": False,
            "empty_reason": "outside_raster_bounds",
        }

    # Find index window that overlaps region bbox (fast)
    minx, miny, maxx, maxy = region_geom.bounds

    # lat is descending
    lat_idx = np.where((lats <= maxy) & (lats >= miny))[0]
    lon_idx = np.where((lons >= minx) & (lons <= maxx))[0]

    if lat_idx.size == 0 or lon_idx.size == 0:
        return {
            "region": region_name,
            "count": 0,
            "min": np.nan,
            "max": np.nan,
            "mean": np.nan,
            "qs": np.full(len(Q_LIST), np.nan, dtype=np.float64),
            "intersects_raster_bounds": True,
            "empty_reason": "all_nodata_or_no_overlap",
        }

    i0, i1 = int(lat_idx.min()), int(lat_idx.max()) + 1
    j0, j1 = int(lon_idx.min()), int(lon_idx.max()) + 1

    # Sub-transform for this window
    window_transform = transform * Affine.translation(j0, i0)
    out_shape = (i1 - i0, j1 - j0)

    # Rasterize the polygon on the window only (mask=1 inside)
    mask = rasterio.features.rasterize(
        [(region_geom, 1)],
        out_shape=out_shape,
        transform=window_transform,
        fill=0,
        dtype=np.uint8,
        all_touched=True,
    ).astype(bool)

    if not mask.any():
        return {
            "region": region_name,
            "count": 0,
            "min": np.nan,
            "max": np.nan,
            "mean": np.nan,
            "qs": np.full(len(Q_LIST), np.nan, dtype=np.float64),
            "intersects_raster_bounds": True,
            "empty_reason": "all_nodata_or_no_overlap",
        }

    # We'll stream inside the region window in blocks to keep memory bounded
    cnt = 0
    s = 0.0
    mn = np.inf
    mx = -np.inf

    # Continuous hazards: collect values per region; if too big, spill per region.
    # We'll estimate region max values = window area; real after mask/nodata filtering.
    window_area = int(mask.sum())
    use_spill = window_area > MAX_REGION_VALUES_IN_RAM

    spill_dir = None
    if use_spill:
        # per region spill dir
        rid_key = hashlib.md5(region_name.encode("utf-8")).hexdigest()[:12]
        spill_dir = os.path.join(spill_root, f"region_{rid_key}")
        cleanup_dir(spill_dir)
        os.makedirs(spill_dir, exist_ok=True)

    values_accum = [] if not use_spill else None
    bucket_paths = None
    if use_spill:
        bucket_paths = [
            os.path.join(spill_dir, f"bucket_{b:04d}.bin")
            for b in range(N_BUCKETS_PER_REGION)
        ]

    for ii in range(i0, i1, WIN_BLOCK):
        ii1 = min(ii + WIN_BLOCK, i1)
        mi0 = ii - i0
        mi1 = ii1 - i0
        for jj in range(j0, j1, WIN_BLOCK):
            jj1 = min(jj + WIN_BLOCK, j1)
            mj0 = jj - j0
            mj1 = jj1 - j0

            block = da_slice.isel(lat=slice(ii, ii1), lon=slice(jj, jj1)).values
            mblock = mask[mi0:mi1, mj0:mj1]
            if not mblock.any():
                continue

            v = block[mblock]
            # special rule in your prior code
            if hazard_type == "Heat":
                v = np.where(v > 300, v, np.nan)

            if (
                hazard_type in HAZARDS_THAT_DONT_FILL_ZERO
                or hazard_indicator in INDICATORS_THAT_DONT_FILL_ZERO
            ):
                if np.issubdtype(v.dtype, np.floating):
                    v = v[~np.isnan(v)]
                else:
                    if nodata_value is not None:
                        v = v[v != np.asarray(nodata_value, dtype=v.dtype)]
            else:
                # Default: fill NaN/NoData with zero (e.g. Flood, Fire weather)
                if np.issubdtype(v.dtype, np.floating):
                    v = np.nan_to_num(v, nan=0.0)
                else:
                    if nodata_value is not None:
                        v = np.where(v == np.asarray(nodata_value, dtype=v.dtype), 0, v)

            if v.size == 0:
                continue

            v64 = v.astype(np.float64, copy=False)
            cnt += int(v64.size)
            s += float(v64.sum())
            mn = min(mn, float(v64.min()))
            mx = max(mx, float(v64.max()))

            if use_spill:
                # bucket by a simple hash of value index (not needed for correctness, only file split)
                # Here we bucket by chunk to avoid huge single files.
                # Write as float32 on disk.
                b = hash((ii, jj)) % N_BUCKETS_PER_REGION
                _spill_write_chunk(bucket_paths[b], v64.astype(np.float32, copy=False))
            else:
                values_accum.append(v64)

    if cnt == 0:
        qs = np.full(len(Q_LIST), np.nan, dtype=np.float64)
        empty_reason = "all_nodata_or_no_overlap"
        return {
            "region": region_name,
            "count": 0,
            "min": np.nan,
            "max": np.nan,
            "mean": np.nan,
            "qs": qs,
            "intersects_raster_bounds": True,
            "empty_reason": empty_reason,
        }

    if use_spill:
        qs = exact_quantiles_from_spill_dir(spill_dir, Q_LIST)
        cleanup_dir(spill_dir)
    else:
        vals = (
            np.concatenate(values_accum) if len(values_accum) > 1 else values_accum[0]
        )
        qs = np.quantile(vals, Q_LIST, method="linear").astype(np.float64, copy=False)

    return {
        "region": region_name,
        "count": cnt,
        "min": float(mn),
        "max": float(mx),
        "mean": float(s / cnt),
        "qs": qs,
        "intersects_raster_bounds": True,
        "empty_reason": np.nan,
    }


# -----------------------
# Process one NetCDF file, one ADM level (region-by-region)
# Writes directly to the part CSV.
# -----------------------
def process_nc_file_region_by_region_to_part(
    *,
    nc_path: str,
    adm_gdf: gpd.GeoDataFrame,
    adm_level: str,
    ensemble_filter: str,
    part_csv: str,
    cache_dir: str,
):
    hazard_type, hazard_indicator = parse_hazard_from_path(nc_path)

    ds = xr.open_dataset(nc_path, engine="netcdf4")  # no dask
    try:
        var_name = list(ds.data_vars.keys())[0]
        da = ds[var_name]

        if "lat" not in da.dims or "lon" not in da.dims:
            raise ValueError(f"Missing lat/lon dims in {nc_path}")

        ensemble_used = None
        if "ensemble" in da.dims:
            available = [str(x) for x in da["ensemble"].values.tolist()]
            if ensemble_filter in available:
                da = da.sel(
                    ensemble=da["ensemble"].values[available.index(ensemble_filter)]
                )
                ensemble_used = ensemble_filter
            else:
                da = da.isel(ensemble=0)
                ensemble_used = str(da["ensemble"].values)

        da = ensure_lat_descending(da)

        lats = da["lat"].values
        lons = da["lon"].values
        transform = grid_transform_from_latlon(lats, lons)
        rb_poly = raster_bounds_polygon(lats, lons)

        nodata_value = da.attrs.get("_FillValue", None)
        if (
            nodata_value is None
            and hasattr(da, "encoding")
            and isinstance(da.encoding, dict)
        ):
            nodata_value = da.encoding.get("_FillValue", None)

        non_spatial_dims = [d for d in da.dims if d not in ("lat", "lon")]
        dim_values = {d: da[d].values for d in non_spatial_dims}
        dim_keys = list(dim_values.keys())
        dim_combos = (
            [
                dict(zip(dim_keys, combo))
                for combo in itertools.product(*[dim_values[k] for k in dim_keys])
            ]
            if dim_keys
            else [{}]
        )

        os.makedirs(os.path.dirname(part_csv), exist_ok=True)
        if os.path.exists(part_csv):
            os.remove(part_csv)
        header_written = False

        spill_root = os.path.join(
            cache_dir,
            "_spill_region_by_region",
            f"{safe_slug(hazard_type)}__{safe_slug(hazard_indicator)}__{adm_level}",
        )
        cleanup_dir(spill_root)
        os.makedirs(spill_root, exist_ok=True)

        # iterate combos
        for dim_combo in tqdm(
            dim_combos,
            desc=f"{adm_level} combos | {hazard_type}/{hazard_indicator}",
            unit="combo",
        ):
            da_slice = da
            for kdim, vdim in dim_combo.items():
                da_slice = da_slice.sel({kdim: vdim})
            da_slice = da_slice.transpose("lat", "lon")

            rows = []
            # iterate regions
            for _, reg in tqdm(
                adm_gdf.iterrows(),
                total=len(adm_gdf),
                desc=f"{adm_level} regions | {hazard_type}/{hazard_indicator}",
                unit="region",
                leave=False,
            ):
                region_name = reg["region"]
                stats = compute_region_stats_for_slice(
                    da_slice=da_slice,
                    lats=lats,
                    lons=lons,
                    transform=transform,
                    region_geom=reg.geometry,
                    region_name=region_name,
                    raster_bounds_poly=rb_poly,
                    hazard_type=hazard_type,
                    hazard_indicator=hazard_indicator,
                    nodata_value=nodata_value,
                    spill_root=spill_root,
                )

                qs = stats["qs"]
                row = {
                    "region": region_name,
                    "adm_level": adm_level,
                    "scenario_name": np.nan,
                    "hazard_return_period": np.nan,
                    "hazard_type": hazard_type,
                    "hazard_indicator": hazard_indicator,
                    "count": int(stats["count"]),
                    "min": stats["min"],
                    "max": stats["max"],
                    "mean": stats["mean"],
                    "median": float(qs[3]) if qs is not None else np.nan,
                    "p2_5": float(qs[0]) if qs is not None else np.nan,
                    "p5": float(qs[1]) if qs is not None else np.nan,
                    "p10": float(qs[2]) if qs is not None else np.nan,
                    "p90": float(qs[4]) if qs is not None else np.nan,
                    "p95": float(qs[5]) if qs is not None else np.nan,
                    "p97_5": float(qs[6]) if qs is not None else np.nan,
                    "ensemble": ensemble_used,
                    "intersects_raster_bounds": bool(stats["intersects_raster_bounds"]),
                    "empty_reason": stats["empty_reason"],
                }

                for dim_name, dim_val in dim_combo.items():
                    if dim_name in ("GWL", "gwl", "scenario"):
                        row["scenario_name"] = str(dim_val)
                    elif dim_name == "return_period":
                        row["hazard_return_period"] = int(dim_val)
                    else:
                        row[dim_name] = dim_val

                rows.append(row)

            df = pd.DataFrame(rows)

            for c in BASE_COLS:
                if c not in df.columns:
                    df[c] = np.nan
            extra_cols = [c for c in df.columns if c not in BASE_COLS]
            df = df[BASE_COLS + extra_cols]

            df.to_csv(
                part_csv,
                mode="a",
                header=(not header_written),
                index=False,
                encoding="utf-8-sig",
            )
            header_written = True

            del df, rows
            gc.collect()

        cleanup_dir(spill_root)

    finally:
        ds.close()


def concat_parts_to_final(parts: List[str], out_csv: str) -> None:
    os.makedirs(os.path.dirname(out_csv), exist_ok=True)
    if os.path.exists(out_csv):
        os.remove(out_csv)

    if not parts:
        return

    print(f"Concatenating {len(parts)} parts to {out_csv}...", flush=True)

    # We use pandas to concatenate parts because different parts might have different
    # extra columns (dimensions from NetCDF). pd.concat handles the union of columns.
    dfs = []
    for p in tqdm(parts, desc="Reading parts"):
        try:
            # Using low_memory=False to avoid DtypeWarning and ensure consistent parsing
            dfs.append(pd.read_csv(p, encoding="utf-8-sig", low_memory=False))
        except Exception as e:
            print(f"Error reading part {p}: {e}")

    if not dfs:
        print("No data found in parts.")
        return

    final_df = pd.concat(dfs, ignore_index=True)

    # Fill missing statistics with 0 for hazards where NaN/NoData implies "no hazard"
    # (e.g., Flood, Fire). Heat, Drought, and land_cover remain NaN if no data found.
    stat_cols = [
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
    ]
    fill_mask = ~final_df["hazard_type"].isin(HAZARDS_THAT_DONT_FILL_ZERO)
    if "hazard_indicator" in final_df.columns:
        fill_mask &= ~final_df["hazard_indicator"].isin(INDICATORS_THAT_DONT_FILL_ZERO)

    final_df.loc[fill_mask, stat_cols] = final_df.loc[fill_mask, stat_cols].fillna(0)

    # Ensure BASE_COLS are at the front, followed by any extra columns
    # Filter BASE_COLS to only those that actually exist in final_df
    existing_base = [c for c in BASE_COLS if c in final_df.columns]
    extra_cols = [c for c in final_df.columns if c not in existing_base]
    final_df = final_df[existing_base + extra_cols]

    final_df.to_csv(out_csv, index=False, encoding="utf-8-sig")
    print(f"Successfully saved {len(final_df)} rows to {out_csv}", flush=True)


def main():
    # reduce native memory/thread oversubscription
    os.environ.setdefault("GDAL_CACHEMAX", GDAL_CACHEMAX)
    os.environ.setdefault("OMP_NUM_THREADS", "1")
    os.environ.setdefault("OPENBLAS_NUM_THREADS", "1")
    os.environ.setdefault("MKL_NUM_THREADS", "1")
    os.environ.setdefault("NUMEXPR_MAX_THREADS", "1")
    os.environ.setdefault("HDF5_USE_FILE_LOCKING", "FALSE")

    # -----------------------
    # PATHS (edit if needed)
    # -----------------------
    HAZARDS_DIR = "workspace/demo_inputs_fullnc/hazards"
    ADM1_PATH = "workspace/demo_inputs_fullnc/areas/state/geoBoundaries-BRA-ADM1.shp"
    ADM2_PATH = (
        "workspace/demo_inputs_fullnc/areas/municipality/geoBoundaries-BRA-ADM2.shp"
    )
    ENSEMBLE_FILTER = "median"

    OUT_DIR = "workspace/Climate Data/Precomputed Regional Data"
    OUT_CSV = os.path.join(OUT_DIR, "precomputed_adm_indicators.csv")

    CACHE_DIR = os.path.join(OUT_DIR, "_cache_region_by_region")
    PARTS_DIR = os.path.join(OUT_DIR, "_parts")
    os.makedirs(CACHE_DIR, exist_ok=True)
    os.makedirs(PARTS_DIR, exist_ok=True)

    nc_files = sorted(
        glob.glob(
            os.path.join(HAZARDS_DIR, "**", "*ensemble_return_period*.nc"),
            recursive=True,
        )
    )
    if not nc_files:
        raise FileNotFoundError("No matching NetCDF files found")

    adm1 = load_adm_shapefile(ADM1_PATH)
    adm2 = load_adm_shapefile(ADM2_PATH)

    for i, nc_path in enumerate(nc_files, 1):
        ht, hi = parse_hazard_from_path(nc_path)
        print(
            f"[{i}/{len(nc_files)}] {ht}/{hi} - {os.path.basename(nc_path)}", flush=True
        )

        base = f"{safe_slug(ht)}__{safe_slug(hi)}"
        part1 = os.path.join(PARTS_DIR, f"{base}__ADM1.csv")
        part2 = os.path.join(PARTS_DIR, f"{base}__ADM2.csv")
        done1 = part1 + ".done"
        done2 = part2 + ".done"

        # ADM1
        if os.path.exists(done1) and os.path.exists(part1):
            print(f"  - skip ADM1 (done): {os.path.basename(part1)}", flush=True)
        else:
            if os.path.exists(part1) and not os.path.exists(done1):
                os.remove(part1)
            process_nc_file_region_by_region_to_part(
                nc_path=nc_path,
                adm_gdf=adm1,
                adm_level="ADM1",
                ensemble_filter=ENSEMBLE_FILTER,
                part_csv=part1,
                cache_dir=CACHE_DIR,
            )
            with open(done1, "w", encoding="utf-8") as f:
                f.write("ok\n")

        # ADM2
        if os.path.exists(done2) and os.path.exists(part2):
            print(f"  - skip ADM2 (done): {os.path.basename(part2)}", flush=True)
        else:
            if os.path.exists(part2) and not os.path.exists(done2):
                os.remove(part2)
            process_nc_file_region_by_region_to_part(
                nc_path=nc_path,
                adm_gdf=adm2,
                adm_level="ADM2",
                ensemble_filter=ENSEMBLE_FILTER,
                part_csv=part2,
                cache_dir=CACHE_DIR,
            )
            with open(done2, "w", encoding="utf-8") as f:
                f.write("ok\n")

        # cleanup spill dir just in case
        cleanup_dir(os.path.join(CACHE_DIR, "_spill_region_by_region"))

    part_files = sorted(glob.glob(os.path.join(PARTS_DIR, "*.csv")))
    completed = [p for p in part_files if os.path.exists(p + ".done")]
    if not completed:
        raise RuntimeError(f"No completed part files found in {PARTS_DIR}")

    concat_parts_to_final(completed, OUT_CSV)
    print(f"Saved: {OUT_CSV}", flush=True)


if __name__ == "__main__":
    main()
