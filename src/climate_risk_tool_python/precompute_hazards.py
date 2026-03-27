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
import argparse
from typing import Dict, List, Tuple, Any, Optional

import numpy as np
import pandas as pd
import geopandas as gpd
import xarray as xr

import yaml
import rasterio
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
    "adm_name",
    "adm_code",
    "shape_id",
    "adm_level",
    "gwl",
    "return_period",
    "indicator_file",
    "indicator_variable",
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
    "season",
    "scenario_name",
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


def safe_slug(s: str) -> str:
    s = fix_text(s)
    s = "".join(ch if (ch.isalnum() or ch in "-_") else "_" for ch in str(s))
    return s[:200]


def resolve_base_dir(input_folder: str) -> str:
    path = os.path.abspath(input_folder)
    parts = path.split(os.sep)
    if "hazards" in parts:
        idx = parts.index("hazards")
        if idx > 0:
            return os.sep.join(parts[:idx])
    return path


def find_adm_boundary(base_dir: str, level: str) -> str:
    if level not in ("ADM1", "ADM2"):
        raise ValueError(f"Unsupported ADM level: {level}")
    subdir = "state" if level == "ADM1" else "municipality"
    area_dir = os.path.join(base_dir, "areas", subdir)
    if not os.path.exists(area_dir):
        raise FileNotFoundError(area_dir)
    candidates = sorted(glob.glob(os.path.join(area_dir, "*.shp")))
    if candidates:
        return candidates[0]
    candidates = sorted(glob.glob(os.path.join(area_dir, "*.geojson")))
    if candidates:
        return candidates[0]
    raise FileNotFoundError(f"No ADM boundary file found in {area_dir}")


def load_hazard_configs(config_dir: str) -> Dict[str, Any]:
    if not os.path.isdir(config_dir):
        raise FileNotFoundError(config_dir)
    configs = {}
    for path in sorted(glob.glob(os.path.join(config_dir, "*.yml"))):
        with open(path, "r", encoding="utf-8") as f:
            cfg = yaml.safe_load(f) or {}
        hazard_type = os.path.splitext(os.path.basename(path))[0]
        configs[hazard_type] = cfg
    if not configs:
        raise FileNotFoundError(f"No hazard configs found in {config_dir}")
    return configs


def select_nc_file(indicators_dir: str, indicator_file: str) -> str:
    direct_path = os.path.join(indicators_dir, indicator_file)
    if os.path.exists(direct_path):
        return direct_path

    base = os.path.splitext(indicator_file)[0]
    pattern = os.path.join(indicators_dir, f"{base}__agg*.nc")
    candidates = sorted(glob.glob(pattern))
    if not candidates:
        raise FileNotFoundError(f"NetCDF file not found for {indicator_file}")

    def agg_key(p):
        name = os.path.splitext(os.path.basename(p))[0]
        if "__agg" in name:
            tail = name.split("__agg", 1)[1]
            try:
                return int(tail)
            except ValueError:
                return 9999
        return 9999

    candidates.sort(key=agg_key)
    return candidates[0]


def select_tif_file(indicator_dir: str, hazard_file: str) -> str:
    direct_path = os.path.join(indicator_dir, hazard_file)
    if os.path.exists(direct_path):
        return direct_path

    base = os.path.splitext(hazard_file)[0]
    pattern = os.path.join(indicator_dir, f"{base}__agg*.tif")
    candidates = sorted(glob.glob(pattern))
    if not candidates:
        raise FileNotFoundError(f"TIF file not found for {hazard_file}")

    def agg_key(p):
        name = os.path.splitext(os.path.basename(p))[0]
        if "__agg" in name:
            tail = name.split("__agg", 1)[1]
            try:
                return int(tail)
            except ValueError:
                return 9999
        return 9999

    candidates.sort(key=agg_key)
    return candidates[0]


def read_indicator_metadata(indicator_dir: str) -> pd.DataFrame:
    metadata_path = os.path.join(indicator_dir, "metadata.csv")
    if not os.path.exists(metadata_path):
        raise FileNotFoundError(metadata_path)
    df = pd.read_csv(metadata_path)
    expected = {"hazard_file", "scenario_name", "return_period"}
    missing = expected - set(df.columns)
    if missing:
        raise ValueError(f"Missing columns in {metadata_path}: {sorted(missing)}")
    return df


def discover_indicators(
    hazard_configs: Dict[str, Any], indicators_dir: str
) -> List[Dict[str, Any]]:
    indicators = []
    for hazard_type, cfg in hazard_configs.items():
        for indicator_key, ind_cfg in (cfg.get("indicators") or {}).items():
            indicator_file = ind_cfg.get("file")
            indicator_variable = ind_cfg.get("variable")
            fixed_ensemble = None
            if isinstance(ind_cfg.get("fixed"), dict):
                fixed_ensemble = ind_cfg["fixed"].get("ensemble")

            if not indicator_file:
                continue

            indicator_path = os.path.join(indicators_dir, indicator_file)
            is_nc = indicator_file.lower().endswith(".nc")
            is_dir = indicator_file.endswith(os.sep) or os.path.isdir(indicator_path)

            if is_nc:
                nc_path = select_nc_file(indicators_dir, indicator_file)
                indicators.append(
                    dict(
                        hazard_type=hazard_type,
                        hazard_indicator=indicator_key,
                        indicator_file=indicator_file,
                        indicator_variable=indicator_variable,
                        source="nc",
                        data_path=nc_path,
                        fixed_ensemble=fixed_ensemble,
                    )
                )
                continue

            if is_dir:
                indicator_dir = indicator_path.rstrip(os.sep)
                metadata = read_indicator_metadata(indicator_dir)
                indicators.append(
                    dict(
                        hazard_type=hazard_type,
                        hazard_indicator=indicator_key,
                        indicator_file=indicator_file.rstrip(os.sep),
                        indicator_variable=indicator_variable,
                        source="tif",
                        indicator_dir=indicator_dir,
                        metadata=metadata,
                        fixed_ensemble=fixed_ensemble,
                    )
                )
                continue

            if indicator_file.lower().endswith(".tif"):
                indicators.append(
                    dict(
                        hazard_type=hazard_type,
                        hazard_indicator=indicator_key,
                        indicator_file=indicator_file,
                        indicator_variable=indicator_variable,
                        source="tif_single",
                        data_path=indicator_path,
                        fixed_ensemble=fixed_ensemble,
                    )
                )
                continue

            raise FileNotFoundError(
                f"Unsupported indicator file reference: {indicator_file}"
            )

    if not indicators:
        raise RuntimeError("No indicators discovered from hazard configs.")
    return indicators


def load_adm_codes(base_dir: str) -> pd.DataFrame:
    path = os.path.join(base_dir, "areas", "brazil_adm_codes.csv")
    if not os.path.exists(path):
        print(f"Warning: {path} not found. Codes/ShapeIDs will be missing.")
        return pd.DataFrame(columns=["adm_code", "adm_name", "adm", "shape_id"])
    return pd.read_csv(path, dtype=str)


def load_adm_shapefile(
    adm_path: str, adm_codes_df: Optional[pd.DataFrame] = None
) -> gpd.GeoDataFrame:
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
    gdf["adm_name"] = gdf[name_col].apply(fix_text)

    # Extract shapeID
    shape_id_col = None
    for col in ["shapeID", "shape_id", "GID_1", "GID_2"]:
        if col in gdf.columns:
            shape_id_col = col
            break

    gdf["shape_id"] = np.nan
    if shape_id_col:
        gdf["shape_id"] = gdf[shape_id_col].astype(str)

    # Match to codes using shapeID
    gdf["adm_code"] = np.nan
    if adm_codes_df is not None and not adm_codes_df.empty and shape_id_col:
        # Create mapping: shape_id -> adm_code (codes CSV is the source of truth)
        shape_map = dict(
            zip(
                adm_codes_df["shape_id"].astype(str), adm_codes_df["adm_code"].astype(str)
            )
        )
        gdf["adm_code"] = gdf["shape_id"].map(shape_map)

    return gdf


# -----------------------
# Grid / transform
# -----------------------
def ensure_lat_descending(da: xr.DataArray) -> xr.DataArray:
    lat = da["lat"].values
    if lat[0] < lat[-1]:
        return da.isel(lat=slice(None, None, -1))
    return da


def normalize_latlon_names(da: xr.DataArray) -> xr.DataArray:
    rename_map = {}
    if "latitude" in da.dims or "latitude" in da.coords:
        rename_map["latitude"] = "lat"
    if "longitude" in da.dims or "longitude" in da.coords:
        rename_map["longitude"] = "lon"
    return da.rename(rename_map) if rename_map else da


def grid_transform_from_latlon(lats: np.ndarray, lons: np.ndarray) -> Affine:
    lon_min, lon_max = float(lons.min()), float(lons.max())
    lat_min, lat_max = float(lats.min()), float(lats.max())
    return from_bounds(lon_min, lat_min, lon_max, lat_max, len(lons), len(lats))


def raster_bounds_polygon(lats: np.ndarray, lons: np.ndarray) -> Any:
    lon_min, lon_max = float(lons.min()), float(lons.max())
    lat_min, lat_max = float(lats.min()), float(lats.max())
    return box(lon_min, lat_min, lon_max, lat_max)


def build_dim_label_map(
    ds: xr.Dataset, da: xr.DataArray, dim_name: str
) -> Dict[Any, Any]:
    if dim_name in da.coords:
        values = da[dim_name].values.tolist()
    else:
        values = list(range(da.sizes[dim_name]))

    labels_raw = None
    if isinstance(ds.attrs, dict):
        labels_raw = ds.attrs.get(f"labels__{dim_name}")
    if labels_raw is None and isinstance(da.attrs, dict):
        labels_raw = da.attrs.get(f"labels__{dim_name}")

    labels = None
    if labels_raw is not None:
        labels = [x.strip() for x in str(labels_raw).splitlines() if x.strip()]
        if len(labels) != len(values):
            labels = None

    if labels is None:
        labels = values

    return dict(zip(values, labels))


def get_dim_value_for_label(dim_map: Dict[Any, Any], label: str) -> Optional[Any]:
    for value, mapped in dim_map.items():
        if str(mapped) == str(label):
            return value
    return None


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
    adm_name: str,
    adm_code: Any,
    shape_id: Any,
    raster_bounds_poly,
    hazard_type: str,
    hazard_indicator: str,
    nodata_value: Optional[float],
    spill_root: str,
) -> Dict[str, Any]:
    # Always use coords from the slice to avoid any misalignment
    if da_slice.sizes.get("lat", 0) == 0 or da_slice.sizes.get("lon", 0) == 0:
        return {
            "adm_name": adm_name,
            "adm_code": adm_code,
            "shape_id": shape_id,
            "count": 0,
            "min": np.nan,
            "max": np.nan,
            "mean": np.nan,
            "qs": np.full(len(Q_LIST), np.nan, dtype=np.float64),
            "intersects_raster_bounds": False,
            "empty_reason": "empty_slice",
        }
    lats = da_slice["lat"].values
    lons = da_slice["lon"].values
    transform = grid_transform_from_latlon(lats, lons)
    raster_bounds_poly = raster_bounds_polygon(lats, lons)

    # bounds intersection check
    intersects_bounds = bool(region_geom.intersects(raster_bounds_poly))
    if not intersects_bounds:
        return {
            "adm_name": adm_name,
            "adm_code": adm_code,
            "shape_id": shape_id,
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
            "adm_name": adm_name,
            "adm_code": adm_code,
            "shape_id": shape_id,
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
            "adm_name": adm_name,
            "adm_code": adm_code,
            "shape_id": shape_id,
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
        rid_key = hashlib.md5(adm_name.encode("utf-8")).hexdigest()[:12]
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
            if block.shape != mblock.shape:
                # Guard against any unexpected shape mismatch
                min0 = min(block.shape[0], mblock.shape[0])
                min1 = min(block.shape[1], mblock.shape[1])
                if min0 == 0 or min1 == 0:
                    continue
                block = block[:min0, :min1]
                mblock = mblock[:min0, :min1]

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
            "adm_name": adm_name,
            "adm_code": adm_code,
            "shape_id": shape_id,
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
        "adm_name": adm_name,
        "adm_code": adm_code,
        "shape_id": shape_id,
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
    indicator_file: str,
    indicator_variable: str,
    hazard_type: str,
    hazard_indicator: str,
    adm_gdf: gpd.GeoDataFrame,
    adm_level: str,
    part_csv: str,
    cache_dir: str,
    fixed_ensemble: Optional[str] = None,
    append: bool = False,
):
    ds = xr.open_dataset(nc_path, engine="netcdf4")  # no dask
    try:
        var_name = list(ds.data_vars.keys())[0]
        da = ds[var_name]
        da = normalize_latlon_names(da)

        if "lat" not in da.dims or "lon" not in da.dims:
            raise ValueError(f"Missing lat/lon dims in {nc_path}")

        ensemble_used = None
        if "ensemble" in da.dims and fixed_ensemble is not None:
            dim_map = build_dim_label_map(ds, da, "ensemble")
            value = get_dim_value_for_label(dim_map, fixed_ensemble)
            if value is not None:
                da = da.sel(ensemble=value)
                ensemble_used = fixed_ensemble
            else:
                da = da.isel(ensemble=0)
                ensemble_used = list(dim_map.values())[0]

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
        dim_values = {}
        dim_label_maps = {}
        dim_index_values = {}
        for d in non_spatial_dims:
            dim_label_map = build_dim_label_map(ds, da, d)
            values = list(dim_label_map.keys())
            dim_values[d] = list(range(len(values)))
            dim_label_maps[d] = dim_label_map
            dim_index_values[d] = values
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
        if not append and os.path.exists(part_csv):
            os.remove(part_csv)
        header_written = os.path.exists(part_csv)

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
                da_slice = da_slice.isel({kdim: int(vdim)})
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
                adm_name = reg["adm_name"]
                adm_code = reg.get("adm_code", np.nan)
                shape_id = reg.get("shape_id", np.nan)
                stats = compute_region_stats_for_slice(
                    da_slice=da_slice,
                    lats=lats,
                    lons=lons,
                    transform=transform,
                    region_geom=reg.geometry,
                    adm_name=adm_name,
                    adm_code=adm_code,
                    shape_id=shape_id,
                    raster_bounds_poly=rb_poly,
                    hazard_type=hazard_type,
                    hazard_indicator=hazard_indicator,
                    nodata_value=nodata_value,
                    spill_root=spill_root,
                )

                qs = stats["qs"]
                row = {
                    "adm_name": adm_name,
                    "adm_code": stats.get("adm_code", np.nan),
                    "shape_id": stats.get("shape_id", np.nan),
                    "adm_level": adm_level,
                    "gwl": np.nan,
                    "return_period": np.nan,
                    "indicator_file": indicator_file,
                    "indicator_variable": indicator_variable,
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
                    "season": np.nan,
                    "scenario_name": np.nan,
                }

                for dim_name, dim_val in dim_combo.items():
                    raw_value = dim_index_values[dim_name][int(dim_val)]
                    dim_label = dim_label_maps[dim_name].get(raw_value, raw_value)
                    if dim_name in ("GWL", "gwl"):
                        row["gwl"] = str(dim_label)
                    elif dim_name in ("scenario", "scenario_name"):
                        row["scenario_name"] = str(dim_label)
                    elif dim_name == "return_period":
                        row["return_period"] = int(float(dim_label))
                    elif dim_name == "season":
                        row["season"] = str(dim_label)
                    elif dim_name == "ensemble":
                        row["ensemble"] = str(dim_label)
                    else:
                        row[dim_name] = dim_label

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

    # Fill missing statistics with 0 for indicators where NaN/NoData implies "no hazard"
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
    if "indicator_variable" in final_df.columns:
        fill_mask = ~final_df["indicator_variable"].isin(
            INDICATORS_THAT_DONT_FILL_ZERO | {"hi", "spi3"}
        )
    else:
        fill_mask = pd.Series(True, index=final_df.index)

    final_df.loc[fill_mask, stat_cols] = final_df.loc[fill_mask, stat_cols].fillna(0)

    # Ensure BASE_COLS are at the front, followed by any extra columns
    # Filter BASE_COLS to only those that actually exist in final_df
    existing_base = [c for c in BASE_COLS if c in final_df.columns]
    extra_cols = [c for c in final_df.columns if c not in existing_base]
    final_df = final_df[existing_base + extra_cols]

    final_df.to_csv(out_csv, index=False, encoding="utf-8-sig")
    print(f"Successfully saved {len(final_df)} rows to {out_csv}", flush=True)


def load_tif_as_dataarray(
    tif_path: str,
) -> Tuple[xr.DataArray, np.ndarray, np.ndarray, Affine, Any, Optional[float]]:
    with rasterio.open(tif_path) as src:
        data = src.read(1)
        nodata = src.nodata
        transform = src.transform
        height, width = data.shape

    cols = np.arange(width)
    rows = np.arange(height)
    lons = transform.c + (cols + 0.5) * transform.a
    lats = transform.f + (rows + 0.5) * transform.e

    if lats[0] < lats[-1]:
        lats = lats[::-1]
        data = data[::-1, :]

    da = xr.DataArray(data, dims=("lat", "lon"), coords={"lat": lats, "lon": lons})
    rb_poly = raster_bounds_polygon(lats, lons)
    return da, lats, lons, transform, rb_poly, nodata


def process_tif_file_region_by_region_to_part(
    *,
    tif_path: str,
    indicator_file: str,
    indicator_variable: str,
    hazard_type: str,
    hazard_indicator: str,
    adm_gdf: gpd.GeoDataFrame,
    adm_level: str,
    part_csv: str,
    cache_dir: str,
    scenario_name: Optional[str],
    return_period: Optional[int],
    fixed_ensemble: Optional[str] = None,
    append: bool = False,
):
    da, lats, lons, transform, rb_poly, nodata_value = load_tif_as_dataarray(tif_path)

    os.makedirs(os.path.dirname(part_csv), exist_ok=True)
    if not append and os.path.exists(part_csv):
        os.remove(part_csv)
    header_written = os.path.exists(part_csv)

    spill_root = os.path.join(
        cache_dir,
        "_spill_region_by_region",
        f"{safe_slug(hazard_type)}__{safe_slug(hazard_indicator)}__{adm_level}",
    )
    cleanup_dir(spill_root)
    os.makedirs(spill_root, exist_ok=True)

    rows = []
    for _, reg in tqdm(
        adm_gdf.iterrows(),
        total=len(adm_gdf),
        desc=f"{adm_level} regions | {hazard_type}/{hazard_indicator}",
        unit="region",
        leave=False,
    ):
        adm_name = reg["adm_name"]
        adm_code = reg.get("adm_code", np.nan)
        shape_id = reg.get("shape_id", np.nan)

        stats = compute_region_stats_for_slice(
            da_slice=da,
            lats=lats,
            lons=lons,
            transform=transform,
            region_geom=reg.geometry,
            adm_name=adm_name,
            adm_code=adm_code,
            shape_id=shape_id,
            raster_bounds_poly=rb_poly,
            hazard_type=hazard_type,
            hazard_indicator=hazard_indicator,
            nodata_value=nodata_value,
            spill_root=spill_root,
        )

        qs = stats["qs"]
        row = {
            "adm_name": adm_name,
            "adm_code": stats.get("adm_code", np.nan),
            "shape_id": stats.get("shape_id", np.nan),
            "adm_level": adm_level,
            "gwl": np.nan,
            "return_period": return_period if return_period is not None else np.nan,
            "indicator_file": indicator_file,
            "indicator_variable": indicator_variable,
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
            "ensemble": fixed_ensemble,
            "season": np.nan,
            "scenario_name": scenario_name if scenario_name is not None else np.nan,
        }

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
    cleanup_dir(spill_root)


def main():
    # reduce native memory/thread oversubscription
    os.environ.setdefault("GDAL_CACHEMAX", GDAL_CACHEMAX)
    os.environ.setdefault("OMP_NUM_THREADS", "1")
    os.environ.setdefault("OPENBLAS_NUM_THREADS", "1")
    os.environ.setdefault("MKL_NUM_THREADS", "1")
    os.environ.setdefault("NUMEXPR_MAX_THREADS", "1")
    os.environ.setdefault("HDF5_USE_FILE_LOCKING", "FALSE")

    parser = argparse.ArgumentParser(
        description="Precompute ADM hazard indicators from input folder"
    )
    parser.add_argument(
        "--input_folder",
        default="tests/tests_data",
        help="Base input folder (same as run_app base_dir).",
    )
    args = parser.parse_args()

    base_dir = resolve_base_dir(args.input_folder)
    hazards_dir = os.path.join(base_dir, "hazards")
    hazards_config_dir = os.path.join(hazards_dir, "config")
    hazards_indicators_dir = os.path.join(hazards_dir, "indicators")

    adm1_path = find_adm_boundary(base_dir, "ADM1")
    adm2_path = find_adm_boundary(base_dir, "ADM2")

    out_csv = os.path.join(hazards_dir, "precomputed_adm_indicators.csv")
    cache_dir = os.path.join(hazards_dir, "_precompute_cache")
    parts_dir = os.path.join(hazards_dir, "_precompute_parts")

    cleanup_dir(cache_dir)
    cleanup_dir(parts_dir)
    os.makedirs(cache_dir, exist_ok=True)
    os.makedirs(parts_dir, exist_ok=True)

    hazard_configs = load_hazard_configs(hazards_config_dir)
    indicators = discover_indicators(hazard_configs, hazards_indicators_dir)

    adm_codes = load_adm_codes(base_dir)
    adm1 = load_adm_shapefile(adm1_path, adm_codes)
    adm2 = load_adm_shapefile(adm2_path, adm_codes)

    for i, ind in enumerate(indicators, 1):
        ht = ind["hazard_type"]
        hi = ind["hazard_indicator"]
        base = f"{safe_slug(ht)}__{safe_slug(hi)}"
        part1 = os.path.join(parts_dir, f"{base}__ADM1.csv")
        part2 = os.path.join(parts_dir, f"{base}__ADM2.csv")

        print(f"[{i}/{len(indicators)}] {ht}/{hi}", flush=True)

        if ind["source"] == "nc":
            nc_path = ind["data_path"]
            print(f"  - NC: {os.path.basename(nc_path)}", flush=True)
            process_nc_file_region_by_region_to_part(
                nc_path=nc_path,
                indicator_file=ind["indicator_file"],
                indicator_variable=ind["indicator_variable"],
                hazard_type=ht,
                hazard_indicator=hi,
                adm_gdf=adm1,
                adm_level="ADM1",
                part_csv=part1,
                cache_dir=cache_dir,
                fixed_ensemble=ind.get("fixed_ensemble"),
                append=False,
            )
            process_nc_file_region_by_region_to_part(
                nc_path=nc_path,
                indicator_file=ind["indicator_file"],
                indicator_variable=ind["indicator_variable"],
                hazard_type=ht,
                hazard_indicator=hi,
                adm_gdf=adm2,
                adm_level="ADM2",
                part_csv=part2,
                cache_dir=cache_dir,
                fixed_ensemble=ind.get("fixed_ensemble"),
                append=False,
            )
        elif ind["source"] == "tif":
            metadata = ind["metadata"]
            indicator_dir = ind["indicator_dir"]
            for adm_level, adm_gdf, part_csv in (
                ("ADM1", adm1, part1),
                ("ADM2", adm2, part2),
            ):
                for j, row in metadata.iterrows():
                    tif_path = select_tif_file(indicator_dir, row["hazard_file"])
                    scenario_name = row.get("scenario_name", None)
                    return_period = row.get("return_period", None)
                    if pd.isna(scenario_name):
                        scenario_name = None
                    if pd.isna(return_period):
                        return_period = None
                    else:
                        return_period = int(return_period)
                    process_tif_file_region_by_region_to_part(
                        tif_path=tif_path,
                        indicator_file=ind["indicator_file"],
                        indicator_variable=ind["indicator_variable"],
                        hazard_type=ht,
                        hazard_indicator=hi,
                        adm_gdf=adm_gdf,
                        adm_level=adm_level,
                        part_csv=part_csv,
                        cache_dir=cache_dir,
                        scenario_name=scenario_name,
                        return_period=return_period,
                        fixed_ensemble=ind.get("fixed_ensemble"),
                        append=j > 0,
                    )
        elif ind["source"] == "tif_single":
            tif_path = select_tif_file(hazards_indicators_dir, ind["indicator_file"])
            for adm_level, adm_gdf, part_csv in (
                ("ADM1", adm1, part1),
                ("ADM2", adm2, part2),
            ):
                process_tif_file_region_by_region_to_part(
                    tif_path=tif_path,
                    indicator_file=ind["indicator_file"],
                    indicator_variable=ind["indicator_variable"],
                    hazard_type=ht,
                    hazard_indicator=hi,
                    adm_gdf=adm_gdf,
                    adm_level=adm_level,
                    part_csv=part_csv,
                    cache_dir=cache_dir,
                    scenario_name=None,
                    return_period=None,
                    fixed_ensemble=ind.get("fixed_ensemble"),
                    append=False,
                )
        else:
            raise ValueError(f"Unknown indicator source: {ind['source']}")

        cleanup_dir(os.path.join(cache_dir, "_spill_region_by_region"))

    part_files = sorted(glob.glob(os.path.join(parts_dir, "*.csv")))
    if not part_files:
        raise RuntimeError(f"No part files found in {parts_dir}")

    concat_parts_to_final(part_files, out_csv)
    print(f"Saved: {out_csv}", flush=True)


if __name__ == "__main__":
    main()
