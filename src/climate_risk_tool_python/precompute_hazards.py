#!/usr/bin/env python3
"""
Precompute hazard statistics aggregated over administrative regions (ADM1/ADM2).

This version is:
- Deterministic (no seed)
- Exact quantiles (no sampling)
- Memory-bounded for quantiles via on-disk spill to bucket files
- Uses a region-id raster (pixel -> region id) for performance
- Uses streaming blocks for hazard reads (does NOT load whole raster)
- Region-id raster is ALWAYS memmapped (same behavior on any machine)

Quantiles:
- Exact p2.5/p5/p10/median/p90/p95/p97.5 are computed from ALL values in each region.
- Implementation: during streaming pass we spill (rid, value) to N_BUCKETS binary files.
  Then we process each bucket file and compute exact quantiles per region.

Empty regions:
- If count==0, stats are NaN. We additionally diagnose:
  - "outside_raster_bounds" if polygon does not intersect raster bounds
  - "all_nodata_or_no_overlap" if it intersects bounds but got no valid pixels
"""

import os
import glob
import gc
import itertools
import warnings
import hashlib
import struct
from typing import Dict, List, Tuple, Any, Optional

import numpy as np
import pandas as pd
import geopandas as gpd
import xarray as xr

import rasterio.features
from rasterio.transform import from_bounds
from rasterio.env import Env
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

# Block size for streaming reads (tune to your RAM / IO)
BLOCK_LAT = 8192
BLOCK_LON = 8192

# Spill buckets for exact quantiles (tradeoff: more buckets => smaller peak RAM in bucket post-pass)
N_BUCKETS_ADM1 = 256
N_BUCKETS_ADM2 = 1024

# GDAL cache (MB-ish; rasterio/GDAL uses MB units for GDAL_CACHEMAX)
GDAL_CACHEMAX = "128"


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


def prepare_shapes(
    adm_gdf: gpd.GeoDataFrame,
) -> Tuple[List[Tuple[Any, int]], Dict[int, str]]:
    # row-order ids 1..N
    region_names = adm_gdf["region"].tolist()
    rid_to_name = {i + 1: name for i, name in enumerate(region_names)}
    shapes = [(geom, i + 1) for i, geom in enumerate(adm_gdf.geometry)]
    return shapes, rid_to_name


# -----------------------
# Grid / transform
# -----------------------
def ensure_lat_descending(da: xr.DataArray) -> xr.DataArray:
    lat = da["lat"].values
    if lat[0] < lat[-1]:
        # make row 0 == max-lat (north), matches raster conventions
        return da.isel(lat=slice(None, None, -1))
    return da


def grid_transform_from_latlon(lats: np.ndarray, lons: np.ndarray) -> Affine:
    # Assumes regular grid in EPSG:4326
    lon_min, lon_max = float(lons.min()), float(lons.max())
    lat_min, lat_max = float(lats.min()), float(lats.max())
    return from_bounds(lon_min, lat_min, lon_max, lat_max, len(lons), len(lats))


def raster_bounds_polygon(lats: np.ndarray, lons: np.ndarray) -> Any:
    # Bounds in lon/lat
    lon_min, lon_max = float(lons.min()), float(lons.max())
    lat_min, lat_max = float(lats.min()), float(lats.max())
    return box(lon_min, lat_min, lon_max, lat_max)


# -----------------------
# Region-id rasterization (ALWAYS memmap, blockwise fill)
# -----------------------
def _cache_key_for_region_ids(
    adm_level: str,
    nlat: int,
    nlon: int,
    adm_path: str,
    lats: np.ndarray,
    lons: np.ndarray,
) -> str:
    """
    Cache key to invalidate region_id raster if:
    - shapefile changes (mtime/size)
    - grid changes (dims + bounds)
    """
    st = os.stat(adm_path)
    payload = (
        f"{adm_level}|{nlat}x{nlon}|{os.path.abspath(adm_path)}|"
        f"mtime={int(st.st_mtime)}|size={st.st_size}|"
        f"latmin={float(lats.min())}|latmax={float(lats.max())}|"
        f"lonmin={float(lons.min())}|lonmax={float(lons.max())}"
    ).encode("utf-8")
    return hashlib.md5(payload).hexdigest()  # deterministic, short


def build_region_id_raster_memmap(
    shapes: List[Tuple[Any, int]],
    lats: np.ndarray,
    lons: np.ndarray,
    adm_level: str,
    adm_path: str,
    cache_dir: str,
) -> np.memmap:
    """
    Build (or reuse) a region_id raster aligned to the hazard grid.
    Always stored as an int32 memmap: (nlat, nlon)
    """
    os.makedirs(cache_dir, exist_ok=True)
    nlat, nlon = len(lats), len(lons)
    key = _cache_key_for_region_ids(adm_level, nlat, nlon, adm_path, lats, lons)
    mm_path = os.path.join(
        cache_dir, f"region_ids__{adm_level}__{nlat}x{nlon}__{key}.dat"
    )

    if os.path.exists(mm_path):
        return np.memmap(mm_path, mode="r", dtype=np.int32, shape=(nlat, nlon))

    transform = grid_transform_from_latlon(lats, lons)
    region_ids = np.memmap(mm_path, mode="w+", dtype=np.int32, shape=(nlat, nlon))

    n_block_lat = (nlat + BLOCK_LAT - 1) // BLOCK_LAT
    n_block_lon = (nlon + BLOCK_LON - 1) // BLOCK_LON
    total_blocks = n_block_lat * n_block_lon

    with Env(GDAL_CACHEMAX=int(GDAL_CACHEMAX)):
        with tqdm(
            total=total_blocks, desc=f"Rasterizing regions ({adm_level})", unit="block"
        ) as pbar:
            for i0 in range(0, nlat, BLOCK_LAT):
                i1 = min(i0 + BLOCK_LAT, nlat)
                for j0 in range(0, nlon, BLOCK_LON):
                    j1 = min(j0 + BLOCK_LON, nlon)
                    block_transform = transform * Affine.translation(j0, i0)
                    block = rasterio.features.rasterize(
                        shapes,
                        out_shape=(i1 - i0, j1 - j0),
                        transform=block_transform,
                        fill=0,
                        dtype=np.int32,
                        all_touched=True,
                    )
                    region_ids[i0:i1, j0:j1] = block
                    del block
                    pbar.update(1)
                region_ids.flush()

    return np.memmap(mm_path, mode="r", dtype=np.int32, shape=(nlat, nlon))


# -----------------------
# Exact quantiles spill format
#   Each bucket file is a sequence of chunks:
#     [uint32 n] [int32 rid * n] [float64 val * n]
# -----------------------
def _spill_write_chunk(path: str, r: np.ndarray, v: np.ndarray) -> None:
    if r.size == 0:
        return
    if r.dtype != np.int32:
        r = r.astype(np.int32, copy=False)
    if v.dtype != np.float64:
        v = v.astype(np.float64, copy=False)

    n = r.size
    with open(path, "ab") as f:
        f.write(struct.pack("<I", n))
        r.tofile(f)
        v.tofile(f)


def _spill_iter_chunks(path: str):
    with open(path, "rb") as f:
        while True:
            header = f.read(4)
            if not header:
                break
            (n,) = struct.unpack("<I", header)
            r = np.fromfile(f, dtype=np.int32, count=n)
            v = np.fromfile(f, dtype=np.float64, count=n)
            if r.size != n or v.size != n:
                raise IOError(f"Corrupt spill file (unexpected EOF): {path}")
            yield r, v


# -----------------------
# Streaming accumulator update for one block (exact reductions + spill for quantiles)
# -----------------------
def update_reductions_and_spill(
    values_block: np.ndarray,
    rid_block: np.ndarray,
    *,
    hazard_type: str,
    count: np.ndarray,
    summ: np.ndarray,
    vmin: np.ndarray,
    vmax: np.ndarray,
    nodata_value: Optional[float],
    spill_dir: str,
    n_buckets: int,
):
    # hazard-specific filtering
    if hazard_type == "Heat":
        # keep only Kelvin-like temps; anything <=300 => NaN
        values_block = np.where(values_block > 300, values_block, np.nan)

    v = values_block.ravel()
    r = rid_block.ravel()

    # Valid mask
    if np.issubdtype(v.dtype, np.floating):
        m = (r > 0) & ~np.isnan(v)
    else:
        if nodata_value is None:
            m = r > 0
        else:
            m = (r > 0) & (v != np.asarray(nodata_value, dtype=v.dtype))

    if not np.any(m):
        return

    r = r[m].astype(np.int32, copy=False)
    v = v[m].astype(np.float64, copy=False)

    # exact streaming stats
    count += np.bincount(r, minlength=count.size)
    summ += np.bincount(r, weights=v, minlength=summ.size)
    np.minimum.at(vmin, r, v)
    np.maximum.at(vmax, r, v)

    # spill to bucket files for exact quantiles
    # bucket by rid mod n_buckets
    buckets = (r % n_buckets).astype(np.int32, copy=False)
    # group by bucket without python loop per element
    order = np.argsort(buckets, kind="mergesort")
    buckets = buckets[order]
    r = r[order]
    v = v[order]

    # find bucket boundaries
    uniq_b, idx, cnts = np.unique(buckets, return_index=True, return_counts=True)
    for b, start, c in zip(uniq_b, idx, cnts):
        bpath = os.path.join(spill_dir, f"bucket_{int(b):04d}.bin")
        _spill_write_chunk(bpath, r[start : start + c], v[start : start + c])


def compute_exact_quantiles_from_spill(
    *,
    spill_dir: str,
    n_buckets: int,
    q_list: np.ndarray,
) -> Dict[int, np.ndarray]:
    """
    Returns dict: rid -> quantiles array (len(q_list))
    """
    out: Dict[int, np.ndarray] = {}

    for b in range(n_buckets):
        bpath = os.path.join(spill_dir, f"bucket_{b:04d}.bin")
        if not os.path.exists(bpath):
            continue

        # accumulate per rid within this bucket
        rid_to_chunks: Dict[int, List[np.ndarray]] = {}

        for r_chunk, v_chunk in _spill_iter_chunks(bpath):
            # group chunk by rid
            order = np.argsort(r_chunk, kind="mergesort")
            r_sorted = r_chunk[order]
            v_sorted = v_chunk[order]

            ids, idx, cnts = np.unique(r_sorted, return_index=True, return_counts=True)
            for rid, start, c in zip(ids.tolist(), idx.tolist(), cnts.tolist()):
                arr = v_sorted[start : start + c]
                rid_to_chunks.setdefault(int(rid), []).append(arr)

        # compute quantiles per rid
        for rid, chunks in rid_to_chunks.items():
            vals = np.concatenate(chunks) if len(chunks) > 1 else chunks[0]
            # exact quantiles (numpy sorts internally)
            qs = np.quantile(vals, q_list, method="linear")
            out[rid] = qs.astype(np.float64, copy=False)

        # free memory and delete bucket file
        rid_to_chunks.clear()
        try:
            os.remove(bpath)
        except OSError:
            pass
        gc.collect()

    return out


# -----------------------
# Process one NetCDF file, one ADM level (single-process)
# -----------------------
def process_nc_file_single_process_exact(
    nc_path: str,
    adm_gdf: gpd.GeoDataFrame,
    adm_level: str,
    adm_path: str,
    ensemble_filter: str,
    out_csv: str,
    cache_dir: str,
):
    hazard_type, hazard_indicator = parse_hazard_from_path(nc_path)

    ds = xr.open_dataset(nc_path, engine="netcdf4")  # no dask
    try:
        var_name = list(ds.data_vars.keys())[0]
        da = ds[var_name]

        if "lat" not in da.dims or "lon" not in da.dims:
            raise ValueError(f"Missing lat/lon dims in {nc_path}")

        # ensemble selection if present
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
        nlat, nlon = len(lats), len(lons)

        # nodata detection (helps for integer rasters)
        nodata_value = da.attrs.get("_FillValue", None)
        if (
            nodata_value is None
            and hasattr(da, "encoding")
            and isinstance(da.encoding, dict)
        ):
            nodata_value = da.encoding.get("_FillValue", None)

        # non-spatial dims
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

        # rasterize regions onto this grid ONCE
        shapes, rid_to_name = prepare_shapes(adm_gdf)
        n_regions = len(rid_to_name)

        region_ids = build_region_id_raster_memmap(
            shapes=shapes,
            lats=lats,
            lons=lons,
            adm_level=adm_level,
            adm_path=adm_path,
            cache_dir=cache_dir,
        )

        # bounds polygon for diagnostics
        rb_poly = raster_bounds_polygon(lats, lons)
        # only compute intersects once for all regions (cheap)
        intersects_bounds = adm_gdf.geometry.intersects(rb_poly).to_numpy()

        # spill buckets
        n_buckets = N_BUCKETS_ADM2 if adm_level.upper() == "ADM2" else N_BUCKETS_ADM1

        # output
        os.makedirs(os.path.dirname(out_csv), exist_ok=True)
        if os.path.exists(out_csv):
            os.remove(out_csv)
        header_written = False

        for dim_combo in tqdm(
            dim_combos,
            desc=f"{adm_level} combos | {hazard_type}/{hazard_indicator}",
            unit="combo",
        ):
            da_slice = da
            for kdim, vdim in dim_combo.items():
                da_slice = da_slice.sel({kdim: vdim})
            da_slice = da_slice.transpose("lat", "lon")

            # accumulators
            count = np.zeros(n_regions + 1, dtype=np.int64)
            summ = np.zeros(n_regions + 1, dtype=np.float64)
            vmin = np.full(n_regions + 1, np.inf, dtype=np.float64)
            vmax = np.full(n_regions + 1, -np.inf, dtype=np.float64)

            # per-combo spill dir (deleted at end)
            combo_key = hashlib.md5(
                repr(sorted(dim_combo.items())).encode("utf-8")
            ).hexdigest()
            spill_dir = os.path.join(
                cache_dir,
                "_spill",
                f"{hazard_type}__{hazard_indicator}__{adm_level}__{combo_key}",
            )
            os.makedirs(spill_dir, exist_ok=True)

            # stream blocks
            n_block_lat = (nlat + BLOCK_LAT - 1) // BLOCK_LAT
            n_block_lon = (nlon + BLOCK_LON - 1) // BLOCK_LON
            total_blocks = n_block_lat * n_block_lon

            with tqdm(
                total=total_blocks,
                desc=f"{adm_level} blocks | {hazard_type}/{hazard_indicator}",
                unit="block",
                leave=False,
            ) as pbar:
                for i0 in range(0, nlat, BLOCK_LAT):
                    i1 = min(i0 + BLOCK_LAT, nlat)
                    for j0 in range(0, nlon, BLOCK_LON):
                        j1 = min(j0 + BLOCK_LON, nlon)

                        values_block = da_slice.isel(
                            lat=slice(i0, i1), lon=slice(j0, j1)
                        ).values
                        rid_block = region_ids[i0:i1, j0:j1]

                        update_reductions_and_spill(
                            values_block,
                            rid_block,
                            hazard_type=hazard_type,
                            count=count,
                            summ=summ,
                            vmin=vmin,
                            vmax=vmax,
                            nodata_value=nodata_value,
                            spill_dir=spill_dir,
                            n_buckets=n_buckets,
                        )
                        pbar.update(1)

            # exact quantiles post-pass
            q_by_rid = compute_exact_quantiles_from_spill(
                spill_dir=spill_dir,
                n_buckets=n_buckets,
                q_list=Q_LIST,
            )

            # remove spill dir if empty
            try:
                os.rmdir(spill_dir)
            except OSError:
                pass

            # build rows
            rows = []
            for rid, name in rid_to_name.items():
                cnt = int(count[rid])
                if cnt == 0:
                    mn = mx = mean = np.nan
                    qs = None
                    # diagnostic
                    ib = bool(intersects_bounds[rid - 1])
                    empty_reason = (
                        "outside_raster_bounds"
                        if not ib
                        else "all_nodata_or_no_overlap"
                    )
                else:
                    mn = float(vmin[rid])
                    mx = float(vmax[rid])
                    mean = float(summ[rid] / cnt)
                    qs = q_by_rid.get(rid, None)
                    ib = bool(intersects_bounds[rid - 1])
                    empty_reason = np.nan

                row = {
                    "region": name,
                    "adm_level": adm_level,
                    "scenario_name": np.nan,
                    "hazard_return_period": np.nan,
                    "hazard_type": hazard_type,
                    "hazard_indicator": hazard_indicator,
                    "count": cnt,
                    "min": mn,
                    "max": mx,
                    "mean": mean,
                    "median": float(qs[3]) if (qs is not None) else np.nan,
                    "p2_5": float(qs[0]) if (qs is not None) else np.nan,
                    "p5": float(qs[1]) if (qs is not None) else np.nan,
                    "p10": float(qs[2]) if (qs is not None) else np.nan,
                    "p90": float(qs[4]) if (qs is not None) else np.nan,
                    "p95": float(qs[5]) if (qs is not None) else np.nan,
                    "p97_5": float(qs[6]) if (qs is not None) else np.nan,
                    "ensemble": ensemble_used,
                    "intersects_raster_bounds": ib,
                    "empty_reason": empty_reason,
                }

                # map common dims
                for dim_name, dim_val in dim_combo.items():
                    if dim_name in ("GWL", "gwl", "scenario"):
                        row["scenario_name"] = str(dim_val)
                    elif dim_name == "return_period":
                        row["hazard_return_period"] = int(dim_val)
                    else:
                        row[dim_name] = dim_val

                rows.append(row)

            df = pd.DataFrame(rows)

            # stable columns
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
                "intersects_raster_bounds",
                "empty_reason",
            ]
            for c in base_cols:
                if c not in df.columns:
                    df[c] = np.nan
            extra_cols = [c for c in df.columns if c not in base_cols]
            df = df[base_cols + extra_cols]

            df.to_csv(
                out_csv,
                mode="a",
                header=(not header_written),
                index=False,
                encoding="utf-8-sig",
            )
            header_written = True

            # cleanup
            del df, rows, q_by_rid, count, summ, vmin, vmax
            gc.collect()

    finally:
        ds.close()


def main():
    # Sanity settings to reduce native-memory growth / thread oversubscription
    os.environ.setdefault("GDAL_CACHEMAX", GDAL_CACHEMAX)
    os.environ.setdefault("OMP_NUM_THREADS", "1")
    os.environ.setdefault("OPENBLAS_NUM_THREADS", "1")
    os.environ.setdefault("MKL_NUM_THREADS", "1")
    os.environ.setdefault("NUMEXPR_MAX_THREADS", "1")
    os.environ.setdefault("HDF5_USE_FILE_LOCKING", "FALSE")

    HAZARDS_DIR = "workspace/demo_inputs_fullnc/hazards"
    ADM1_PATH = "workspace/demo_inputs_fullnc/areas/state/geoBoundaries-BRA-ADM1.shp"
    ADM2_PATH = (
        "workspace/demo_inputs_fullnc/areas/municipality/geoBoundaries-BRA-ADM2.shp"
    )
    ENSEMBLE_FILTER = "median"

    OUT_DIR = "workspace/Climate Data/Precomputed Regional Data"
    OUT_CSV = os.path.join(OUT_DIR, "precomputed_adm_hazards.csv")
    CACHE_DIR = os.path.join(OUT_DIR, "_cache_region_ids")

    # collect netcdfs
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

    if os.path.exists(OUT_CSV):
        os.remove(OUT_CSV)

    first = True
    for i, nc_path in enumerate(nc_files, 1):
        ht, hi = parse_hazard_from_path(nc_path)
        print(f"[{i}/{len(nc_files)}] {ht}/{hi} - {os.path.basename(nc_path)}")

        tmp1 = os.path.join(OUT_DIR, f"_tmp_{ht}__{hi}__ADM1.csv")
        tmp2 = os.path.join(OUT_DIR, f"_tmp_{ht}__{hi}__ADM2.csv")

        process_nc_file_single_process_exact(
            nc_path=nc_path,
            adm_gdf=adm1,
            adm_level="ADM1",
            adm_path=ADM1_PATH,
            ensemble_filter=ENSEMBLE_FILTER,
            out_csv=tmp1,
            cache_dir=CACHE_DIR,
        )
        process_nc_file_single_process_exact(
            nc_path=nc_path,
            adm_gdf=adm2,
            adm_level="ADM2",
            adm_path=ADM2_PATH,
            ensemble_filter=ENSEMBLE_FILTER,
            out_csv=tmp2,
            cache_dir=CACHE_DIR,
        )

        # append tmp files into OUT_CSV
        for tmp in (tmp1, tmp2):
            with open(tmp, "r", encoding="utf-8-sig") as f_in:
                lines = f_in.readlines()
            with open(OUT_CSV, "a", encoding="utf-8-sig") as f_out:
                if first:
                    f_out.writelines(lines)
                    first = False
                else:
                    f_out.writelines(lines[1:])  # skip header
            os.remove(tmp)

    print(f"Saved: {OUT_CSV}")


if __name__ == "__main__":
    main()
