#!/usr/bin/env python3
import os
import glob
import gc
import itertools
import warnings
from typing import Dict, List, Tuple, Any, Optional

import numpy as np
import pandas as pd
import geopandas as gpd
import xarray as xr

import rasterio.features
from rasterio.transform import from_bounds
from rasterio.env import Env
from affine import Affine
from unidecode import unidecode
from tqdm import tqdm

warnings.filterwarnings(
    "ignore", message="The specified chunks separate the stored chunks"
)

# -----------------------
# CONFIG
# -----------------------
SEED = 123456789
Q_LIST = np.array([0.025, 0.05, 0.10, 0.50, 0.90, 0.95, 0.975], dtype=np.float64)

# Sample size per region (quantiles from sample)
K_SAMPLES_ADM1 = 1024
K_SAMPLES_ADM2 = 128

# Block size for streaming reads (tune to your RAM / IO)
BLOCK_LAT = 4096
BLOCK_LON = 4096

# If region-id raster would exceed this in-RAM size, use memmap instead
MAX_RID_RAM_BYTES = 1_200_000_000  # ~1.2GB


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


# -----------------------
# Deterministic bottom-k sampling (splitmix64)
# -----------------------
def _splitmix64(x: np.ndarray) -> np.ndarray:
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
    h1: np.ndarray, v1: np.ndarray, h2: np.ndarray, v2: np.ndarray, k: int
) -> Tuple[np.ndarray, np.ndarray]:
    if h1.size == 0:
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
    return np.asarray(out_h, np.uint64), np.asarray(out_v, np.float64)


# -----------------------
# Region-id rasterization (once per ADM)
#   - If huge, writes to memmap file (still accessible like ndarray)
# -----------------------
def build_region_id_raster(
    shapes: List[Tuple[Any, int]],
    lats: np.ndarray,
    lons: np.ndarray,
    adm_level: str,
    cache_dir: str,
) -> np.ndarray:
    os.makedirs(cache_dir, exist_ok=True)
    nlat, nlon = len(lats), len(lons)
    bytes_needed = nlat * nlon * np.dtype(np.int32).itemsize

    transform = grid_transform_from_latlon(lats, lons)

    # Decide RAM vs memmap
    if bytes_needed <= MAX_RID_RAM_BYTES:
        region_ids = np.zeros((nlat, nlon), dtype=np.int32)
        # One-shot rasterize (fast, but needs RAM)
        with Env(GDAL_CACHEMAX=128):
            region_ids[:, :] = rasterio.features.rasterize(
                shapes,
                out_shape=(nlat, nlon),
                transform=transform,
                fill=0,
                dtype=np.int32,
                all_touched=True,
            )
        return region_ids

    # Memmap path
    mm_path = os.path.join(cache_dir, f"region_ids_{adm_level}_{nlat}x{nlon}.dat")
    region_ids = np.memmap(mm_path, mode="w+", dtype=np.int32, shape=(nlat, nlon))

    # Blockwise rasterize into memmap (aligned using global transform + translation)
    n_block_lat = (nlat + BLOCK_LAT - 1) // BLOCK_LAT
    n_block_lon = (nlon + BLOCK_LON - 1) // BLOCK_LON
    total_blocks = n_block_lat * n_block_lon

    with Env(GDAL_CACHEMAX=128):
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
    return region_ids


# -----------------------
# Streaming accumulator update for one block
# -----------------------
def update_accumulators(
    values_block: np.ndarray,
    rid_block: np.ndarray,
    *,
    hazard_type: str,
    start_lat: int,
    start_lon: int,
    nlon_full: int,
    seed: int,
    k: int,
    count: np.ndarray,
    summ: np.ndarray,
    vmin: np.ndarray,
    vmax: np.ndarray,
    samp_h: List[np.ndarray],
    samp_v: List[np.ndarray],
    nodata_value: Optional[float],
):
    # hazard-specific filtering
    if hazard_type == "Heat":
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

    # deterministic bottom-k sample per region (approx quantiles)
    # global index per kept pixel within this block
    flat_idx = np.nonzero(m)[0].astype(np.int64, copy=False)
    h, w = values_block.shape
    ilat = flat_idx // w
    ilon = flat_idx - ilat * w
    glat = ilat.astype(np.uint64) + np.uint64(start_lat)
    glon = ilon.astype(np.uint64) + np.uint64(start_lon)
    gidx = glat * np.uint64(nlon_full) + glon
    hh = _splitmix64(gidx ^ np.uint64(seed))

    # sort by (rid asc, hash asc)
    order = np.lexsort((hh, r))
    r2 = r[order]
    v2 = v[order]
    h2 = hh[order]

    ids, idx, counts = np.unique(r2, return_index=True, return_counts=True)
    for rid, start, cnt in zip(ids, idx, counts):
        take = int(min(k, cnt))
        sh = h2[start : start + take]
        sv = v2[start : start + take]
        mh, mv = merge_bottomk(samp_h[rid], samp_v[rid], sh, sv, k)
        samp_h[rid] = mh
        samp_v[rid] = mv


# -----------------------
# Process one NetCDF file, one ADM level (single-process)
# -----------------------
def process_nc_file_single_process(
    nc_path: str,
    adm_gdf: gpd.GeoDataFrame,
    adm_level: str,
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

        region_ids = build_region_id_raster(
            shapes=shapes,
            lats=lats,
            lons=lons,
            adm_level=adm_level,
            cache_dir=cache_dir,
        )

        k = K_SAMPLES_ADM2 if adm_level.upper() == "ADM2" else K_SAMPLES_ADM1

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
            samp_h = [np.empty(0, dtype=np.uint64) for _ in range(n_regions + 1)]
            samp_v = [np.empty(0, dtype=np.float64) for _ in range(n_regions + 1)]

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

                        update_accumulators(
                            values_block,
                            rid_block,
                            hazard_type=hazard_type,
                            start_lat=i0,
                            start_lon=j0,
                            nlon_full=nlon,
                            seed=SEED,
                            k=k,
                            count=count,
                            summ=summ,
                            vmin=vmin,
                            vmax=vmax,
                            samp_h=samp_h,
                            samp_v=samp_v,
                            nodata_value=nodata_value,
                        )

                        pbar.update(1)

            # build rows
            rows = []
            for rid, name in rid_to_name.items():
                cnt = int(count[rid])
                if cnt == 0:
                    mn = mx = mean = np.nan
                    qs = [np.nan] * len(Q_LIST)
                else:
                    mn = float(vmin[rid])
                    mx = float(vmax[rid])
                    mean = float(summ[rid] / cnt)
                    sample_vals = samp_v[rid]
                    # NOTE: quantiles are approximate (sample-based)
                    qs = (
                        np.quantile(sample_vals, Q_LIST, method="linear")
                        if sample_vals.size
                        else [np.nan] * len(Q_LIST)
                    )

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
                    "median": float(qs[3]) if cnt else np.nan,
                    "p2_5": float(qs[0]) if cnt else np.nan,
                    "p5": float(qs[1]) if cnt else np.nan,
                    "p10": float(qs[2]) if cnt else np.nan,
                    "p90": float(qs[4]) if cnt else np.nan,
                    "p95": float(qs[5]) if cnt else np.nan,
                    "p97_5": float(qs[6]) if cnt else np.nan,
                    "ensemble": ensemble_used,
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

            del df, rows, count, summ, vmin, vmax, samp_h, samp_v
            gc.collect()

    finally:
        ds.close()


def main():
    # Sanity settings to reduce native-memory growth / thread oversubscription
    os.environ.setdefault("GDAL_CACHEMAX", "128")  # MB-ish (GDAL uses MB units)
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

    # write per-part then concat (simple version: append everything to OUT_CSV)
    # If you want per-part + done flags again, keep your part logic; call process_nc_file_single_process per part.
    if os.path.exists(OUT_CSV):
        os.remove(OUT_CSV)

    first = True
    for i, nc_path in enumerate(nc_files, 1):
        ht, hi = parse_hazard_from_path(nc_path)
        print(f"[{i}/{len(nc_files)}] {ht}/{hi} - {os.path.basename(nc_path)}")

        tmp1 = os.path.join(OUT_DIR, f"_tmp_{ht}__{hi}__ADM1.csv")
        tmp2 = os.path.join(OUT_DIR, f"_tmp_{ht}__{hi}__ADM2.csv")

        process_nc_file_single_process(
            nc_path, adm1, "ADM1", ENSEMBLE_FILTER, tmp1, CACHE_DIR
        )
        process_nc_file_single_process(
            nc_path, adm2, "ADM2", ENSEMBLE_FILTER, tmp2, CACHE_DIR
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
