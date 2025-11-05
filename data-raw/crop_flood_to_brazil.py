#!/usr/bin/env python3
"""
Crop Flood NetCDF to Brazil Borders with buffer using Dask correctly.

- One Dask Client for the whole run.
- Keep everything lazy; compute only at the end.
- Use client.compute()/client.progress() so tasks appear in the dashboard.
- Per-variable 'chunksizes' to avoid eager materialization.
"""

import os
import sys
import argparse
import time
import warnings
import shutil

import geopandas as gpd
import xarray as xr
import rioxarray as rxr  # noqa: F401  (required to enable .rio)
import numpy as np
from shapely.geometry import box
from shapely.ops import unary_union

import dask
import dask.array as da
from dask.distributed import Client, wait


# -------------------------
# CLI
# -------------------------
def parse_args():
    p = argparse.ArgumentParser(
        description="Crop flood NetCDF to Brazil boundaries with buffer"
    )
    p.add_argument(
        "--brazil-shp",
        default="workspace/Brazil Borders/geoBoundaries-BRA-ADM0-all/geoBoundaries-BRA-ADM0.shp",
        help="Path to Brazil ADM0 shapefile",
    )
    p.add_argument(
        "--input",
        default="workspace/hazards/Flood/depth(cm)/ensemble_return_period.nc",
        help="Path to input NetCDF file",
    )
    p.add_argument(
        "--output",
        default="workspace/hazards/Flood/depth(cm)/ensemble_return_period_brazil_cropped.nc",
        help="Path to output NetCDF file",
    )
    p.add_argument(
        "--buffer", type=int, default=50, help="Buffer distance in kilometers"
    )
    p.add_argument("--n-workers", type=int, default=None, help="Number of Dask workers")
    p.add_argument(
        "--clip-chunk-size",
        type=int,
        default=4096,
        help="Chunk size for lat/lon when opening",
    )
    p.add_argument(
        "--write-chunk-size",
        type=int,
        default=4096,
        help="Chunk size for lat/lon when writing",
    )
    p.add_argument(
        "--no-dask",
        action="store_true",
        help="Run without a distributed Client (uses threaded scheduler)",
    )
    return p.parse_args()


# -------------------------
# Geospatial helpers
# -------------------------
def load_and_buffer_brazil(shapefile_path: str, buffer_km: int) -> gpd.GeoDataFrame:
    print(f"[INFO] Loading Brazil boundaries: {shapefile_path}")
    gdf = gpd.read_file(shapefile_path)

    if gdf.empty:
        raise ValueError("Brazil shapefile loaded empty.")

    if gdf.crs is None:
        raise ValueError("Brazil shapefile has no CRS. Expected EPSG:4326.")

    if gdf.crs.to_string() != "EPSG:4326":
        gdf = gdf.to_crs("EPSG:4326")

    # union (shapely 2.0 has .union_all; fallback to unary_union)
    try:
        union_geom = gdf.geometry.union_all()
    except Exception:
        union_geom = unary_union(gdf.geometry)

    # buffer in meters in a projected CRS
    metric = gpd.GeoSeries([union_geom], crs="EPSG:4326").to_crs("EPSG:3857")
    buffer_m = buffer_km * 1000
    buffered = metric.buffer(buffer_m)
    buffered_wgs84 = buffered.to_crs("EPSG:4326")

    out = gpd.GeoDataFrame(geometry=buffered_wgs84, crs="EPSG:4326")
    bounds = out.geometry.iloc[0].bounds
    print(f"[INFO] Buffered bounds (lon_min, lat_min, lon_max, lat_max): {bounds}")
    return out


def bbox_from_gdf(gdf: gpd.GeoDataFrame):
    xmin, ymin, xmax, ymax = gdf.total_bounds
    return xmin, ymin, xmax, ymax


# -------------------------
# NetCDF / Xarray helpers
# -------------------------
def open_netcdf_chunked(path: str, chunk_lat: int, chunk_lon: int) -> xr.Dataset:
    print(f"[INFO] Opening NetCDF: {path}")
    ds = xr.open_dataset(path, chunks={"lat": chunk_lat, "lon": chunk_lon})

    # attach spatial info for rioxarray
    if "lat" in ds.coords and "lon" in ds.coords:
        if "spatial_ref" not in ds.coords:
            ds = ds.rio.write_crs("EPSG:4326")
        ds = ds.rio.set_spatial_dims(x_dim="lon", y_dim="lat")

    print(f"[INFO] Dimensions: {dict(ds.sizes)}")
    print(f"[INFO] Variables: {list(ds.data_vars)}")
    try:
        print(f"[INFO] Bounds: {ds.rio.bounds()}")
    except Exception as e:
        warnings.warn(f"Could not read .rio.bounds(): {e}")

    # confirm dask-backed
    for v in ds.data_vars:
        print(
            f"[INFO] '{v}': {'dask' if isinstance(ds[v].data, da.Array) else 'numpy'}-backed"
        )
    return ds


def dask_friendly_clip_box(ds: xr.Dataset, bbox):
    xmin, ymin, xmax, ymax = bbox
    print(f"[INFO] clip_box lon=[{xmin:.3f},{xmax:.3f}] lat=[{ymin:.3f},{ymax:.3f}]")
    t0 = time.time()
    out = ds.rio.clip_box(minx=xmin, miny=ymin, maxx=xmax, maxy=ymax)
    print(f"[INFO] clip_box applied lazily in {time.time()-t0:.2f}s")
    for v in out.data_vars:
        print(
            f"[INFO] After clip, '{v}': {'dask' if isinstance(out[v].data, da.Array) else 'numpy'}-backed"
        )
    return out


def build_encoding(ds: xr.Dataset, write_chunk: int) -> dict:
    """
    Per-variable chunksizes that match the variable dim order.
    Small dims -> 1, spatial dims -> write_chunk.
    """
    enc = {}
    for v, da_ in ds.data_vars.items():
        chunks = tuple(write_chunk if d in ("lat", "lon") else 1 for d in da_.dims)
        enc[v] = {
            "zlib": True,
            "complevel": 1,
            "dtype": "float32",
            "_FillValue": np.float32(np.nan),
            "chunksizes": chunks,
        }
    return enc


def try_select_engine(preferred="h5netcdf"):
    engine = preferred
    try:
        if preferred == "h5netcdf":
            import h5netcdf  # noqa: F401
        elif preferred == "netcdf4":
            import netCDF4  # noqa: F401
    except Exception:
        engine = "netcdf4" if preferred == "h5netcdf" else "scipy"
    return engine


# -------------------------
# Main
# -------------------------
def main():
    args = parse_args()

    print("=" * 80)
    print("Crop Flood NetCDF to Brazil Borders")
    print("=" * 80)
    print(f"Brazil shapefile : {args.brazil_shp}")
    print(f"Input NetCDF     : {args.input}")
    print(f"Output NetCDF    : {args.output}")
    print(f"Buffer           : {args.buffer} km")
    print(f"Workers          : {args.n_workers or 'auto'}")
    print(f"Open chunks      : {args.clip_chunk_size}")
    print(f"Write chunks     : {args.write_chunk_size}")
    print(
        f"Use Dask Client  : {not args.no-dask if hasattr(args, 'no-dask') else True}"
    )
    print("=" * 80)

    # safer slicing behavior for big chunks
    dask.config.set({"array.slicing.split_large_chunks": True})

    client = None
    if not args.no_dask:
        client = Client(
            n_workers=args.n_workers, threads_per_worker=2, memory_limit="4GB"
        )
        print(f"[INFO] Dask dashboard: {client.dashboard_link}")

    t_all = time.time()

    try:
        # 1) Clip geometry
        clip_gdf = load_and_buffer_brazil(args.brazil_shp, args.buffer)
        bbox = bbox_from_gdf(clip_gdf)

        # 2) Open dataset lazily with spatial metadata
        ds = open_netcdf_chunked(args.input, args.clip_chunk_size, args.clip_chunk_size)

        # 3) Lazy crop by bbox (keeps graph lazy)
        ds_cropped = dask_friendly_clip_box(ds, bbox)

        # 4) Rechunk for write; keep small dims at 1 to minimize task overhead
        #    If your dataset has other dims (e.g., 'ensemble','GWL','return_period'), leave them as 1-chunk.
        chunk_map = {"lat": args.write_chunk_size, "lon": args.write_chunk_size}
        for d in ds_cropped.dims:
            if d not in ("lat", "lon"):
                chunk_map[d] = -1  # single chunk
        ds_write = ds_cropped.chunk(chunk_map)

        # 5) Encoding and attrs
        encoding = build_encoding(ds_write, args.write_chunk_size)
        ds_write.attrs.update(
            {
                **ds.attrs,
                "note": f"Cropped to Brazil ADM0 + {args.buffer}km buffer from {os.path.basename(args.input)}",
            }
        )

        # 6) Ensure output folder exists
        os.makedirs(os.path.dirname(args.output), exist_ok=True)

        # 7) Choose engine
        engine = try_select_engine("h5netcdf")
        print(f"[INFO] Using engine: {engine}")

        # ================== PARALLEL SECTION ==================

        print("[INFO] Persisting chunks on cluster...")
        ds_p = ds_write.persist()
        wait(ds_p)

        # write Zarr (parallel-safe)
        zarr_path = args.output + ".zarr"
        if os.path.exists(zarr_path):
            shutil.rmtree(zarr_path)

        print("[INFO] Writing Zarr in parallel...")
        fut = client.compute(
            ds_p.to_zarr(zarr_path, mode="w", consolidated=True, compute=False)
        )
        wait(fut)

        # ================== SERIAL SECTION ==================

        print("[INFO] Converting Zarr → NetCDF (serial)...")
        tmp = xr.open_zarr(zarr_path, consolidated=True)
        tmp.to_netcdf(args.output, engine="netcdf4")

        print("[INFO] Done.")

        # 10) Size comparison
        if os.path.exists(args.output):
            in_sz = os.path.getsize(args.input) / (4096**3)
            out_sz = os.path.getsize(args.output) / (4096**3)
            print(f"[INFO] Input size : {in_sz:.2f} GB")
            print(f"[INFO] Output size: {out_sz:.2f} GB")
            if in_sz > 0:
                print(f"[INFO] Reduction  : {(1 - out_sz / in_sz) * 100:.1f}%")

        print(f"[SUCCESS] Total time: {(time.time() - t_all)/60:.1f} min")

    finally:
        if client is not None:
            client.close()


if __name__ == "__main__":
    try:
        main()
    except KeyboardInterrupt:
        print("\n[INTERRUPTED] Exiting.")
        sys.exit(130)
