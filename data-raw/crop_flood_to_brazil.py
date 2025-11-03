#!/usr/bin/env python3
"""
Crop Flood NetCDF to Brazil Borders with 50km Buffer

This script crops the ensemble_return_period.nc file to Brazil ADM0 boundaries + 50km buffer.
Uses Dask for parallel processing to handle large NetCDF files efficiently.

Usage:
    python crop_flood_to_brazil.py

    Or with custom parameters:
    python crop_flood_to_brazil.py --input /path/to/input.nc --output /path/to/output.nc --buffer 100
"""

import os
import sys
import argparse
import time
import geopandas as gpd
import xarray as xr
import rioxarray as rxr
import numpy as np
from shapely.geometry import box
import dask
import dask.array as da
from dask.distributed import Client
from dask.diagnostics import ProgressBar


def parse_args():
    """Parse command line arguments."""
    parser = argparse.ArgumentParser(
        description="Crop flood NetCDF to Brazil boundaries with buffer"
    )
    parser.add_argument(
        "--brazil-shp",
        default="../workspace/Brazil Borders/geoBoundaries-BRA-ADM0-all/geoBoundaries-BRA-ADM0.shp",
        help="Path to Brazil ADM0 shapefile",
    )
    parser.add_argument(
        "--input",
        default="../workspace/hazards/Flood/depth(cm)/ensemble_return_period.nc",
        help="Path to input NetCDF file",
    )
    parser.add_argument(
        "--output",
        default="../workspace/hazards/Flood/depth(cm)/ensemble_return_period_brazil_cropped.nc",
        help="Path to output NetCDF file",
    )
    parser.add_argument(
        "--buffer",
        type=int,
        default=50,
        help="Buffer distance in kilometers (default: 50)",
    )
    parser.add_argument(
        "--n-workers",
        type=int,
        default=None,
        help="Number of Dask workers (default: all available cores)",
    )
    parser.add_argument(
        "--clip-chunk-size",
        type=int,
        default=1024,
        help="Chunk size for reading/clipping operations (default: 1024)",
    )
    parser.add_argument(
        "--write-chunk-size",
        type=int,
        default=1024,
        help="Chunk size for writing operations (default: 1024)",
    )
    parser.add_argument(
        "--no-dask-clip",
        action="store_true",
        help="Disable Dask client for clipping operations",
    )

    return parser.parse_args()


def load_and_buffer_brazil(shapefile_path, buffer_km):
    """
    Load Brazil boundaries and create buffer.

    Args:
        shapefile_path: Path to Brazil ADM0 shapefile
        buffer_km: Buffer distance in kilometers

    Returns:
        gpd.GeoDataFrame: Buffered geometry
    """
    print(f"[INFO] Loading Brazil boundaries from: {shapefile_path}")
    brazil_gdf = gpd.read_file(shapefile_path)

    print(f"[INFO] Original CRS: {brazil_gdf.crs}")

    # Ensure we're in WGS84 (EPSG:4326)
    if brazil_gdf.crs != "EPSG:4326":
        print(f"[INFO] Transforming to EPSG:4326...")
        brazil_gdf = brazil_gdf.to_crs("EPSG:4326")

    # Union all polygons into a single geometry
    print(f"[INFO] Unioning {len(brazil_gdf)} polygon(s)...")
    brazil_union = brazil_gdf.geometry.union_all()
    print(f"[INFO] Brazil union geometry ready")

    # Create buffer in meters
    print(f"[INFO] Creating {buffer_km}km buffer...")
    brazil_metric = gpd.GeoSeries([brazil_union], crs="EPSG:4326").to_crs("EPSG:3857")
    buffer_meters = buffer_km * 1000
    brazil_buffered = brazil_metric.buffer(buffer_meters)
    brazil_buffered_wgs84 = brazil_buffered.to_crs("EPSG:4326")

    print(f"[INFO] Buffer created successfully")
    print(f"[INFO] Buffered geometry bounds: {brazil_buffered_wgs84.iloc[0].bounds}")

    return gpd.GeoDataFrame(geometry=brazil_buffered_wgs84, crs="EPSG:4326")


def open_netcdf(input_path, chunk_lat, chunk_lon):
    """
    Open NetCDF file with optimized chunking.

    Args:
        input_path: Path to input NetCDF file
        chunk_lat: Chunk size for latitude dimension
        chunk_lon: Chunk size for longitude dimension

    Returns:
        xr.Dataset: Opened dataset with spatial reference set
    """
    print(f"[INFO] Opening NetCDF file: {input_path}")
    ds = xr.open_dataset(input_path, chunks={"lat": chunk_lat, "lon": chunk_lon})

    print(f"[INFO] NetCDF dimensions: {dict(ds.sizes)}")
    print(f"[INFO] NetCDF variables: {list(ds.data_vars)}")
    print(f"[INFO] NetCDF coordinates: {list(ds.coords)}")

    # Set up spatial reference for rioxarray
    if "lat" in ds.coords and "lon" in ds.coords:
        if "spatial_ref" not in ds.coords:
            ds = ds.rio.write_crs("EPSG:4326")
        ds = ds.rio.set_spatial_dims(x_dim="lon", y_dim="lat")

    print(f"[INFO] NetCDF CRS: {ds.rio.crs}")
    print(f"[INFO] NetCDF bounds: {ds.rio.bounds()}")

    return ds


def crop_dataset(ds, clip_geometry, use_dask_clip, n_workers, buffer_km):
    """
    Crop dataset to geometry bounds.

    Args:
        ds: xarray Dataset to crop
        clip_geometry: GeoDataFrame with clipping geometry
        use_dask_clip: Whether to use Dask client for clipping
        n_workers: Number of Dask workers
        buffer_km: Buffer distance (for logging)

    Returns:
        xr.Dataset: Cropped dataset
    """
    print(f"[INFO] Cropping NetCDF to Brazil boundaries + {buffer_km}km buffer...")

    # Optionally start Dask client for clipping
    clip_client = None
    if use_dask_clip:
        print(f"[INFO] Starting Dask client for parallel clipping operations...")
        clip_client = Client(
            n_workers=n_workers, threads_per_worker=2, memory_limit="4GB"
        )
        print(f"[INFO] Dask dashboard for clipping: {clip_client.dashboard_link}")

    try:
        # Get bounding box
        bbox = clip_geometry.total_bounds
        lon_min, lat_min, lon_max, lat_max = bbox

        print(f"[INFO] Using dask-friendly clip_box() method...")
        print(
            f"[INFO] Bounding box: lon=[{lon_min:.2f}, {lon_max:.2f}], lat=[{lat_min:.2f}, {lat_max:.2f}]"
        )

        # Check if data is chunked with dask
        for var in ds.data_vars:
            is_dask = isinstance(ds[var].data, da.Array)
            print(
                f"[INFO] Variable '{var}' is {'dask' if is_dask else 'NOT dask'} array"
            )
            if is_dask:
                print(f"[INFO]   Chunks: {ds[var].chunks}")

        clip_start = time.time()

        # Use rio.clip_box() - dask-friendly and stays lazy
        print(f"[INFO] Cropping to bounding box with rio.clip_box() (dask-friendly)...")
        ds_cropped = ds.rio.clip_box(
            minx=lon_min, miny=lat_min, maxx=lon_max, maxy=lat_max
        )

        clip_elapsed = time.time() - clip_start
        print(
            f"[INFO] clip_box() completed in {clip_elapsed:.1f} seconds (should be fast/instant if lazy)"
        )

        # Verify it's still dask
        for var in ds_cropped.data_vars:
            is_dask = isinstance(ds_cropped[var].data, da.Array)
            print(
                f"[INFO] After clip_box, '{var}' is {'still dask' if is_dask else 'NOT dask anymore'}"
            )

        print(f"[INFO] Cropping complete")
        print(f"[INFO] Original dimensions: {dict(ds.sizes)}")
        print(f"[INFO] Cropped dimensions: {dict(ds_cropped.sizes)}")
        print(f"[INFO] Cropped bounds: {ds_cropped.rio.bounds()}")

        return ds_cropped

    finally:
        # Close dask client if we started one
        if clip_client is not None:
            print(f"[INFO] Closing Dask client used for clipping...")
            clip_client.close()


def write_netcdf(
    ds_cropped,
    output_path,
    original_ds,
    buffer_km,
    n_workers,
    write_chunk_lat,
    write_chunk_lon,
    input_path,
):
    """
    Write cropped dataset to NetCDF using Dask for parallel processing.

    Args:
        ds_cropped: Cropped xarray Dataset
        output_path: Path to output NetCDF file
        original_ds: Original dataset (for attributes)
        buffer_km: Buffer distance (for metadata)
        n_workers: Number of Dask workers
        write_chunk_lat: Chunk size for latitude dimension
        write_chunk_lon: Chunk size for longitude dimension
        input_path: Original input path (for metadata)
    """
    print(f"[INFO] Saving cropped NetCDF to: {output_path}")

    # Ensure output directory exists
    os.makedirs(os.path.dirname(output_path), exist_ok=True)

    # Start Dask client for parallel processing
    print(f"[INFO] Starting Dask client with {n_workers or 'all available'} workers...")
    client = Client(n_workers=n_workers, threads_per_worker=2, memory_limit="4GB")
    dashboard_url = client.dashboard_link
    print(f"[INFO] Dask dashboard available at: {dashboard_url}")
    print(f"[INFO] Open this URL in your browser to monitor progress")

    try:
        # Optimize chunking for writing
        print(
            f"[INFO] Rechunking data for optimal writing (chunks: lat={write_chunk_lat}, lon={write_chunk_lon})..."
        )
        ds_cropped_write = ds_cropped.chunk(
            {
                "lat": write_chunk_lat,
                "lon": write_chunk_lon,
                "ensemble": 1,
                "GWL": 1,
                "return_period": 1,
            }
        )

        # Prepare encoding
        encoding = {}
        for var in ds_cropped_write.data_vars:
            encoding[var] = {
                "zlib": True,
                "complevel": 1,  # Lower compression = faster
                "dtype": "float32",
                "_FillValue": np.float32(np.nan),
                "chunksizes": (1, 1, 1, write_chunk_lat, write_chunk_lon),
            }

        # Update attributes
        ds_cropped_write.attrs.update(
            {
                **original_ds.attrs,
                "note": f"Cropped to Brazil ADM0 boundaries with {buffer_km}km buffer. Original file: {os.path.basename(input_path)}",
            }
        )

        # Print dataset info
        print(f"[INFO] Preparing NetCDF write operation...")
        print(f"[INFO] Dataset size: {dict(ds_cropped_write.sizes)}")
        total_points = (
            ds_cropped_write.sizes["lat"]
            * ds_cropped_write.sizes["lon"]
            * ds_cropped_write.sizes["GWL"]
            * ds_cropped_write.sizes["return_period"]
        )
        print(f"[INFO] Total data points: {total_points:,}")

        # Check chunk info
        for var in ds_cropped_write.data_vars:
            n_lat_chunks = len(ds_cropped_write[var].chunks[3])
            n_lon_chunks = len(ds_cropped_write[var].chunks[4])
            print(
                f"[INFO] Variable '{var}' has {n_lat_chunks} lat chunks, {n_lon_chunks} lon chunks"
            )
            total_chunks = 1
            for dim_chunks in ds_cropped_write[var].chunks:
                total_chunks *= len(dim_chunks)
            print(f"[INFO]   Total chunks to write: {total_chunks}")

        print(f"[INFO] Starting parallel write operation...")
        print(f"[INFO] Monitor progress at: {client.dashboard_link}")
        print(
            f"[INFO] This will take several minutes - watch the dashboard for task progress..."
        )

        write_start = time.time()

        # Write to NetCDF
        print(f"[INFO] Writing NetCDF file with parallel dask computation...")
        print(
            f"[INFO] You should see tasks appearing in the dashboard now: {client.dashboard_link}"
        )

        try:
            with ProgressBar():
                ds_cropped_write.to_netcdf(
                    output_path, encoding=encoding, engine="netcdf4", compute=True
                )
        except Exception as e:
            # Fallback without progress bar
            print(f"[WARN] ProgressBar failed: {e}, writing without progress bar...")
            ds_cropped_write.to_netcdf(
                output_path, encoding=encoding, engine="netcdf4", compute=True
            )

        write_elapsed = time.time() - write_start
        print(
            f"[INFO] Write completed in {write_elapsed/60:.1f} minutes ({write_elapsed:.1f} seconds)"
        )
        print(f"[OK] Cropped NetCDF saved successfully!")

    finally:
        # Close dask client
        print(f"[INFO] Closing Dask client...")
        client.close()


def print_size_comparison(input_path, output_path):
    """Print file size comparison."""
    if os.path.exists(output_path):
        original_size = os.path.getsize(input_path) / (1024**3)  # GB
        cropped_size = os.path.getsize(output_path) / (1024**3)  # GB
        print(f"[INFO] Original file size: {original_size:.2f} GB")
        print(f"[INFO] Cropped file size: {cropped_size:.2f} GB")
        print(f"[INFO] Size reduction: {(1 - cropped_size/original_size)*100:.1f}%")


def main():
    """Main execution function."""
    args = parse_args()

    print("=" * 80)
    print("Crop Flood NetCDF to Brazil Borders")
    print("=" * 80)
    print(f"Configuration:")
    print(f"  Brazil shapefile: {args.brazil_shp}")
    print(f"  Input NetCDF: {args.input}")
    print(f"  Output NetCDF: {args.output}")
    print(f"  Buffer: {args.buffer} km")
    print(f"  Workers: {args.n_workers or 'all available'}")
    print(f"  Clip chunk size: {args.clip_chunk_size}")
    print(f"  Write chunk size: {args.write_chunk_size}")
    print("=" * 80)

    overall_start = time.time()

    try:
        # Step 1: Load and buffer Brazil boundaries
        clip_geometry = load_and_buffer_brazil(args.brazil_shp, args.buffer)

        # Step 2: Open NetCDF
        ds = open_netcdf(args.input, args.clip_chunk_size, args.clip_chunk_size)

        # Step 3: Crop dataset
        ds_cropped = crop_dataset(
            ds, clip_geometry, not args.no_dask_clip, args.n_workers, args.buffer
        )

        # Step 4: Write to NetCDF
        write_netcdf(
            ds_cropped,
            args.output,
            ds,
            args.buffer,
            args.n_workers,
            args.write_chunk_size,
            args.write_chunk_size,
            args.input,
        )

        # Step 5: Print size comparison
        print_size_comparison(args.input, args.output)

        overall_elapsed = time.time() - overall_start
        print(f"\n[SUCCESS] Total processing time: {overall_elapsed/60:.1f} minutes")
        print("=" * 80)

    except Exception as e:
        print(f"\n[ERROR] Processing failed: {e}")
        import traceback

        traceback.print_exc()
        sys.exit(1)


if __name__ == "__main__":
    main()
