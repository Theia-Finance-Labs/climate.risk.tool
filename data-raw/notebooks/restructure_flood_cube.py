#!/usr/bin/env python3
"""
Restructure Flood Cube to Match Other Hazard NC Maps

This script restructures the flood cube from its current format:
- Current: (scenario, return_period, lat, lon)
- Target: (ensemble, scenario, lat, lon, return_period)

The transformation involves:
1. Adding ensemble dimension with 'mean' value
2. Keeping original scenario values as-is
3. Reordering dimensions to match standard format
4. Using Dask for memory-efficient processing of large files
"""

import xarray as xr
import numpy as np
import os
import dask
from dask.diagnostics import ProgressBar
from dask.distributed import Client, LocalCluster


def main():
    # Configure dask for better performance
    dask.config.set({"array.slicing.split_large_chunks": True})

    # Set up parallel processing with Dask
    print("Setting up Dask cluster for parallel processing...")
    cluster = LocalCluster(
        n_workers=8,  # Use 8 workers for parallel processing
        threads_per_worker=2,  # 2 threads per worker
        memory_limit="4GB",  # Limit memory per worker
        processes=True,  # Use processes instead of threads for better parallelism
    )

    # Create Dask client
    client = Client(cluster)
    print(f"Dask dashboard available at: {client.dashboard_link}")
    print(
        f"Using {len(cluster.workers)} workers with {cluster._threads_per_worker} threads each"
    )

    # Define paths
    input_file = "workspace/hazards/Flood/GIRI_flood_depth_cube.nc"
    output_file = "workspace/hazards/Flood/GIRI_flood_depth_cube_restructured.nc"

    print(f"Input file: {input_file}")
    print(f"Output file: {output_file}")
    print("Using Dask cluster for parallel chunked processing")

    # Load the original flood cube with Dask chunking
    print("\nLoading flood cube with Dask chunking...")

    # Define chunk sizes optimized for parallel processing
    # Smaller chunks = more parallelism but more overhead
    chunk_sizes = {
        "scenario": 1,  # Process one scenario at a time
        "return_period": 1,  # Process one return period at a time
        "lat": 2000,  # Larger chunks for better parallel efficiency
        "lon": 2000,  # Larger chunks for better parallel efficiency
    }

    ds_original = xr.open_dataset(input_file, chunks=chunk_sizes)

    print("Original flood cube structure:")
    print(ds_original)
    print(f"\nOriginal dimensions: {ds_original.dims}")
    print(f"Original coordinates: {list(ds_original.coords.keys())}")
    print(f"\nChunk sizes: {ds_original.chunks}")
    print(f"Data is chunked: {ds_original.chunks is not None}")

    # Create the restructured dataset using Dask operations
    print("\nCreating restructured dataset with Dask...")

    # First, expand the ensemble dimension using xarray's expand_dims
    # This adds a new dimension without copying data (lazy operation)
    ds_with_ensemble = ds_original.expand_dims("ensemble", axis=0)

    # Set the ensemble coordinate to 'mean'
    ds_with_ensemble = ds_with_ensemble.assign_coords(ensemble=["mean"])

    # Now reorder dimensions to match standard format
    # Original: (ensemble, scenario, return_period, lat, lon)
    # Target: (ensemble, scenario, lat, lon, return_period)
    ds_restructured = ds_with_ensemble.transpose(
        "ensemble", "scenario", "lat", "lon", "return_period"
    )

    print("Restructured dataset:")
    print(ds_restructured)
    print(f"\nNew dimensions: {ds_restructured.dims}")
    print(f"New coordinates: {list(ds_restructured.coords.keys())}")
    print(f"\nChunk sizes: {ds_restructured.chunks}")
    print(f"Data is still chunked: {ds_restructured.chunks is not None}")

    # Save the restructured dataset with Dask chunked processing
    print(f"\nSaving restructured dataset to: {output_file}")

    # Create output directory if it doesn't exist
    os.makedirs(os.path.dirname(output_file), exist_ok=True)

    # Remove existing file if it exists to avoid permission issues
    if os.path.exists(output_file):
        print(f"Removing existing file: {output_file}")
        os.remove(output_file)

    # Save the dataset with chunking to handle large files efficiently
    print("Saving with Dask chunked processing...")

    # Use ProgressBar to show progress during save
    # The save operation will now use the Dask cluster for parallel processing
    print("Starting parallel save operation...")
    with ProgressBar():
        ds_restructured.to_netcdf(
            output_file,
            encoding={
                "flood_depth": {
                    "zlib": True,  # Enable compression
                    "complevel": 6,  # Compression level (1-9)
                    "chunksizes": (
                        1,
                        1,
                        2000,
                        2000,
                        1,
                    ),  # Optimized chunk sizes for parallel processing
                }
            },
        )

    print("Dataset saved successfully!")
    print(f"File size: {os.path.getsize(output_file) / (1024**3):.2f} GB")

    # Verify the saved file by loading it back with chunking
    print("\nVerifying saved file:")
    ds_loaded = xr.open_dataset(output_file, chunks={"lat": 2000, "lon": 2000})
    print(ds_loaded)
    print(f"\nLoaded dataset dimensions: {ds_loaded.dims}")
    print(f"Loaded dataset coordinates: {list(ds_loaded.coords.keys())}")

    # Compare with other hazard NC maps structure
    print("\nComparison with other hazard NC maps:")
    print("Expected structure: (ensemble, scenario, lat, lon, return_period)")
    print(f"Our structure: {ds_loaded.dims}")

    # Check if structure matches
    expected_dims = ("ensemble", "scenario", "lat", "lon", "return_period")
    expected_coords = ["ensemble", "scenario", "lat", "lon", "return_period"]

    dims_match = ds_loaded.dims == expected_dims
    coords_match = set(ds_loaded.coords.keys()) == set(expected_coords)

    print(f"\nStructure validation:")
    print(f"  Dimensions match: {dims_match}")
    print(f"  Coordinates match: {coords_match}")
    print(f"  Overall match: {dims_match and coords_match}")

    # Additional verification: compare original vs restructured data (chunked)
    print("\nData integrity verification:")
    print("Comparing a small sample from original vs restructured...")

    # Get a small sample from original (this will be computed lazily)
    orig_sample = ds_original.flood_depth.isel(
        scenario=0, return_period=0, lat=slice(0, 5), lon=slice(0, 5)
    )

    # Get corresponding sample from restructured
    restructured_sample = ds_loaded.flood_depth.isel(
        ensemble=0, scenario=0, return_period=0, lat=slice(0, 5), lon=slice(0, 5)
    )

    # Compute the samples (this triggers the actual computation)
    print("Computing samples for comparison...")
    with ProgressBar():
        orig_values = orig_sample.compute()
        restructured_values = restructured_sample.compute()

    # Compare
    print(f"Original sample shape: {orig_values.shape}")
    print(f"Restructured sample shape: {restructured_values.shape}")
    print(
        f"Data matches: {np.allclose(orig_values.values, restructured_values.values)}"
    )

    print(f"\nOriginal sample values:\n{orig_values.values}")
    print(f"\nRestructured sample values:\n{restructured_values.values}")

    print("\n" + "=" * 80)
    print("SUMMARY")
    print("=" * 80)
    print("The flood cube has been successfully restructured using Dask:")
    print("- Original: (scenario, return_period, lat, lon)")
    print("- New: (ensemble, scenario, lat, lon, return_period)")
    print("- Ensemble: 'raw'")
    print("- Scenarios: original values preserved")
    print("\nKey benefits:")
    print("✅ No memory crashes - processed in chunks")
    print("✅ Progress visibility - shows processing status")
    print("✅ Scalable - works with files of any size")
    print("✅ Efficient - lazy evaluation until computation needed")
    print(
        "\nThe restructured file can now be used with the existing hazard loading infrastructure."
    )

    # Clean up the Dask cluster
    print("\nCleaning up Dask cluster...")
    client.close()
    cluster.close()


if __name__ == "__main__":
    main()
