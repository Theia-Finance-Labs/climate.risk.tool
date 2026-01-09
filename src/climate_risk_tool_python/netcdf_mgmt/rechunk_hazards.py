"""
Utility to re-chunk NetCDF files that lack on-disk chunking.
This is necessary for processing very high-resolution datasets (e.g., 2.5B+ points)
that would otherwise crash Dask workers.
"""

import os
import glob
import xarray as xr
import dask
from dask.distributed import Client, LocalCluster
import time

def rechunk_file(nc_path, target_chunks={"lat": 2000, "lon": 2000}):
    """Re-saves a NetCDF file with proper chunking."""
    print(f"\n📦 Re-chunking: {nc_path}")
    
    # 1. Inspect without loading
    with xr.open_dataset(nc_path) as ds:
        var_name = list(ds.data_vars.keys())[0]
        da = ds[var_name]
        
        # We always re-chunk to the target size to ensure consistency
        print(f"   ℹ️ Current shape: {da.shape}")
        if da.encoding.get("chunksizes"):
            print(f"   ℹ️ Current chunks: {da.encoding.get('chunksizes')}")
        
    # 2. Open with dask using temporary chunks for the read
    # We use size 1 for other dimensions to minimize memory footprint during re-chunking
    read_chunks = {k: v for k, v in target_chunks.items() if k in da.dims}
    for dim in da.dims:
        if dim not in read_chunks:
            read_chunks[dim] = 1
            
    print(f"   🚀 Opening with dask chunks: {read_chunks}")
    ds = xr.open_dataset(nc_path, chunks=read_chunks)
    
    # 3. Setup encoding for the write
    # We must explicitly set the chunking in the encoding dictionary
    encoding = {}
    for var in ds.data_vars:
        # Get dimensions for this variable
        var_dims = ds[var].dims
        var_chunks = []
        for d in var_dims:
            if d in target_chunks:
                var_chunks.append(min(target_chunks[d], ds.sizes[d]))
            else:
                var_chunks.append(1)
        
        encoding[var] = {
            "chunksizes": tuple(var_chunks),
            "zlib": True,
            "complevel": 4
        }
    
    # 4. Save to temporary file then replace
    temp_path = nc_path + ".rechunking.tmp"
    if os.path.exists(temp_path):
        os.remove(temp_path)
        
    print(f"   💾 Saving to temporary file... (this may take a while)")
    start_time = time.time()
    
    try:
        ds.to_netcdf(temp_path, encoding=encoding)
        elapsed = time.time() - start_time
        print(f"   ✅ Finished re-chunking in {elapsed:.1f}s")
        
        # Replace original file
        ds.close()
        os.remove(nc_path)
        os.rename(temp_path, nc_path)
        print(f"   🔄 Original file replaced.")
        return True
    except Exception as e:
        print(f"   ❌ Error during re-chunking: {e}")
        if os.path.exists(temp_path):
            os.remove(temp_path)
        return False

def main():
    HAZARDS_DIR = "workspace/demo_inputs_fullnc/hazards"
    
    # Setup Dask for the heavy lifting
    cluster = LocalCluster(n_workers=4, threads_per_worker=1, memory_limit="4GB")
    client = Client(cluster)
    print(f"Dask Dashboard: {client.dashboard_link}")
    
    # Find all .nc files
    nc_pattern = os.path.join(HAZARDS_DIR, "**", "*.nc")
    nc_files = glob.glob(nc_pattern, recursive=True)
    
    print(f"Found {len(nc_files)} NetCDF files to check.")
    
    processed_count = 0
    for nc_path in nc_files:
        if rechunk_file(nc_path):
            processed_count += 1
            
    print(f"\nSummary: {processed_count} files re-chunked.")
    
    client.close()
    cluster.close()

if __name__ == "__main__":
    main()

