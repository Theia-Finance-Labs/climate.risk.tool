#!/usr/bin/env python3
"""
Convert TIFF file to NetCDF format with metadata columns as dimensions.
"""

import os
import sys
import numpy as np
import rasterio
from netCDF4 import Dataset
import pandas as pd

# Input paths
tif_path = "workspace/demo_inputs_fullnc/hazards/Fire/land_cover/2024_brazil_land_cover.tif"
metadata_path = "workspace/demo_inputs_fullnc/hazards_metadata.csv"

# Read metadata
metadata_df = pd.read_csv(metadata_path)
# Strip whitespace from column names
metadata_df.columns = metadata_df.columns.str.strip()
# Filter to the row for our TIFF file
tif_filename = os.path.basename(tif_path)
metadata_row = metadata_df[metadata_df['hazard_file'] == tif_filename]

if len(metadata_row) == 0:
    raise ValueError(f"No metadata found for file: {tif_filename}")

row = metadata_row.iloc[0]

# Extract metadata values
hazard_type = str(row['hazard_type']).strip()
hazard_indicator = str(row['hazard_indicator']).strip()
scenario_code = str(row['scenario_code']).strip()
scenario_name = str(row['scenario_name']).strip()
hazard_return_period = float(row['hazard_return_period'])

print(f"Converting {tif_path} to NetCDF...")
print(f"  hazard_type: {hazard_type} (from folder structure)")
print(f"  hazard_indicator: {hazard_indicator} (data variable name)")
print(f"  GWL (scenario_name): {scenario_name}")
print(f"  return_period: {hazard_return_period}")

# Read TIFF file
with rasterio.open(tif_path) as src:
    data = src.read(1)  # Read first band
    transform = src.transform
    crs = src.crs
    
    # Get dimensions
    height, width = data.shape
    
    # Calculate lat/lon coordinates from transform
    # Transform gives us pixel coordinates to geographic coordinates
    lon_min = transform[2]
    lon_res = transform[0]
    lat_max = transform[5]
    lat_res = -transform[4]  # Usually negative
    
    # Create coordinate arrays (cell centers)
    lon = np.linspace(lon_min + lon_res/2, lon_min + (width - 0.5) * lon_res, width)
    lat = np.linspace(lat_max - lat_res/2, lat_max - (height - 0.5) * lat_res, height)
    
    print(f"  Data shape: {data.shape}")
    print(f"  Lon range: [{lon.min():.4f}, {lon.max():.4f}]")
    print(f"  Lat range: [{lat.min():.4f}, {lat.max():.4f}]")
    print(f"  Data type: {data.dtype}")
    print(f"  Data range: [{data.min()}, {data.max()}]")

# Create output directory if needed
output_dir = f"workspace/demo_inputs_fullnc/hazards/{hazard_type}/{hazard_indicator}/ensemble"
os.makedirs(output_dir, exist_ok=True)

# Output NetCDF file path
output_nc_path = os.path.join(output_dir, "ensemble_return_period.nc")

print(f"\nCreating NetCDF file: {output_nc_path}")

# Create NetCDF file
with Dataset(output_nc_path, 'w', format='NETCDF4') as nc:
    # Create dimensions
    # Metadata dimensions (single value each)
    nc.createDimension('GWL', 1)
    nc.createDimension('return_period', 1)
    
    # Spatial dimensions
    nc.createDimension('lat', height)
    nc.createDimension('lon', width)
    
    # Create dimension variables (coordinate variables)
    gwl_var = nc.createVariable('GWL', str, ('GWL',))
    gwl_var[:] = np.array([scenario_name], dtype=object)
    gwl_var.long_name = 'Global Warming Level'
    
    return_period_var = nc.createVariable('return_period', 'f8', ('return_period',))
    return_period_var[:] = [hazard_return_period]
    return_period_var.long_name = 'Return Period'
    return_period_var.units = 'years'
    
    # Spatial coordinate variables
    lat_var = nc.createVariable('lat', 'f8', ('lat',))
    lat_var[:] = lat
    lat_var.units = 'degrees_north'
    lat_var.long_name = 'Latitude'
    lat_var.standard_name = 'latitude'
    
    lon_var = nc.createVariable('lon', 'f8', ('lon',))
    lon_var[:] = lon
    lon_var.units = 'degrees_east'
    lon_var.long_name = 'Longitude'
    lon_var.standard_name = 'longitude'
    
    # Create data variable
    # Use hazard_indicator as the variable name (land_cover)
    # Determine appropriate fill_value based on data type
    if np.issubdtype(data.dtype, np.integer):
        fill_val = 255 if data.dtype == np.uint8 else -9999
    else:
        fill_val = np.nan
    
    data_var = nc.createVariable(hazard_indicator, data.dtype, 
                                  ('GWL', 'return_period', 'lat', 'lon'),
                                  fill_value=fill_val)
    
    # Assign data - need to expand dimensions to match all metadata dims
    # Data is 2D (lat, lon), need to add singleton dimensions for metadata
    data_expanded = np.expand_dims(data, axis=(0, 1))
    data_var[:] = data_expanded
    
    data_var.long_name = f'{hazard_indicator.replace("_", " ").title()}'
    data_var.units = 'categorical' if hazard_indicator == 'land_cover' else 'unknown'
    
    # Global attributes
    nc.title = f'{hazard_type} {hazard_indicator} hazard data'
    nc.source = f'Converted from {tif_filename}'
    nc.history = f'Created by convert_tif_to_nc.py on {pd.Timestamp.now()}'
    nc.Conventions = 'CF-1.6'

print(f"\n✅ Successfully created NetCDF file: {output_nc_path}")
print(f"   File size: {os.path.getsize(output_nc_path) / (1024*1024):.2f} MB")

