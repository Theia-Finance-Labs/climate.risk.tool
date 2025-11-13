#!/usr/bin/env python3
"""Fix FloodNC: rename scenarios and filter to present/rcp85 and RP 25/100"""
import xarray as xr

file_path = "tests/tests_data/hazards/Flood/depth(cm)/ensemble/ensemble_return_period.nc"

print(f"Processing: {file_path}")

ds = xr.open_dataset(file_path)

print(f"\nOriginal structure:")
print(f"  GWL values: {ds.GWL.values}")
print(f"  return_period values: {ds.return_period.values}")
print(f"  Shape: {ds.flood_depth.shape}")

# Step 1: Rename GWL values
scenario_mapping = {
    "Present Climate": "present",
    "SSP1-2.6": "rcp26",
    "SSP5-8.5": "rcp85"
}

current_gwl = ds.GWL.values.tolist()
new_gwl = [scenario_mapping.get(str(val), str(val)) for val in current_gwl]
ds = ds.assign_coords(GWL=new_gwl)

print(f"\nAfter renaming:")
print(f"  GWL values: {ds.GWL.values}")

# Step 2: Filter to only present and rcp85
ds = ds.sel(GWL=["present", "rcp85"])

print(f"\nAfter filtering GWL:")
print(f"  GWL values: {ds.GWL.values}")

# Step 3: Filter to only return periods 25 and 100
ds = ds.sel(return_period=[25, 100])

print(f"\nAfter filtering return_period:")
print(f"  return_period values: {ds.return_period.values}")
print(f"  Final shape: {ds.flood_depth.shape}")

# Save
temp_file = file_path + ".tmp"
encoding = {
    'flood_depth': {
        'zlib': True,
        'complevel': 4,
        'dtype': 'float32',
        '_FillValue': float('nan')
    }
}

print(f"\nSaving...")
ds.to_netcdf(temp_file, encoding=encoding)
ds.close()

import os
os.replace(temp_file, file_path)

print("✓ Done!")
print(f"\nFinal dataset: 2 scenarios (present, rcp85) × 2 return periods (25, 100) = 4 hazards")

