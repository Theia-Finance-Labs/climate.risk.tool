#!/usr/bin/env python3
"""
Rename a variable in a NetCDF file.

This script opens a NetCDF file, renames a specified data variable, and saves
the modified file (either overwriting the original or to a new path).
"""

import os
import sys
import argparse
import tempfile
import shutil

import xarray as xr


def _filter_encoding(ds):
    """
    Filter dataset encoding to only include valid netCDF4 parameters.
    
    Args:
        ds: xarray Dataset
    
    Returns:
        dict: Filtered encoding dictionary
    """
    encoding = {}
    valid_encoding_keys = {
        '_FillValue', 'dtype', 'scale_factor', 'add_offset',
        'zlib', 'complevel', 'shuffle', 'fletcher32',
        'contiguous', 'chunksizes', 'endian'
    }
    for var in ds.data_vars:
        var_encoding = {}
        for key, value in ds[var].encoding.items():
            if key in valid_encoding_keys:
                var_encoding[key] = value
        if var_encoding:
            encoding[var] = var_encoding
    return encoding if encoding else None


def rename_nc_variable(input_path, old_name, new_name, output_path=None, overwrite=False):
    """
    Rename a variable in a NetCDF file.
    
    Args:
        input_path: Path to input NetCDF file
        old_name: Current name of the variable to rename
        new_name: New name for the variable
        output_path: Path to output file (default: overwrite input)
        overwrite: Whether to overwrite if output file exists (default: False)
    
    Returns:
        Path to the output file
    """
    if not os.path.exists(input_path):
        raise FileNotFoundError(f"Input file not found: {input_path}")
    
    # Determine output path
    if output_path is None:
        output_path = input_path
        overwrite = True  # Force overwrite if no output specified
    
    if os.path.exists(output_path) and not overwrite:
        raise FileExistsError(
            f"Output file already exists: {output_path}\n"
            "Use --overwrite to overwrite it."
        )
    
    print(f"[INFO] Opening NetCDF: {input_path}")
    ds = xr.open_dataset(input_path)
    
    # Check if old variable exists
    if old_name not in ds.data_vars:
        available = list(ds.data_vars.keys())
        ds.close()
        raise ValueError(
            f"Variable '{old_name}' not found in file.\n"
            f"Available variables: {available}"
        )
    
    # Check if new name already exists
    if new_name in ds.data_vars:
        ds.close()
        raise ValueError(
            f"Variable '{new_name}' already exists in file.\n"
            "Choose a different new name."
        )
    
    print(f"[INFO] Variables before: {list(ds.data_vars.keys())}")
    print(f"[INFO] Renaming '{old_name}' -> '{new_name}'")
    
    # Rename the variable
    ds = ds.rename_vars({old_name: new_name})
    
    print(f"[INFO] Variables after: {list(ds.data_vars.keys())}")
    
    # If overwriting input file, use temporary file to avoid permission issues
    if output_path == input_path:
        # Create temporary file in same directory
        temp_dir = os.path.dirname(output_path) or "."
        temp_fd, temp_path = tempfile.mkstemp(suffix=".nc", dir=temp_dir)
        os.close(temp_fd)  # Close file descriptor, we'll use xarray to write
        
        try:
            print(f"[INFO] Writing to temporary file: {temp_path}")
            # Save to temporary file
            encoding = _filter_encoding(ds)
            ds.to_netcdf(temp_path, encoding=encoding)
            ds.close()
            
            # Replace original file with temporary file
            print(f"[INFO] Replacing original file: {output_path}")
            shutil.move(temp_path, output_path)
        except Exception as e:
            # Clean up temp file on error
            if os.path.exists(temp_path):
                os.remove(temp_path)
            raise
    else:
        print(f"[INFO] Writing to: {output_path}")
        # Save to output file
        encoding = _filter_encoding(ds)
        ds.to_netcdf(output_path, encoding=encoding)
        ds.close()
    
    print(f"[INFO] Successfully renamed variable and saved to: {output_path}")
    
    return output_path


def main():
    parser = argparse.ArgumentParser(
        description="Rename a variable in a NetCDF file",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  # Overwrite original file
  python rename_nc_variable.py input.nc --old-name HI_days_hot_total --new-name HI --overwrite
  
  # Save to new file
  python rename_nc_variable.py input.nc --old-name HI_days_hot_total --new-name HI -o output.nc
        """
    )
    
    parser.add_argument(
        "input",
        help="Path to input NetCDF file"
    )
    parser.add_argument(
        "--old-name",
        required=True,
        help="Current name of the variable to rename"
    )
    parser.add_argument(
        "--new-name",
        required=True,
        help="New name for the variable"
    )
    parser.add_argument(
        "-o", "--output",
        default=None,
        help="Path to output file (default: overwrite input)"
    )
    parser.add_argument(
        "--overwrite",
        action="store_true",
        help="Overwrite output file if it exists (required if output is same as input)"
    )
    
    args = parser.parse_args()
    
    try:
        rename_nc_variable(
            input_path=args.input,
            old_name=args.old_name,
            new_name=args.new_name,
            output_path=args.output,
            overwrite=args.overwrite
        )
    except Exception as e:
        print(f"[ERROR] {e}", file=sys.stderr)
        sys.exit(1)


if __name__ == "__main__":
    main()

