#!/usr/bin/env python3
"""
Build a refactored hazard_indicators folder.

Input folder contract (as described by user):
- `input_root` contains ONLY subfolders.
- Each subfolder contains either:
  - GeoTIFF files (.tif/.tiff), OR
  - NetCDF files (.nc)

Output folder contract:
- Writes to `output_root/<indicator_name>/`
- If input is GeoTIFFs: writes a compressed NetCDF named `<indicator_name>.nc`
  (variable name is also `<indicator_name>`), stacking scenario x return_period
  when multiple TIFFs are present (e.g. flood_depth).
- If input is NetCDFs: copies the file but renames it to `<indicator_name>.nc`

This intentionally ignores CSV-backed indicators.
"""

from __future__ import annotations

import argparse
import os
import re
import shutil
import tempfile
import time
from typing import Dict, List, Optional, Tuple

import pandas as pd
import xarray as xr

# Allow both:
# - python -m climate_risk_tool_python.netcdf_mgmt.build_hazard_indicators_refacto
# - python path/to/build_hazard_indicators_refacto.py
try:
    from .convert_tif_to_nc import TifHazardSpec, convert_tifs_to_ensemble_return_period_nc
except ImportError:  # pragma: no cover
    from convert_tif_to_nc import TifHazardSpec, convert_tifs_to_ensemble_return_period_nc


FLOOD_RE = re.compile(
    r"^flood_(?P<scenario>pc|rcp26|rcp85)_(?P<rp>[0-9]+)_glob\.tiff?$",
    flags=re.IGNORECASE,
)

def _ts() -> str:
    return time.strftime("%Y-%m-%d %H:%M:%S")


def _log(msg: str) -> None:
    print(f"[{_ts()}] {msg}", flush=True)


def _list_files_with_ext(root: str, exts: Tuple[str, ...]) -> List[str]:
    out: List[str] = []
    for name in os.listdir(root):
        p = os.path.join(root, name)
        if os.path.isfile(p) and name.lower().endswith(exts):
            out.append(p)
    return sorted(out)


def _load_metadata_from_csv(input_root: str) -> Optional[pd.DataFrame]:
    """Load hazards_metadata.csv from input root if it exists."""
    metadata_path = os.path.join(input_root, "hazards_metadata.csv")
    if os.path.exists(metadata_path):
        md = pd.read_csv(metadata_path)
        md.columns = md.columns.str.strip()
        return md
    return None


def _get_metadata_for_indicator(
    indicator_dir: str,
    indicator_name: str,
    input_root: str,
    metadata_df: Optional[pd.DataFrame],
) -> Tuple[pd.DataFrame, str, str]:
    """
    Get metadata for an indicator folder, using hazards_metadata.csv if available.
    
    Returns:
        (metadata_df, hazard_type, hazard_indicator)
    """
    tifs = _list_files_with_ext(indicator_dir, (".tif", ".tiff"))
    if not tifs:
        raise ValueError(f"No TIFFs found in: {indicator_dir}")

    # If metadata CSV exists, use it to find matching rows
    if metadata_df is not None:
        # Find TIF files in the indicator directory
        tif_filenames = {os.path.basename(t) for t in tifs}
        
        # Filter metadata to rows matching files in this indicator folder
        # Match by hazard_file column
        md_filtered = metadata_df[
            metadata_df["hazard_file"].isin(tif_filenames)
        ].copy()
        
        if not md_filtered.empty:
            # Extract hazard_type and hazard_indicator from first matching row
            hazard_type = str(md_filtered["hazard_type"].iloc[0])
            hazard_indicator = str(md_filtered["hazard_indicator"].iloc[0])
            
            # Ensure all required columns exist
            required_cols = ["hazard_file", "hazard_type", "hazard_indicator", 
                           "scenario_code", "scenario_name", "hazard_return_period"]
            for col in required_cols:
                if col not in md_filtered.columns:
                    raise ValueError(
                        f"Missing required column '{col}' in hazards_metadata.csv"
                    )
            
            return md_filtered, hazard_type, hazard_indicator
    
    # Fallback: infer from filenames (backward compatibility)
    rows: List[Dict[str, object]] = []
    
    if indicator_name == "flood_depth":
        for tif_path in tifs:
            fn = os.path.basename(tif_path)
            m = FLOOD_RE.match(fn)
            if not m:
                raise ValueError(
                    f"Unexpected flood_depth tif name: {fn}\n"
                    "Expected pattern like flood_rcp26_100_glob.tif"
                )
            scenario_raw = m.group("scenario").lower()
            scenario_name = "present" if scenario_raw == "pc" else scenario_raw
            rp = int(m.group("rp"))
            rows.append(
                {
                    "hazard_file": fn,
                    "hazard_type": "Flood",
                    "hazard_indicator": "depth(cm)",
                    "scenario_code": scenario_name,
                    "scenario_name": scenario_name,
                    "hazard_return_period": rp,
                }
            )
        return pd.DataFrame(rows), "Flood", "depth(cm)"
    elif indicator_name == "land_cover":
        if len(tifs) != 1:
            raise ValueError(
                f"land_cover should contain exactly 1 tif; found {len(tifs)}"
            )
        fn = os.path.basename(tifs[0])
        rows.append(
            {
                "hazard_file": fn,
                "hazard_type": "Fire",
                "hazard_indicator": "land_cover",
                "scenario_code": "present",
                "scenario_name": "present",
                "hazard_return_period": 0,
            }
        )
        return pd.DataFrame(rows), "Fire", "land_cover"
    else:
        raise ValueError(
            f"Cannot infer metadata for indicator folder: {indicator_name}\n"
            "Please provide hazards_metadata.csv in the input root directory."
        )


def _convert_indicator_tifs_to_nc(
    *,
    indicator_dir: str,
    indicator_name: str,
    output_nc_path: str,
    tile: int,
    complevel: int,
    input_root: str,
    metadata_df: Optional[pd.DataFrame],
) -> None:
    md, hazard_type, hazard_indicator = _get_metadata_for_indicator(
        indicator_dir, indicator_name, input_root, metadata_df
    )
    
    with tempfile.TemporaryDirectory() as td:
        md_path = os.path.join(td, "hazards_metadata.csv")
        md.to_csv(md_path, index=False)

        # Use variable_name based on indicator_name (folder name) for output NetCDF
        spec = TifHazardSpec(
            hazard_type=hazard_type,
            hazard_indicator=hazard_indicator,
            variable_name=indicator_name,  # Use folder name as variable name
            include_ensemble_dim=(hazard_type == "Flood" and "depth" in hazard_indicator.lower()),
            ensemble_value="mean",
        )

        # hazards_dir should include the tifs referenced in md (this folder is enough)
        convert_tifs_to_ensemble_return_period_nc(
            hazards_dir=indicator_dir,
            metadata_csv=md_path,
            spec=spec,
            output_nc_path=output_nc_path,
            tile=tile,
            zlib=True,
            complevel=complevel,
            shuffle=True,
            log_progress=True,
            log_every_n_tiles=50,
        )


def _is_nc_already_ok_for_indicator(
    *,
    src_nc_path: str,
    indicator_name: str,
    complevel: int,
) -> bool:
    """Fast metadata-only check: variable name + stored chunking + compression."""
    ds = xr.open_dataset(src_nc_path, engine="netcdf4")
    try:
        vars_ = list(ds.data_vars.keys())
        if len(vars_) != 1:
            return False
        v = vars_[0]
        if v != indicator_name:
            return False
        enc = ds[v].encoding or {}
        if not enc.get("chunksizes"):
            return False
        if not bool(enc.get("zlib", False)):
            return False
        if int(enc.get("complevel", -1)) != int(complevel):
            return False
        return True
    finally:
        ds.close()


def _rewrite_nc_with_variable_name_and_encoding(
    *,
    src_nc_path: str,
    dst_nc_path: str,
    indicator_name: str,
    tile: int,
    complevel: int,
) -> None:
    """
    Ensure:
    - single data var is renamed to indicator_name
    - output is NetCDF4 deflate-compressed with explicit chunksizes
    
    Optimizes chunking for large rasters to improve read performance:
    - For rasters >100k pixels in lat/lon: uses larger chunks (2048-4096) for better spatial access
    - Reduces compression level for very large rasters (level 6 instead of 9) for faster decompression
    """
    ds0 = xr.open_dataset(src_nc_path, engine="netcdf4")
    try:
        vars_ = list(ds0.data_vars.keys())
        if len(vars_) != 1:
            raise ValueError(
                f"Expected exactly 1 data variable in {src_nc_path}, found {len(vars_)}: {vars_}"
            )
        src_var = vars_[0]
        ds = ds0.rename_vars({src_var: indicator_name}) if src_var != indicator_name else ds0
        da = ds[indicator_name]

        # Adaptive chunking: use larger chunks for very large rasters to improve spatial access performance
        # This is critical for polygon extraction operations (crop/mask) which need to decompress many chunks
        lat_size = ds.sizes.get("lat", 0)
        lon_size = ds.sizes.get("lon", 0)
        max_spatial_dim = max(lat_size, lon_size)
        
        # For very large rasters (>100k pixels), use larger chunks to reduce decompression overhead
        # Larger chunks mean fewer chunks to decompress during spatial operations
        if max_spatial_dim > 100000:
            # Use 4096 chunks for extremely large rasters (>200k), 2048 for large rasters (100k-200k)
            adaptive_tile = 4096 if max_spatial_dim > 200000 else 2048
            # Reduce compression level for very large rasters (level 6 is a good balance)
            adaptive_complevel = min(complevel, 6)
        else:
            adaptive_tile = tile
            adaptive_complevel = complevel

        chunks: Dict[str, int] = {}
        for d in da.dims:
            if d in ("lat", "lon"):
                chunks[d] = int(min(adaptive_tile, ds.sizes[d]))
            else:
                chunks[d] = 1

        ds_chunked = ds.chunk(chunks)
        chunksizes = tuple(int(chunks[d]) for d in da.dims)
        encoding = {
            indicator_name: {
                "zlib": True,
                "complevel": int(adaptive_complevel),
                "shuffle": True,
                "chunksizes": chunksizes,
            }
        }

        tmp = dst_nc_path + ".tmp"
        if os.path.exists(tmp):
            os.remove(tmp)
        if os.path.exists(dst_nc_path):
            os.remove(dst_nc_path)

        ds_chunked.to_netcdf(tmp, engine="netcdf4", encoding=encoding)
        os.replace(tmp, dst_nc_path)
    finally:
        ds0.close()


def build_refacto_hazard_indicators(
    *,
    input_root: str,
    output_root: str,
    tile: int,
    complevel: int,
    overwrite: bool,
) -> None:
    input_root = os.path.abspath(input_root)
    output_root = os.path.abspath(output_root)

    if not os.path.isdir(input_root):
        raise FileNotFoundError(f"Input root not found: {input_root}")

    if overwrite and os.path.exists(output_root):
        shutil.rmtree(output_root)

    if os.path.exists(output_root):
        raise FileExistsError(
            f"Output root already exists: {output_root}\n"
            "Pass --overwrite to remove it first."
        )

    os.makedirs(output_root, exist_ok=True)

    # Load metadata CSV if it exists
    metadata_df = _load_metadata_from_csv(input_root)
    if metadata_df is not None:
        _log(f"loaded metadata CSV: {len(metadata_df)} rows")

    indicators = sorted([d for d in os.listdir(input_root) if os.path.isdir(os.path.join(input_root, d))])
    total = len(indicators)
    t0 = time.time()
    _log(f"start input={input_root}")
    _log(f"start output={output_root}")
    _log(f"start indicators={total} tile={tile} complevel={complevel}")

    for i, indicator_name in enumerate(indicators, 1):
        indicator_dir = os.path.join(input_root, indicator_name)

        tifs = _list_files_with_ext(indicator_dir, (".tif", ".tiff"))
        ncs = _list_files_with_ext(indicator_dir, (".nc",))

        # Output layout: write NetCDFs directly under output_root
        out_nc = os.path.join(output_root, f"{indicator_name}.nc")

        if tifs and ncs:
            raise ValueError(
                f"Indicator folder has both tif and nc files: {indicator_dir}"
            )

        _log(f"{i}/{total} indicator={indicator_name}")

        if tifs:
            _log(f"  mode=tif->nc files={len(tifs)}")
            t_indicator = time.time()
            _convert_indicator_tifs_to_nc(
                indicator_dir=indicator_dir,
                indicator_name=indicator_name,
                output_nc_path=out_nc,
                tile=tile,
                complevel=complevel,
                input_root=input_root,
                metadata_df=metadata_df,
            )
            _log(
                f"  wrote={out_nc} size_mb={os.path.getsize(out_nc)/(1024*1024):.2f} "
                f"elapsed_s={time.time()-t_indicator:.1f}"
            )
            continue

        if ncs:
            if len(ncs) != 1:
                raise ValueError(
                    f"Expected exactly 1 .nc in {indicator_dir}, found {len(ncs)}"
                )
            src = ncs[0]
            if _is_nc_already_ok_for_indicator(
                src_nc_path=src,
                indicator_name=indicator_name,
                complevel=complevel,
            ):
                _log(
                    f"  mode=copy-nc (already ok) src={os.path.basename(src)} dst={os.path.basename(out_nc)}"
                )
                shutil.copy2(src, out_nc)
            else:
                _log(
                    f"  mode=rewrite-nc (rename var + enforce chunks/compression) "
                    f"src={os.path.basename(src)} dst={os.path.basename(out_nc)}"
                )
                _rewrite_nc_with_variable_name_and_encoding(
                    src_nc_path=src,
                    dst_nc_path=out_nc,
                    indicator_name=indicator_name,
                    tile=tile,
                    complevel=complevel,
                )
            continue

        _log("  skip: no .tif/.nc files found")

    _log(f"done elapsed_s={time.time()-t0:.1f}")


def _parse_args() -> argparse.Namespace:
    p = argparse.ArgumentParser(
        description="Build workspace/demo_inputs_refacto/hazard_indicators from workspace/hazard_indicators."
    )
    p.add_argument(
        "--input-root",
        default="workspace/hazard_indicators",
        help="Input root (default: workspace/hazard_indicators)",
    )
    p.add_argument(
        "--output-root",
        default="workspace/demo_inputs_refacto/hazard_indicators",
        help="Output root (default: workspace/demo_inputs_refacto/hazard_indicators)",
    )
    # Defaults chosen to make NetCDF outputs competitive with GeoTIFF size:
    # - GeoTIFFs here are tiled (often 256/512) and compressed (deflate/LZW).
    # - Using 512 chunking + stronger deflate helps keep NetCDF smaller.
    p.add_argument("--tile", type=int, default=512, help="Tile/chunk size (default: 512)")
    p.add_argument("--complevel", type=int, default=9, help="Deflate level 1..9 (default: 9)")
    p.add_argument("--overwrite", action="store_true", help="Remove output-root if it exists")
    return p.parse_args()


def main() -> None:
    args = _parse_args()
    build_refacto_hazard_indicators(
        input_root=args.input_root,
        output_root=args.output_root,
        tile=int(args.tile),
        complevel=int(args.complevel),
        overwrite=bool(args.overwrite),
    )


if __name__ == "__main__":
    main()


