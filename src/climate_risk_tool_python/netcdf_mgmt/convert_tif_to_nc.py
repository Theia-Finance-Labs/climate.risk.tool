#!/usr/bin/env python3
"""
Convert hazard GeoTIFFs to NetCDF, stacking (GWL × return_period) using `hazards_metadata.csv`.

Notes:
- NetCDF4 is NOT automatically compressed; compression must be enabled per-variable.
- Some rasters (Flood / land_cover) are huge; this script writes using windowed IO.
"""

from __future__ import annotations

import argparse
import os
from dataclasses import dataclass
from typing import Dict, List, Optional, Tuple

import numpy as np
import pandas as pd
import rasterio
import netCDF4
from rasterio.windows import Window

# Defaults aligned with existing scripts / files in this repo
ZLIB_DEFAULT = True
COMPRESSION_LEVEL_DEFAULT = 4
SHUFFLE_DEFAULT = True
TILE_DEFAULT = 2000


def _nc_dtype_from_np(dt: np.dtype) -> str:
    dt = np.dtype(dt)
    if dt == np.uint8:
        return "u1"
    if dt == np.uint16:
        return "u2"
    if dt == np.uint32:
        return "u4"
    if dt == np.int8:
        return "i1"
    if dt == np.int16:
        return "i2"
    if dt == np.int32:
        return "i4"
    # Prefer float32 for float sources (smaller, adequate for hazards)
    if dt == np.float64:
        return "f4"
    if dt == np.float32:
        return "f4"
    raise ValueError(f"Unsupported raster dtype for NetCDF output: {dt}")


@dataclass(frozen=True)
class TifHazardSpec:
    hazard_type: str
    hazard_indicator: str
    variable_name: str
    include_ensemble_dim: bool = False
    ensemble_value: str = "mean"


def _read_metadata(metadata_csv: str) -> pd.DataFrame:
    df = pd.read_csv(metadata_csv)
    df.columns = df.columns.str.strip()
    for c in [
        "hazard_file",
        "hazard_type",
        "hazard_indicator",
        "scenario_code",
        "scenario_name",
    ]:
        if c in df.columns:
            df[c] = df[c].astype(str).str.strip()
    return df


def _normalize_filename(name: str) -> str:
    # Normalize for tolerant matching: ignore case and punctuation differences like
    # "2024_Brazil_LandCover.tif" vs "2024_brazil_land_cover.tif".
    return "".join(ch for ch in str(name).lower() if ch.isalnum())


def _index_tifs(hazards_dir: str) -> Dict[str, str]:
    out: Dict[str, str] = {}
    for root, _, files in os.walk(hazards_dir):
        for fn in files:
            if fn.lower().endswith((".tif", ".tiff")):
                out[_normalize_filename(fn)] = os.path.join(root, fn)
    return out


def _coords_from_transform(
    transform, height: int, width: int
) -> Tuple[np.ndarray, np.ndarray]:
    lon_min = transform[2]
    lon_res = transform[0]
    lat_max = transform[5]
    lat_res = -transform[4]

    lon = np.linspace(
        lon_min + lon_res / 2, lon_min + (width - 0.5) * lon_res, width
    ).astype(np.float32)
    lat = np.linspace(
        lat_max - lat_res / 2, lat_max - (height - 0.5) * lat_res, height
    ).astype(np.float32)
    return lat, lon


def convert_tifs_to_ensemble_return_period_nc(
    *,
    hazards_dir: str,
    metadata_csv: str,
    spec: TifHazardSpec,
    output_nc_path: str,
    tile: int = TILE_DEFAULT,
    zlib: bool = ZLIB_DEFAULT,
    complevel: int = COMPRESSION_LEVEL_DEFAULT,
    shuffle: bool = SHUFFLE_DEFAULT,
    log_progress: bool = True,
    log_every_n_tiles: int = 50,
) -> str:
    md = _read_metadata(metadata_csv)
    md = md[
        (md["hazard_type"] == spec.hazard_type)
        & (md["hazard_indicator"] == spec.hazard_indicator)
    ].copy()
    if md.empty:
        raise ValueError(
            f"No metadata rows for hazard_type={spec.hazard_type!r}, "
            f"hazard_indicator={spec.hazard_indicator!r}"
        )

    tif_index = _index_tifs(hazards_dir)
    md["hazard_file_norm"] = md["hazard_file"].apply(_normalize_filename)
    missing = sorted(set(md["hazard_file_norm"]) - set(tif_index.keys()))
    if missing:
        raise FileNotFoundError(
            "Missing TIFF files referenced in metadata (normalized filename match): "
            + ", ".join(missing[:10])
            + (" ..." if len(missing) > 10 else "")
        )

    # Dimension values
    gwl_vals = [str(x) for x in sorted(md["scenario_name"].unique(), key=str)]
    rp_vals = sorted({int(float(x)) for x in md["hazard_return_period"].unique()})
    gwl_to_idx = {v: i for i, v in enumerate(gwl_vals)}
    rp_to_idx = {v: i for i, v in enumerate(rp_vals)}

    # Open all sources once; validate grid consistency
    sources: List[Tuple[rasterio.io.DatasetReader, int, int, str]] = []
    ref = None
    try:
        for _, row in md.iterrows():
            tif_path = tif_index[row["hazard_file_norm"]]
            src = rasterio.open(tif_path)
            rp = int(float(row["hazard_return_period"]))
            gwl = str(row["scenario_name"])
            sources.append((src, gwl_to_idx[gwl], rp_to_idx[rp], tif_path))
            if ref is None:
                ref = src
            else:
                if (src.height != ref.height) or (src.width != ref.width):
                    raise ValueError(
                        f"Grid mismatch between {tif_path} and reference raster"
                    )
                if src.transform != ref.transform:
                    raise ValueError(
                        f"Transform mismatch between {tif_path} and reference raster"
                    )
                if str(src.crs) != str(ref.crs):
                    raise ValueError(
                        f"CRS mismatch between {tif_path} and reference raster"
                    )

        if ref is None:
            raise RuntimeError("No source rasters opened (unexpected).")

        height, width = ref.height, ref.width
        lat, lon = _coords_from_transform(ref.transform, height=height, width=width)

        src_np_dtype = np.dtype(ref.dtypes[0])
        is_int = np.issubdtype(src_np_dtype, np.integer) or np.issubdtype(
            src_np_dtype, np.unsignedinteger
        )
        nc_dtype = _nc_dtype_from_np(src_np_dtype)

        # Fill value:
        # - Keep integer rasters as integer (much better compression; avoids float upcast).
        # - Use nodata as _FillValue for integer rasters (required: cannot use NaN).
        # - For float rasters, use NaN as fill.
        fill_value = np.nan
        if is_int:
            if ref.nodata is None:
                # Some GeoTIFFs (e.g. land_cover) omit nodata while still being integer.
                # Use a conventional fill value at the max of the dtype.
                # Note: this does not change stored values; it only defines _FillValue.
                fill_value = np.iinfo(src_np_dtype).max
            else:
                fill_value = np.asarray(ref.nodata, dtype=src_np_dtype).item()

        os.makedirs(os.path.dirname(output_nc_path), exist_ok=True)
        if os.path.exists(output_nc_path):
            os.remove(output_nc_path)

        # Adaptive chunking for NetCDF: use larger chunks for very large rasters to improve spatial access performance
        # This is critical for polygon extraction operations (crop/mask) which need to decompress many chunks
        max_spatial_dim = max(height, width)
        if max_spatial_dim > 100000:
            # Use 4096 chunks for extremely large rasters (>200k), 2048 for large rasters (100k-200k)
            adaptive_chunk_size = 4096 if max_spatial_dim > 200000 else 2048
            # Reduce compression level for very large rasters (level 6 is a good balance)
            adaptive_complevel = min(complevel, 6)
        else:
            adaptive_chunk_size = tile
            adaptive_complevel = complevel

        # chunk_lat/chunk_lon for NetCDF chunksizes (affects read performance)
        chunk_lat = min(adaptive_chunk_size, height)
        chunk_lon = min(adaptive_chunk_size, width)
        
        # tile_lat/tile_lon for windowed IO (keep original tile size for write efficiency)
        tile_lat = min(tile, height)
        tile_lon = min(tile, width)

        with netCDF4.Dataset(output_nc_path, "w", format="NETCDF4") as nc:
            # Dimensions
            if spec.include_ensemble_dim:
                nc.createDimension("ensemble", 1)
            nc.createDimension("GWL", len(gwl_vals))
            nc.createDimension("return_period", len(rp_vals))
            nc.createDimension("lat", height)
            nc.createDimension("lon", width)

            # Coordinate variables
            if spec.include_ensemble_dim:
                ens_var = nc.createVariable("ensemble", str, ("ensemble",))
                ens_var[:] = np.array([spec.ensemble_value], dtype=object)
                ens_var.long_name = "Ensemble statistic"

            gwl_var = nc.createVariable("GWL", str, ("GWL",))
            gwl_var[:] = np.array(gwl_vals, dtype=object)
            gwl_var.long_name = "Global Warming Level / Scenario label"

            rp_var = nc.createVariable("return_period", "i8", ("return_period",))
            rp_var[:] = np.array(rp_vals, dtype=np.int64)
            rp_var.long_name = "Return Period"
            rp_var.units = "years"

            lat_var = nc.createVariable("lat", "f4", ("lat",))
            lat_var[:] = lat
            lat_var.units = "degrees_north"
            lat_var.long_name = "Latitude"
            lat_var.standard_name = "latitude"

            lon_var = nc.createVariable("lon", "f4", ("lon",))
            lon_var[:] = lon
            lon_var.units = "degrees_east"
            lon_var.long_name = "Longitude"
            lon_var.standard_name = "longitude"

            # Data variable
            if spec.include_ensemble_dim:
                dims = ("ensemble", "GWL", "return_period", "lat", "lon")
                chunks = (1, 1, 1, chunk_lat, chunk_lon)
            else:
                dims = ("GWL", "return_period", "lat", "lon")
                chunks = (1, 1, chunk_lat, chunk_lon)

            data_var = nc.createVariable(
                spec.variable_name,
                nc_dtype,
                dims,
                fill_value=fill_value,
                zlib=zlib,
                complevel=adaptive_complevel,
                shuffle=shuffle,
                chunksizes=chunks,
            )
            data_var.long_name = f"{spec.hazard_type} {spec.hazard_indicator}"
            data_var.units = "unknown"

            nc.title = f"{spec.hazard_type} {spec.hazard_indicator} hazard data"
            nc.source = f"Stacked from GeoTIFFs using {os.path.basename(metadata_csv)}"
            nc.history = f"Created by convert_tif_to_nc.py on {pd.Timestamp.now()}"
            nc.Conventions = "CF-1.6"

            # Write each source into its slice, streaming by spatial tiles
            for src, gwl_idx, rp_idx, tif_path in sources:
                print(
                    f"[tif->nc] {spec.hazard_type}/{spec.hazard_indicator}: "
                    f"GWL={gwl_vals[gwl_idx]} RP={rp_vals[rp_idx]} from {os.path.basename(tif_path)}"
                , flush=True)
                nodata = src.nodata
                tile_i = 0
                for y0 in range(0, height, tile_lat):
                    h = min(tile_lat, height - y0)
                    for x0 in range(0, width, tile_lon):
                        w = min(tile_lon, width - x0)
                        win = Window(x0, y0, w, h)
                        arr = src.read(1, window=win)
                        if is_int:
                            # Preserve integer dtype for much better compression.
                            arr = arr.astype(src_np_dtype, copy=False)
                            # Keep nodata as-is (integer _FillValue). Don't convert to NaN.
                        else:
                            arr = arr.astype(np.float32, copy=False)
                            if nodata is not None:
                                arr[arr == nodata] = np.nan

                        ys = slice(y0, y0 + h)
                        xs = slice(x0, x0 + w)
                        if spec.include_ensemble_dim:
                            data_var[0, gwl_idx, rp_idx, ys, xs] = arr
                        if not spec.include_ensemble_dim:
                            data_var[gwl_idx, rp_idx, ys, xs] = arr

                        tile_i += 1
                        if (
                            log_progress
                            and log_every_n_tiles > 0
                            and (tile_i % int(log_every_n_tiles) == 0)
                        ):
                            print(
                                f"  - progress: wrote {tile_i} tiles "
                                f"(io_tile={tile_lat}x{tile_lon}, nc_chunks={chunk_lat}x{chunk_lon})"
                            , flush=True)

        return output_nc_path
    finally:
        for src, _, _, _ in sources:
            try:
                src.close()
            except (OSError, RuntimeError, ValueError):
                pass


def _parse_args(argv: Optional[List[str]] = None) -> argparse.Namespace:
    p = argparse.ArgumentParser(
        description="Stack hazard GeoTIFFs into a compressed NetCDF (GWL × return_period)."
    )
    p.add_argument(
        "--hazards-dir",
        required=True,
        help="Input hazards directory (contains GeoTIFFs)",
    )
    p.add_argument("--metadata-csv", required=True, help="Path to hazards_metadata.csv")
    p.add_argument(
        "--hazard-type", required=True, help="Hazard type (e.g. Flood, Fire)"
    )
    p.add_argument(
        "--hazard-indicator", required=True, help="Hazard indicator (folder name)"
    )
    p.add_argument(
        "--variable-name", required=True, help="NetCDF data variable name to write"
    )
    p.add_argument("--output-nc", required=True, help="Output NetCDF path")
    p.add_argument(
        "--include-ensemble-dim",
        action="store_true",
        help="Include ensemble dim size 1",
    )
    p.add_argument(
        "--ensemble-value",
        default="mean",
        help="Value for ensemble dim (default: mean)",
    )
    p.add_argument(
        "--tile",
        type=int,
        default=TILE_DEFAULT,
        help="Tile size for windowed IO (default: 2000)",
    )
    p.add_argument(
        "--complevel",
        type=int,
        default=COMPRESSION_LEVEL_DEFAULT,
        help="Deflate level (1..9)",
    )
    p.add_argument("--no-shuffle", action="store_true", help="Disable shuffle filter")
    return p.parse_args(argv)


def main(argv: Optional[List[str]] = None) -> None:
    args = _parse_args(argv)
    spec = TifHazardSpec(
        hazard_type=args.hazard_type,
        hazard_indicator=args.hazard_indicator,
        variable_name=args.variable_name,
        include_ensemble_dim=bool(args.include_ensemble_dim),
        ensemble_value=str(args.ensemble_value),
    )
    out = convert_tifs_to_ensemble_return_period_nc(
        hazards_dir=args.hazards_dir,
        metadata_csv=args.metadata_csv,
        spec=spec,
        output_nc_path=args.output_nc,
        tile=int(args.tile),
        zlib=True,
        complevel=int(args.complevel),
        shuffle=(not args.no_shuffle),
    )
    print(f"✅ Wrote: {out}")
    print(f"   Size: {os.path.getsize(out) / (1024*1024):.2f} MB")


if __name__ == "__main__":
    main()
