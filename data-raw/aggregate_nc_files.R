#!/usr/bin/env Rscript
#' Pre-aggregate NetCDF files for faster loading
#'
#' This script creates aggregated versions of NetCDF files by reducing
#' the spatial resolution. This is useful for testing and development
#' where full resolution is not needed.
#'
#' Usage:
#'   Rscript data-raw/aggregate_nc_files.R <nc_file_path> <aggregate_factor>
#'
#' Example:
#'   Rscript data-raw/aggregate_nc_files.R tests/tests_data/hazards/Drought/SPI3/ensemble_return_period.nc 16
#'
#' Or from R:
#'   source("data-raw/aggregate_nc_files.R")
#'   aggregate_nc_file("tests/tests_data/hazards/Drought/SPI3/ensemble_return_period.nc", 16)

library(terra)
library(ncdf4)

#' Aggregate a NetCDF file by spatial resolution
#'
#' @param nc_file Path to the NetCDF file
#' @param aggregate_factor Integer. Factor by which to aggregate (e.g., 16 means 16x16 cells -> 1 cell)
#' @param overwrite Logical. Whether to overwrite existing aggregated file
#' @return Path to the aggregated file
aggregate_nc_file <- function(nc_file, aggregate_factor = 16L, overwrite = FALSE) {
  if (!file.exists(nc_file)) {
    stop("File not found: ", nc_file)
  }
  
  if (aggregate_factor <= 1) {
    message("Aggregate factor must be > 1. Skipping.")
    return(invisible(nc_file))
  }
  
  # Build output filename
  base_name <- tools::file_path_sans_ext(basename(nc_file))
  output_name <- paste0(base_name, "__agg", aggregate_factor, ".nc")
  output_path <- file.path(dirname(nc_file), output_name)
  
  if (file.exists(output_path) && !overwrite) {
    message("Aggregated file already exists: ", output_path)
    message("Use overwrite=TRUE to regenerate.")
    return(invisible(output_path))
  }
  
  message("Aggregating: ", basename(nc_file))
  message("  Factor: ", aggregate_factor)
  message("  Output: ", output_name)
  
  # Load the NetCDF file with terra
  r <- terra::rast(nc_file)
  
  message("  Original layers: ", terra::nlyr(r))
  message("  Original dimensions: ", paste(dim(r), collapse = " x "))
  
  # Aggregate all layers at once
  r_agg <- terra::aggregate(
    r,
    fact = aggregate_factor,
    fun = mean,
    na.rm = TRUE
  )
  
  message("  New dimensions: ", paste(dim(r_agg), collapse = " x "))
  reduction <- prod(dim(r)) / prod(dim(r_agg))
  message("  Reduction factor: ", format(reduction, big.mark = ","), "x")
  
  # Write to NetCDF
  terra::writeCDF(
    r_agg,
    filename = output_path,
    overwrite = TRUE,
    compression = 6  # Good balance between speed and compression
  )
  
  message("  Written: ", output_path)
  message("  Original size: ", format(file.size(nc_file) / 1024^2, digits = 2), " MB")
  message("  New size: ", format(file.size(output_path) / 1024^2, digits = 2), " MB")
  
  return(invisible(output_path))
}

#' Aggregate all NC files in a directory recursively
#'
#' @param hazards_dir Path to hazards directory
#' @param aggregate_factor Integer. Factor by which to aggregate
#' @param overwrite Logical. Whether to overwrite existing aggregated files
#' @return Character vector of output paths
aggregate_all_nc_files <- function(hazards_dir, aggregate_factor = 16L, overwrite = FALSE) {
  nc_files <- list.files(hazards_dir, pattern = "\\.nc$", full.names = TRUE, recursive = TRUE)
  
  # Filter out already aggregated files
  nc_files <- nc_files[!grepl("__agg\\d+\\.nc$", nc_files)]
  
  if (length(nc_files) == 0) {
    message("No NC files found in: ", hazards_dir)
    return(invisible(character(0)))
  }
  
  message("Found ", length(nc_files), " NC file(s) to aggregate")
  
  output_paths <- character(length(nc_files))
  
  for (i in seq_along(nc_files)) {
    message("\n[", i, "/", length(nc_files), "]")
    output_paths[i] <- aggregate_nc_file(nc_files[i], aggregate_factor, overwrite)
  }
  
  message("\nDone! Aggregated ", length(nc_files), " file(s)")
  
  return(invisible(output_paths))
}

# Command-line interface
if (!interactive()) {
  args <- commandArgs(trailingOnly = TRUE)
  
  if (length(args) < 2) {
    cat("Usage: Rscript aggregate_nc_files.R <nc_file_path> <aggregate_factor>\n")
    cat("Example: Rscript aggregate_nc_files.R tests/tests_data/hazards/Drought/SPI3/ensemble_return_period.nc 16\n")
    quit(status = 1)
  }
  
  nc_file <- args[1]
  aggregate_factor <- as.integer(args[2])
  
  aggregate_nc_file(nc_file, aggregate_factor, overwrite = TRUE)
}

