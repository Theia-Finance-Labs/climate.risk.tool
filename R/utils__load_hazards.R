#' Load hazard raster files
#'
#' @title Load hazard raster data from directory (with hazard-type subfolders)
#' @description Reads all .tif raster files from a specified `hazards_dir`, looking recursively
#'   into hazard-type subfolders (e.g., `floods/`, `heat/`). Each file within a subfolder is
#'   treated as a scenario for that hazard type. Returns a named list of `terra::SpatRaster`
#'   objects where names follow the convention `"<hazardType>__<scenario>"`.
#' @param hazards_dir Character string specifying the directory containing hazard subfolders with .tif files
#' @param aggregate_factor Integer >= 1. If >1, aggregate rasters by this factor during loading for speed (default: 1)
#' @param cache_aggregated Logical. If TRUE and aggregate_factor > 1, save and reuse aggregated rasters next to originals
#'   with filename suffix `__agg{factor}.tif` (default: TRUE)
#' @param force_reaggregate Logical. If TRUE, recompute aggregated rasters even if cached files exist (default: FALSE)
#' @param memfrac Numeric in (0,1]. Memory fraction hint passed to terra options during load (default: 0.3)
#' @return Named list of SpatRaster objects, one for each scenario raster found
#' @examples
#' \dontrun{
#' hazards_dir <- system.file("tests_data/hazards", package = "climate.risk.tool")
#' hazards <- load_hazards(hazards_dir)
#' }
#' @export
load_hazards <- function(hazards_dir,
                         aggregate_factor = 16L,
                         cache_aggregated = TRUE,
                         force_reaggregate = FALSE,
                         memfrac = 0.3) {
  message("🗺️ [load_hazards] Loading hazard rasters from: ", hazards_dir)

  if (!dir.exists(hazards_dir)) {
    stop("Hazards directory not found: ", hazards_dir)
  }

  # Find all .tif files recursively
  all_tif_files <- list.files(hazards_dir, pattern = "\\.tif$", full.names = TRUE, recursive = TRUE)

  if (length(all_tif_files) == 0) {
    stop("No .tif files found in directory: ", hazards_dir)
  }

  # Filter files based on aggregation factor
  if (is.numeric(aggregate_factor) && aggregate_factor > 1L) {
    # If aggregate_factor > 1, prefer aggregated files that match this factor
    agg_pattern <- paste0("__agg", aggregate_factor, "\\.tif$")
    aggregated_files <- all_tif_files[grepl(agg_pattern, all_tif_files)]
    
    if (length(aggregated_files) > 0) {
      # Use aggregated files that match the factor
      tif_files <- aggregated_files
      message("🗺️ [load_hazards] Found ", length(aggregated_files), " pre-aggregated files matching factor ", aggregate_factor)
    } else {
      # No matching aggregated files, use original files (will be aggregated on-the-fly)
      original_files <- all_tif_files[!grepl("__agg\\d+\\.tif$", all_tif_files)]
      tif_files <- original_files
      message("🗺️ [load_hazards] No pre-aggregated files found, will aggregate ", length(original_files), " original files")
    }
  } else {
    # If aggregate_factor <= 1, use only original files (not pre-aggregated)
    original_files <- all_tif_files[!grepl("__agg\\d+\\.tif$", all_tif_files)]
    tif_files <- original_files
    message("🗺️ [load_hazards] Using ", length(original_files), " original files (no aggregation)")
  }

  if (length(tif_files) == 0) {
    stop("No suitable .tif files found for aggregate_factor = ", aggregate_factor)
  }

  # Build display of hazard type and scenario
  pretty_names <- character(length(tif_files))
  for (i in seq_along(tif_files)) {
    tif_file <- tif_files[i]
    parent_dir <- basename(dirname(tif_file))
    scenario <- tools::file_path_sans_ext(basename(tif_file))
    
    # Remove aggregation suffix from scenario name for cleaner display
    scenario <- sub("__agg\\d+$", "", scenario)
    
    # If files are directly under hazards_dir, use parent_dir of hazards_dir's name
    # but still produce a stable name
    if (dirname(tif_file) == normalizePath(hazards_dir, mustWork = FALSE)) {
      parent_dir <- "default"
    }
    pretty_names[i] <- paste0(parent_dir, "__", scenario)
  }

  # Configure terra for efficient on-disk processing while loading
  old_opts <- terra::terraOptions()
  on.exit(try(terra::terraOptions(old_opts), silent = TRUE), add = TRUE)
  terra::terraOptions(todisk = TRUE, memfrac = memfrac, progress = 0)

  # Load each raster and create named list, with optional aggregation & caching
  hazards <- stats::setNames(vector("list", length(tif_files)), nm = pretty_names)
  attr(hazards, "source_files") <- tif_files
  for (i in seq_along(tif_files)) {
    tif_file <- tif_files[i]
    message("🗺️ [load_hazards] Loading ", basename(tif_file), "...")
    
    # Load raster
    r <- terra::rast(tif_file)
    
    # Check if this file is already aggregated at the target factor
    base_filename <- basename(tif_file)
    target_agg_pattern <- paste0("__agg", aggregate_factor, "\\.tif$")
    is_already_target_aggregated <- grepl(target_agg_pattern, base_filename)
    
    # Aggregate during loading if factor > 1 and file is not already at target aggregation
    if (is.numeric(aggregate_factor) && aggregate_factor > 1L && !is_already_target_aggregated) {
      orig_dims <- dim(r)
      message("   Original dimensions: ", paste(orig_dims, collapse = "x"))
      message("   Aggregating by factor ", aggregate_factor, "...")

      # Detect if current file is already aggregated to avoid recursive aggregation
      base_filename <- basename(tif_file)
      if (grepl("__agg\\d+\\.tif$", base_filename)) {
        # File is already aggregated, extract original name by removing the last __agg{factor} suffix
        original_name <- sub("__agg\\d+\\.tif$", ".tif", base_filename)
        message("   Detected already aggregated file, using original name: ", original_name)
      } else {
        original_name <- base_filename
      }
      
      cache_filename <- paste0(tools::file_path_sans_ext(original_name), "__agg", aggregate_factor, ".tif")
      cache_file <- file.path(dirname(tif_file), cache_filename)

      if (isTRUE(cache_aggregated) && file.exists(cache_file) && !isTRUE(force_reaggregate)) {
        message("   Using cached aggregated raster: ", cache_filename)
        r <- terra::rast(cache_file)
      } else {
        # Perform aggregation, writing directly to disk to avoid RAM spikes
        r_agg <- try(
          terra::aggregate(
            r,
            fact = aggregate_factor,
            fun = mean,
            na.rm = TRUE,
            filename = if (isTRUE(cache_aggregated)) cache_file else "",
            overwrite = TRUE,
            wopt = list(
              datatype = "FLT4S",
              gdal = c("BIGTIFF=YES", "COMPRESS=LZW", "TILED=YES")
            )
          ),
          silent = TRUE
        )
        if (!inherits(r_agg, "try-error")) {
          new_dims <- dim(r_agg)
          reduction <- prod(orig_dims) / prod(new_dims)
          message("   New dimensions: ", paste(new_dims, collapse = "x"), " (", 
                  format(reduction, big.mark = ","), "x smaller)")
          r <- if (isTRUE(cache_aggregated)) terra::rast(cache_file) else r_agg
        } else {
          message("   Aggregation failed, using original raster")
        }
      }
    } else if (is_already_target_aggregated) {
      message("   Using pre-aggregated raster (factor ", aggregate_factor, ")")
    }
    
    hazards[[i]] <- r
  }

  message("✅ [load_hazards] Successfully loaded ", length(hazards), " hazard rasters")
  return(hazards)
}
