#' Downscale hazard rasters by aggregating to lower resolution
#'
#' @title Downscale hazard rasters
#' @description Create lower-resolution versions of hazard rasters by aggregating
#'   cells using a summary function (default: mean). This preserves the full
#'   spatial extent and CRS while reducing the number of rows/columns by the
#'   aggregation factor. Useful for faster experimentation and testing.
#'
#' @param hazards Named list of `terra::SpatRaster` as returned by `load_hazards()`
#' @param factor Positive integer aggregation factor (e.g., 2, 4, 8, 16). Each
#'   block of `factor x factor` cells is summarized into one cell.
#' @param fun Aggregation function to apply; either a function or one of
#'   `"mean"`, `"median"`, `"max"`, `"min"`. Defaults to `"mean"`.
#' @return Named list of `terra::SpatRaster` with same names as input and lower
#'   resolution. Extent and CRS are preserved.
#' @examples
#' \dontrun{
#' base_dir <- system.file("tests_data", package = "climate.risk.tool")
#' hz <- load_hazards(file.path(base_dir, "hazards"))
#' hz_ds <- downscale_hazard_rasters(hz, factor = 8)
#' }
#' @export
downscale_hazard_rasters <- function(hazards, factor = 8L, fun = "mean") {
  if (!is.list(hazards) || length(hazards) == 0) {
    stop("hazards must be a non-empty named list of terra::SpatRaster")
  }
  if (is.null(names(hazards)) || anyNA(names(hazards)) || any(names(hazards) == "")) {
    stop("hazards must be a named list")
  }
  if (!is.numeric(factor) || length(factor) != 1 || factor < 1) {
    stop("factor must be a positive integer >= 1")
  }

  # Resolve aggregation function
  agg_fun <- fun
  if (is.character(fun)) {
    fun_lc <- tolower(fun)
    if (fun_lc %in% c("mean", "median", "max", "min")) {
      # Use functions from base stats
      agg_fun <- switch(fun_lc,
        mean = mean,
        median = stats::median,
        max = max,
        min = min
      )
    } else {
      stop("Unsupported fun: ", fun, ". Provide a function or one of 'mean','median','max','min'.")
    }
  }

  result <- stats::setNames(vector("list", length(hazards)), names(hazards))

  for (i in seq_along(hazards)) {
    r <- hazards[[i]]
    if (!inherits(r, "SpatRaster")) {
      stop("hazards[[", i, "]] is not a terra::SpatRaster")
    }

    # Aggregate by factor. terra::aggregate preserves extent and CRS.
    rd <- terra::aggregate(r, fact = as.integer(factor), fun = agg_fun, na.rm = TRUE)
    result[[i]] <- rd
  }

  result
}


#' Downscale all hazard rasters in a directory tree
#'
#' @description Reads all `.tif` rasters under `input_dir`, aggregates them by
#'   `factor`, and writes them into `output_dir` mirroring the subdirectory
#'   structure and filenames.
#' @param input_dir Directory containing hazard rasters (possibly in subfolders)
#' @param output_dir Output directory to write downscaled rasters; will be
#'   created if it does not exist
#' @param factor Positive integer aggregation factor (e.g., 2, 4, 8, 16)
#' @param fun Aggregation function or one of `"mean"`, `"median"`, `"max"`, `"min"`
#' @param overwrite Logical; whether to overwrite existing files
#' @return Invisibly returns `output_dir`
#' @examples
#' \dontrun{
#' base_dir <- system.file("tests_data", package = "climate.risk.tool")
#' in_dir <- file.path(base_dir, "hazards")
#' out_dir <- tempfile("hz_ds_")
#' downscale_hazard_dir(in_dir, out_dir, factor = 8)
#' }
#' @export
downscale_hazard_dir <- function(input_dir, output_dir, factor = 8L, fun = "mean", overwrite = FALSE) {
  if (!dir.exists(input_dir)) {
    stop("Input directory not found: ", input_dir)
  }
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }

  tif_files <- list.files(input_dir, pattern = "\\.tif$", full.names = TRUE, recursive = TRUE)
  if (length(tif_files) == 0) stop("No .tif files found under ", input_dir)

  # Prepare aggregation function mapping; validate early if character
  agg_fun <- fun
  if (is.character(fun)) {
    fun_lc <- tolower(fun)
    if (fun_lc %in% c("mean", "median", "max", "min")) {
      agg_fun <- switch(fun_lc,
        mean = mean,
        median = stats::median,
        max = max,
        min = min
      )
    } else {
      stop("Unsupported fun: ", fun, ". Provide a function or one of 'mean','median','max','min'.")
    }
  }

  for (tif in tif_files) {
    r <- terra::rast(tif)
    rd <- terra::aggregate(r, fact = as.integer(factor), fun = agg_fun, na.rm = TRUE)

    # Mirror structure
    rel <- substr(dirname(tif), nchar(normalizePath(input_dir, mustWork = FALSE)) + 2L, nchar(dirname(tif)))
    if (!nchar(rel)) rel <- ""
    out_dir_full <- if (nchar(rel) > 0) file.path(output_dir, rel) else output_dir
    if (!dir.exists(out_dir_full)) dir.create(out_dir_full, recursive = TRUE, showWarnings = FALSE)
    out_path <- file.path(out_dir_full, basename(tif))

    terra::writeRaster(rd, out_path, overwrite = isTRUE(overwrite))
  }

  invisible(output_dir)
}


