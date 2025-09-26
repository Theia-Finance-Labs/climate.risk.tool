#' Load hazard raster files
#'
#' @title Load hazard raster data from directory (with hazard-type subfolders)
#' @description Reads all .tif raster files from a specified `hazards_dir`, looking recursively
#'   into hazard-type subfolders (e.g., `floods/`, `heat/`). Each file within a subfolder is
#'   treated as a scenario for that hazard type. Returns a named list of `terra::SpatRaster`
#'   objects where names follow the convention `"<hazardType>__<scenario>"`.
#' @param hazards_dir Character string specifying the directory containing hazard subfolders with .tif files
#' @return Named list of SpatRaster objects, one for each scenario raster found
#' @examples
#' \dontrun{
#' hazards_dir <- system.file("tests_data/hazards", package = "climate.risk.tool")
#' hazards <- load_hazards(hazards_dir)
#' }
#' @export
load_hazards <- function(hazards_dir) {
  message("🗺️ [load_hazards] Loading hazard rasters from: ", hazards_dir)

  if (!dir.exists(hazards_dir)) {
    stop("Hazards directory not found: ", hazards_dir)
  }

  # Find all .tif files recursively
  tif_files <- list.files(hazards_dir, pattern = "\\.tif$", full.names = TRUE, recursive = TRUE)

  if (length(tif_files) == 0) {
    stop("No .tif files found in directory: ", hazards_dir)
  }

  # Build display of hazard type and scenario
  pretty_names <- character(length(tif_files))
  for (i in seq_along(tif_files)) {
    tif_file <- tif_files[i]
    parent_dir <- basename(dirname(tif_file))
    scenario <- tools::file_path_sans_ext(basename(tif_file))
    # If files are directly under hazards_dir, use parent_dir of hazards_dir's name
    # but still produce a stable name
    if (dirname(tif_file) == normalizePath(hazards_dir, mustWork = FALSE)) {
      parent_dir <- "default"
    }
    pretty_names[i] <- paste0(parent_dir, "__", scenario)
  }

  # Load each raster and create named list
  hazards <- stats::setNames(vector("list", length(tif_files)), nm = pretty_names)
  for (i in seq_along(tif_files)) {
    tif_file <- tif_files[i]
    hazards[[i]] <- terra::rast(tif_file)
  }

  message("✅ [load_hazards] Successfully loaded ", length(hazards), " hazard rasters")
  return(hazards)
}
