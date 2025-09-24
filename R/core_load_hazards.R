#' Load hazard raster files
#'
#' @title Load hazard raster data from directory
#' @description Reads all .tif raster files from a specified directory and returns them as a named list.
#'   Names are derived from the file basenames without extension.
#' @param hazards_dir Character string specifying the directory containing hazard .tif files
#' @return Named list of SpatRaster objects, one for each .tif file found
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
  
  # Find all .tif files
  message("🔍 [load_hazards] Searching for .tif files...")
  tif_files <- list.files(hazards_dir, pattern = "\\.tif$", full.names = TRUE)
  
  if (length(tif_files) == 0) {
    stop("No .tif files found in directory: ", hazards_dir)
  }
  
  message("📊 [load_hazards] Found ", length(tif_files), " hazard raster files:")
  for (tif_file in tif_files) {
    name <- tools::file_path_sans_ext(basename(tif_file))
    message("  - ", name)
  }
  
  # Load each raster and create named list
  message("⏳ [load_hazards] Loading rasters...")
  hazards <- list()
  for (i in seq_along(tif_files)) {
    tif_file <- tif_files[i]
    name <- tools::file_path_sans_ext(basename(tif_file))
    
    message("  Loading ", i, "/", length(tif_files), ": ", name)
    
    # Load the raster using terra
    hazards[[name]] <- terra::rast(tif_file)
  }
  
  message("✅ [load_hazards] Successfully loaded ", length(hazards), " hazard rasters")
  return(hazards)
}
