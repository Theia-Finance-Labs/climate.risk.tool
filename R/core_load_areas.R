#' Load location area boundary files
#'
#' @title Load administrative boundaries from municipalities and provinces directories
#' @description Reads all .geojson files from both municipality and province directories and returns them as separate named lists.
#'   Names are derived from the file basenames without extension.
#' @param municipalities_dir Character string specifying the directory containing municipality .geojson files
#' @param provinces_dir Character string specifying the directory containing province .geojson files
#' @return List with two elements: municipalities (named list of sf objects) and provinces (named list of sf objects)
#' @examples
#' \dontrun{
#' base_dir <- file.path("tests_data", "areas")
#' areas <- load_location_areas(file.path(base_dir, "municipality"), file.path(base_dir, "province"))
#' municipalities <- areas$municipalities
#' provinces <- areas$provinces
#' }
#' @export
load_location_areas <- function(municipalities_dir, provinces_dir) {
  message("🗺️ [load_location_areas] Loading administrative boundaries...")
  message("  - Municipalities from: ", municipalities_dir)
  message("  - Provinces from: ", provinces_dir)
  
  # Helper function to load geojson files from a directory
  load_geojson_files <- function(dir_path, dir_type) {
    message("🔍 [load_location_areas] Searching for .geojson files in ", dir_type, " directory...")
    
    if (!dir.exists(dir_path)) {
      stop(dir_type, " directory not found: ", dir_path)
    }
    
    # Find all .geojson files
    geojson_files <- list.files(dir_path, pattern = "\\.geojson$", full.names = TRUE)
    
    if (length(geojson_files) == 0) {
      stop("No .geojson files found in ", dir_type, " directory: ", dir_path)
    }
    
    message("📊 [load_location_areas] Found ", length(geojson_files), " ", dir_type, " files:")
    for (geojson_file in geojson_files) {
      name <- tools::file_path_sans_ext(basename(geojson_file))
      message("  - ", name)
    }
    
    # Load each file and create named list
    message("⏳ [load_location_areas] Loading ", dir_type, " boundaries...")
    areas <- list()
    for (i in seq_along(geojson_files)) {
      geojson_file <- geojson_files[i]
      name <- tools::file_path_sans_ext(basename(geojson_file))
      
      message("  Loading ", i, "/", length(geojson_files), ": ", name)
      
      # Load the sf object
      areas[[name]] <- sf::st_read(geojson_file, quiet = TRUE)
    }
    
    message("✅ [load_location_areas] Successfully loaded ", length(areas), " ", dir_type, " boundaries")
    return(areas)
  }
  
  # Load both municipalities and provinces
  municipalities <- load_geojson_files(municipalities_dir, "Municipalities")
  provinces <- load_geojson_files(provinces_dir, "Provinces")
  
  message("✅ [load_location_areas] Completed loading all administrative boundaries")
  return(list(
    municipalities = municipalities,
    provinces = provinces
  ))
}


#' Load municipality boundary files
#'
#' @title Load municipality administrative boundaries
#' @description Reads all .geojson files from a municipality directory and returns them as a named list of sf objects.
#'   Names are derived from the file basenames without extension.
#' @param municipalities_dir Character string specifying the directory containing municipality .geojson files
#' @return Named list of sf objects, one for each .geojson file found
#' @examples
#' \dontrun{
#' municipalities_dir <- file.path("tests_data", "areas", "municipality")
#' municipalities <- load_municipalities(municipalities_dir)
#' }
#' @export
load_municipalities <- function(municipalities_dir) {
  message("🏘️ [load_municipalities] Loading municipality boundaries from: ", municipalities_dir)
  areas <- load_location_areas(municipalities_dir, municipalities_dir)
  message("✅ [load_municipalities] Successfully loaded ", length(areas$municipalities), " municipality boundaries")
  return(areas$municipalities)
}


#' Load province boundary files
#'
#' @title Load province administrative boundaries
#' @description Reads all .geojson files from a province directory and returns them as a named list of sf objects.
#'   Names are derived from the file basenames without extension.
#' @param provinces_dir Character string specifying the directory containing province .geojson files
#' @return Named list of sf objects, one for each .geojson file found
#' @examples
#' \dontrun{
#' provinces_dir <- file.path("tests_data", "areas", "province")
#' provinces <- load_provinces(provinces_dir)
#' }
#' @export
load_provinces <- function(provinces_dir) {
  message("🏛️ [load_provinces] Loading province boundaries from: ", provinces_dir)
  areas <- load_location_areas(provinces_dir, provinces_dir)
  message("✅ [load_provinces] Successfully loaded ", length(areas$provinces), " province boundaries")
  return(areas$provinces)
}
