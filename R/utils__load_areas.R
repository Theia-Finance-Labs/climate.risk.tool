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
  message("[load_location_areas] Loading administrative boundaries...")

  # Helper function to load geojson files from a directory
  load_geojson_files <- function(dir_path, dir_type) {
    if (!dir.exists(dir_path)) {
      stop(dir_type, " directory not found: ", dir_path)
    }

    # Find all .geojson files
    geojson_files <- list.files(dir_path, pattern = "\\.geojson$", full.names = TRUE)

    if (length(geojson_files) == 0) {
      stop("No .geojson files found in ", dir_type, " directory: ", dir_path)
    }

    # Load each file and create named list
    areas <- list()
    for (i in seq_along(geojson_files)) {
      geojson_file <- geojson_files[i]
      name <- tools::file_path_sans_ext(basename(geojson_file))

      # Load the sf object
      sf_obj <- sf::st_read(geojson_file, quiet = TRUE)

      # Set UTF-8 encoding on shapeName column for proper string comparison
      # (handles accented characters like e, a, n, etc.)
      if ("shapeName" %in% names(sf_obj)) {
        sf_obj <- sf_obj |>
          dplyr::mutate(shapeName = {
            col_data <- .data$shapeName
            Encoding(col_data) <- "UTF-8"
            col_data
          })
      }

      areas[[name]] <- sf_obj
    }

    return(areas)
  }

  # Load both municipalities and provinces
  municipalities <- load_geojson_files(municipalities_dir, "Municipalities")
  provinces <- load_geojson_files(provinces_dir, "Provinces")

  message("[load_location_areas] Loaded ", length(municipalities), " municipalities, ", length(provinces), " provinces")
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
  areas <- load_location_areas(municipalities_dir, municipalities_dir)
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
  areas <- load_location_areas(provinces_dir, provinces_dir)
  return(areas$provinces)
}
