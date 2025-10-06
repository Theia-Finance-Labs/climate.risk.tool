#' Geolocate assets using coordinates or administrative boundaries
#'
#' @title Geolocate assets with priority-based fallback
#' @description Adds geometry and centroid columns to assets data using a priority system:
#'   1. Use lat/lon coordinates when available (create buffer around point)
#'   2. Fall back to municipality matching against ADM2 boundaries
#'   3. Fall back to province matching against ADM1 boundaries
#' @param assets_df Data frame with asset information including latitude, longitude, municipality, province columns
#' @param municipalities_areas Named list of sf objects for municipality boundaries (from load_municipalities)
#' @param provinces_areas Named list of sf objects for province boundaries (from load_provinces)
#' @param default_buffer_size_m Numeric. Default buffer size in meters for point geometries when size_in_m2 is not available (default: 1111)
#' @param output_crs Character or numeric. Output CRS for the geometries (default: 4326 for WGS84). Can be EPSG code or proj4string.
#' @return Data frame with original columns plus geometry (polygon) and centroid (point) columns in the specified output CRS
#' @examples
#' \dontrun{
#' base_dir <- system.file("tests_data", package = "climate.risk.tool")
#' assets <- read_inputs(base_dir)$assets
#' municipalities <- load_municipalities(file.path(base_dir, "areas", "municipality"))
#' provinces <- load_provinces(file.path(base_dir, "areas", "province"))
#' assets_geo <- geolocate_assets(assets, municipalities, provinces)
#' }
#' @export
geolocate_assets <- function(assets_df, municipalities_areas, provinces_areas, default_buffer_size_m = 1111, output_crs = 4326) {
  message("[geolocate_assets] Starting asset geolocation for ", nrow(assets_df), " assets...")

  # Use CRS 3857 (Web Mercator) for buffering - it uses meters as units
  # This ensures buffer distances are in actual meters, not degrees (needed for default_buffer_size_m)
  buffer_crs <- 3857

  # Combine all municipality and province boundaries
  # For simplicity, take the first municipality and province files
  adm2 <- municipalities_areas[[1]]
  adm1 <- provinces_areas[[1]]

  # Transform boundaries to buffer CRS for accurate metric operations
  adm2 <- sf::st_transform(adm2, buffer_crs)
  adm1 <- sf::st_transform(adm1, buffer_crs)

  # Initialize geometry, centroid, and method columns
  n_assets <- nrow(assets_df)

  # Use purrr::map for functional approach (more readable than for loop)
  geolocation_results <- purrr::map(seq_len(n_assets), function(i) {
    row <- assets_df |> dplyr::slice(i)
    geom <- NULL
    method <- "failed"

    # Priority 1: Use lat/lon coordinates
    lat_val <- row |> dplyr::pull(.data$latitude)
    lon_val <- row |> dplyr::pull(.data$longitude)
    
    if (!is.na(lat_val) && !is.na(lon_val)) {
      # Create point geometry in WGS84
      point <- sf::st_point(c(lon_val, lat_val))
      point_sf <- sf::st_sfc(point, crs = 4326) # WGS84
      # Transform to CRS 3857 for metric buffering
      point_sf <- sf::st_transform(point_sf, buffer_crs)

      # Use size_in_m2 if available, otherwise default buffer
      size_m2_val <- row |> dplyr::pull(.data$size_in_m2)
      if (!is.na(size_m2_val) && is.numeric(size_m2_val) && size_m2_val > 0) {
        # Calculate radius from area (assuming circular area: A = pi * r^2)
        radius <- sqrt(size_m2_val / pi)
        geom <- sf::st_buffer(point_sf, dist = radius)
      } else {
        # Default buffer in meters (CRS 3857 uses meters)
        geom <- sf::st_buffer(point_sf, dist = default_buffer_size_m)
      }
      method <- "coordinates"
    } else {
      municipality_val <- row |> dplyr::pull(.data$municipality)
      if (!is.na(municipality_val) && nzchar(as.character(municipality_val))) {
        # Priority 2: Match municipality
        municipality_name <- as.character(municipality_val)
        municipality_match <- adm2 |> dplyr::filter(.data$shapeName == municipality_name)
        if (nrow(municipality_match) > 0) {
          geom <- sf::st_geometry(municipality_match |> dplyr::slice(1))
          method <- "municipality"
        }
      } else {
        province_val <- row |> dplyr::pull(.data$province)
        if (!is.na(province_val) && nzchar(as.character(province_val))) {
          # Priority 3: Match province
          province_name <- as.character(province_val)
          province_match <- adm1 |> dplyr::filter(.data$shapeName == province_name)
          if (nrow(province_match) > 0) {
            geom <- sf::st_geometry(province_match |> dplyr::slice(1))
            method <- "province"
          }
        }
      }
    }

    # Check if geometry was successfully created
    if (is.null(geom)) {
      asset_name <- row |> dplyr::pull(.data$asset)  
      stop(
        "Failed to geolocate asset ", i, " (", asset_name, "). ",
        "Asset must have valid coordinates, municipality, or province information."
      )
    }

    # Transform geometry to output CRS (WGS84)
    # extract_hazard_statistics will handle transformation to each hazard's specific CRS
    geom <- sf::st_transform(geom, output_crs)

    list(
      geometry = geom,
      centroid = sf::st_centroid(geom),
      method = method
    )
  })

  # Extract results
  geometry_list <- purrr::map(geolocation_results, "geometry")
  centroid_list <- purrr::map(geolocation_results, "centroid")
  method_list <- purrr::map_chr(geolocation_results, "method")

  # Convert to sfc objects
  geometry_sfc <- do.call(c, geometry_list)
  centroid_sfc <- do.call(c, centroid_list)

  # Add columns to original dataframe using mutate for consistency
  assets_df <- assets_df |>
    dplyr::mutate(
      geometry = geometry_sfc,
      centroid = centroid_sfc,
      geolocation_method = method_list
    )

  # Summary statistics
  method_counts <- table(method_list)
  message("[geolocate_assets] Geolocation completed for ", n_assets, " assets")

  return(assets_df)
}
