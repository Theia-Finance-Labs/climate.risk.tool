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
  message("📍 [geolocate_assets] Starting asset geolocation for ", nrow(assets_df), " assets...")

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
  geometry_list <- vector("list", n_assets)
  centroid_list <- vector("list", n_assets)
  method_list <- character(n_assets)

  for (i in seq_len(n_assets)) {
    row <- assets_df[i, ]
    geom <- NULL

    # Priority 1: Use lat/lon coordinates
    if (!is.na(row$latitude) && !is.na(row$longitude)) {
      # Create point geometry in WGS84
      point <- sf::st_point(c(row$longitude, row$latitude))
      point_sf <- sf::st_sfc(point, crs = 4326) # WGS84
      # Transform to CRS 3857 for metric buffering
      point_sf <- sf::st_transform(point_sf, buffer_crs)

      # Use size_in_m2 if available, otherwise default buffer
      if (!is.na(row$size_in_m2) && is.numeric(row$size_in_m2) && row$size_in_m2 > 0) {
        # Calculate radius from area (assuming circular area: A = π * r²)
        radius <- sqrt(row$size_in_m2 / pi)
        geom <- sf::st_buffer(point_sf, dist = radius)
      } else {
        # Default buffer in meters (CRS 3857 uses meters)
        geom <- sf::st_buffer(point_sf, dist = default_buffer_size_m)
      }
      method_list[i] <- "coordinates"
    } else if (!is.na(row$municipality) && nzchar(as.character(row$municipality))) {
      # Priority 2: Match municipality
      municipality_name <- as.character(row$municipality)
      municipality_match <- adm2[adm2$shapeName == municipality_name, ]
          if (nrow(municipality_match) > 0) {
            geom <- sf::st_geometry(municipality_match[1, ])
            method_list[i] <- "municipality"
          }
    } else if (!is.na(row$province) && nzchar(as.character(row$province))) {
      # Priority 3: Match province
      province_name <- as.character(row$province)
      province_match <- adm1[adm1$shapeName == province_name, ]
          if (nrow(province_match) > 0) {
            geom <- sf::st_geometry(province_match[1, ])
            method_list[i] <- "province"
          }
    }

    # Check if geometry was successfully created
    if (is.null(geom)) {
      asset_name <- if (!is.null(row$asset)) row$asset else paste0("row ", i)
      stop("Failed to geolocate asset ", i, " (", asset_name, "). ",
           "Asset must have valid coordinates, municipality, or province information.")
    }

    # Transform geometry to output CRS (WGS84)
    # extract_hazard_statistics will handle transformation to each hazard's specific CRS
    geom <- sf::st_transform(geom, output_crs)

    # Store geometry and calculate centroid
    geometry_list[[i]] <- geom
    centroid_list[[i]] <- sf::st_centroid(geom)
  }

  # Convert to sfc objects
  geometry_sfc <- do.call(c, geometry_list)
  centroid_sfc <- do.call(c, centroid_list)

  # Add columns to original dataframe
  assets_df$geometry <- geometry_sfc
  assets_df$centroid <- centroid_sfc
  assets_df$geolocation_method <- method_list

  # Summary statistics
  method_counts <- table(method_list)
  message("✅ [geolocate_assets] Geolocation completed for ", n_assets, " assets")

  return(assets_df)
}
