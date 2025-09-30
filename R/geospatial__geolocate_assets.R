#' Geolocate assets using coordinates or administrative boundaries
#'
#' @title Geolocate assets with priority-based fallback
#' @description Adds geometry and centroid columns to assets data using a priority system:
#'   1. Use lat/lon coordinates when available (create buffer around point)
#'   2. Fall back to municipality matching against ADM2 boundaries
#'   3. Fall back to province matching against ADM1 boundaries
#' @param assets_df Data frame with asset information including latitude, longitude, municipality, province columns
#' @param hazards Named list of hazard rasters (from load_hazards) used to determine target CRS
#' @param municipalities_areas Named list of sf objects for municipality boundaries (from load_municipalities)
#' @param provinces_areas Named list of sf objects for province boundaries (from load_provinces)
#' @return Data frame with original columns plus geometry (polygon) and centroid (point) columns
#' @examples
#' \dontrun{
#' base_dir <- system.file("tests_data", package = "climate.risk.tool")
#' assets <- read_inputs(base_dir)$assets
#' hazards <- load_hazards(file.path(base_dir, "hazards"))
#' assets_geo <- geolocate_assets(assets, hazards)
#' }
#' @export
geolocate_assets <- function(assets_df, hazards, municipalities_areas, provinces_areas, default_buffer_size_m = 1111) {
  message("📍 [geolocate_assets] Starting asset geolocation for ", nrow(assets_df), " assets...")

  if (length(hazards) == 0) {
    stop("Hazards list is empty")
  }

  if (length(municipalities_areas) == 0) {
    stop("Municipalities areas list is empty")
  }

  if (length(provinces_areas) == 0) {
    stop("Provinces areas list is empty")
  }

  # Get target CRS from first hazard raster for final output
  target_crs <- terra::crs(hazards[[1]])

  # Use CRS 3857 (Web Mercator) for buffering - it uses meters as units
  # This ensures buffer distances are in actual meters, not degrees
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
      # Handle special characters safely
      municipality_name <- as.character(row$municipality)
      tryCatch(
        {
          municipality_match <- adm2[grepl(municipality_name, adm2$shapeName, ignore.case = TRUE), ]
          if (nrow(municipality_match) > 0) {
            geom <- sf::st_geometry(municipality_match[1, ])
            method_list[i] <- "municipality"
          }
        },
        error = function(e) {
          # If grepl fails due to encoding issues, try exact match
          municipality_match <- adm2[adm2$shapeName == municipality_name, ]
          if (nrow(municipality_match) > 0) {
            geom <- sf::st_geometry(municipality_match[1, ])
            method_list[i] <- "municipality"
          }
        }
      )
    } else if (!is.na(row$province) && nzchar(as.character(row$province))) {
      # Priority 3: Match province
      # Handle special characters safely
      province_name <- as.character(row$province)
      tryCatch(
        {
          province_match <- adm1[grepl(province_name, adm1$shapeName, ignore.case = TRUE), ]
          if (nrow(province_match) > 0) {
            geom <- sf::st_geometry(province_match[1, ])
            method_list[i] <- "province"
          }
        },
        error = function(e) {
          # If grepl fails due to encoding issues, try exact match
          province_match <- adm1[adm1$shapeName == province_name, ]
          if (nrow(province_match) > 0) {
            geom <- sf::st_geometry(province_match[1, ])
            method_list[i] <- "province"
          }
        }
      )
    }

    # If no geometry found, create a default small polygon at origin
    if (is.null(geom) || length(geom) == 0) {
      warning("No geometry found for asset ", i, ", using default location")
      # Create a small square polygon at origin in buffer CRS
      coords <- matrix(c(0, 0, 0, 1000, 1000, 1000, 1000, 0, 0, 0), ncol = 2, byrow = TRUE)
      geom <- sf::st_sfc(sf::st_polygon(list(coords)), crs = buffer_crs)
      method_list[i] <- "default"
    }

    # Transform geometry to target CRS (hazard raster CRS) for extraction
    geom <- sf::st_transform(geom, target_crs)

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
