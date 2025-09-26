#' Cut out hazard values for asset geometries
#'
#' @title Extract hazard raster values for asset polygons
#' @description For each hazard raster, extracts values within each asset's geometry polygon
#'   and adds them as new columns to the assets dataframe. Column names are derived from hazard names.
#'   Optimized to process unique geometries only once per geolocation method.
#' @param assets_with_geometry Data frame with asset information including geometry and geolocation_method columns (from geolocate_assets)
#' @param hazards Named list of hazard rasters (from load_hazards)
#' @return Data frame with original columns plus one numeric column per hazard containing extracted values
#' @examples
#' \dontrun{
#' base_dir <- system.file("tests_data", package = "climate.risk.tool")
#' assets <- read_inputs(base_dir)$assets
#' hazards <- load_hazards(file.path(base_dir, "hazards"))
#' assets_geo <- geolocate_assets(assets, hazards, base_dir)
#' assets_hazards <- cutout_hazards(assets_geo, hazards)
#' }
#' @export
cutout_hazards <- function(assets_with_geometry, hazards) {
  message("✂️ [cutout_hazards] Starting hazard extraction for ", nrow(assets_with_geometry), " assets...")

  if (!"geometry" %in% names(assets_with_geometry)) {
    stop("Input dataframe must have a 'geometry' column. Use geolocate_assets() first.")
  }

  if (!"geolocation_method" %in% names(assets_with_geometry)) {
    stop("Input dataframe must have a 'geolocation_method' column. Use updated geolocate_assets() first.")
  }

  if (length(hazards) == 0) {
    stop("Hazards list is empty")
  }
  
  # Start with the input dataframe
  result <- assets_with_geometry
  
  # Convert to sf object for spatial operations
  assets_sf <- sf::st_as_sf(assets_with_geometry)

  # Pre-compute indices for each geolocation method (avoid recomputing per hazard)
  municipality_assets <- which(assets_sf$geolocation_method == "municipality")
  province_assets <- which(assets_sf$geolocation_method == "province")
  individual_assets <- which(assets_sf$geolocation_method %in% c("coordinates", "default"))
  
  # Optimize by processing unique geometries by method
  # For municipality method, group by municipality name
  # For province method, group by province name
  # For coordinates method, process individually (each is unique)
  
  # Process each hazard
  for (i in seq_along(hazards)) {
    hazard_name <- names(hazards)[i]
    hazard_raster <- hazards[[hazard_name]]

    # Initialize column with NA values
    hazard_values <- rep(NA_real_, nrow(assets_sf))

    # Build unique geometry groups to minimize extraction calls
    group_indices <- list()
    group_geoms <- list()

    # Municipality groups
    if (length(municipality_assets) > 0) {
      unique_municipalities <- unique(assets_sf$municipality[municipality_assets])
      unique_municipalities <- unique_municipalities[!is.na(unique_municipalities)]
      for (muni in unique_municipalities) {
        idxs <- municipality_assets[assets_sf$municipality[municipality_assets] == muni]
        if (length(idxs) > 0) {
          group_indices[[length(group_indices) + 1L]] <- idxs
          group_geoms[[length(group_geoms) + 1L]] <- assets_sf$geometry[[idxs[1]]]
        }
      }
    }

    # Province groups
    if (length(province_assets) > 0) {
      unique_provinces <- unique(assets_sf$province[province_assets])
      unique_provinces <- unique_provinces[!is.na(unique_provinces)]
      for (prov in unique_provinces) {
        idxs <- province_assets[assets_sf$province[province_assets] == prov]
        if (length(idxs) > 0) {
          group_indices[[length(group_indices) + 1L]] <- idxs
          group_geoms[[length(group_geoms) + 1L]] <- assets_sf$geometry[[idxs[1]]]
        }
      }
    }

    # Individual assets
    if (length(individual_assets) > 0) {
      for (idx in individual_assets) {
        group_indices[[length(group_indices) + 1L]] <- idx
        group_geoms[[length(group_geoms) + 1L]] <- assets_sf$geometry[[idx]]
      }
    }

    # If nothing to extract, continue
    if (length(group_geoms) == 0) {
      result[[hazard_name]] <- hazard_values
      next
    }

    # Project all geometries once to hazard CRS and convert to terra vect in batch
    geoms_sf <- sf::st_sfc(group_geoms, crs = sf::st_crs(assets_sf))
    geoms_sf <- sf::st_transform(geoms_sf, terra::crs(hazard_raster))
    geoms_vect <- terra::vect(geoms_sf)

    # Optional: crop raster to geometry extent (+ small margin) to reduce IO
    ext <- terra::ext(geoms_vect)
    margin <- 0.0001
    ext <- terra::ext(terra::xmin(ext) - margin, terra::xmax(ext) + margin, terra::ymin(ext) - margin, terra::ymax(ext) + margin)
    hr <- try(terra::crop(hazard_raster, ext), silent = TRUE)
    if (!inherits(hr, "SpatRaster")) {
      hr <- hazard_raster
    }

    # Single extract call for all geometries
    extracted <- terra::extract(hr, geoms_vect, fun = mean, na.rm = TRUE)
    # extracted columns: ID, value; align to groups order
    vals <- extracted[, 2]

    # Map values back to all asset indices in each group
    for (k in seq_along(group_indices)) {
      idxs <- group_indices[[k]]
      v <- as.numeric(vals[k])
      hazard_values[idxs] <- v
    }

    # Add the column to the result dataframe
    result[[hazard_name]] <- hazard_values
  }

  message("✅ [cutout_hazards] Hazard extraction completed for ", nrow(assets_with_geometry), " assets")
  return(result)
}

# Helper function to extract hazard value for a single geometry
#' @noRd
extract_hazard_value <- function(asset_geom, hazard_raster) {
  # Convert sf geometry to terra SpatVector for extraction
  asset_vect <- terra::vect(asset_geom)
  
  # Ensure CRS compatibility
  if (!identical(terra::crs(asset_vect), terra::crs(hazard_raster))) {
    asset_vect <- terra::project(asset_vect, terra::crs(hazard_raster))
  }
  
  # Extract values within the polygon
  extracted <- terra::extract(hazard_raster, asset_vect, fun = mean, na.rm = TRUE)
  
  # Return the mean value (extracted is a data.frame with one row)
  if (nrow(extracted) > 0 && !is.na(extracted[1, 2])) {
    return(extracted[1, 2])
  } else {
    return(NA_real_)
  }
}
