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
  
  message("🗺️ [cutout_hazards] Processing ", length(hazards), " hazard types:")
  for (hazard_name in names(hazards)) {
    message("  - ", hazard_name)
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
    
    message("⏳ [cutout_hazards] Processing hazard ", i, "/", length(hazards), ": ", hazard_name)
    
    # Initialize column with NA values
    hazard_values <- rep(NA_real_, nrow(assets_sf))
    
    # Process municipality method assets (group by municipality)
    if (length(municipality_assets) > 0) {
      unique_municipalities <- unique(assets_sf$municipality[municipality_assets])
      unique_municipalities <- unique_municipalities[!is.na(unique_municipalities)]
      
      for (muni in unique_municipalities) {
        muni_indices <- municipality_assets[assets_sf$municipality[municipality_assets] == muni]
        if (length(muni_indices) > 0) {
          # Use first geometry (they should be identical for same municipality)
          asset_geom <- assets_sf$geometry[muni_indices[1]]
          extracted_value <- extract_hazard_value(asset_geom, hazard_raster)
          # Apply to all assets with this municipality
          hazard_values[muni_indices] <- extracted_value
        }
      }
    }
    
    # Process province method assets (group by province)
    if (length(province_assets) > 0) {
      unique_provinces <- unique(assets_sf$province[province_assets])
      unique_provinces <- unique_provinces[!is.na(unique_provinces)]
      
      for (prov in unique_provinces) {
        prov_indices <- province_assets[assets_sf$province[province_assets] == prov]
        if (length(prov_indices) > 0) {
          # Use first geometry (they should be identical for same province)
          asset_geom <- assets_sf$geometry[prov_indices[1]]
          extracted_value <- extract_hazard_value(asset_geom, hazard_raster)
          # Apply to all assets with this province
          hazard_values[prov_indices] <- extracted_value
        }
      }
    }
    
    # Process coordinates and default method assets individually
    if (length(individual_assets) > 0) {
      for (idx in individual_assets) {
        asset_geom <- assets_sf$geometry[idx]
        hazard_values[idx] <- extract_hazard_value(asset_geom, hazard_raster)
      }
    }
    
    # Add the column to the result dataframe
    result[[hazard_name]] <- hazard_values
    
    # Count non-NA values for this hazard
    non_na_count <- sum(!is.na(hazard_values))
    message("  ✅ Extracted values for ", non_na_count, "/", nrow(assets_sf), " assets")
  }
  
  message("✅ [cutout_hazards] Hazard extraction completed for all ", length(hazards), " hazard types")
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
