#' Create geometries for assets using coordinates only
#'
#' @title Create asset geometries from latitude/longitude coordinates
#' @description Creates point-based geometries with buffers for assets that have coordinate information.
#'   This is a simplified version that only handles coordinate-based geolocation. Assets without 
#'   coordinates should use precomputed administrative hazard data instead.
#' @param assets_df Data frame with asset information including latitude and longitude columns
#' @param default_buffer_size_m Numeric. Default buffer size in meters for point geometries when size_in_m2 is not available (default: 1111)
#' @param output_crs Character or numeric. Output CRS for the geometries (default: 4326 for WGS84). Can be EPSG code or proj4string.
#' @return Data frame with original columns plus geometry (polygon) and centroid (point) columns in the specified output CRS
#' @examples
#' \dontrun{
#' base_dir <- system.file("tests_data", package = "climate.risk.tool")
#' assets <- read_assets(base_dir)
#' # Filter to assets with coordinates
#' assets_with_coords <- assets |> dplyr::filter(!is.na(latitude), !is.na(longitude))
#' assets_geo <- create_asset_geometries(assets_with_coords)
#' }
#' @export
create_asset_geometries <- function(assets_df, default_buffer_size_m = 1111, output_crs = 4326) {
  message("[create_asset_geometries] Creating geometries for ", nrow(assets_df), " assets...")

  # Use CRS 3857 (Web Mercator) for buffering - it uses meters as units
  # This ensures buffer distances are in actual meters, not degrees
  buffer_crs <- 3857

  # Initialize geometry and centroid columns
  n_assets <- nrow(assets_df)

  # Use purrr::map for functional approach
  geolocation_results <- purrr::map(seq_len(n_assets), function(i) {
    row <- assets_df |> dplyr::slice(i)

    # Extract lat/lon
    lat_val <- row |> dplyr::pull(.data$latitude)
    lon_val <- row |> dplyr::pull(.data$longitude)

    # Check if coordinates are available
    if (is.na(lat_val) || is.na(lon_val)) {
      asset_name <- row |> dplyr::pull(.data$asset)
      stop(
        "Asset ", i, " (", asset_name, ") does not have valid latitude/longitude coordinates. ",
        "Assets without coordinates should use precomputed administrative hazard data."
      )
    }

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

    # Transform geometry to output CRS
    geom <- sf::st_transform(geom, output_crs)

    list(
      geometry = geom,
      centroid = sf::st_centroid(geom)
    )
  })

  # Extract results
  geometry_list <- purrr::map(geolocation_results, "geometry")
  centroid_list <- purrr::map(geolocation_results, "centroid")

  # Convert to sfc objects
  geometry_sfc <- do.call(c, geometry_list)
  centroid_sfc <- do.call(c, centroid_list)

  # Add columns to original dataframe using mutate for consistency
  assets_df <- assets_df |>
    dplyr::mutate(
      geometry = geometry_sfc,
      centroid = centroid_sfc
    )

  message("[create_asset_geometries] Created geometries for ", n_assets, " assets")

  return(assets_df)
}

