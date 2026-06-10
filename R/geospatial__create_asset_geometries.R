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

  n_assets <- nrow(assets_df)
  invalid_rows <- which(is.na(assets_df$latitude) | is.na(assets_df$longitude))
  if (length(invalid_rows) > 0) {
    first_invalid_row <- invalid_rows[1]
    asset_name <- if ("asset" %in% names(assets_df)) {
      assets_df$asset[first_invalid_row]
    } else {
      NA_character_
    }
    stop(
      "Asset ", first_invalid_row, " (", asset_name, ") does not have valid latitude/longitude coordinates. ",
      "Assets without coordinates should use precomputed administrative hazard data."
    )
  }

  points_sf <- sf::st_as_sf(
    assets_df,
    coords = c("longitude", "latitude"),
    crs = 4326,
    remove = FALSE
  )
  points_buffer_sf <- sf::st_transform(points_sf, buffer_crs)

  size_m2_vals <- if ("size_in_m2" %in% names(assets_df)) {
    suppressWarnings(as.numeric(assets_df$size_in_m2))
  } else {
    rep(NA_real_, nrow(assets_df))
  }
  buffer_radii <- ifelse(
    !is.na(size_m2_vals) & size_m2_vals > 0,
    sqrt(size_m2_vals / pi),
    default_buffer_size_m
  )

  geometry_sfc <- sf::st_buffer(sf::st_geometry(points_buffer_sf), dist = buffer_radii)
  geometry_sfc <- sf::st_transform(geometry_sfc, output_crs)
  centroid_sfc <- sf::st_centroid(geometry_sfc)

  # Add columns to original dataframe using mutate for consistency
  assets_df <- assets_df |>
    dplyr::mutate(
      geometry = geometry_sfc,
      centroid = centroid_sfc
    )

  message("[create_asset_geometries] Created geometries for ", n_assets, " assets")

  return(assets_df)
}
