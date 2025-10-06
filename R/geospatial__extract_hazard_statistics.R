#' Extract hazard statistics for asset geometries in long format (internal function)
#'
#' @param assets_with_geometry Data frame with asset information including geometry and geolocation_method columns (from geolocate_assets)
#' @param hazards Named list of hazard rasters (from load_hazards)
#' @param use_exactextractr Logical. If TRUE and package is available, use exactextractr; otherwise use terra::extract.
#' @return Data frame in long format with columns: asset, company, latitude, longitude,
#'   municipality, province, asset_category, size_in_m2, share_of_economic_activity,
#'   geometry, centroid, geolocation_method, hazard_name, hazard_type, hazard_intensity,
#'   hazard_mean, hazard_median, hazard_max, hazard_p2_5, hazard_p5, hazard_p95, hazard_p97_5
#' @noRd
extract_hazard_statistics <- function(assets_with_geometry, hazards, use_exactextractr = FALSE) {
  # ========= Ensure sf structure =========
  # TODO what is this and why is it necessary
  assets_sf <- assets_with_geometry
  if (!inherits(assets_sf$geometry, "sfc")) {
    assets_sf <- sf::st_as_sf(assets_sf)
  } else {
    assets_sf <- sf::st_as_sf(assets_sf, sf_column_name = "geometry")
  }
  # ========= End Ensure sf structure =========

  out_list <- vector("list", length(hazards))
  hn <- names(hazards)
  if (is.null(hn)) hn <- as.character(seq_along(hazards))

  for (i in seq_along(hazards)) {
    hazard_rast <- hazards[[i]]
    hazard_name <- hn[[i]]
    parts <- strsplit(hazard_name, "__", fixed = TRUE)[[1]]
    hazard_type <- if (length(parts) >= 1) parts[[1]] else "unknown"

    message("[extract_hazard_statistics] Processing hazard ", i, "/", length(hazards), ": ", hazard_name)

    # Pre-allocate statistics tibble
    n_geoms <- nrow(assets_sf)
    stats_df <- tibble::tibble(
      ID = seq_len(n_geoms),
      hazard_mean = NA_real_,
      hazard_median = NA_real_,
      hazard_max = NA_real_,
      hazard_p2_5 = NA_real_,
      hazard_p5 = NA_real_,
      hazard_p95 = NA_real_,
      hazard_p97_5 = NA_real_
    )

    # Get raster CRS
    r_crs <- terra::crs(hazard_rast)
    if (is.na(r_crs) || r_crs == "") r_crs <- "EPSG:4326"

    # Process each geometry individually using crop + mask approach (much faster for large polygons)
    for (j in seq_len(n_geoms)) {
      # Progress reporting every 10% of assets
      if (j %% max(1, floor(n_geoms / 10)) == 0 || j == n_geoms) {
        message("   Processing asset ", j, "/", n_geoms, " (", round(100 * j / n_geoms), "%)")
      }

      geom_j <- assets_sf |> dplyr::slice(j)

      # Transform geometry to raster CRS
      geom_j_transformed <- sf::st_transform(geom_j, r_crs)
      geom_vect <- terra::vect(geom_j_transformed)

      # Crop raster to geometry extent (reduces data to process)
      r_crop <- tryCatch(
        suppressWarnings(terra::crop(hazard_rast, geom_vect)),
        error = function(e) NULL
      )

      if (!is.null(r_crop) && terra::ncell(r_crop) > 0) {
        # Mask to exact geometry shape
        r_mask <- tryCatch(
          suppressWarnings(terra::mask(r_crop, geom_vect)),
          error = function(e) NULL
        )

        if (!is.null(r_mask) && terra::ncell(r_mask) > 0) {
          # Extract all values at once
          vals <- as.numeric(terra::values(r_mask, mat = FALSE, na.rm = TRUE))

          if (length(vals) > 0) {
            # Update stats_df using mutate for single row
            stats_df <- stats_df |>
              dplyr::mutate(
                hazard_mean = dplyr::if_else(.data$ID == j, mean(vals, na.rm = TRUE), .data$hazard_mean),
                hazard_median = dplyr::if_else(.data$ID == j, stats::median(vals, na.rm = TRUE), .data$hazard_median),
                hazard_max = dplyr::if_else(.data$ID == j, max(vals, na.rm = TRUE), .data$hazard_max),
                hazard_p2_5 = dplyr::if_else(.data$ID == j, as.numeric(stats::quantile(vals, 0.025, na.rm = TRUE, type = 7)), .data$hazard_p2_5),
                hazard_p5 = dplyr::if_else(.data$ID == j, as.numeric(stats::quantile(vals, 0.05, na.rm = TRUE, type = 7)), .data$hazard_p5),
                hazard_p95 = dplyr::if_else(.data$ID == j, as.numeric(stats::quantile(vals, 0.95, na.rm = TRUE, type = 7)), .data$hazard_p95),
                hazard_p97_5 = dplyr::if_else(.data$ID == j, as.numeric(stats::quantile(vals, 0.975, na.rm = TRUE, type = 7)), .data$hazard_p97_5)
              )
          }
        }
      }
    }

    df_i <- dplyr::bind_cols(
      assets_sf,
      stats_df |> dplyr::select(-ID)
    ) |>
      dplyr::mutate(
        hazard_name = hazard_name,
        hazard_type = hazard_type,
        hazard_intensity = .data$hazard_mean
      ) |>
      # Replace NA hazard values with 0 (treat as no hazard exposure)
      # This is for assets outside the hazard zone or where raster data is missing
      dplyr::mutate(
        hazard_intensity = dplyr::coalesce(.data$hazard_intensity, 0),
        hazard_mean = dplyr::coalesce(.data$hazard_mean, 0),
        hazard_median = dplyr::coalesce(.data$hazard_median, 0),
        hazard_max = dplyr::coalesce(.data$hazard_max, 0),
        hazard_p2_5 = dplyr::coalesce(.data$hazard_p2_5, 0),
        hazard_p5 = dplyr::coalesce(.data$hazard_p5, 0),
        hazard_p95 = dplyr::coalesce(.data$hazard_p95, 0),
        hazard_p97_5 = dplyr::coalesce(.data$hazard_p97_5, 0)
      )

    out_list[[i]] <- df_i
  }

  out <- do.call(rbind, out_list)
  out <- sf::st_drop_geometry(out)
  return(out)
}
