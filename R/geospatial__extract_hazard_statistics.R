

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
  if (!is.data.frame(assets_with_geometry)) {
    stop("assets_with_geometry must be a data.frame")
  }
  if (!"geometry" %in% names(assets_with_geometry)) {
    stop("Input dataframe must have a 'geometry' column (sf geometry)")
  }
  if (!"geolocation_method" %in% names(assets_with_geometry)) {
    stop("Input dataframe must have a 'geolocation_method' column")
  }
  if (!is.list(hazards) || length(hazards) == 0) {
    stop("hazards must be a non-empty named list")
  }

  # Ensure sf structure
  assets_sf <- assets_with_geometry
  if (!inherits(assets_sf$geometry, "sfc")) {
    assets_sf <- sf::st_as_sf(assets_sf)
  } else {
    assets_sf <- sf::st_as_sf(assets_sf, sf_column_name = "geometry")
  }

  out_list <- vector("list", length(hazards))
  hn <- names(hazards)
  if (is.null(hn)) hn <- as.character(seq_along(hazards))

  for (i in seq_along(hazards)) {
    hazard_rast <- hazards[[i]]
    hazard_name <- hn[[i]]
    parts <- strsplit(hazard_name, "__", fixed = TRUE)[[1]]
    hazard_type <- if (length(parts) >= 1) parts[[1]] else "unknown"

    message("🗺️  [extract_hazard_statistics] Processing hazard ", i, "/", length(hazards), ": ", hazard_name)

    # Pre-allocate statistics data frame
    n_geoms <- nrow(assets_sf)
    stats_df <- data.frame(
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

      geom_j <- assets_sf[j, ]

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
            # Compute all statistics at once
            stats_df$hazard_mean[j] <- mean(vals, na.rm = TRUE)
            stats_df$hazard_median[j] <- stats::median(vals, na.rm = TRUE)
            stats_df$hazard_max[j] <- max(vals, na.rm = TRUE)
            stats_df$hazard_p2_5[j] <- as.numeric(stats::quantile(vals, 0.025, na.rm = TRUE, type = 7))
            stats_df$hazard_p5[j] <- as.numeric(stats::quantile(vals, 0.05, na.rm = TRUE, type = 7))
            stats_df$hazard_p95[j] <- as.numeric(stats::quantile(vals, 0.95, na.rm = TRUE, type = 7))
            stats_df$hazard_p97_5[j] <- as.numeric(stats::quantile(vals, 0.975, na.rm = TRUE, type = 7))
          }
        }
      }
    }

    df_i <- cbind(assets_sf, stats_df[, setdiff(names(stats_df), "ID"), drop = FALSE])
    df_i$hazard_name <- hazard_name
    df_i$hazard_type <- hazard_type
    df_i$hazard_intensity <- df_i$hazard_mean

    out_list[[i]] <- df_i
  }

  out <- do.call(rbind, out_list)
  out <- sf::st_drop_geometry(out)
  out
}


#' Join damage and cost factors based on hazard type, intensity and asset category (internal function)
#'
#' @param assets_with_hazards Data frame in long format with asset and hazard information
#'   including hazard_type, hazard_intensity columns (from extract_hazard_statistics)
#' @param damage_factors_df Data frame with damage and cost factors lookup table
#' @return Data frame with original columns plus damage_factor and cost_factor columns
#' @noRd
join_damage_cost_factors <- function(assets_with_hazards, damage_factors_df) {
  if (!is.data.frame(assets_with_hazards) || nrow(assets_with_hazards) == 0) {
    stop("assets_with_hazards must be a non-empty data.frame")
  }
  if (!is.data.frame(damage_factors_df) || nrow(damage_factors_df) == 0) {
    stop("damage_factors_df must be a non-empty data.frame")
  }

  # REQUIRED columns
  req_cols_assets <- c("hazard_type", "asset_category", "hazard_intensity")
  if (!all(req_cols_assets %in% names(assets_with_hazards))) {
    stop("assets_with_hazards missing required columns: ",
         paste(setdiff(req_cols_assets, names(assets_with_hazards)), collapse = ", "))
  }
  req_cols_factors <- c("hazard_type", "asset_category", "hazard_intensity", "damage_factor", "cost_factor")
  if (!all(req_cols_factors %in% names(damage_factors_df))) {
    stop("damage_factors_df missing required columns: ",
         paste(setdiff(req_cols_factors, names(damage_factors_df)), collapse = ", "))
  }

  assets_tmp <- assets_with_hazards
  assets_tmp$.__intensity_key__. <- as.integer(round(as.numeric(assets_tmp$hazard_intensity)))

  factors_tmp <- damage_factors_df
  factors_tmp$.__intensity_key__. <- as.integer(round(as.numeric(factors_tmp$hazard_intensity)))

  # Compute max available intensity key per (hazard_type, asset_category)
  max_key_by_group <- stats::aggregate(
    .__intensity_key__. ~ hazard_type + asset_category,
    data = factors_tmp,
    FUN = max
  )
  names(max_key_by_group)[names(max_key_by_group) == ".__intensity_key__."] <- ".__max_intensity_key__."

  # Attach group max to assets and cap effective key to the group's max
  assets_tmp <- merge(
    assets_tmp,
    max_key_by_group,
    by = c("hazard_type", "asset_category"),
    all.x = TRUE,
    sort = FALSE
  )

  # Effective key: if asset key > group max, cap to max; otherwise use asset key
  # If there is no matching group in factors (NA max), keep original key (merge later may yield NAs)
  assets_tmp$.__effective_intensity_key__. <- ifelse(
    !is.na(assets_tmp$.__max_intensity_key__.) &
      assets_tmp$.__intensity_key__. > assets_tmp$.__max_intensity_key__.,
    assets_tmp$.__max_intensity_key__.,
    assets_tmp$.__intensity_key__.
  )

  factors_key_cols <- c("hazard_type", "asset_category", ".__intensity_key__.", "damage_factor", "cost_factor")
  factors_key <- factors_tmp[, factors_key_cols, drop = FALSE]

  merged <- merge(
    assets_tmp,
    factors_key,
    by.x = c("hazard_type", "asset_category", ".__effective_intensity_key__."),
    by.y = c("hazard_type", "asset_category", ".__intensity_key__."),
    all.x = TRUE,
    sort = FALSE
  )

  if (!"damage_factor" %in% names(merged)) merged$damage_factor <- NA_real_
  if (!"cost_factor" %in% names(merged)) merged$cost_factor <- NA_real_
  merged$damage_factor <- as.numeric(merged$damage_factor)
  merged$cost_factor <- as.numeric(merged$cost_factor)

  # Clean helper columns
  merged$.__intensity_key__. <- NULL
  merged$.__max_intensity_key__. <- NULL
  merged$.__effective_intensity_key__. <- NULL

  merged
}
