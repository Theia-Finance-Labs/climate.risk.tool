#' Extract hazard statistics using spatial extraction or precomputed lookups (internal function)
#'
#' @param assets_df Data frame with asset information. Can have geometry column (for spatial extraction)
#'   or municipality/province columns (for precomputed lookups)
#' @param hazards Named list of hazard rasters (from load_hazards) - used for spatial extraction
#' @param precomputed_hazards Data frame with precomputed hazard statistics (from read_precomputed_hazards)
#' @param use_exactextractr Logical. If TRUE and package is available, use exactextractr; otherwise use terra::extract.
#' @return Data frame in long format with columns: asset, company, latitude, longitude,
#'   municipality, province, asset_category, size_in_m2, share_of_economic_activity,
#'   hazard_name, hazard_type, hazard_intensity, hazard_mean, hazard_median, hazard_max, 
#'   hazard_p2_5, hazard_p5, hazard_p95, hazard_p97_5, matching_method
#' @noRd
extract_hazard_statistics <- function(assets_df, hazards, precomputed_hazards = NULL, use_exactextractr = FALSE) {
  message("[extract_hazard_statistics] Processing ", nrow(assets_df), " assets...")
  
  # Separate assets into coordinate-based and administrative-based
  assets_with_coords <- assets_df |>
    dplyr::filter(!is.na(.data$latitude), !is.na(.data$longitude))

  assets_without_coords <- assets_df |>
    dplyr::filter(is.na(.data$latitude) | is.na(.data$longitude))
  
  message("  Assets with coordinates (spatial extraction): ", nrow(assets_with_coords))
  message("  Assets without coordinates (precomputed lookup): ", nrow(assets_without_coords))
  
  # Initialize results list
  all_results <- list()
  
  # ========= Process assets WITH coordinates (spatial extraction) =========
  if (nrow(assets_with_coords) > 0) {
    message("[extract_hazard_statistics] Processing coordinate-based assets...")
    
    # Create geometries for assets with coordinates
    assets_with_geom <- create_asset_geometries(
      assets_with_coords,
      default_buffer_size_m = 1111,
      output_crs = 4326
    )
    
    # Convert to sf for spatial operations
    assets_sf <- assets_with_geom
    if (!inherits(assets_sf$geometry, "sfc")) {
      assets_sf <- sf::st_as_sf(assets_sf)
    } else {
      assets_sf <- sf::st_as_sf(assets_sf, sf_column_name = "geometry")
    }
    
    # Process each hazard raster
    coord_results_list <- vector("list", length(hazards))
    hn <- names(hazards)
    if (is.null(hn)) hn <- as.character(seq_along(hazards))
    
    for (i in seq_along(hazards)) {
      hazard_rast <- hazards[[i]]
      hazard_name <- hn[[i]]
      parts <- strsplit(hazard_name, "__", fixed = TRUE)[[1]]
      hazard_type <- if (length(parts) >= 1) parts[[1]] else "unknown"
      
      message("  Processing hazard ", i, "/", length(hazards), ": ", hazard_name)
      
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
      
      # Process each geometry
      for (j in seq_len(n_geoms)) {
        if (j %% max(1, floor(n_geoms / 10)) == 0 || j == n_geoms) {
          message("    Asset ", j, "/", n_geoms, " (", round(100 * j / n_geoms), "%)")
        }
        
        geom_j <- assets_sf |> dplyr::slice(j)
        geom_j_transformed <- sf::st_transform(geom_j, r_crs)
        geom_vect <- terra::vect(geom_j_transformed)
        
        r_crop <- tryCatch(
          suppressWarnings(terra::crop(hazard_rast, geom_vect)),
          error = function(e) NULL
        )
        
        if (!is.null(r_crop) && terra::ncell(r_crop) > 0) {
          r_mask <- tryCatch(
            suppressWarnings(terra::mask(r_crop, geom_vect)),
            error = function(e) NULL
          )
          
          if (!is.null(r_mask) && terra::ncell(r_mask) > 0) {
            vals <- as.numeric(terra::values(r_mask, mat = FALSE, na.rm = TRUE))
            
            if (length(vals) > 0) {
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
        sf::st_drop_geometry(assets_sf),
        stats_df |> dplyr::select(-"ID")
      ) |>
        dplyr::mutate(
          hazard_name = hazard_name,
          hazard_type = hazard_type,
          hazard_intensity = .data$hazard_mean,
          matching_method = "coordinates"
        ) |>
        dplyr::mutate(
          hazard_intensity = dplyr::coalesce(.data$hazard_intensity, 0),
          hazard_mean = dplyr::coalesce(.data$hazard_mean, 0),
          hazard_median = dplyr::coalesce(.data$hazard_median, 0),
          hazard_max = dplyr::coalesce(.data$hazard_max, 0),
          hazard_p2_5 = dplyr::coalesce(.data$hazard_p2_5, 0),
          hazard_p5 = dplyr::coalesce(.data$hazard_p5, 0),
          hazard_p95 = dplyr::coalesce(.data$hazard_p95, 0),
          hazard_p97_5 = dplyr::coalesce(.data$hazard_p97_5, 0)
        ) |>
        # Select only standard columns to match precomputed results format
        dplyr::select(
          "asset", "company", "latitude", "longitude",
          "municipality", "province", "asset_category", "size_in_m2",
          "share_of_economic_activity", "hazard_name", "hazard_type",
          "hazard_intensity", "hazard_mean", "hazard_median", "hazard_max",
          "hazard_p2_5", "hazard_p5", "hazard_p95", "hazard_p97_5", "matching_method"
        )
      
      coord_results_list[[i]] <- df_i
    }
    
    all_results[[length(all_results) + 1]] <- do.call(rbind, coord_results_list)
  }
  
  # ========= Process assets WITHOUT coordinates (precomputed lookup) =========
  if (nrow(assets_without_coords) > 0) {
    if (is.null(precomputed_hazards)) {
      stop("Assets without coordinates require precomputed_hazards data, but it was not provided")
    }
    
    message("[extract_hazard_statistics] Processing administrative-based assets...")
    
    precomp_results_list <- vector("list", nrow(assets_without_coords))
    
    for (i in seq_len(nrow(assets_without_coords))) {
      asset_row <- assets_without_coords |> dplyr::slice(i)
      asset_name <- asset_row |> dplyr::pull(.data$asset)
      municipality <- asset_row |> dplyr::pull(.data$municipality)
      province <- asset_row |> dplyr::pull(.data$province)
      
      # Try municipality first (ADM2), then province (ADM1)
      matched_data <- NULL
      match_level <- NULL
      
      if (!is.na(municipality) && nzchar(as.character(municipality))) {
        matched_data <- precomputed_hazards |>
          dplyr::filter(
            .data$region == municipality,
            .data$adm_level == "ADM2"
          )
        match_level <- "municipality"
      }
      
      if (is.null(matched_data) || nrow(matched_data) == 0) {
        if (!is.na(province) && nzchar(as.character(province))) {
          matched_data <- precomputed_hazards |>
            dplyr::filter(
              .data$region == province,
              .data$adm_level == "ADM1"
            )
          match_level <- "province"
        }
      }
      
      if (is.null(matched_data) || nrow(matched_data) == 0) {
        stop(
          "Cannot determine hazard statistics for asset ", i, " (", asset_name, "). ",
          "No match found in precomputed data for municipality='", municipality, 
          "' or province='", province, "'"
        )
      }
      
      # Transform precomputed data to match expected output format
      asset_hazard_data <- matched_data |>
        dplyr::mutate(
          asset = asset_name,
          company = asset_row |> dplyr::pull(.data$company),
          latitude = NA_real_,
          longitude = NA_real_,
          municipality = asset_row |> dplyr::pull(.data$municipality),
          province = asset_row |> dplyr::pull(.data$province),
          asset_category = asset_row |> dplyr::pull(.data$asset_category),
          size_in_m2 = asset_row |> dplyr::pull(.data$size_in_m2),
          share_of_economic_activity = asset_row |> dplyr::pull(.data$share_of_economic_activity),
          # Map precomputed columns to output format
          hazard_name = paste0(.data$hazard_type, "__", .data$scenario_code, "_h", .data$hazard_return_period, "glob"),
          hazard_intensity = .data$mean,
          hazard_mean = .data$mean,
          hazard_median = .data$median,
          hazard_max = .data$max,
          hazard_p2_5 = .data$p2_5,
          hazard_p5 = .data$p5,
          hazard_p95 = .data$p95,
          hazard_p97_5 = .data$p97_5,
          matching_method = match_level
        ) |>
        dplyr::select(
          "asset", "company", "latitude", "longitude",
          "municipality", "province", "asset_category", "size_in_m2",
          "share_of_economic_activity", "hazard_name", "hazard_type",
          "hazard_intensity", "hazard_mean", "hazard_median", "hazard_max",
          "hazard_p2_5", "hazard_p5", "hazard_p95", "hazard_p97_5", "matching_method"
        )
      
      precomp_results_list[[i]] <- asset_hazard_data
    }
    
    all_results[[length(all_results) + 1]] <- do.call(rbind, precomp_results_list)
  }
  
  # Combine all results
  if (length(all_results) == 0) {
    stop("No assets to process")
  }
  
  final_result <- do.call(rbind, all_results)
  message("[extract_hazard_statistics] Completed processing for ", nrow(assets_df), " assets")
  
  return(final_result)
}
