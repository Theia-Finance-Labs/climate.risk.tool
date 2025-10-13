#' Extract hazard statistics using spatial extraction or precomputed lookups (internal function)
#'
#' @param assets_df Data frame with asset information. Can have geometry column (for spatial extraction)
#'   or municipality/province columns (for precomputed lookups)
#' @param hazards Named list of hazard rasters (from load_hazards) - used for spatial extraction
#' @param hazards_inventory Data frame with hazard metadata (hazard_name, hazard_type, hazard_indicator, etc.)
#' @param precomputed_hazards Data frame with precomputed hazard statistics (from read_precomputed_hazards)
#' @return Data frame in long format with columns: asset, company, latitude, longitude,
#'   municipality, province, asset_category, size_in_m2, share_of_economic_activity,
#'   hazard_name, hazard_type, hazard_indicator, hazard_intensity, hazard_mean, hazard_median, hazard_max, 
#'   hazard_p2_5, hazard_p5, hazard_p95, hazard_p97_5, matching_method
#' @noRd
extract_hazard_statistics <- function(assets_df, hazards, hazards_inventory, precomputed_hazards = NULL) {
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
    
    # Detect source type from inventory to determine extraction workflow
    inventory_sources <- hazards_inventory |>
      dplyr::distinct(.data$source) |>
      dplyr::pull(.data$source)
    
    has_nc_sources <- "nc" %in% inventory_sources
    has_tif_sources <- "tif" %in% inventory_sources
    
    if (has_nc_sources && has_tif_sources) {
      message("  Detected MIXED sources (TIF + NC) - using appropriate workflow for each")
    } else if (has_nc_sources) {
      message("  Detected NC sources - will extract pre-computed statistics directly")
    } else {
      message("  Detected TIF sources - will compute statistics from spatial extraction")
    }
    
    # Determine extraction workflow based on source
    if (has_nc_sources) {
      # NC workflow: Extract pre-computed statistics from ensemble rasters
      all_results[[length(all_results) + 1]] <- extract_nc_statistics(
        assets_with_coords, hazards, hazards_inventory
      )
    }
    
    if (has_tif_sources) {
      # TIF workflow: Compute statistics from spatial extraction
      all_results[[length(all_results) + 1]] <- extract_tif_statistics(
        assets_with_coords, hazards, hazards_inventory
      )
    }
    
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
          hazard_indicator = .data$hazard_type,  # Default to hazard_type for precomputed data
          hazard_intensity = .data$mean,
          hazard_mean = .data$mean,
          hazard_median = .data$median,
          hazard_max = .data$max,
          hazard_min = NA_real_,  # Not available in precomputed data
          hazard_p2_5 = .data$p2_5,
          hazard_p5 = .data$p5,
          hazard_p10 = NA_real_,  # Not available in precomputed data
          hazard_p90 = NA_real_,  # Not available in precomputed data
          hazard_p95 = .data$p95,
          hazard_p97_5 = .data$p97_5,
          matching_method = match_level
        ) |>
        dplyr::select(
          "asset", "company", "latitude", "longitude",
          "municipality", "province", "asset_category", "size_in_m2",
          "share_of_economic_activity", "hazard_name", "hazard_type",
          "hazard_indicator", "hazard_intensity", "hazard_mean", "hazard_median", "hazard_max", "hazard_min",
          "hazard_p2_5", "hazard_p5", "hazard_p10", "hazard_p90", "hazard_p95", "hazard_p97_5", "matching_method"
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

#' Extract statistics from NC-sourced hazards (pre-computed ensemble values)
#' @noRd
extract_nc_statistics <- function(assets_df, hazards, hazards_inventory) {
  message("  [extract_nc_statistics] Extracting pre-computed statistics from NC files...")
  
  # Filter to NC hazards only
  nc_inventory <- hazards_inventory |>
    dplyr::filter(.data$source == "nc")
  
  if (nrow(nc_inventory) == 0) {
    return(tibble::tibble())
  }
  
  # Group NC hazards by base event (same hazard_type/indicator/scenario/RP, different ensemble)
  # Parse hazard names to extract base event info
  nc_inventory <- nc_inventory |>
    dplyr::mutate(
      # Extract base event ID (everything before __ensemble=)
      base_event_id = sub("__ensemble=.*$", "", .data$hazard_name)
    )
  
  base_events <- nc_inventory |>
    dplyr::distinct(.data$base_event_id, .data$hazard_type, .data$hazard_indicator,
                    .data$scenario_name, .data$hazard_return_period, .data$scenario_code)
  
  message("    Found ", nrow(base_events), " unique NC events")
  
  # Create geometries for assets
  assets_sf <- create_asset_geometries(
    assets_df,
    default_buffer_size_m = 1111,
    output_crs = 4326
  )
  
  # Convert to sf
  if (!inherits(assets_sf$geometry, "sfc")) {
    assets_sf <- sf::st_as_sf(assets_sf)
  } else {
    assets_sf <- sf::st_as_sf(assets_sf, sf_column_name = "geometry")
  }
  
  results_list <- list()
  
  # Process each base event
  for (i in seq_len(nrow(base_events))) {
    base_event <- base_events |> dplyr::slice(i)
    base_event_id <- base_event$base_event_id
    
    message("    Processing NC event ", i, "/", nrow(base_events), ": ", base_event_id)
    
    # Get all ensemble rasters for this base event
    # Use !! to force evaluation and ensure exact string matching
    current_base_event_id <- base_event_id
    event_hazards <- nc_inventory |>
      dplyr::filter(.data$base_event_id == !!current_base_event_id)
    
    # Map ensemble values to output column names
    # Match exactly what NC files provide
    ensemble_mapping <- list(
      mean = "hazard_mean",
      median = "hazard_median",
      max = "hazard_max",
      min = "hazard_min",
      p10 = "hazard_p10",
      p90 = "hazard_p90",
      "p2.5" = "hazard_p2_5",
      "p97.5" = "hazard_p97_5",
      p5 = "hazard_p5",
      p95 = "hazard_p95"
    )
    
    # Initialize statistics data frame with all possible NC ensemble statistics
    stats_df <- tibble::tibble(
      asset_id = seq_len(nrow(assets_sf)),
      hazard_mean = NA_real_,
      hazard_median = NA_real_,
      hazard_max = NA_real_,
      hazard_min = NA_real_,
      hazard_p2_5 = NA_real_,
      hazard_p5 = NA_real_,
      hazard_p10 = NA_real_,
      hazard_p90 = NA_real_,
      hazard_p95 = NA_real_,
      hazard_p97_5 = NA_real_
    )
    
    # Extract from each ensemble raster
    for (j in seq_len(nrow(event_hazards))) {
      haz_row <- event_hazards |> dplyr::slice(j)
      haz_name <- haz_row$hazard_name
      ensemble_val <- haz_row$ensemble
      
      # Get the raster
      if (!haz_name %in% names(hazards)) {
        message("      Warning: Hazard '", haz_name, "' not found in hazards list, skipping")
        next
      }
      
      hazard_rast <- hazards[[haz_name]]
      
      # Determine which column to fill based on ensemble value
      col_name <- NULL
      for (ens_key in names(ensemble_mapping)) {
        if (tolower(ensemble_val) == tolower(ens_key) || 
            gsub("\\.", "_", tolower(ensemble_val)) == tolower(ens_key)) {
          col_name <- ensemble_mapping[[ens_key]]
          break
        }
      }
      
      if (is.null(col_name)) {
        message("      Warning: Unknown ensemble value '", ensemble_val, "', skipping")
        next
      }
      
      message("      Extracting ensemble='", ensemble_val, "' -> ", col_name)
      
      # Extract values for all assets (point extraction at centroid)
      r_crs <- terra::crs(hazard_rast)
      if (is.na(r_crs) || r_crs == "") r_crs <- "EPSG:4326"
      
      # For NC files with pre-computed statistics, extract from centroids (points), not polygons
      # This ensures we get exactly one value per asset
      centroids_sf <- sf::st_set_geometry(assets_sf, assets_sf$centroid)
      centroids_transformed <- sf::st_transform(centroids_sf, r_crs)
      centroids_vect <- terra::vect(centroids_transformed)
      
      # Extract values (using centroids for NC since they have pre-computed stats)
      extracted_vals <- terra::extract(hazard_rast, centroids_vect, method = "simple", ID = FALSE)
      
      # Assign to appropriate column
      if (ncol(extracted_vals) > 0) {
        stats_df[[col_name]] <- as.numeric(extracted_vals[[1]])
      }
    }
    
    # Combine with asset data
    event_result <- dplyr::bind_cols(
      sf::st_drop_geometry(assets_sf),
      stats_df |> dplyr::select(-"asset_id")
    ) |>
      dplyr::mutate(
        # Use the mean ensemble as the canonical hazard_name for this event
        hazard_name = paste0(base_event_id, "__ensemble=mean"),
        hazard_type = base_event$hazard_type,
        hazard_indicator = base_event$hazard_indicator,
        hazard_intensity = .data$hazard_mean,
        matching_method = "coordinates"
      ) |>
      dplyr::mutate(
        hazard_intensity = dplyr::coalesce(.data$hazard_intensity, 0),
        hazard_mean = dplyr::coalesce(.data$hazard_mean, 0),
        hazard_median = dplyr::coalesce(.data$hazard_median, 0),
        hazard_max = dplyr::coalesce(.data$hazard_max, 0),
        hazard_min = dplyr::coalesce(.data$hazard_min, 0),
        hazard_p2_5 = dplyr::coalesce(.data$hazard_p2_5, 0),
        hazard_p5 = dplyr::coalesce(.data$hazard_p5, 0),
        hazard_p10 = dplyr::coalesce(.data$hazard_p10, 0),
        hazard_p90 = dplyr::coalesce(.data$hazard_p90, 0),
        hazard_p95 = dplyr::coalesce(.data$hazard_p95, 0),
        hazard_p97_5 = dplyr::coalesce(.data$hazard_p97_5, 0)
      ) |>
      dplyr::select(
        "asset", "company", "latitude", "longitude",
        "municipality", "province", "asset_category", "size_in_m2",
        "share_of_economic_activity", "hazard_name", "hazard_type",
        "hazard_indicator", "hazard_intensity", "hazard_mean", "hazard_median", "hazard_max", "hazard_min",
        "hazard_p2_5", "hazard_p5", "hazard_p10", "hazard_p90", "hazard_p95", "hazard_p97_5", "matching_method"
      )
    
    results_list[[i]] <- event_result
  }
  
  if (length(results_list) == 0) {
    return(tibble::tibble())
  }
  
  return(do.call(rbind, results_list))
}

#' Extract and compute statistics from TIF-sourced hazards
#' @noRd
extract_tif_statistics <- function(assets_df, hazards, hazards_inventory) {
  message("  [extract_tif_statistics] Computing statistics from TIF spatial extraction...")
  
  # Filter to TIF hazards only
  tif_inventory <- hazards_inventory |>
    dplyr::filter(.data$source == "tif")
  
  if (nrow(tif_inventory) == 0) {
    return(tibble::tibble())
  }
  
  # Create geometries for assets
  assets_sf <- create_asset_geometries(
    assets_df,
    default_buffer_size_m = 1111,
    output_crs = 4326
  )
  
  # Convert to sf
  if (!inherits(assets_sf$geometry, "sfc")) {
    assets_sf <- sf::st_as_sf(assets_sf)
  } else {
    assets_sf <- sf::st_as_sf(assets_sf, sf_column_name = "geometry")
  }
  
  # Get TIF hazard names
  tif_hazard_names <- tif_inventory$hazard_name
  
  results_list <- vector("list", length(tif_hazard_names))
  
  message("    Found ", length(tif_hazard_names), " TIF hazards")
  
  for (i in seq_along(tif_hazard_names)) {
    hazard_name <- tif_hazard_names[[i]]
    
    if (!hazard_name %in% names(hazards)) {
      message("      Warning: TIF hazard '", hazard_name, "' not found in hazards list, skipping")
      next
    }
    
    hazard_rast <- hazards[[hazard_name]]
    
    # Get metadata
    hazard_meta <- tif_inventory |>
      dplyr::filter(.data$hazard_name == hazard_name) |>
      dplyr::slice(1)
    
    hazard_type <- hazard_meta$hazard_type
    hazard_indicator <- hazard_meta$hazard_indicator
    
    message("    Processing TIF hazard ", i, "/", length(tif_hazard_names), ": ", hazard_name)
    
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
        message("      Asset ", j, "/", n_geoms, " (", round(100 * j / n_geoms), "%)")
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
        hazard_indicator = hazard_indicator,
        hazard_intensity = .data$hazard_mean,
        matching_method = "coordinates",
        # Add missing columns for consistency with NC output
        hazard_min = NA_real_,
        hazard_p10 = NA_real_,
        hazard_p90 = NA_real_
      ) |>
      dplyr::mutate(
        hazard_intensity = dplyr::coalesce(.data$hazard_intensity, 0),
        hazard_mean = dplyr::coalesce(.data$hazard_mean, 0),
        hazard_median = dplyr::coalesce(.data$hazard_median, 0),
        hazard_max = dplyr::coalesce(.data$hazard_max, 0),
        hazard_min = dplyr::coalesce(.data$hazard_min, 0),
        hazard_p2_5 = dplyr::coalesce(.data$hazard_p2_5, 0),
        hazard_p5 = dplyr::coalesce(.data$hazard_p5, 0),
        hazard_p10 = dplyr::coalesce(.data$hazard_p10, 0),
        hazard_p90 = dplyr::coalesce(.data$hazard_p90, 0),
        hazard_p95 = dplyr::coalesce(.data$hazard_p95, 0),
        hazard_p97_5 = dplyr::coalesce(.data$hazard_p97_5, 0)
      ) |>
      dplyr::select(
        "asset", "company", "latitude", "longitude",
        "municipality", "province", "asset_category", "size_in_m2",
        "share_of_economic_activity", "hazard_name", "hazard_type",
        "hazard_indicator", "hazard_intensity", "hazard_mean", "hazard_median", "hazard_max", "hazard_min",
        "hazard_p2_5", "hazard_p5", "hazard_p10", "hazard_p90", "hazard_p95", "hazard_p97_5", "matching_method"
      )
    
    results_list[[i]] <- df_i
  }
  
  # Filter out NULL entries
  results_list <- results_list[!sapply(results_list, is.null)]
  
  if (length(results_list) == 0) {
    return(tibble::tibble())
  }
  
  return(do.call(rbind, results_list))
}
