#' Extract hazard statistics using spatial extraction or precomputed lookups (internal function)
#'
#' @param assets_df Data frame with asset information. Can have geometry column (for spatial extraction)
#'   or municipality/province columns (for precomputed lookups)
#' @param hazards Named list of hazard rasters (from load_hazards) - used for spatial extraction
#' @param hazards_inventory Data frame with hazard metadata (hazard_name, hazard_type, hazard_indicator, etc.)
#' @param precomputed_hazards Data frame with precomputed hazard statistics (from read_precomputed_hazards)
#' @param events Data frame with event specifications (used to filter which hazards to validate in precomputed data)
#' @param aggregation_method Character. Statistical aggregation method for hazard extraction (default: "mean")
#' @return Data frame in long format with columns: asset, company, latitude, longitude,
#'   municipality, province, asset_category, size_in_m2, share_of_economic_activity,
#'   hazard_name, hazard_type, hazard_indicator, hazard_intensity, hazard_mean, hazard_median, hazard_max, 
#'   hazard_p2_5, hazard_p5, hazard_p95, hazard_p97_5, matching_method
#' @noRd
extract_hazard_statistics <- function(assets_df, hazards, hazards_inventory, precomputed_hazards = NULL, events = NULL, aggregation_method = "mean") {
  message("[extract_hazard_statistics] Processing ", nrow(assets_df), " assets...")
  browser()
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
        assets_with_coords, hazards, hazards_inventory, aggregation_method
      )
    }
    
    if (has_tif_sources) {
      # TIF workflow: Compute statistics from spatial extraction
      all_results[[length(all_results) + 1]] <- extract_tif_statistics(
        assets_with_coords, hazards, hazards_inventory, aggregation_method
      )
    }
    
  }
  
  # ========= Process assets WITHOUT coordinates (precomputed lookup) =========
  if (nrow(assets_without_coords) > 0) {
    message("[extract_hazard_statistics] Processing administrative-based assets...")
    
    all_results[[length(all_results) + 1]] <- extract_precomputed_statistics(
      assets_without_coords, 
      precomputed_hazards, 
      hazards_inventory,
      events
    )
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
extract_nc_statistics <- function(assets_df, hazards, hazards_inventory, aggregation_method = "mean") {
  message("  [extract_nc_statistics] Extracting pre-computed statistics from NC files...")
  message("    Using aggregation method (ensemble): ", aggregation_method)
  
  # Validate that the requested ensemble exists in the loaded hazards
  # Use the ensemble column from inventory (NC hazards have this column)
  nc_inventory_check <- hazards_inventory |>
    dplyr::filter(.data$source == "nc")
  
  if (nrow(nc_inventory_check) > 0 && "ensemble" %in% names(nc_inventory_check)) {
    available_ensembles <- unique(nc_inventory_check$ensemble)
    if (!aggregation_method %in% available_ensembles) {
      stop(
        "Requested aggregation_method '", aggregation_method, "' not found in NC hazards. ",
        "Available ensembles: ", paste(available_ensembles, collapse = ", ")
      )
    }
  }
  
  # Filter to NC hazards only
  nc_inventory <- hazards_inventory |>
    dplyr::filter(.data$source == "nc")
  
  if (nrow(nc_inventory) == 0) {
    return(tibble::tibble())
  }
  
  # For NC hazards, the hazard_name in inventory is already the base event (no ensemble suffix)
  # Use hazard_name directly as base_event_id
  nc_inventory <- nc_inventory |>
    dplyr::mutate(
      base_event_id = .data$hazard_name
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
    
    # Get the hazard raster for this base event (now only mean ensemble is loaded)
    # Filter by base_event_id (hazard_name without ensemble suffix)
    event_hazards <- nc_inventory |>
      dplyr::filter(
        .data$hazard_name == base_event_id,
        .data$ensemble == "mean"  # Only mean ensemble is loaded
      )
    
    # Extract from the mean ensemble raster
    haz_row <- event_hazards |> dplyr::slice(1)
    
    # Use the base hazard name directly (no ensemble suffix needed)
    target_hazard_name <- base_event_id
    
    hazard_rast <- hazards[[target_hazard_name]]
    
    message("      Extracting mean ensemble as hazard_intensity")
    
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
    
    # Create stats dataframe with the extracted value as hazard_intensity
    # Set all other stats to the same value (since we only extracted one ensemble)
    intensity_vals <- if (ncol(extracted_vals) > 0) as.numeric(extracted_vals[[1]]) else rep(NA_real_, nrow(assets_sf))
    
    stats_df <- tibble::tibble(
      asset_id = seq_len(nrow(assets_sf)),
      hazard_intensity = intensity_vals,
      hazard_mean = intensity_vals,
      hazard_median = intensity_vals,
      hazard_max = intensity_vals,
      hazard_min = NA_real_,
      hazard_p2_5 = intensity_vals,
      hazard_p5 = intensity_vals,
      hazard_p10 = NA_real_,
      hazard_p90 = NA_real_,
      hazard_p95 = intensity_vals,
      hazard_p97_5 = intensity_vals
    )
    
    # Combine with asset data
    event_result <- dplyr::bind_cols(
      sf::st_drop_geometry(assets_sf),
      stats_df |> dplyr::select(-"asset_id")
    ) |>
      dplyr::mutate(
        # Use the chosen aggregation_method ensemble as the canonical hazard_name
        hazard_name = paste0(base_event_id, "__ensemble=", aggregation_method),
        hazard_type = base_event$hazard_type,
        hazard_indicator = base_event$hazard_indicator,
        # hazard_intensity already set from extracted ensemble value
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
extract_tif_statistics <- function(assets_df, hazards, hazards_inventory, aggregation_method = "mean") {
  message("  [extract_tif_statistics] Computing statistics from TIF spatial extraction...")
  message("    Using aggregation method: ", aggregation_method)
  
  # Define aggregation function mapping
  aggregation_functions <- list(
    "mean" = function(x) mean(x, na.rm = TRUE),
    "median" = function(x) stats::median(x, na.rm = TRUE),
    "max" = function(x) max(x, na.rm = TRUE),
    "min" = function(x) min(x, na.rm = TRUE),
    "p2_5" = function(x) as.numeric(stats::quantile(x, 0.025, na.rm = TRUE, type = 7)),
    "p5" = function(x) as.numeric(stats::quantile(x, 0.05, na.rm = TRUE, type = 7)),
    "p10" = function(x) as.numeric(stats::quantile(x, 0.10, na.rm = TRUE, type = 7)),
    "p90" = function(x) as.numeric(stats::quantile(x, 0.90, na.rm = TRUE, type = 7)),
    "p95" = function(x) as.numeric(stats::quantile(x, 0.95, na.rm = TRUE, type = 7)),
    "p97_5" = function(x) as.numeric(stats::quantile(x, 0.975, na.rm = TRUE, type = 7))
  )
  
  # Validate aggregation method
  if (!aggregation_method %in% names(aggregation_functions)) {
    stop(
      "Invalid aggregation_method '", aggregation_method, "' for TIF extraction. ",
      "Valid options: ", paste(names(aggregation_functions), collapse = ", ")
    )
  }
  
  # Get the aggregation function
  agg_func <- aggregation_functions[[aggregation_method]]
  
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
  
  # Get TIF inventory
  n_tif <- nrow(tif_inventory)
  
  results_list <- vector("list", n_tif)
  
  message("    Found ", n_tif, " TIF hazards")
  
  for (i in seq_len(n_tif)) {
    hazard_meta <- tif_inventory |> dplyr::slice(i)
    
    base_hazard_name <- hazard_meta$hazard_name
    hazard_rast <- hazards[[base_hazard_name]]
    
    # Get metadata
    hazard_type <- hazard_meta$hazard_type
    hazard_indicator <- hazard_meta$hazard_indicator
    # Add ensemble suffix for output
    hazard_name_with_ensemble <- paste0(base_hazard_name, "__ensemble=", aggregation_method)
    
    message("    Processing TIF hazard ", i, "/", n_tif, ": ", base_hazard_name)
    
    # Pre-allocate statistics tibble
    n_geoms <- nrow(assets_sf)
    stats_df <- tibble::tibble(
      ID = seq_len(n_geoms),
      hazard_intensity = NA_real_,
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
            # Compute all statistics (for completeness)
            mean_val <- mean(vals, na.rm = TRUE)
            median_val <- stats::median(vals, na.rm = TRUE)
            max_val <- max(vals, na.rm = TRUE)
            p2_5_val <- as.numeric(stats::quantile(vals, 0.025, na.rm = TRUE, type = 7))
            p5_val <- as.numeric(stats::quantile(vals, 0.05, na.rm = TRUE, type = 7))
            p95_val <- as.numeric(stats::quantile(vals, 0.95, na.rm = TRUE, type = 7))
            p97_5_val <- as.numeric(stats::quantile(vals, 0.975, na.rm = TRUE, type = 7))
            
            # Use the chosen aggregation method for hazard_intensity
            intensity_val <- agg_func(vals)
            
            stats_df <- stats_df |>
              dplyr::mutate(
                hazard_mean = dplyr::if_else(.data$ID == j, mean_val, .data$hazard_mean),
                hazard_median = dplyr::if_else(.data$ID == j, median_val, .data$hazard_median),
                hazard_max = dplyr::if_else(.data$ID == j, max_val, .data$hazard_max),
                hazard_p2_5 = dplyr::if_else(.data$ID == j, p2_5_val, .data$hazard_p2_5),
                hazard_p5 = dplyr::if_else(.data$ID == j, p5_val, .data$hazard_p5),
                hazard_p95 = dplyr::if_else(.data$ID == j, p95_val, .data$hazard_p95),
                hazard_p97_5 = dplyr::if_else(.data$ID == j, p97_5_val, .data$hazard_p97_5),
                hazard_intensity = dplyr::if_else(.data$ID == j, intensity_val, .data$hazard_intensity)
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
        # Use hazard_name with ensemble suffix added
        hazard_name = hazard_name_with_ensemble,
        hazard_type = hazard_type,
        hazard_indicator = hazard_indicator,
        # hazard_intensity already set by aggregation function
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

#' Extract statistics from precomputed administrative data (municipality/province lookup)
#' @noRd
extract_precomputed_statistics <- function(assets_df, precomputed_hazards, hazards_inventory, events = NULL) {
  message("  [extract_precomputed_statistics] Looking up precomputed data for ", nrow(assets_df), " assets...")
  
  # Precomputed data should have correct hazard indicators already
  message("    Using precomputed hazard indicators directly from data")
  
  # Build a mapping of required hazards from events (not all loaded hazards)
  # Only validate hazards that are actually selected in the events dataframe
  if (!is.null(events) && nrow(events) > 0) {
    # Extract unique event hazard names (now without ensemble suffix)
    event_hazard_names <- events |>
      dplyr::distinct(.data$hazard_name) |>
      dplyr::pull(.data$hazard_name)
    
    # Filter inventory to only include hazards that match events
    # Since inventory hazard_name now doesn't have ensemble suffix, it should match directly
    required_hazards <- hazards_inventory |>
      dplyr::filter(.data$hazard_name %in% event_hazard_names) |>
      dplyr::distinct(
        .data$hazard_type,
        .data$scenario_code,
        .data$hazard_return_period
      )
  } else {
    # Fallback: use all hazards from inventory if events not provided
    required_hazards <- hazards_inventory |>
      dplyr::distinct(
        .data$hazard_type,
        .data$scenario_code,
        .data$hazard_return_period
      )
  }
  
  message("    Required hazards from events: ", nrow(required_hazards))
  
  precomp_results_list <- list()
  
  for (i in seq_len(nrow(assets_df))) {
    asset_row <- assets_df |> dplyr::slice(i)
    asset_name <- asset_row |> dplyr::pull(.data$asset)
    municipality <- asset_row |> dplyr::pull(.data$municipality)
    province <- asset_row |> dplyr::pull(.data$province)
    
    # Normalize administrative region names using stringi (similar to unidecode)
    municipality_normalized <- if (!is.na(municipality) && nzchar(as.character(municipality))) {
      stringi::stri_trans_general(as.character(municipality), "Latin-ASCII")
    } else {
      municipality
    }
    
    province_normalized <- if (!is.na(province) && nzchar(as.character(province))) {
      stringi::stri_trans_general(as.character(province), "Latin-ASCII")
    } else {
      province
    }
    
    # Try municipality first (ADM2), then province (ADM1)
    matched_data <- NULL
    match_level <- NULL
    matched_region <- NULL
    
    if (!is.na(municipality_normalized) && nzchar(as.character(municipality_normalized))) {
      matched_data <- precomputed_hazards |>
        dplyr::filter(
          .data$region == municipality_normalized,
          .data$adm_level == "ADM2"
        )
      match_level <- "municipality"
      matched_region <- municipality_normalized
    }
    
    if (is.null(matched_data) || nrow(matched_data) == 0) {
      if (!is.na(province_normalized) && nzchar(as.character(province_normalized))) {
        matched_data <- precomputed_hazards |>
          dplyr::filter(
            .data$region == province_normalized,
            .data$adm_level == "ADM1"
          )
        match_level <- "province"
        matched_region <- province_normalized
      }
    }
    
    if (is.null(matched_data) || nrow(matched_data) == 0) {
      stop(
        "Cannot determine hazard statistics for asset ", i, " (", asset_name, "). ",
        "No match found in precomputed data for municipality='", municipality_normalized, 
        "' or province='", province_normalized, "'"
      )
    }
    
    # FILTER: Only keep hazards that match the required hazards from inventory
    # Match by hazard_type, scenario_code, and hazard_return_period
    # Convert scenario_code to character for both datasets to handle type mismatches
    matched_data <- matched_data |>
      dplyr::mutate(scenario_code = as.character(.data$scenario_code))
    
    required_hazards_char <- required_hazards |>
      dplyr::mutate(scenario_code = as.character(.data$scenario_code)) |>
      dplyr::select("hazard_type", "scenario_code", "hazard_return_period")
    
    matched_data <- matched_data |>
      dplyr::inner_join(
        required_hazards_char,
        by = c("hazard_type", "scenario_code", "hazard_return_period")
      )
    
    # VALIDATE: Check that we found data for all required hazards
    # Only strict validation when events were explicitly provided
    should_validate_strictly <- !is.null(events) && nrow(events) > 0
    
    if (nrow(matched_data) == 0 && should_validate_strictly) {
      # No matching hazards found for this region
      required_list <- required_hazards |>
        dplyr::mutate(
          hazard_spec = paste0(.data$hazard_type, " (scenario=", 
                              .data$scenario_code, ", RP=", 
                              .data$hazard_return_period, ")")
        ) |>
        dplyr::pull(.data$hazard_spec)
      
      # Get available hazards for this region
      available_in_region <- precomputed_hazards |>
        dplyr::filter(
          .data$region == matched_region,
          .data$adm_level == dplyr::if_else(match_level == "municipality", "ADM2", "ADM1")
        ) |>
        dplyr::distinct(.data$hazard_type, .data$scenario_code, .data$hazard_return_period) |>
        dplyr::mutate(
          hazard_spec = paste0(.data$hazard_type, " (scenario=", 
                              .data$scenario_code, ", RP=", 
                              .data$hazard_return_period, ")")
        ) |>
        dplyr::pull(.data$hazard_spec)
      
      stop(
        "Required hazards from events selection are not available in precomputed data.\n",
        "  Asset: ", asset_name, " (", match_level, ": ", matched_region, ")\n",
        "  Required: ", paste(required_list, collapse = ", "), "\n",
        "  Available: ", paste(head(available_in_region, 10), collapse = ", "), 
        if (length(available_in_region) > 10) paste0(" (and ", length(available_in_region) - 10, " more)") else "", "\n",
        "  Tip: Check that events match available hazard types, scenarios, and return periods."
      )
    }
    
    # Check if we got all required hazards
    found_combos <- matched_data |>
      dplyr::distinct(.data$hazard_type, .data$scenario_code, .data$hazard_return_period)
    
    if (nrow(found_combos) < nrow(required_hazards) && should_validate_strictly) {
      # Some required hazards are missing
      missing_hazards <- dplyr::anti_join(
        required_hazards,
        found_combos,
        by = c("hazard_type", "scenario_code", "hazard_return_period")
      ) |>
        dplyr::mutate(
          hazard_spec = paste0(.data$hazard_type, " (scenario=", 
                              .data$scenario_code, ", RP=", 
                              .data$hazard_return_period, ")")
        ) |>
        dplyr::pull(.data$hazard_spec)
      
      found_list <- found_combos |>
        dplyr::mutate(
          hazard_spec = paste0(.data$hazard_type, " (scenario=", 
                              .data$scenario_code, ", RP=", 
                              .data$hazard_return_period, ")")
        ) |>
        dplyr::pull(.data$hazard_spec)
      
      stop(
        "Some required hazards are missing from precomputed data.\n",
        "  Asset: ", asset_name, " (", match_level, ": ", matched_region, ")\n",
        "  Missing: ", paste(missing_hazards, collapse = ", "), "\n",
        "  Found: ", paste(found_list, collapse = ", "), "\n",
        "  Tip: Check that events match available hazard types, scenarios, and return periods in precomputed data."
      )
    }
    
    # If matched_data is empty and we're not strictly validating, skip this asset
    if (nrow(matched_data) == 0) {
      message("    Warning: No matching hazards found for asset ", i, " (", asset_name, ") in precomputed data. Skipping.")
      next
    }
    
    # Transform precomputed data to match expected output format
    # Since we only load mean ensemble now, we can simplify the logic
    
    # Use the base hazard name directly (no ensemble suffix needed)

    # Now construct the asset-level output with all ensemble statistics
    asset_hazard_data <- matched_data |>
    filter(ensemble == "mean") |>
      dplyr::mutate(
        hazard_intensity = dplyr::coalesce(.data$mean, 0),
        hazard_mean = dplyr::coalesce(.data$mean, 0),
        hazard_median = dplyr::coalesce(.data$median, 0),
        hazard_max = dplyr::coalesce(.data$max, 0),
        hazard_min = NA_real_,  # Not available in precomputed data
        hazard_p2_5 = dplyr::coalesce(.data$p2_5, 0),
        hazard_p5 = dplyr::coalesce(.data$p5, 0),
        hazard_p10 = NA_real_,  # Not available in precomputed data
        hazard_p90 = NA_real_,  # Not available in precomputed data
        hazard_p95 = dplyr::coalesce(.data$p95, 0),
        hazard_p97_5 = dplyr::coalesce(.data$p97_5, 0),
        matching_method = matching_method
      ) |>
      dplyr::select(
        "hazard_name", "hazard_type", "hazard_indicator", "hazard_intensity", "hazard_mean", "hazard_median", "hazard_max", "hazard_min",
        "hazard_p2_5", "hazard_p5", "hazard_p95", "hazard_p97_5"
      )
    
    precomp_results_list[[length(precomp_results_list) + 1]] <- asset_hazard_data
  }
  
  if (length(precomp_results_list) == 0) {
    return(tibble::tibble())
  }
  
  return(do.call(rbind, precomp_results_list))
}
