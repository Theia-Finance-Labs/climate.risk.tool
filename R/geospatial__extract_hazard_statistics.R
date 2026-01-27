#' Extract hazard statistics using spatial extraction or precomputed lookups (internal function)
#'
#' @param assets_df Data frame with asset information. Can have geometry column (for spatial extraction)
#'   or municipality/state columns (for precomputed lookups)
#' @param hazards Named list of hazard rasters (from load_hazards) - used for spatial extraction
#' @param hazards_inventory Data frame with hazard metadata (hazard_name, hazard_type, hazard_indicator, etc.)
#' @param precomputed_hazards Data frame with precomputed hazard statistics (from read_precomputed_hazards)
#' @param hazard_configs Named list of hazard configurations (from load_hazards_and_inventory()$configs)
#' @param aggregation_method Character. Statistical aggregation method for hazard extraction (default: "mean").
#'   Determines which statistic to compute from extracted pixel values for precomputed sources.
#'   Options: "mean", "median", "p90", "p10", "max", "min", "mode", "closest"
#' @return Data frame in long format with columns: asset, company, latitude, longitude,
#'   municipality, state, asset_category, asset_subtype, size_in_m2, share_of_economic_activity,
#'   hazard_name, hazard_type, hazard_indicator, indicator-specific columns, matching_method
#' @noRd
extract_hazard_statistics <- function(assets_df, hazards, hazards_inventory, precomputed_hazards = NULL, hazard_configs = NULL, aggregation_method = "mean") {
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

    # Unified extraction workflow handles both NC and TIF sources
    all_results[[length(all_results) + 1]] <- extract_spatial_statistics(
      assets_with_coords, hazards, hazards_inventory, aggregation_method
    )
  }

    # ========= Process assets WITHOUT coordinates (precomputed lookup) =========
  if (nrow(assets_without_coords) > 0) {
    message("[extract_hazard_statistics] Processing administrative-based assets...")

    precomputed_results <- extract_precomputed_statistics(
      assets_without_coords,
      precomputed_hazards,
      hazards_inventory,
      aggregation_method
    )

    # Apply inference values from hazard configs if defined for assets without coordinates
    if (nrow(precomputed_results) > 0 && !is.null(hazard_configs)) {
      for (h_type in names(hazard_configs)) {
        cfg <- hazard_configs[[h_type]]
        for (ind_key in names(cfg$indicators)) {
          ind_cfg <- cfg$indicators[[ind_key]]
          if (!is.null(ind_cfg$inference) && length(ind_cfg$inference) > 0) {
            # Check if this indicator column exists in results
            if (ind_key %in% names(precomputed_results)) {
              # Apply inference values based on asset_category or other criteria if needed
              # For now, we support a simple 'default' or category-specific inference
              for (inf_key in names(ind_cfg$inference)) {
                inf_val <- ind_cfg$inference[[inf_key]]
                if (inf_key == "default") {
                  precomputed_results <- precomputed_results |>
                    dplyr::mutate(
                      !!ind_key := dplyr::if_else(
                        .data$hazard_type == h_type & (is.na(.data[[ind_key]]) | .data[[ind_key]] == 0),
                        as.numeric(inf_val),
                        .data[[ind_key]]
                      )
                    )
                } else {
                  # Assume inf_key is an asset_category
                  precomputed_results <- precomputed_results |>
                    dplyr::mutate(
                      !!ind_key := dplyr::if_else(
                        .data$hazard_type == h_type & .data$asset_category == inf_key & (is.na(.data[[ind_key]]) | .data[[ind_key]] == 0),
                        as.numeric(inf_val),
                        .data[[ind_key]]
                      )
                    )
                }
              }
            }
          }
        }
      }
    }

    all_results[[length(all_results) + 1]] <- precomputed_results
  }

  # Combine all results
  if (length(all_results) == 0) {
    stop("No assets to process")
  }

  final_result <- dplyr::bind_rows(all_results)
  message("[extract_hazard_statistics] Completed processing for ", nrow(assets_df), " assets")

  return(final_result)
}

#' Extract statistics from spatial hazards (NetCDF sources)
#' @noRd
extract_spatial_statistics <- function(assets_df, hazards, hazards_inventory, aggregation_method = "mean") {
  message("  [extract_spatial_statistics] Extracting hazard statistics...")

  # Filter to raster hazards (NetCDF or TIF) that actually exist in the hazards list
  available_hazard_keys <- names(hazards)
  raster_inventory <- hazards_inventory |>
    dplyr::filter(.data$source %in% c("nc", "tif"), .data$indicator_key %in% available_hazard_keys)

  all_results <- list()

  # ========= Process raster hazards (NetCDF or TIF with polygon extraction) =========
  if (nrow(raster_inventory) > 0) {
    message("  [extract_spatial_statistics] Processing raster hazards (NC/TIF) with vectorized extraction...")

    # Define aggregation function mapping (used for both NC and TIF sources)
    aggregation_functions <- list(
      # Note: terra::extract() may forward extra arguments to `fun` (e.g., na.rm).
      # All aggregation functions accept `...` to avoid unused-argument errors.
      "mean" = function(x, ...) mean(x, na.rm = TRUE),
      "median" = function(x, ...) stats::median(x, na.rm = TRUE),
      "max" = function(x, ...) max(x, na.rm = TRUE),
      "min" = function(x, ...) min(x, na.rm = TRUE),
      "p10" = function(x, ...) as.numeric(stats::quantile(x, 0.10, na.rm = TRUE, type = 7)),
      "p90" = function(x, ...) as.numeric(stats::quantile(x, 0.90, na.rm = TRUE, type = 7)),
      "mode" = function(x, ...) {
        # Get most common value (for categorical data like land cover)
        x_clean <- x[!is.na(x)]
        if (length(x_clean) == 0) {
          return(NA_real_)
        }
        ux <- unique(x_clean)
        ux[which.max(tabulate(match(x_clean, ux)))]
      }
    )
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

    # Process raster inventory
    combined_inventory <- raster_inventory

    n_hazards <- nrow(combined_inventory)
    results_list <- vector("list", n_hazards)

    for (i in seq_len(n_hazards)) {
      hazard_meta <- combined_inventory |> dplyr::slice(i)

      base_hazard_name <- hazard_meta$hazard_name
      base_indicator_key <- hazard_meta$indicator_key
      hazard_source <- hazard_meta$source
      hazard_rast <- hazards[[base_indicator_key]]
      
      # Skip if hazard raster is not found
      if (is.null(hazard_rast)) {
        # Fallback: try looking up by hazard_name just in case
        hazard_rast <- hazards[[base_hazard_name]]
      }

      # Skip if hazard raster is not found (really)
      if (is.null(hazard_rast)) {
        warning(
          "Hazard '", base_hazard_name, "' (key: ", base_indicator_key, ") not found in hazards list. ",
          "Skipping extraction for this hazard."
        )
        results_list[[i]] <- NULL
        next
      }

      # Get metadata
      hazard_type <- hazard_meta$hazard_type
      hazard_indicator <- hazard_meta$hazard_indicator
      hazard_return_period <- hazard_meta$return_period
      hazard_scenario_name <- hazard_meta$scenario_name
      hazard_season <- if ("season" %in% names(hazard_meta)) hazard_meta$season else NA_character_
      hazard_ensemble <- if ("ensemble" %in% names(hazard_meta)) hazard_meta$ensemble else NA_character_
      
      # Extract all index dimension columns from inventory row (e.g., gwl)
      # Exclude metadata columns that shouldn't be passed through
      inventory_index_cols <- setdiff(
        names(hazard_meta),
        c("hazard_type", "hazard_indicator", "hazard_name", "hazard_key", "indicator_key", "scenario_name", "return_period", 
          "season", "ensemble", "source", "agg", "categorical", "variable", "indicator_file", "indicator_variable", "indicator_file_key")
      )
      extra_index_values <- hazard_meta[inventory_index_cols]

    # Determine aggregation method for this indicator (config-driven)
    # Use per-indicator agg from inventory if available, otherwise fallback to global parameter
    effective_aggregation_method <- if (!is.null(hazard_meta$agg) && !is.na(hazard_meta$agg)) {
      hazard_meta$agg
    } else {
      aggregation_method
    }
    
    # For categorical hazards, if no valid method is set, default to "mode"
    if (isTRUE(hazard_meta$categorical)) {
      if (!effective_aggregation_method %in% c("mode", "closest")) {
        effective_aggregation_method <- "mode"
      }
    }

    if (is.null(effective_aggregation_method) ||
      !effective_aggregation_method %in% c(names(aggregation_functions), "closest")) {
      stop(
        "Invalid aggregation method '", effective_aggregation_method, "' for indicator ",
        hazard_indicator, ". Valid options: ", paste(c(names(aggregation_functions), "closest"), collapse = ", ")
      )
    }

    agg_func <- if (effective_aggregation_method == "closest") NULL else aggregation_functions[[effective_aggregation_method]]

    message("    Processing ", toupper(hazard_source), " hazard ", i, "/", n_hazards, ": ", base_hazard_name)

    # Get raster CRS - should already be set during load_nc_hazards_with_metadata
    r_crs <- terra::crs(hazard_rast)
    if (is.na(r_crs) || r_crs == "") stop("Raster CRS is not set")

    # Fast path: vectorized terra::extract over all geometries at once (huge speedup vs per-asset crop/mask)
    if (effective_aggregation_method == "closest") {
      # Use centroid column as the active geometry for point extraction
      assets_centroids_sf <- sf::st_set_geometry(assets_sf, "centroid")
      assets_centroids_sf <- sf::st_transform(assets_centroids_sf, r_crs)
      geom_vect <- terra::vect(assets_centroids_sf)

      extracted <- tryCatch(
        terra::extract(hazard_rast, geom_vect),
        error = function(e) NULL
      )
    } else {
      assets_sf_transformed <- sf::st_transform(assets_sf, r_crs)
      geom_vect <- terra::vect(assets_sf_transformed)

      extracted <- tryCatch(
        terra::extract(hazard_rast, geom_vect, fun = agg_func, na.rm = TRUE, small = TRUE),
        error = function(e) NULL
      )
    }

    n_geoms <- nrow(assets_sf)
    hazard_vals <- if (!is.null(extracted) && nrow(extracted) == n_geoms) {
      # terra::extract returns an ID column + one column per layer
      # The raster should be single-layer, so we get the last column (skipping ID column)
      if (ncol(extracted) == 2) {
        # Expected: ID + value column
        as.numeric(extracted[[2]])
      } else if (ncol(extracted) > 2) {
        # Multiple layers (shouldn't happen, but handle it)
        warning("[extract_spatial_statistics] Extraction returned ", ncol(extracted) - 1, " layers for ", base_hazard_name, ", expected 1")
        as.numeric(extracted[[2]])  # Use first value column
      } else {
        # Only ID column, no values
        warning("[extract_spatial_statistics] Extraction returned no value columns for ", base_hazard_name)
        rep(NA_real_, n_geoms)
      }
    } else {
      if (!is.null(extracted)) {
        warning("[extract_spatial_statistics] Extraction returned ", nrow(extracted), " rows, expected ", n_geoms, " for ", base_hazard_name)
      }
      rep(NA_real_, n_geoms)
    }

    # For categorical indicators, round to nearest integer
    if (isTRUE(hazard_meta$categorical)) {
      hazard_vals <- ifelse(is.na(hazard_vals), NA_real_, round(hazard_vals))
    }

      # Use variable name from inventory if available, otherwise fallback to hazard_indicator
    indicator_col <- if (!is.null(hazard_meta$variable) && !is.na(hazard_meta$variable)) {
      as.character(hazard_meta$variable)
    } else {
      as.character(hazard_indicator)
    }

    df_i <- dplyr::bind_cols(
      sf::st_drop_geometry(assets_sf),
      tibble::tibble(.indicator_value = hazard_vals)
    ) |>
      dplyr::mutate(
        # Use hazard_name directly (no extra suffix)
        hazard_name = base_hazard_name,
        hazard_key = base_hazard_name,  # Public key is hazard_name
        indicator_key = base_indicator_key, # Internal key
        hazard_type = hazard_type,
        scenario_name = hazard_scenario_name,
        hazard_indicator = hazard_indicator,
        return_period = hazard_return_period,
        season = hazard_season,
        ensemble = hazard_ensemble,
        source = hazard_source,
        hazard_intensity = .data$.indicator_value,
        matching_method = "geolocated extracted",
        !!rlang::sym(indicator_col) := .data$.indicator_value
        # DO NOT replace NAs with 0 - keep NAs to indicate extraction failures
      ) |>
      # Add extra index columns from inventory
      dplyr::bind_cols(extra_index_values)
    
    # Select columns: use all standard columns plus any extra columns from bind_cols
    # This ensures gwl and other index dimensions are included
    # IMPORTANT: Exclude .indicator_value (internal temp column) and ID (from terra::extract)
    df_i <- df_i |>
      dplyr::select(
        dplyr::any_of(c(
          "asset", "company", "latitude", "longitude",
          "municipality", "state", "asset_category", "asset_subtype", "size_in_m2",
          "share_of_economic_activity", "cnae", "hazard_name", "hazard_key", "indicator_key", "hazard_type",
          "hazard_indicator", "return_period", "scenario_name", "season", "ensemble", "source",
          indicator_col, "matching_method"
        )),
        # Include ALL remaining columns (this will pick up gwl and any other index cols)
        dplyr::everything(),
        # Explicitly exclude internal temp columns
        -dplyr::any_of(c(".indicator_value", "ID", "id"))
      )

      results_list[[i]] <- df_i
    }

    # Filter out NULL entries for raster results
    results_list <- results_list[!sapply(results_list, is.null)]

    if (length(results_list) > 0) {
      raster_results <- dplyr::bind_rows(results_list)
      
      # Propagate event-level index values (like gwl) across all indicators with the same hazard_name
      # Some indicators (like TIF files) don't have these columns, but they should inherit them from the event
      if ("hazard_name" %in% names(raster_results)) {
        # For each hazard_name group, find the most complete set of index columns
        for (hazard_name_val in unique(raster_results$hazard_name)) {
          hazard_rows <- raster_results$hazard_name == hazard_name_val & !is.na(raster_results$hazard_name)
          
          # Find columns that are index-like (gwl, return_period, scenario_name, season, etc.)
          potential_index_cols <- c("gwl", "scenario_name", "season", "return_period")
          existing_index_cols <- intersect(potential_index_cols, names(raster_results))
          
          # For each index column, if some rows have values and others don't, fill the empty ones
          for (idx_col in existing_index_cols) {
            if (idx_col %in% names(raster_results)) {
              # Get non-NA values for this hazard_name
              non_na_values <- raster_results[[idx_col]][hazard_rows & !is.na(raster_results[[idx_col]])]
              
              # If there are non-NA values, use the most common one to fill NAs
              if (length(non_na_values) > 0) {
                fill_value <- non_na_values[1]  # Use first non-NA value
                # Fill NA values for this hazard_name
                raster_results[[idx_col]][hazard_rows & is.na(raster_results[[idx_col]])] <- fill_value
              }
            }
          }
        }
      }
      
      all_results[[length(all_results) + 1]] <- raster_results
    }
  }

  # Combine CSV and raster results
  if (length(all_results) == 0) {
    return(tibble::tibble())
  }

  return(dplyr::bind_rows(all_results))
}

#' Extract statistics from precomputed administrative data (municipality/state lookup)
#' @noRd
extract_precomputed_statistics <- function(assets_df, precomputed_hazards, hazards_inventory, aggregation_method = "mean") {
  message("  [extract_precomputed_statistics] Looking up precomputed data for ", nrow(assets_df), " assets...")
  message("    Using aggregation method: ", aggregation_method)

  # Check if precomputed_hazards is NULL or empty
  if (is.null(precomputed_hazards) || (inherits(precomputed_hazards, "data.frame") && nrow(precomputed_hazards) == 0)) {
    stop("precomputed_hazards is NULL or empty. Cannot perform precomputed lookup.")
  }

  # Identify required hazards (excluding special cases like fire/land_cover)
  required_hazards_inventory <- hazards_inventory |>
    dplyr::filter(!(.data$hazard_type == "Fire" & .data$hazard_indicator == "land_cover"))
  
  # Early check: if no hazards in inventory, that's the real problem
  if (nrow(required_hazards_inventory) == 0) {
    stop(
      "No hazards available for precomputed lookup.\n",
      "  Total hazards in inventory: ", nrow(hazards_inventory), "\n",
      "  This usually means filter_hazards_by_events selected 0 hazards.\n",
      "  Check your event configuration and ensure events match available hazards."
    )
  }
  
  # Use indicator_key for precomputed lookups (matches the file-based keys in precomputed data)
  # For multi-indicator hazards, this will include multiple keys per event
  required_indicator_keys <- required_hazards_inventory |>
    dplyr::pull(.data$indicator_key) |>
    unique()
  
  message("    Required indicators (", length(required_indicator_keys), "): ", 
          paste(head(required_indicator_keys, 3), collapse = ", "),
          if (length(required_indicator_keys) > 3) paste0(" (+ ", length(required_indicator_keys) - 3, " more)") else "")

  # Step 1: Matching Strategy (Vectorized Joins)
  # Pre-filter precomputed data for required hazards to speed up joins
  # Match by indicator_key (file-based identifier that exists in precomputed data)
  # The indicator_key includes the ensemble value (e.g., __ensemble=mean), so this filters by ensemble too
  precomp_filtered <- precomputed_hazards |>
    dplyr::filter(.data$indicator_key %in% required_indicator_keys)

  # Join with Municipality (ADM2) first
  assets_with_adm2 <- assets_df |>
    dplyr::filter(!is.na(.data$municipality), .data$municipality != "") |>
    dplyr::inner_join(
      precomp_filtered |> dplyr::filter(.data$adm_level == "ADM2"),
      by = c("municipality" = "region"),
      relationship = "many-to-many"
    ) |>
    dplyr::mutate(matching_method = "municipality lookup")

  # Join with State (ADM1) for assets that didn't match ADM2
  matched_assets_adm2 <- unique(assets_with_adm2$asset)
  assets_with_adm1 <- assets_df |>
    dplyr::filter(!(.data$asset %in% matched_assets_adm2)) |>
    dplyr::filter(!is.na(.data$state), .data$state != "") |>
    dplyr::inner_join(
      precomp_filtered |> dplyr::filter(.data$adm_level == "ADM1"),
      by = c("state" = "region"),
      relationship = "many-to-many"
    ) |>
    dplyr::mutate(matching_method = "state lookup")

  combined_matches <- dplyr::bind_rows(assets_with_adm2, assets_with_adm1)
  
  # Filter by required indicator keys
  combined_matches <- combined_matches |>
    dplyr::filter(.data$indicator_key %in% required_indicator_keys)

  # Validate all assets matched
  missing_assets <- setdiff(assets_df$asset, combined_matches$asset)
  if (length(missing_assets) > 0) {
    missing_df <- assets_df |>
      dplyr::filter(.data$asset %in% missing_assets)
    missing_municipalities <- missing_df |>
      dplyr::filter(!is.na(.data$municipality), nzchar(as.character(.data$municipality))) |>
      dplyr::pull(.data$municipality) |>
      unique()
    missing_states <- missing_df |>
      dplyr::filter(!is.na(.data$state), nzchar(as.character(.data$state))) |>
      dplyr::pull(.data$state) |>
      unique()
    
    # Check which regions are actually missing from precomputed data
    present_municipalities <- unique(precomputed_hazards$region[precomputed_hazards$adm_level == "ADM2"])
    present_states <- unique(precomputed_hazards$region[precomputed_hazards$adm_level == "ADM1"])
    
    truly_missing_municipalities <- setdiff(missing_municipalities, present_municipalities)
    truly_missing_states <- setdiff(missing_states, present_states)
    
    # Build detailed error message showing which hazards are missing for which regions
    error_parts <- character()
    
    # Show truly missing regions first
    if (length(truly_missing_municipalities) > 0) {
      error_parts <- c(error_parts, paste0("Missing regions (ADM2): ", paste(truly_missing_municipalities, collapse = ", ")))
    }
    if (length(truly_missing_states) > 0) {
      error_parts <- c(error_parts, paste0("Missing regions (ADM1): ", paste(truly_missing_states, collapse = ", ")))
    }
    
    # Show which regions exist but are missing required hazards
    regions_with_some_data <- intersect(
      c(missing_municipalities, missing_states),
      c(present_municipalities, present_states)
    )
    
    if (length(regions_with_some_data) > 0) {
      # For each region with partial data, show which hazards are missing
      for (region in regions_with_some_data) {
        # Check if it's a municipality or state
        is_municipality <- region %in% present_municipalities
        adm_level_filter <- if (is_municipality) "ADM2" else "ADM1"
        
        # Get available hazards for this region
        available_for_region <- precomputed_hazards |>
          dplyr::filter(.data$region == !!region, .data$adm_level == !!adm_level_filter)
        
          available_keys <- unique(available_for_region$indicator_key)
          missing_keys_for_region <- setdiff(required_indicator_keys, available_keys)
          missing_keys_for_region <- unique(missing_keys_for_region)
        
        error_parts <- c(
          error_parts,
          paste0(region, " (", adm_level_filter, "): missing ", length(missing_keys_for_region), 
                 " indicator keys - ", paste(head(missing_keys_for_region, 3), collapse = ", "),
                 if (length(missing_keys_for_region) > 3) paste0(" (+ ", length(missing_keys_for_region) - 3, " more)") else "")
        )
      }
    }
    
    # Build final error message with hazard names being searched
    error_msg_parts <- character()
    
    # Show what indicators we're looking for
    error_msg_parts <- c(
      error_msg_parts,
      paste0("Searching for ", length(required_indicator_keys), " indicators:"),
      paste0("  ", paste(head(required_indicator_keys, 5), collapse = "\n  "),
             if (length(required_indicator_keys) > 5) paste0("\n  (+ ", length(required_indicator_keys) - 5, " more)") else "")
    )
    
    # Show region/hazard specific issues
    if (length(error_parts) > 0) {
      error_msg_parts <- c(
        error_msg_parts,
        "",
        "Issues found:",
        paste0("  ", error_parts)
      )
    } else {
      error_msg_parts <- c(
        error_msg_parts,
        "",
        "All regions found in precomputed data but got 0 matches after filtering.",
        "Check that indicator_key construction matches precomputed inputs."
      )
    }
    
    stop(paste(error_msg_parts, collapse = "\n"))
  }

  # Step 2: Filter by correct aggregation method (Vectorized)
  # Respect per-hazard overrides from inventory
  
  # Ensure inventory has required columns for join
  inventory_for_join <- hazards_inventory
  if (!"agg" %in% names(inventory_for_join)) inventory_for_join$agg <- NA_character_
  
  # Join to get agg info
  combined_matches <- combined_matches |>
    dplyr::left_join(
      inventory_for_join |> dplyr::select("hazard_name", "agg"),
      by = "hazard_name",
      suffix = c("", "_inv")
    ) |>
    dplyr::mutate(
      agg_from_inv = .data$agg_inv
    ) |>
    dplyr::select(-"agg_inv")
  
  # Now determine effective aggregation method for each row
  combined_matches <- combined_matches |>
    dplyr::mutate(
      effective_agg = dplyr::coalesce(.data$agg_from_inv, aggregation_method),
      # Handle aliases: 'closest' is treated as 'mean' for precomputed data
      effective_agg = dplyr::if_else(.data$effective_agg == "closest", "mean", .data$effective_agg)
    )

  # Validate each asset has all required indicators
  asset_hazard_counts <- combined_matches |>
    dplyr::group_by(.data$asset) |>
    dplyr::summarise(n_indicators = dplyr::n_distinct(.data$indicator_key), .groups = "drop")
  
  if (any(asset_hazard_counts$n_indicators < length(required_indicator_keys))) {
    bad_assets <- asset_hazard_counts$asset[asset_hazard_counts$n_indicators < length(required_indicator_keys)]
    stop(
      "Some assets are missing required indicators in precomputed data: ",
      paste(bad_assets, collapse = ", ")
    )
  }

  # Step 3: Transform to final format (Vectorized)
  # Select the appropriate aggregation column (mean, median, p10, etc.) based on effective_agg
  # and rename it to the indicator variable name
  
  # Get variable name for each row
  combined_matches <- combined_matches |>
    dplyr::mutate(
      indicator_column_name = dplyr::coalesce(.data$variable, .data$hazard_indicator)
    )
  
  # For each row, extract the value from the appropriate aggregation column
  # The precomputed data has columns like: mean, median, p10, p90, p2_5, p5, p95, p97_5, min, max, mode
  agg_cols <- c("mean", "median", "p10", "p90", "p2_5", "p5", "p95", "p97_5", "min", "max", "mode")
  available_agg_cols <- intersect(agg_cols, names(combined_matches))
  
  if (length(available_agg_cols) == 0) {
    stop("No aggregation columns (mean, median, p10, p90, etc.) found in precomputed data")
  }
  
  # Extract the value from the correct aggregation column for each row
  combined_matches$hazard_value <- NA_real_
  for (agg_col in available_agg_cols) {
    combined_matches <- combined_matches |>
      dplyr::mutate(
        hazard_value = dplyr::if_else(
          .data$effective_agg == !!agg_col & !is.na(.data[[agg_col]]),
          .data[[agg_col]],
          .data$hazard_value
        )
      )
  }

  # Keep a consistent intensity column for downstream fallback
  combined_matches$hazard_intensity <- combined_matches$hazard_value
  
  # Define standard columns to keep
  metadata_cols <- c(
    "asset", "company", "latitude", "longitude", "municipality", "state",
    "asset_category", "asset_subtype", "size_in_m2", "share_of_economic_activity",
    "cnae", "hazard_name", "hazard_key", "indicator_key", "hazard_type", "hazard_indicator", "return_period",
    "scenario_name", "season", "ensemble", "source", "matching_method"
  )
  
  # Add dynamic index columns from inventory
  inventory_index_cols <- setdiff(
    names(hazards_inventory),
    c("hazard_type", "hazard_indicator", "hazard_name", "hazard_key", "indicator_key", "ensemble", "source", "agg", "categorical", "variable",
      "indicator_file", "indicator_variable", "indicator_file_key")
  )
  metadata_cols <- unique(c(metadata_cols, inventory_index_cols))
  
  # Create indicator-specific columns using variable names
  unique_variable_names <- unique(combined_matches$indicator_column_name)
  final_data <- combined_matches 
  
  for (col_name in unique_variable_names) {
    final_data[[col_name]] <- dplyr::if_else(final_data$indicator_column_name == col_name, final_data$hazard_value, NA_real_)
  }
  
  final_data <- final_data |>
    dplyr::select(-"indicator_column_name", -"hazard_value", -"effective_agg", -"agg_from_inv", -dplyr::any_of(available_agg_cols)) |>
    dplyr::select(dplyr::any_of(c(metadata_cols, "hazard_intensity", unique_variable_names)))

  # Step 4: Handle special fire/land_cover (not precomputed)
  fire_land_cover <- hazards_inventory |>
    dplyr::filter(.data$hazard_type == "Fire", .data$hazard_indicator == "land_cover")
  
  if (nrow(fire_land_cover) > 0) {
    # For each asset, add a land_cover row
    asset_metadata <- final_data |>
      dplyr::distinct(.data$asset, .keep_all = TRUE) |>
      dplyr::select(dplyr::any_of(metadata_cols))
    
    # We create a row for each asset and each fire/land_cover entry
    land_cover_rows_list <- lapply(seq_len(nrow(fire_land_cover)), function(j) {
      asset_metadata |>
        dplyr::mutate(
          hazard_name = fire_land_cover$hazard_name[j],
          hazard_type = fire_land_cover$hazard_type[j],
          hazard_indicator = fire_land_cover$hazard_indicator[j],
          indicator_key = fire_land_cover$indicator_key[j],  # CRITICAL: Set indicator_key for event matching
          return_period = fire_land_cover$return_period[j],
          scenario_name = fire_land_cover$scenario_name[j],
          source = "tif",
          land_cover = NA_real_,
          hazard_intensity = NA_real_
        )
    })
    
    land_cover_rows <- dplyr::bind_rows(land_cover_rows_list)
    final_data <- dplyr::bind_rows(final_data, land_cover_rows)
    
    # Remove source column if it exists (it might have been added in land_cover_rows)
    if ("source" %in% names(final_data)) {
      final_data$source <- NULL
    }
    
    message("    Added fire/land_cover with default NA for ", nrow(asset_metadata), " assets")
  }

  return(final_data)
}
