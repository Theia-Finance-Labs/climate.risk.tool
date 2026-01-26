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
  available_hazard_names <- names(hazards)
  raster_inventory <- hazards_inventory |>
    dplyr::filter(.data$source %in% c("nc", "tif"), .data$hazard_name %in% available_hazard_names)

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
    combined_inventory <- raster_inventory |>
      dplyr::mutate(base_event_id = .data$hazard_name)

    n_hazards <- nrow(combined_inventory)
    results_list <- vector("list", n_hazards)

    for (i in seq_len(n_hazards)) {
      hazard_meta <- combined_inventory |> dplyr::slice(i)

      base_hazard_name <- hazard_meta$hazard_name
      hazard_source <- hazard_meta$source
      hazard_rast <- hazards[[base_hazard_name]]

      # Skip if hazard raster is not found
      if (is.null(hazard_rast)) {
        warning(
          "Hazard '", base_hazard_name, "' not found in hazards list. ",
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
        c("hazard_type", "hazard_indicator", "hazard_name", "scenario_name", "return_period", 
          "season", "ensemble", "source", "agg", "categorical")
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
        assets_centroids_sf <- sf::st_as_sf(assets_sf, sf_column_name = "centroid")
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
        # terra::extract returns an ID column + one column per layer; hazard_rast is single-layer here
        as.numeric(extracted[[ncol(extracted)]])
      } else {
        rep(NA_real_, n_geoms)
      }

      # For categorical indicators, round to nearest integer
      if (isTRUE(hazard_meta$categorical)) {
        hazard_vals <- ifelse(is.na(hazard_vals), NA_real_, round(hazard_vals))
      }

      indicator_col <- as.character(hazard_indicator)
      df_i <- dplyr::bind_cols(
        sf::st_drop_geometry(assets_sf),
        tibble::tibble(.indicator_value = hazard_vals)
      ) |>
        dplyr::mutate(
          # Use hazard_name directly (no extra suffix)
          hazard_name = base_hazard_name,
          hazard_type = hazard_type,
          scenario_name = hazard_scenario_name,
          hazard_indicator = hazard_indicator,
          return_period = hazard_return_period,
          season = hazard_season,
          ensemble = hazard_ensemble,
          source = hazard_source,
          matching_method = "coordinates",
          !!rlang::sym(indicator_col) := .data$.indicator_value,
          # Replace NAs with 0
          !!rlang::sym(indicator_col) := dplyr::coalesce(.data[[indicator_col]], 0)
        ) |>
        # Add extra index columns from inventory
        dplyr::bind_cols(extra_index_values) |>
        dplyr::select(
          "asset", "company", "latitude", "longitude",
          "municipality", "state", "asset_category", "asset_subtype", "size_in_m2",
          "share_of_economic_activity", "cnae", "hazard_name", "hazard_type",
          "hazard_indicator", "return_period", "scenario_name", "season", "ensemble", "source",
          dplyr::all_of(indicator_col), "matching_method", dplyr::any_of(inventory_index_cols)
        )

      results_list[[i]] <- df_i
    }

    # Filter out NULL entries for raster results
    results_list <- results_list[!sapply(results_list, is.null)]

    if (length(results_list) > 0) {
      raster_results <- dplyr::bind_rows(results_list)
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
  
  required_hazard_names <- required_hazards_inventory |>
    dplyr::pull(.data$hazard_name) |>
    unique()

  # Step 1: Matching Strategy (Vectorized Joins)
  # Pre-filter precomputed data for required hazards to speed up joins
  precomp_filtered <- precomputed_hazards |>
    dplyr::filter(.data$hazard_name %in% required_hazard_names)

  # Join with Municipality (ADM2) first
  assets_with_adm2 <- assets_df |>
    dplyr::filter(!is.na(.data$municipality), .data$municipality != "") |>
    dplyr::inner_join(
      precomp_filtered |> dplyr::filter(.data$adm_level == "ADM2"),
      by = c("municipality" = "region"),
      relationship = "many-to-many"
    ) |>
    dplyr::mutate(matching_method = "municipality", source = "precomputed (municipality)")

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
    dplyr::mutate(matching_method = "state", source = "precomputed (state)")

  combined_matches <- dplyr::bind_rows(assets_with_adm2, assets_with_adm1)

  # Validate all assets matched
  missing_assets <- setdiff(assets_df$asset, combined_matches$asset)
  if (length(missing_assets) > 0) {
    stop(
      "Could not find precomputed hazard data for assets: ", paste(missing_assets, collapse = ", "),
      ". Please check that municipality and state names match the precomputed data regions."
    )
  }

  # Step 2: Filter by correct aggregation method (Vectorized)
  # Respect per-hazard overrides from inventory
  combined_matches <- combined_matches |>
    dplyr::left_join(
      hazards_inventory |> dplyr::select("hazard_name", "agg"),
      by = "hazard_name"
    ) |>
    dplyr::mutate(
      effective_agg = dplyr::coalesce(.data$agg, .env$aggregation_method),
      # Handle aliases: 'closest' is treated as 'mean' for precomputed data
      effective_agg = dplyr::if_else(.data$effective_agg == "closest", "mean", .data$effective_agg)
    ) |>
    dplyr::filter(.data$aggregation_method == .data$effective_agg)

  # Validate each asset has all required hazards with the correct aggregation
  asset_hazard_counts <- combined_matches |>
    dplyr::group_by(.data$asset) |>
    dplyr::summarise(n_hazards = dplyr::n_distinct(.data$hazard_name), .groups = "drop")
  
  if (any(asset_hazard_counts$n_hazards < length(required_hazard_names))) {
    bad_assets <- asset_hazard_counts$asset[asset_hazard_counts$n_hazards < length(required_hazard_names)]
    stop(
      "Some assets are missing required aggregation methods in precomputed data: ",
      paste(bad_assets, collapse = ", ")
    )
  }

  # Step 3: Transform to final format (Vectorized)
  # Define standard columns to keep
  metadata_cols <- c(
    "asset", "company", "latitude", "longitude", "municipality", "state",
    "asset_category", "asset_subtype", "size_in_m2", "share_of_economic_activity",
    "cnae", "hazard_name", "hazard_type", "hazard_indicator", "return_period",
    "scenario_name", "season", "ensemble", "source", "matching_method"
  )
  
  # Add dynamic index columns from inventory
  inventory_index_cols <- setdiff(
    names(hazards_inventory),
    c("hazard_type", "hazard_indicator", "hazard_name", "ensemble", "source", "agg", "categorical")
  )
  metadata_cols <- unique(c(metadata_cols, inventory_index_cols))

  # Create indicator-specific columns (vectorized)
  unique_indicators <- unique(combined_matches$hazard_indicator)
  final_data <- combined_matches
  
  for (ind in unique_indicators) {
    final_data <- final_data |>
      dplyr::mutate(!!ind := dplyr::if_else(.data$hazard_indicator == !!ind, .data$hazard_value, NA_real_))
  }
  
  final_data <- final_data |>
    dplyr::select(dplyr::any_of(c(metadata_cols, unique_indicators)))

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
          return_period = fire_land_cover$return_period[j],
          scenario_name = fire_land_cover$scenario_name[j],
          source = "tif",
          land_cover = NA_real_
        )
    })
    
    land_cover_rows <- dplyr::bind_rows(land_cover_rows_list)
    final_data <- dplyr::bind_rows(final_data, land_cover_rows)
    message("    Added fire/land_cover with default NA for ", nrow(asset_metadata), " assets")
  }

  return(final_data)
}
