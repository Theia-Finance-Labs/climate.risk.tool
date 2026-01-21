#' Extract hazard statistics using spatial extraction or precomputed lookups (internal function)
#'
#' @param assets_df Data frame with asset information. Can have geometry column (for spatial extraction)
#'   or municipality/state columns (for precomputed lookups)
#' @param hazards Named list of hazard rasters (from load_hazards) - used for spatial extraction
#' @param hazards_inventory Data frame with hazard metadata (hazard_name, hazard_type, hazard_indicator, etc.)
#' @param precomputed_hazards Data frame with precomputed hazard statistics (from read_precomputed_hazards)
#' @param aggregation_method Character. Statistical aggregation method for hazard extraction (default: "mean").
#'   Determines which statistic to compute from extracted pixel values for precomputed sources.
#'   Options: "mean", "median", "p90", "p10", "max", "min", "mode", "closest"
#' @return Data frame in long format with columns: asset, company, latitude, longitude,
#'   municipality, state, asset_category, asset_subtype, size_in_m2, share_of_economic_activity,
#'   hazard_name, hazard_type, hazard_indicator, hazard_intensity, matching_method
#' @noRd
extract_hazard_statistics <- function(assets_df, hazards, hazards_inventory, precomputed_hazards = NULL, aggregation_method = "mean") {
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

    all_results[[length(all_results) + 1]] <- extract_precomputed_statistics(
      assets_without_coords,
      precomputed_hazards,
      hazards_inventory,
      aggregation_method
    )
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
      "closest" = function(x, ...) mean(x, na.rm = TRUE), # Alias for mean when used with buffers
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
      hazard_gwl <- hazard_meta$gwl
      hazard_season <- if ("season" %in% names(hazard_meta)) hazard_meta$season else NA_character_
      hazard_ensemble <- if ("ensemble" %in% names(hazard_meta)) hazard_meta$ensemble else NA_character_

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

      if (is.null(effective_aggregation_method) || !effective_aggregation_method %in% names(aggregation_functions)) {
        stop(
          "Invalid aggregation method '", effective_aggregation_method, "' for indicator ",
          hazard_indicator, ". Valid options: ", paste(names(aggregation_functions), collapse = ", ")
        )
      }

      agg_func <- aggregation_functions[[effective_aggregation_method]]

      message("    Processing ", toupper(hazard_source), " hazard ", i, "/", n_hazards, ": ", base_hazard_name)

      # Get raster CRS - should already be set during load_nc_hazards_with_metadata
      r_crs <- terra::crs(hazard_rast)
      if (is.na(r_crs) || r_crs == "") stop("Raster CRS is not set")

      # Fast path: vectorized terra::extract over all geometries at once (huge speedup vs per-asset crop/mask)
      assets_sf_transformed <- sf::st_transform(assets_sf, r_crs)
      geom_vect <- terra::vect(assets_sf_transformed)

      extracted <- tryCatch(
        terra::extract(hazard_rast, geom_vect, fun = agg_func, na.rm = TRUE),
        error = function(e) NULL
      )

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

      # Combine statistics with asset data
      df_i <- dplyr::bind_cols(
        sf::st_drop_geometry(assets_sf),
        tibble::tibble(hazard_intensity = hazard_vals)
      ) |>
        dplyr::mutate(
          # Use hazard_name directly (no extra suffix)
          hazard_name = base_hazard_name,
          hazard_type = hazard_type,
          gwl = hazard_gwl,
          hazard_indicator = hazard_indicator,
          return_period = hazard_return_period,
          season = hazard_season,
          ensemble = hazard_ensemble,
          source = hazard_source,
          matching_method = "coordinates",
          # Replace NAs with 0
          hazard_intensity = dplyr::coalesce(.data$hazard_intensity, 0)
        ) |>
        dplyr::select(
          "asset", "company", "latitude", "longitude",
          "municipality", "state", "asset_category", "asset_subtype", "size_in_m2",
          "share_of_economic_activity", "cnae", "hazard_name", "hazard_type",
          "hazard_indicator", "return_period", "gwl", "season", "ensemble", "source", "hazard_intensity", "matching_method"
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

  # Precomputed data should have correct hazard indicators already
  message("    Using precomputed hazard indicators directly from data")

  required_hazards <- hazards_inventory

  precomp_results_list <- list()

  for (i in seq_len(nrow(assets_df))) {
    asset_row <- assets_df |> dplyr::slice(i)
    asset_name <- asset_row |> dplyr::pull(.data$asset)
    municipality <- asset_row |> dplyr::pull(.data$municipality)
    state <- asset_row |> dplyr::pull(.data$state)
    asset_category <- asset_row |> dplyr::pull(.data$asset_category)
    asset_subtype <- asset_row |> dplyr::pull(.data$asset_subtype)

    # Try municipality first (ADM2), then state (ADM1)
    # Note: Names are already normalized in read_assets() and read_precomputed_hazards()
    matched_data <- NULL
    match_level <- NULL
    matched_region <- NULL

    if (!is.na(municipality) && nzchar(as.character(municipality))) {
      matched_data <- precomputed_hazards |>
        dplyr::filter(
          .data$region == municipality,
          .data$adm_level == "ADM2"
        )
      match_level <- "municipality"
      matched_region <- municipality
    }

    if (is.null(matched_data) || nrow(matched_data) == 0) {
      if (!is.na(state) && nzchar(as.character(state))) {
        matched_data <- precomputed_hazards |>
          dplyr::filter(
            .data$region == state,
            .data$adm_level == "ADM1"
          )
        match_level <- "state"
        matched_region <- state
      }
    }

    if (is.null(matched_data) || nrow(matched_data) == 0) {
      stop(
        "Cannot determine hazard statistics for asset ", i, " (", asset_name, "). ",
        "No match found in precomputed data for municipality='", municipality,
        "' or state='", state, "'"
      )
    }

    # FILTER: Only keep hazards that match the required hazards from inventory
    # Match by hazard_name + season (for drought with season column)

    # Separate fire/land_cover (handled specially - no precomputed lookup)
    fire_land_cover <- required_hazards |>
      dplyr::filter(.data$hazard_type == "Fire", .data$hazard_indicator == "land_cover")

    required_hazards_other <- required_hazards |>
      dplyr::filter(!(.data$hazard_type == "Fire" & .data$hazard_indicator == "land_cover"))

    # Get list of required hazard_names (inventory format; must match precomputed exactly)
    required_hazard_names <- required_hazards_other |>
      dplyr::pull(.data$hazard_name) |>
      as.character()

    # Keep only hazards needed for this run
    hazard_matches <- matched_data |>
      dplyr::filter(.data$hazard_name %in% required_hazard_names)

    if (length(required_hazard_names) > 0) {
      missing_hazards <- setdiff(required_hazard_names, unique(hazard_matches$hazard_name))
      if (length(missing_hazards) > 0) {
        stop(
          "Missing precomputed hazard data for asset ", i, " (", asset_name, "). ",
          "Could not find hazards: ", paste(missing_hazards, collapse = ", "),
          " when matching municipality='", municipality, "' or state='", state, "'."
        )
      }
    }

    matched_data <- hazard_matches

    # Transform precomputed data to match expected output format
    # Join with inventory to determine the correct aggregation method for each hazard
    # (respects per-hazard overrides in the config/inventory)
    asset_hazard_data <- hazard_matches |>
      dplyr::left_join(
        hazards_inventory |> dplyr::select("hazard_name", "agg"),
        by = "hazard_name"
      ) |>
      # Use per-hazard agg from inventory if available, otherwise fallback to global parameter
      dplyr::mutate(
        effective_agg = dplyr::coalesce(.data$agg, .env$aggregation_method),
        # Handle aliases: 'closest' is treated as 'mean' for precomputed data
        effective_agg = dplyr::if_else(.data$effective_agg == "closest", "mean", .data$effective_agg)
      ) |>
      dplyr::filter(.data$aggregation_method == .data$effective_agg)
    
    # Validate that only one aggregation method remains after filtering
    if (nrow(asset_hazard_data) > 0) {
      # Group by hazard_name and check if multiple aggregation methods remain
      agg_counts <- asset_hazard_data |>
        dplyr::group_by(.data$hazard_name) |>
        dplyr::summarise(n_agg = dplyr::n_distinct(.data$aggregation_method), .groups = "drop") |>
        dplyr::filter(.data$n_agg > 1)

      if (nrow(agg_counts) > 0) {
        warning(
          "Multiple aggregation methods found after filtering for asset ", asset_name,
          " and hazards: ", paste(agg_counts$hazard_name, collapse = ", "),
          ". This indicates a bug in the filter logic."
        )
      }
    }

    if (length(required_hazard_names) > 0) {
      missing_agg_hazards <- setdiff(required_hazard_names, unique(asset_hazard_data$hazard_name))
      if (length(missing_agg_hazards) > 0) {
        stop(
          "Missing precomputed hazard data for asset ", i, " (", asset_name, "). ",
          "Aggregation method '", aggregation_method, "' not available for hazards: ",
          paste(missing_agg_hazards, collapse = ", "),
          ". Checked municipality='", municipality, "' and state='", state, "'."
        )
      }
    }

    asset_hazard_data <- asset_hazard_data |>
      dplyr::mutate(
        # Extract the value from the column matching the aggregation method
        hazard_intensity = .data$hazard_value,
        # Emit the inventory hazard name (already has ensemble suffix if needed)
        hazard_name = .data$hazard_name,
        matching_method = match_level,
        source = paste0("precomputed (", match_level, ")"), # Add source column indicating municipality or state
        # Add asset information to each hazard row
        asset = asset_row$asset,
        company = asset_row$company,
        latitude = asset_row$latitude,
        longitude = asset_row$longitude,
        municipality = asset_row$municipality,
        state = asset_row$state,
        asset_category = asset_row$asset_category,
        asset_subtype = asset_row$asset_subtype,
        size_in_m2 = asset_row$size_in_m2,
        share_of_economic_activity = asset_row$share_of_economic_activity,
        cnae = asset_row$cnae,
        # Ensure season/ensemble columns exist (fill with NA if missing in precomputed data)
        season = if ("season" %in% names(.data)) .data$season else NA_character_,
        ensemble = if ("ensemble" %in% names(.data)) .data$ensemble else NA_character_
      ) |>
      dplyr::select(
        "asset", "company", "latitude", "longitude",
        "municipality", "state", "asset_category", "asset_subtype", "size_in_m2",
        "share_of_economic_activity", "cnae", "hazard_name", "hazard_type",
        "hazard_indicator", "return_period", "gwl", "season", "ensemble", "source", "hazard_intensity", "matching_method"
      )

    # Add fire/land_cover rows with default value 0.5 (not precomputed)
    if (nrow(fire_land_cover) > 0) {
      # Get the first matched row to extract metadata (for constructing hazard_name)
      # We need to find the corresponding hazard_name from inventory for land_cover
      land_cover_meta <- hazards_inventory |>
        dplyr::filter(.data$hazard_type == "Fire", .data$hazard_indicator == "land_cover") |>
        dplyr::slice(1)

      if (nrow(land_cover_meta) > 0) {
        # Create synthetic land_cover rows with default value 0.5
        land_cover_rows <- tibble::tibble(
          asset = asset_row$asset,
          company = asset_row$company,
          latitude = asset_row$latitude,
          longitude = asset_row$longitude,
          municipality = asset_row$municipality,
          state = asset_row$state,
          asset_category = asset_row$asset_category,
          asset_subtype = asset_row$asset_subtype,
          size_in_m2 = asset_row$size_in_m2,
          share_of_economic_activity = asset_row$share_of_economic_activity,
          cnae = asset_row$cnae,
          hazard_name = land_cover_meta$hazard_name[1],
          hazard_type = "Fire",
          hazard_indicator = "land_cover",
          return_period = land_cover_meta$return_period[1],
          gwl = land_cover_meta$gwl[1],
          source = if ("source" %in% names(land_cover_meta)) land_cover_meta$source[1] else "tif",
          hazard_intensity = 0.5, # Default land_cover_risk value
          matching_method = match_level
        )

        # Combine with other hazard data
        asset_hazard_data <- dplyr::bind_rows(asset_hazard_data, land_cover_rows)
        message("      Added fire/land_cover with default value 0.5 for asset ", asset_name)
      }
    }

    precomp_results_list[[length(precomp_results_list) + 1]] <- asset_hazard_data
  }

  if (length(precomp_results_list) == 0) {
    return(tibble::tibble())
  }

  return(do.call(rbind, precomp_results_list))
}
