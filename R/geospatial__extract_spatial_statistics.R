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
            "share_of_economic_activity", "cost_factor", "cnae", "hazard_name", "hazard_key", "indicator_key", "hazard_type",
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
