#' Extract hazard statistics using spatial extraction or precomputed lookups (internal function)
#'
#' @param assets_df Data frame with asset information. Can have geometry column (for spatial extraction)
#'   or municipality/province columns (for precomputed lookups)
#' @param hazards Named list of hazard rasters (from load_hazards) - used for spatial extraction
#' @param hazards_inventory Data frame with hazard metadata (hazard_name, hazard_type, hazard_indicator, etc.)
#' @param precomputed_hazards Data frame with precomputed hazard statistics (from read_precomputed_hazards)
#' @param aggregation_method Character. Statistical aggregation method for hazard extraction (default: "mean").
#'   For NC sources: selects which ensemble raster to extract. For TIF sources: determines which statistic
#'   to compute from extracted pixel values. Options: "mean", "median", "max", "min", "p2_5", "p5", "p95", "p97_5"
#' @return Data frame in long format with columns: asset, company, latitude, longitude,
#'   municipality, province, asset_category, asset_subtype, size_in_m2, share_of_economic_activity,
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

#' Extract statistics from spatial hazards (unified for NC, TIF, and CSV sources)
#' @noRd
extract_spatial_statistics <- function(assets_df, hazards, hazards_inventory, aggregation_method = "mean") {
  message("  [extract_spatial_statistics] Extracting hazard statistics...")
  message("    Using aggregation method: ", aggregation_method)

  # Separate CSV hazards from raster hazards (TIF/NC)
  csv_inventory <- hazards_inventory |>
    dplyr::filter(.data$source == "csv")

  raster_inventory <- hazards_inventory |>
    dplyr::filter(.data$source %in% c("tif", "nc"))

  all_results <- list()

  # ========= Process CSV hazards (closest-point assignment) =========
  if (nrow(csv_inventory) > 0) {
    message("  [extract_spatial_statistics] Processing CSV hazards with closest-point assignment...")

    # Extract CSV hazards from the hazards list
    csv_hazard_names <- csv_inventory$hazard_name
    hazards_csv <- hazards[csv_hazard_names]
    hazards_csv <- hazards_csv[!sapply(hazards_csv, is.null)]

    if (length(hazards_csv) > 0) {
      csv_results <- extract_csv_statistics(
        assets_df,
        hazards_csv,
        csv_inventory,
        aggregation_method
      )
      all_results[[length(all_results) + 1]] <- csv_results
    }
  }

  # ========= Process raster hazards (TIF/NC with polygon extraction) =========
  if (nrow(raster_inventory) > 0) {
    message("  [extract_spatial_statistics] Processing raster hazards (TIF/NC) with polygon extraction...")

    # Define aggregation function mapping (used for TIF sources)
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
      "p97_5" = function(x) as.numeric(stats::quantile(x, 0.975, na.rm = TRUE, type = 7)),
      "mode" = function(x) {
        # Get most common value (for categorical data like land cover)
        x_clean <- x[!is.na(x)]
        if (length(x_clean) == 0) return(NA_real_)
        ux <- unique(x_clean)
        ux[which.max(tabulate(match(x_clean, ux)))]
      }
    )
    # Validate aggregation method for TIF sources
    if (!aggregation_method %in% names(aggregation_functions)) {
      stop(
        "Invalid aggregation_method '", aggregation_method, "' for TIF extraction. ",
        "Valid options: ", paste(names(aggregation_functions), collapse = ", ")
      )
    }

    # Create geometries for assets (shared by both NC and TIF)
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

      # Get metadata
      hazard_type <- hazard_meta$hazard_type
      hazard_indicator <- hazard_meta$hazard_indicator
      hazard_return_period <- hazard_meta$hazard_return_period
      hazard_scenario_code <- hazard_meta$scenario_code
      hazard_scenario_name <- hazard_meta$scenario_name
      
      # Special handling for Fire land_cover: force mode aggregation (categorical data)
      effective_aggregation_method <- aggregation_method
      if (hazard_type == "Fire" && hazard_indicator == "land_cover") {
        effective_aggregation_method <- "mode"
        message("      Fire land_cover detected - forcing 'mode' aggregation for categorical data")
      }
      
      # Get the aggregation function based on effective method
      agg_func <- aggregation_functions[[effective_aggregation_method]]
      
      # Add extraction_method suffix for output
      hazard_name_with_ensemble <- paste0(base_hazard_name, "__extraction_method=", effective_aggregation_method)

      message("    Processing ", toupper(hazard_source), " hazard ", i, "/", n_hazards, ": ", base_hazard_name)

      # Get raster CRS
      r_crs <- terra::crs(hazard_rast)
      if (is.na(r_crs) || r_crs == "") stop("Raster CRS is not set")

      # Unified extraction method: polygon extraction with masking and aggregation for both NC and TIF
      message("      Using polygon extraction (crop/mask) for ", toupper(hazard_source), " source")

      n_geoms <- nrow(assets_sf)
      stats_df <- tibble::tibble(
        ID = seq_len(n_geoms),
        hazard_intensity = NA_real_
      )

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
              # Apply the chosen aggregation method
              intensity_val <- agg_func(vals)
              
              # For Fire land_cover (categorical codes), round to nearest integer
              # Aggregated rasters may have fractional values, but land_cover codes are whole numbers
              if (hazard_type == "Fire" && hazard_indicator == "land_cover" && !is.na(intensity_val)) {
                intensity_val <- round(intensity_val)
              }

              # Update hazard_intensity
              stats_df <- stats_df |>
                dplyr::mutate(
                  hazard_intensity = dplyr::if_else(.data$ID == j, intensity_val, .data$hazard_intensity)
                )
            }
          }
        }
      }

      # Combine statistics with asset data (same format for both NC and TIF)
      df_i <- dplyr::bind_cols(
        sf::st_drop_geometry(assets_sf),
        stats_df |> dplyr::select(-"ID")
      ) |>
        dplyr::mutate(
          # Use hazard_name with ensemble suffix added
          hazard_name = hazard_name_with_ensemble,
          hazard_type = hazard_type,
          scenario_code = hazard_scenario_code,
          scenario_name = hazard_scenario_name,
          hazard_indicator = hazard_indicator,
          hazard_return_period = hazard_return_period,
          source = hazard_source,
          matching_method = "coordinates",
          # Replace NAs with 0
          hazard_intensity = dplyr::coalesce(.data$hazard_intensity, 0)
        ) |>
        dplyr::select(
          "asset", "company", "latitude", "longitude",
          "municipality", "province", "asset_category", "asset_subtype", "size_in_m2",
          "share_of_economic_activity", "cnae", "hazard_name", "hazard_type",
          "hazard_indicator", "hazard_return_period", "scenario_code", "scenario_name", "source", "hazard_intensity", "matching_method"
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

#' Extract statistics from precomputed administrative data (municipality/province lookup)
#' @noRd
extract_precomputed_statistics <- function(assets_df, precomputed_hazards, hazards_inventory, aggregation_method = "mean") {
  message("  [extract_precomputed_statistics] Looking up precomputed data for ", nrow(assets_df), " assets...")
  message("    Using aggregation method: ", aggregation_method)

  # Precomputed data should have correct hazard indicators already
  message("    Using precomputed hazard indicators directly from data")

  required_hazards <- hazards_inventory


  precomp_results_list <- list()

  for (i in seq_len(nrow(assets_df))) {
    asset_row <- assets_df |> dplyr::slice(i)
    asset_name <- asset_row |> dplyr::pull(.data$asset)
    municipality <- asset_row |> dplyr::pull(.data$municipality)
    province <- asset_row |> dplyr::pull(.data$province)

    # Try municipality first (ADM2), then province (ADM1)
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
      if (!is.na(province) && nzchar(as.character(province))) {
        matched_data <- precomputed_hazards |>
          dplyr::filter(
            .data$region == province,
            .data$adm_level == "ADM1"
          )
        match_level <- "province"
        matched_region <- province
      }
    }

    if (is.null(matched_data) || nrow(matched_data) == 0) {
      stop(
        "Cannot determine hazard statistics for asset ", i, " (", asset_name, "). ",
        "No match found in precomputed data for municipality='", municipality,
        "' or province='", province, "'"
      )
    }

    # FILTER: Only keep hazards that match the required hazards from inventory
    # Match by hazard_type, scenario_code, and hazard_return_period
    # Convert scenario_code to character for both datasets to handle type mismatches
    matched_data <- matched_data |>
      dplyr::mutate(scenario_code = as.character(.data$scenario_code))

    required_hazards_char <- required_hazards |>
      dplyr::mutate(scenario_code = as.character(.data$scenario_code)) |>
      dplyr::select("hazard_type", "hazard_indicator", "scenario_name", "scenario_code", "hazard_return_period", "source")

    matched_data <- matched_data |>
      dplyr::inner_join(
        required_hazards_char,
        by = c("hazard_type", "hazard_indicator", "scenario_name", "scenario_code", "hazard_return_period")
      )

    # Check if we got all required hazards
    found_combos <- matched_data |>
      dplyr::distinct(.data$hazard_type, .data$hazard_indicator, .data$scenario_name, .data$scenario_code, .data$hazard_return_period)

    # If matched_data is empty and we're not strictly validating, skip this asset
    if (nrow(matched_data) == 0) {
      stop(
        "No data found for asset ", i, " (", asset_name, "). No match found in precomputed data for municipality='", municipality,
        "' or province='", province, "' for the required hazards."
      )
    }

    # Transform precomputed data to match expected output format
    # Filter by the chosen aggregation method (ensemble)
    asset_hazard_data <- matched_data |>
      dplyr::filter(.data$aggregation_method == aggregation_method) |>
      dplyr::mutate(
        # Extract the value from the column matching the aggregation method
        hazard_intensity = .data$hazard_value,
        hazard_name = paste0(.data$hazard_name, "__extraction_method=", aggregation_method),
        matching_method = match_level,
        # Add asset information to each hazard row
        asset = asset_row$asset,
        company = asset_row$company,
        latitude = asset_row$latitude,
        longitude = asset_row$longitude,
        municipality = asset_row$municipality,
        province = asset_row$province,
        asset_category = asset_row$asset_category,
        asset_subtype = asset_row$asset_subtype,
        size_in_m2 = asset_row$size_in_m2,
        share_of_economic_activity = asset_row$share_of_economic_activity,
        cnae = asset_row$cnae
      ) |>
      dplyr::select(
        "asset", "company", "latitude", "longitude",
        "municipality", "province", "asset_category", "asset_subtype", "size_in_m2",
        "share_of_economic_activity", "cnae", "hazard_name", "hazard_type",
        "hazard_indicator", "hazard_return_period", "scenario_code", "scenario_name", "source", "hazard_intensity", "matching_method"
      )

    precomp_results_list[[length(precomp_results_list) + 1]] <- asset_hazard_data
  }

  if (length(precomp_results_list) == 0) {
    return(tibble::tibble())
  }

  return(do.call(rbind, precomp_results_list))
}

#' Extract statistics from CSV hazards using closest-point assignment
#' @noRd
extract_csv_statistics <- function(assets_df, hazards_csv, hazards_inventory, aggregation_method = "mean") {
  message("  [extract_csv_statistics] Extracting hazard statistics from CSV point data...")
  message("    Using closest-point assignment (aggregation_method parameter kept for consistency)")

  # Filter inventory to only CSV sources
  csv_inventory <- hazards_inventory |>
    dplyr::filter(.data$source == "csv")

  n_hazards <- nrow(csv_inventory)
  results_list <- vector("list", n_hazards)

  for (i in seq_len(n_hazards)) {
    hazard_meta <- csv_inventory |> dplyr::slice(i)

    base_hazard_name <- hazard_meta$hazard_name
    hazard_csv_data <- hazards_csv[[base_hazard_name]]

    if (is.null(hazard_csv_data)) {
      warning("CSV hazard data not found for: ", base_hazard_name)
      next
    }

    # Get metadata
    hazard_type <- hazard_meta$hazard_type
    hazard_indicator <- hazard_meta$hazard_indicator
    hazard_return_period <- hazard_meta$hazard_return_period
    hazard_scenario_code <- hazard_meta$scenario_code
    hazard_scenario_name <- hazard_meta$scenario_name

    # Add extraction_method suffix for output consistency
    hazard_name_with_ensemble <- paste0(base_hazard_name, "__extraction_method=", aggregation_method)

    message("    Processing CSV hazard ", i, "/", n_hazards, ": ", base_hazard_name)

    n_assets <- nrow(assets_df)
    asset_intensities <- numeric(n_assets)

    # For each asset, find closest CSV point
    for (j in seq_len(n_assets)) {
      asset_lat <- assets_df$latitude[j]
      asset_lon <- assets_df$longitude[j]

      # Calculate Euclidean distance to all CSV points
      distances <- sqrt(
        (hazard_csv_data$lat - asset_lat)^2 +
          (hazard_csv_data$lon - asset_lon)^2
      )

      # Find minimum distance
      min_idx <- which.min(distances)

      # Extract hazard intensity from closest point
      asset_intensities[j] <- hazard_csv_data$hazard_intensity[min_idx]
    }

    # Combine with asset data
    df_i <- assets_df |>
      dplyr::mutate(
        hazard_intensity = asset_intensities,
        hazard_name = hazard_name_with_ensemble,
        hazard_type = hazard_type,
        scenario_code = hazard_scenario_code,
        scenario_name = hazard_scenario_name,
        hazard_indicator = hazard_indicator,
        hazard_return_period = hazard_return_period,
        source = "csv",
        matching_method = "coordinates"
      ) |>
      dplyr::select(
        "asset", "company", "latitude", "longitude",
        "municipality", "province", "asset_category", "asset_subtype", "size_in_m2",
        "share_of_economic_activity", "cnae", "hazard_name", "hazard_type",
        "hazard_indicator", "hazard_return_period", "scenario_code", "scenario_name", "source", "hazard_intensity", "matching_method"
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
