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
#'   municipality, province, asset_category, size_in_m2, share_of_economic_activity,
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
  
  final_result <- do.call(rbind, all_results)
  message("[extract_hazard_statistics] Completed processing for ", nrow(assets_df), " assets")
  
  return(final_result)
}

#' Extract statistics from spatial hazards (unified for NC and TIF sources)
#' @noRd
extract_spatial_statistics <- function(assets_df, hazards, hazards_inventory, aggregation_method = "mean") {
  message("  [extract_spatial_statistics] Extracting hazard statistics from rasters...")
  message("    Using aggregation method: ", aggregation_method)

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
    "p97_5" = function(x) as.numeric(stats::quantile(x, 0.975, na.rm = TRUE, type = 7))
  )
  
  # Separate NC and TIF hazards
  nc_inventory <- hazards_inventory |> dplyr::filter(.data$source == "nc")
  tif_inventory <- hazards_inventory |> dplyr::filter(.data$source == "tif")
  
  # Validate aggregation method for NC sources
  if (nrow(nc_inventory) > 0 && "ensemble" %in% names(nc_inventory)) {
    available_ensembles <- unique(nc_inventory$ensemble)
    if (!aggregation_method %in% available_ensembles) {
      stop(
        "Requested aggregation_method '", aggregation_method, "' not found in NC hazards. ",
        "Available ensembles: ", paste(available_ensembles, collapse = ", ")
      )
    }
  }
  
  # Validate aggregation method for TIF sources
  if (nrow(tif_inventory) > 0 && !aggregation_method %in% names(aggregation_functions)) {
    stop(
      "Invalid aggregation_method '", aggregation_method, "' for TIF extraction. ",
      "Valid options: ", paste(names(aggregation_functions), collapse = ", ")
    )
  }
  
  # Get the aggregation function for TIF
  agg_func <- if (aggregation_method %in% names(aggregation_functions)) {
    aggregation_functions[[aggregation_method]]
  } else {
    stop("Invalid aggregation method: ", aggregation_method)
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
  
  # Combine both inventories for unified processing
  combined_inventory <- dplyr::bind_rows(
    nc_inventory |> dplyr::mutate(base_event_id = .data$hazard_name),
    tif_inventory |> dplyr::mutate(base_event_id = .data$hazard_name)
  )
  
  n_hazards <- nrow(combined_inventory)
  results_list <- vector("list", n_hazards)
  
  message("    Found ", nrow(nc_inventory), " NC hazards and ", nrow(tif_inventory), " TIF hazards (total: ", n_hazards, ")")
  
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
    # Add ensemble suffix for output
    hazard_name_with_ensemble <- paste0(base_hazard_name, "__ensemble=", aggregation_method)
    
    message("    Processing ", toupper(hazard_source), " hazard ", i, "/", n_hazards, ": ", base_hazard_name)
    
    # Get raster CRS
    r_crs <- terra::crs(hazard_rast)
    if (is.na(r_crs) || r_crs == "") r_crs <- "EPSG:4326"
    
    # Different extraction methods for NC vs TIF, but both produce only hazard_intensity
    if (hazard_source == "nc") {
      # NC: Point extraction from centroids (pre-computed statistics in raster)
      message("      Using point extraction (centroids) for NC source")
      
      centroids_sf <- sf::st_set_geometry(assets_sf, assets_sf$centroid)
      centroids_transformed <- sf::st_transform(centroids_sf, r_crs)
      centroids_vect <- terra::vect(centroids_transformed)
      
      # Extract values (using centroids for NC since they have pre-computed stats)
      extracted_vals <- terra::extract(hazard_rast, centroids_vect, method = "simple", ID = FALSE)
      
      # The extracted value represents the chosen aggregation method (ensemble statistic)
      intensity_vals <- if (ncol(extracted_vals) > 0) as.numeric(extracted_vals[[1]]) else rep(NA_real_, nrow(assets_sf))
      
      # Store only hazard_intensity
      stats_df <- tibble::tibble(
        ID = seq_len(nrow(assets_sf)),
        hazard_intensity = intensity_vals
      )
      
    } else {
      # TIF: Polygon extraction with statistics computation
      message("      Using polygon extraction (crop/mask) for TIF source")
      
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
              # Apply ONLY the chosen aggregation method
              intensity_val <- agg_func(vals)
              
              # Update only hazard_intensity
              stats_df <- stats_df |>
                dplyr::mutate(
                  hazard_intensity = dplyr::if_else(.data$ID == j, intensity_val, .data$hazard_intensity)
                )
            }
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
        source=hazard_source,
        matching_method = "coordinates",
        # Replace NAs with 0
        hazard_intensity = dplyr::coalesce(.data$hazard_intensity, 0)
      ) |>
      dplyr::select(
        "asset", "company", "latitude", "longitude",
        "municipality", "province", "asset_category", "size_in_m2",
        "share_of_economic_activity", "hazard_name", "hazard_type",
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
      dplyr::select("hazard_type", "hazard_indicator", "scenario_name", "scenario_code", "hazard_return_period", "source")
    
    matched_data <- matched_data |>
      dplyr::inner_join(
        required_hazards_char,
        by = c("hazard_type", "hazard_indicator", "scenario_name", "scenario_code", "hazard_return_period")
      )
    
    # Check if we got all required hazards
    found_combos <- matched_data |>
      dplyr::distinct(.data$hazard_type, .data$hazard_indicator, .data$scenario_name, .data$scenario_code, .data$hazard_return_period )
    
    # If matched_data is empty and we're not strictly validating, skip this asset
    if (nrow(matched_data) == 0) {
      stop("No data found for asset ", i, " (", asset_name, "). No match found in precomputed data for municipality='", municipality_normalized, 
           "' or province='", province_normalized, "' for hazard_type='", hazard_type, "', scenario_code='", scenario_code, "', hazard_return_period='", hazard_return_period, "'")
    }

    # Transform precomputed data to match expected output format
    # Filter by the chosen aggregation method (ensemble)
    asset_hazard_data <- matched_data |>
      dplyr::filter(.data$ensemble == aggregation_method) |>
      dplyr::mutate(
        # Extract the value from the column matching the aggregation method
        hazard_intensity = hazard_value,
        hazard_name = paste0(.data$hazard_name, "__ensemble=", aggregation_method),
        matching_method = match_level,
        # Add asset information to each hazard row
        asset = asset_row$asset,
        company = asset_row$company,
        latitude = asset_row$latitude,
        longitude = asset_row$longitude,
        municipality = asset_row$municipality,
        province = asset_row$province,
        asset_category = asset_row$asset_category,
        size_in_m2 = asset_row$size_in_m2,
        share_of_economic_activity = asset_row$share_of_economic_activity
      ) |>
      dplyr::select(
        "asset", "company", "latitude", "longitude",
        "municipality", "province", "asset_category", "size_in_m2",
        "share_of_economic_activity", "hazard_name", "hazard_type",
        "hazard_indicator", "hazard_return_period", "scenario_code", "scenario_name", "source", "hazard_intensity", "matching_method"
      )
    
    precomp_results_list[[length(precomp_results_list) + 1]] <- asset_hazard_data
  }
  
  if (length(precomp_results_list) == 0) {
    return(tibble::tibble())
  }
  
  return(do.call(rbind, precomp_results_list))
}
