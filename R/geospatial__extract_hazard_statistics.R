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
      hazard_configs,
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
