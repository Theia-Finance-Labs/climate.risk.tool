#' Extract statistics from precomputed administrative data (municipality/state lookup)
#' @noRd
extract_precomputed_statistics <- function(assets_df, precomputed_hazards, hazards_inventory, hazard_configs = NULL, aggregation_method = "mean") {
  message("  [extract_precomputed_statistics] Looking up precomputed data for ", nrow(assets_df), " assets...")
  message("    Using aggregation method: ", aggregation_method)

  # Check if precomputed_hazards is NULL or empty
  if (is.null(precomputed_hazards) || (inherits(precomputed_hazards, "data.frame") && nrow(precomputed_hazards) == 0)) {
    stop("precomputed_hazards is NULL or empty. Cannot perform precomputed lookup.")
  }

  config_precomputed <- NULL
  if (!is.null(hazard_configs) && length(hazard_configs) > 0) {
    config_precomputed <- purrr::map_dfr(names(hazard_configs), function(hazard_type) {
      cfg <- hazard_configs[[hazard_type]]
      purrr::map_dfr(names(cfg$indicators), function(ind_key) {
        ind_cfg <- cfg$indicators[[ind_key]]
        precomputed_flag <- if (is.null(ind_cfg$precomputed)) TRUE else isTRUE(ind_cfg$precomputed)
        tibble::tibble(
          hazard_type = hazard_type,
          hazard_indicator = ind_key,
          precomputed = precomputed_flag
        )
      })
    })
  }

  # Identify required hazards (exclude indicators marked as precomputed = FALSE)
  required_hazards_inventory <- hazards_inventory
  if (!is.null(config_precomputed) && nrow(config_precomputed) > 0) {
    required_hazards_inventory <- required_hazards_inventory |>
      dplyr::left_join(config_precomputed, by = c("hazard_type", "hazard_indicator")) |>
      dplyr::mutate(precomputed = dplyr::coalesce(.data$precomputed, TRUE)) |>
      dplyr::filter(.data$precomputed) |>
      dplyr::select(-"precomputed")
  }

  # Early check: if no hazards in inventory, that's the real problem
  if (nrow(required_hazards_inventory) == 0) {
    stop(
      "No hazards available for precomputed lookup.\n",
      "  Total hazards in inventory: ", nrow(hazards_inventory), "\n",
      "  This usually means filter_hazards_by_events selected 0 hazards.\n",
      "  Check your event configuration and ensure events match available hazards."
    )
  }

  # For NC indicators with a fixed ensemble (e.g. fire fwi with fixed: {ensemble: median}),
  # the uncertainty run loads rasters with ensemble_filter="p10/p90", which sets indicator_key
  # to ensemble=p10 in the inventory. But the precomputed CSV was keyed with the fixed ensemble
  # (ensemble=median). We need to:
  #   1. Replace ensemble in indicator_key to find the correct CSV row (csv_key).
  #   2. After the lookup, restore indicator_key to the original inventory key (p10/p90)
  #      so the inner_join in compute_risk by indicator_key still matches.
  nc_csv_key_map <- NULL   # will hold: original_key -> csv_key for NC indicators
  if (!is.null(hazard_configs) && length(hazard_configs) > 0) {
    nc_fixed_ensemble <- purrr::map_dfr(names(hazard_configs), function(ht) {
      cfg <- hazard_configs[[ht]]
      purrr::map_dfr(names(cfg$indicators), function(ind_key) {
        ind <- cfg$indicators[[ind_key]]
        if (identical(ind$source, "nc") && !is.null(ind$fixed$ensemble)) {
          tibble::tibble(
            hazard_type      = ht,
            hazard_indicator = ind_key,
            fixed_ensemble   = as.character(ind$fixed$ensemble)
          )
        } else {
          tibble::tibble(hazard_type = character(0), hazard_indicator = character(0), fixed_ensemble = character(0))
        }
      })
    })

    if (nrow(nc_fixed_ensemble) > 0) {
      # Build a mapping from original_key (p10) -> csv_key (median) before modifying the inventory
      nc_csv_key_map <- required_hazards_inventory |>
        dplyr::inner_join(nc_fixed_ensemble, by = c("hazard_type", "hazard_indicator")) |>
        dplyr::mutate(
          original_key = .data$indicator_key,
          csv_key = mapply(function(key, ens) sub("__ensemble=[^_]+$", paste0("__ensemble=", ens), key),
                           .data$indicator_key, .data$fixed_ensemble, USE.NAMES = FALSE)
        ) |>
        dplyr::filter(.data$original_key != .data$csv_key) |>
        dplyr::select("original_key", "csv_key") |>
        dplyr::distinct()

      required_hazards_inventory <- required_hazards_inventory |>
        dplyr::left_join(nc_fixed_ensemble, by = c("hazard_type", "hazard_indicator")) |>
        dplyr::mutate(
          indicator_key = dplyr::if_else(
            !is.na(.data$fixed_ensemble),
            mapply(function(key, ens) sub("__ensemble=[^_]+$", paste0("__ensemble=", ens), key),
                   .data$indicator_key, .data$fixed_ensemble, USE.NAMES = FALSE),
            .data$indicator_key
          )
        ) |>
        dplyr::select(-"fixed_ensemble")
    }
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
  assets_adm2_candidates <- assets_df |>
    dplyr::filter(!is.na(.data$municipality), .data$municipality != "")
  
  precomp_adm2 <- precomp_filtered |> dplyr::filter(.data$adm_level == "ADM2")
  
  assets_with_adm2 <- NULL
  
  # Try joining by code first if available
  if ("municipality_code" %in% names(assets_adm2_candidates) && "adm_code" %in% names(precomp_adm2)) {
    # 1. Assets with codes that match
    assets_with_adm2_code <- assets_adm2_candidates |>
      dplyr::filter(!is.na(.data$municipality_code), .data$municipality_code != "") |>
      dplyr::inner_join(
        precomp_adm2,
        by = c("municipality_code" = "adm_code"),
        relationship = "many-to-many"
      ) |>
      dplyr::mutate(matching_method = "municipality code lookup")
      
    # 2. Remaining assets (no code or code didn't match)
    matched_assets_code <- unique(assets_with_adm2_code$asset)
    assets_remaining_adm2 <- assets_adm2_candidates |>
      dplyr::filter(!(.data$asset %in% matched_assets_code))
      
    # 3. Match remaining by name
    assets_with_adm2_name <- assets_remaining_adm2 |>
      dplyr::inner_join(
        precomp_adm2,
        by = c("municipality" = "adm_name"),
        relationship = "many-to-many"
      ) |>
      dplyr::mutate(matching_method = "municipality name lookup")
      
    assets_with_adm2 <- dplyr::bind_rows(assets_with_adm2_code, assets_with_adm2_name)
  } else {
    # Fallback to name only
    assets_with_adm2 <- assets_adm2_candidates |>
      dplyr::inner_join(
        precomp_adm2,
        by = c("municipality" = "adm_name"),
        relationship = "many-to-many"
      ) |>
      dplyr::mutate(matching_method = "municipality name lookup")
  }

  # Join with State (ADM1) for assets that didn't match ADM2
  matched_assets_adm2 <- unique(assets_with_adm2$asset)
  assets_adm1_candidates <- assets_df |>
    dplyr::filter(!(.data$asset %in% matched_assets_adm2)) |>
    dplyr::filter(!is.na(.data$state), .data$state != "")
    
  precomp_adm1 <- precomp_filtered |> dplyr::filter(.data$adm_level == "ADM1")
  
  assets_with_adm1 <- NULL
  
  # Try joining by code first if available
  if ("state_code" %in% names(assets_adm1_candidates) && "adm_code" %in% names(precomp_adm1)) {
    # 1. Assets with codes that match
    assets_with_adm1_code <- assets_adm1_candidates |>
      dplyr::filter(!is.na(.data$state_code), .data$state_code != "") |>
      dplyr::inner_join(
        precomp_adm1,
        by = c("state_code" = "adm_code"),
        relationship = "many-to-many"
      ) |>
      dplyr::mutate(matching_method = "state code lookup")
      
    # 2. Remaining assets
    matched_assets_code <- unique(assets_with_adm1_code$asset)
    assets_remaining_adm1 <- assets_adm1_candidates |>
      dplyr::filter(!(.data$asset %in% matched_assets_code))
      
    # 3. Match remaining by name
    assets_with_adm1_name <- assets_remaining_adm1 |>
      dplyr::inner_join(
        precomp_adm1,
        by = c("state" = "adm_name"),
        relationship = "many-to-many"
      ) |>
      dplyr::mutate(matching_method = "state name lookup")
      
    assets_with_adm1 <- dplyr::bind_rows(assets_with_adm1_code, assets_with_adm1_name)
  } else {
    # Fallback to name only
    assets_with_adm1 <- assets_adm1_candidates |>
      dplyr::inner_join(
        precomp_adm1,
        by = c("state" = "adm_name"),
        relationship = "many-to-many"
      ) |>
      dplyr::mutate(matching_method = "state name lookup")
  }

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
    present_municipalities <- unique(precomputed_hazards$adm_name[precomputed_hazards$adm_level == "ADM2"])
    present_states <- unique(precomputed_hazards$adm_name[precomputed_hazards$adm_level == "ADM1"])

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
          dplyr::filter(.data$adm_name == !!region, .data$adm_level == !!adm_level_filter)

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
    )

  if ("agg_inv" %in% names(combined_matches)) {
    combined_matches <- combined_matches |>
      dplyr::mutate(agg_from_inv = .data$agg_inv) |>
      dplyr::select(-"agg_inv")
  } else if ("agg" %in% names(combined_matches)) {
    combined_matches <- combined_matches |>
      dplyr::mutate(agg_from_inv = .data$agg)
  } else {
    combined_matches$agg_from_inv <- NA_character_
  }

  # Now determine effective aggregation method for each row
  combined_matches <- combined_matches |>
    dplyr::mutate(
      effective_agg = dplyr::coalesce(.data$agg_from_inv, aggregation_method),
      # 'closest' means "use the aggregation_method" for precomputed data.
      # For NC indicators this gives p10/median/p90 on uncertainty runs, mean on normal runs.
      effective_agg = dplyr::if_else(.data$effective_agg == "closest", aggregation_method, .data$effective_agg)
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
    "asset_category", "asset_subtype", "size_in_m2", "share_of_economic_activity", "cost_factor",
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

  # Restore the original inventory indicator_key (e.g. ensemble=p10) for NC indicators whose
  # key was temporarily replaced with the CSV key (ensemble=median) for the lookup.
  # compute_risk joins assets_long back to the inventory by indicator_key, so the key must
  # match the inventory's value, not the CSV's.
  if (!is.null(nc_csv_key_map) && nrow(nc_csv_key_map) > 0) {
    final_data <- final_data |>
      dplyr::left_join(nc_csv_key_map, by = c("indicator_key" = "csv_key")) |>
      dplyr::mutate(
        indicator_key = dplyr::coalesce(.data$original_key, .data$indicator_key)
      ) |>
      dplyr::select(-"original_key")
  }

  return(final_data)
}
