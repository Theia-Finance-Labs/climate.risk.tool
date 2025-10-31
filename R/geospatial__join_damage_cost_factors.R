#' Join damage and cost factors based on hazard type, indicator, intensity and asset category (internal function)
#'
#' @param assets_with_hazards Data frame in long format with asset and hazard information
#'   including hazard_type, hazard_indicator,<｜place▁holder▁no▁118｜> hazard_intensity, scenario_name columns
#'   (from extract_hazard_statistics joined with events)
#' @param damage_factors_df Data frame with damage and cost factors lookup table
#' @param cnae_exposure Optional tibble with CNAE exposure data for sector-based metric selection (columns: cnae, lp_exposure)
#' @return Data frame with original columns plus damage_factor, cost_factor, and business_disruption columns
#' @noRd
join_damage_cost_factors <- function(assets_with_hazards, damage_factors_df, cnae_exposure = NULL) {
  # Separate assets by hazard type
  flood_assets <- assets_with_hazards |>
    dplyr::filter(.data$hazard_type == "FloodTIF")
  compound_assets <- assets_with_hazards |>
    dplyr::filter(.data$hazard_type == "Compound")
  drought_assets <- assets_with_hazards |>
    dplyr::filter(.data$hazard_type == "Drought")

  # Process each hazard type with its specific logic
  flood_merged <- if (nrow(flood_assets) > 0) {
    join_flood_damage_factors(flood_assets, damage_factors_df)
  } else {
    NULL
  }

  compound_merged <- if (nrow(compound_assets) > 0) {
    join_compound_damage_factors(compound_assets, damage_factors_df, cnae_exposure)
  } else {
    NULL
  }

  drought_merged <- if (nrow(drought_assets) > 0) {
    join_drought_damage_factors(drought_assets, damage_factors_df)
  } else {
    NULL
  }

  # Combine results (filter out NULLs and empty data frames)
  all_results <- list(flood_merged, compound_merged, drought_merged)
  non_empty_results <- all_results[sapply(all_results, function(x) !is.null(x) && nrow(x) > 0)]

  if (length(non_empty_results) == 0) {
    stop("No hazard merged with damage and cost factors")
  }

  merged <- dplyr::bind_rows(non_empty_results)

  return(merged)
}

#' Join FloodTIF damage factors using closest intensity matching (internal function)
#'
#' @param flood_assets Data frame with FloodTIF hazard assets
#' @param damage_factors_df Data frame with damage and cost factors lookup table
#' @return Data frame with damage_factor, cost_factor, and business_disruption columns
#' @noRd
join_flood_damage_factors <- function(flood_assets, damage_factors_df) {
  # Filter flood damage factors
  flood_factors <- damage_factors_df |>
    dplyr::filter(.data$hazard_type == "FloodTIF") |>
    dplyr::mutate(
      hazard_intensity_num = as.numeric(.data$hazard_intensity)
    ) |>
    dplyr::select(
      "hazard_type", "hazard_indicator", "asset_category", "hazard_intensity_num",
      "damage_factor", "cost_factor", "business_disruption"
    )

  # Prepare assets with numeric intensity
  flood_assets_prepared <- flood_assets |>
    dplyr::mutate(
      hazard_intensity_num = as.numeric(.data$hazard_intensity)
    )

  # Find closest intensity match for each asset
  result_list <- vector("list", nrow(flood_assets_prepared))

  for (i in seq_len(nrow(flood_assets_prepared))) {
    asset_row <- flood_assets_prepared[i, ]
    asset_intensity <- asset_row$hazard_intensity_num

    # Find matching factors for this hazard type, indicator, and category
    factors_subset <- flood_factors |>
      dplyr::filter(
        .data$hazard_type == asset_row$hazard_type,
        .data$hazard_indicator == asset_row$hazard_indicator,
        .data$asset_category == asset_row$asset_category
      )

    if (nrow(factors_subset) > 0) {
      # Find closest intensity match and cap at maximum available
      factors_subset <- factors_subset |>
        dplyr::mutate(
          intensity_diff = abs(.data$hazard_intensity_num - asset_intensity)
        ) |>
        dplyr::arrange(.data$intensity_diff)

      # If asset intensity exceeds max available, use max
      max_intensity <- max(factors_subset$hazard_intensity_num, na.rm = TRUE)
      if (asset_intensity > max_intensity) {
        factors_subset <- factors_subset |>
          dplyr::filter(.data$hazard_intensity_num == max_intensity) |>
          dplyr::slice(1)
      } else {
        factors_subset <- factors_subset |>
          dplyr::slice(1)
      }

      result_list[[i]] <- asset_row |>
        dplyr::bind_cols(
          factors_subset |>
            dplyr::select("damage_factor", "cost_factor", "business_disruption")
        )
    } else {
      # No match found - set to NA
      result_list[[i]] <- asset_row |>
        dplyr::mutate(
          damage_factor = NA_real_,
          cost_factor = NA_real_,
          business_disruption = NA_real_
        )
    }
  }

  flood_merged <- dplyr::bind_rows(result_list) |>
    dplyr::mutate(
      damage_factor = dplyr::coalesce(as.numeric(.data$damage_factor), NA_real_),
      cost_factor = dplyr::coalesce(as.numeric(.data$cost_factor), NA_real_),
      business_disruption = dplyr::coalesce(as.numeric(.data$business_disruption), NA_real_)
    ) |>
    dplyr::select(-"hazard_intensity_num")

  return(flood_merged)
}

#' Join Compound (heat) damage factors using province, GWL, and sector-based metric matching (internal function)
#'
#' @param compound_assets Data frame with Compound hazard assets
#' @param damage_factors_df Data frame with damage and cost factors lookup table
#' @param cnae_exposure Optional tibble with CNAE exposure data (columns: cnae, lp_exposure). If NULL, uses median metric.
#' @return Data frame with damage_factor column (cost_factor and business_disruption are NA)
#' @noRd
join_compound_damage_factors <- function(compound_assets, damage_factors_df, cnae_exposure = NULL) {
  # Determine metric for each asset based on CNAE exposure code
  # If cnae_exposure is provided, use CNAE-based lookup
  # Otherwise, default to median/high fallback
  if (!is.null(cnae_exposure) && nrow(cnae_exposure) > 0) {
    compound_assets_with_metric <- compound_assets |>
      dplyr::left_join(
        cnae_exposure |>
          dplyr::select("cnae", "lp_exposure"),
        by = "cnae"
      ) |>
      dplyr::mutate(
        # Determine metric based on lookup or defaults; only cnae is used
        metric = dplyr::case_when(
          !is.na(.data$lp_exposure) ~ .data$lp_exposure,
          is.na(.data$cnae) & .data$asset_category == "agriculture" ~ "high",
          TRUE ~ "median"
        )
      ) |>
      dplyr::select(-"lp_exposure")
  } else {
    # If no CNAE exposure data provided, use fallback logic based on asset_category
    compound_assets_with_metric <- compound_assets |>
      dplyr::mutate(
        metric = dplyr::if_else(
          is.na(.data$cnae) & .data$asset_category == "agriculture",
          "high",
          "median"
        )
      )
  }

  # Join with damage factors matching on hazard_type, province, gwl, AND metric
  compound_factors <- damage_factors_df |>
    dplyr::filter(.data$hazard_type == "Compound") |>
    dplyr::select("hazard_type", "province", "gwl", "metric", "damage_factor")

  compound_merged <- dplyr::left_join(
    compound_assets_with_metric,
    compound_factors,
    by = c("hazard_type", "province", "scenario_name" = "gwl", "metric")
  ) |>
    dplyr::mutate(
      cost_factor = NA_real_,
      business_disruption = NA_real_
    )

  return(compound_merged)
}


#' Join Drought damage factors for agriculture assets with crop/province/season matching using closest intensity (internal function)
#'
#' Matching strategy (in order of priority):
#' 1. Province + Subtype: First attempts to match the asset's province and crop subtype
#' 2. Fallback Province: If province not found, uses the first available province with data for that crop
#' 3. No match: If crop not found at all, sets damage_factor = 0 with NA for growing_season and off_window
#'
#' After finding the correct province and crop:
#' - Finds the closest intensity match (caps at -3 for values below -3)
#' - For multi-season crops (e.g., Sugarcane with Winter and Autumn):
#'   * If user-selected season matches a growing season: use that season's damage factor
#'   * If user-selected season doesn't match: average all growing seasons' damage factors and off_windows,
#'     then apply: avg_damage_factor * avg_off_window
#'
#' @param drought_assets Data frame with Drought hazard assets including season column from events
#' @param damage_factors_df Data frame with damage and cost factors lookup table
#' @return Data frame with damage_factor, growing_season, off_window, and season columns (cost_factor and business_disruption are NA)
#' @noRd
join_drought_damage_factors <- function(drought_assets, damage_factors_df) {
  # Filter damage factors for drought (hazard_type = "Drought", hazard_indicator = "SPI3")
  drought_factors <- damage_factors_df |>
    dplyr::filter(
      .data$hazard_type == "Drought",
      .data$hazard_indicator == "SPI3"
    ) |>
    dplyr::mutate(
      hazard_intensity_num = as.numeric(.data$hazard_intensity),
      damage_factor_value = as.numeric(.data$damage_factor),
      off_window_value = as.numeric(.data$off_window)
    ) |>
    dplyr::select("province", "subtype", "season", "damage_factor_value", "off_window_value", "hazard_intensity_num") |>
    dplyr::rename(growing_season = "season") # Rename to avoid conflict with event season

  # Create "Other" province entries for each crop/intensity/season by averaging across all provinces
  drought_factors_other_province <- drought_factors |>
    dplyr::group_by(.data$subtype, .data$growing_season, .data$hazard_intensity_num) |>
    dplyr::summarize(
      damage_factor_value = mean(.data$damage_factor_value, na.rm = TRUE),
      off_window_value = mean(.data$off_window_value, na.rm = TRUE),
      province = "Other",
      .groups = "drop"
    ) |>
    dplyr::select("province", "subtype", "growing_season", "damage_factor_value", "off_window_value", "hazard_intensity_num")
  
  # Create "Other" crop entries by duplicating Soybean data (for missing crop types)
  drought_factors_other_crop <- drought_factors |>
    dplyr::filter(.data$subtype == "Soybean") |>
    dplyr::mutate(subtype = "Other")
  
  # Also create "Other" crop + "Other" province combination
  drought_factors_other_both <- drought_factors_other_province |>
    dplyr::filter(.data$subtype == "Soybean") |>
    dplyr::mutate(subtype = "Other")
  
  # Combine all damage factors
  drought_factors <- dplyr::bind_rows(
    drought_factors, 
    drought_factors_other_province,
    drought_factors_other_crop,
    drought_factors_other_both
  )

  # Get list of provinces that exist in damage factors (excluding "Other")
  available_provinces <- drought_factors |>
    dplyr::filter(.data$province != "Other") |>
    dplyr::pull(.data$province) |>
    unique()
  
  # Get list of crops that exist in damage factors (excluding "Other")
  available_crops <- drought_factors |>
    dplyr::filter(.data$subtype != "Other") |>
    dplyr::pull(.data$subtype) |>
    unique()
  
  # Prepare assets: map to available provinces/crops or use "Other"
  drought_assets_prepared <- drought_assets |>
    dplyr::mutate(
      # If subtype is missing or not in available crops, use "Other"
      subtype_for_match = dplyr::case_when(
        is.na(.data$asset_subtype) | .data$asset_subtype == "" ~ "Other",
        !(.data$asset_subtype %in% available_crops) ~ .data$asset_subtype, # Keep original for first try
        TRUE ~ .data$asset_subtype
      ),
      # If province is missing or not in available provinces, use "Other" for matching
      province_for_match = dplyr::case_when(
        is.na(.data$province) | .data$province == "" ~ "Other",
        !(.data$province %in% available_provinces) ~ "Other",  # Use "Other" if province doesn't exist
        TRUE ~ .data$province
      ),
      # Asset identifier for tracking
      asset_id = dplyr::row_number()
    )
  
  # Handle assets with intensity > -1 (no damage) early
  assets_no_damage <- drought_assets_prepared |>
    dplyr::filter(.data$hazard_intensity > -1) |>
    dplyr::mutate(
      damage_factor = 0,
      off_window = NA_real_,
      growing_season = NA_character_,
      cost_factor = NA_real_,
      business_disruption = NA_real_
    ) |>
    dplyr::select(-dplyr::any_of(c("subtype_for_match", "province_for_match", "asset_id")))
  
  # Continue with assets that need damage factor lookup (intensity <= -1)
  drought_assets_prepared <- drought_assets_prepared |>
    dplyr::filter(.data$hazard_intensity <= -1)

  # Step 1: EXACT MATCH - Try province + subtype + season combinations
  # Priority: actual province + actual crop, then fallbacks
  merged_exact <- drought_assets_prepared |>
    dplyr::inner_join(
      drought_factors,
      by = c("province_for_match" = "province", "subtype_for_match" = "subtype", "season" = "growing_season"),
      relationship = "many-to-many"
    ) |>
    dplyr::mutate(
      intensity_diff = abs(.data$hazard_intensity_num - as.numeric(.data$hazard_intensity))
    ) |>
    dplyr::group_by(.data$asset_id) |>
    dplyr::filter(.data$intensity_diff == min(.data$intensity_diff)) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      match_type = "exact_season",
      growing_season = .data$season # After join, season contains the matched growing season
    )

  # Step 2: OFF-SEASON MATCH - province + subtype (all seasons, will average)
  # Join handles all combinations: actual prov + actual crop, actual prov + Other crop, Other prov + actual crop, Other prov + Other crop
  assets_no_exact_match <- drought_assets_prepared |>
    dplyr::anti_join(
      merged_exact |> dplyr::select("asset_id") |> dplyr::distinct(),
      by = "asset_id"
    )

  merged_off_season <- NULL
  if (nrow(assets_no_exact_match) > 0) {
    merged_off_season <- assets_no_exact_match |>
      dplyr::inner_join(
        drought_factors,
        by = c("province_for_match" = "province", "subtype_for_match" = "subtype"),
        relationship = "many-to-many"
      ) |>
      dplyr::mutate(
        intensity_diff = abs(.data$hazard_intensity_num - as.numeric(.data$hazard_intensity))
      ) |>
      dplyr::group_by(.data$asset_id) |>
      dplyr::filter(.data$intensity_diff == min(.data$intensity_diff)) |>
      dplyr::ungroup() |>
      dplyr::mutate(match_type = "off_season")
  }

  # Combine all matches (no Step 3 needed - "Other" crop is already in damage factors)
  all_matched <- dplyr::bind_rows(merged_exact, merged_off_season)

  # Step 4: Process results - intensity matching already done in earlier steps
  if (nrow(all_matched) > 0) {
    # For exact_season: use damage factor directly
    # These already have closest intensity from Step 1
    result_exact_matches <- all_matched |>
      dplyr::filter(.data$match_type == "exact_season") |>
      dplyr::group_by(.data$asset_id) |>
      dplyr::slice(1) |>
      dplyr::ungroup() |>
      dplyr::mutate(
        damage_factor = .data$damage_factor_value,
        off_window = .data$off_window_value
        # growing_season is already in the data from the join
      )

    # For off_season: average and apply off_window
    # These already have closest intensity from Step 2
    off_season_data <- all_matched |>
      dplyr::filter(.data$match_type == "off_season")
    
    if (nrow(off_season_data) > 0) {
      result_off_season_matches <- off_season_data |>
        dplyr::group_by(.data$asset_id) |>
        dplyr::summarize(
          # Keep all asset columns from first row
          dplyr::across(dplyr::any_of(names(drought_assets_prepared)), dplyr::first),
          # Average the damage factors and off_windows across all growing seasons
          avg_damage_factor = mean(.data$damage_factor_value, na.rm = TRUE),
          avg_off_window = mean(.data$off_window_value, na.rm = TRUE),
          # Collect all growing seasons for this crop/province
          seasons_list = paste(sort(unique(dplyr::pick("growing_season")$growing_season)), collapse = ", "),
          .groups = "drop"
        ) |>
        dplyr::mutate(
          damage_factor = .data$avg_damage_factor * .data$avg_off_window,
          off_window = .data$avg_off_window,
          growing_season = paste0("Averaged (", .data$seasons_list, ")")
        ) |>
        dplyr::select(-"avg_damage_factor", -"avg_off_window", -"seasons_list")
    } else {
      result_off_season_matches <- data.frame()
    }

    # Only keep off-season assets that weren't already exact matched
    if (nrow(result_off_season_matches) > 0 && nrow(result_exact_matches) > 0) {
      assets_with_exact_match <- result_exact_matches |> dplyr::pull(.data$asset_id)
      result_off_season_matches <- result_off_season_matches |>
        dplyr::filter(!(.data$asset_id %in% assets_with_exact_match))
    }

    # Combine both cases
    result_with_factors <- dplyr::bind_rows(
      result_exact_matches |> dplyr::select(dplyr::any_of(c(names(drought_assets_prepared), "damage_factor", "off_window", "growing_season"))),
      result_off_season_matches
    )
  } else {
    result_with_factors <- data.frame()
  }

  # Step 5: Handle unmatched assets (those that didn't match any crop/province)
  all_asset_ids <- if (nrow(result_with_factors) > 0) {
    result_with_factors |> dplyr::pull(.data$asset_id)
  } else {
    integer(0)
  }

  result_no_factors <- drought_assets_prepared |>
    dplyr::filter(!(.data$asset_id %in% all_asset_ids)) |>
    dplyr::mutate(
      damage_factor = 0, # No match means no damage
      off_window = NA_real_,
      growing_season = NA_character_
    )

  # Combine all results (matched, unmatched, and no-damage assets)
  drought_merged <- dplyr::bind_rows(result_with_factors, result_no_factors, assets_no_damage) |>
    dplyr::mutate(
      cost_factor = NA_real_,
      business_disruption = NA_real_
    ) |>
    dplyr::select(
      -dplyr::any_of(c(
        "subtype_for_match",
        "province_for_match",
        "asset_id",
        "intensity_diff",
        "match_type",
        "damage_factor_value",
        "off_window_value",
        "hazard_intensity_num"
      ))
    )

  # Filter to agriculture assets only (drought only affects agriculture)
  drought_merged <- drought_merged |>
    dplyr::filter(.data$asset_category == "agriculture")

  return(drought_merged)
}
