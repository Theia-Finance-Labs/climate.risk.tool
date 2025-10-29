#' Join damage and cost factors based on hazard type, indicator, intensity and asset category (internal function)
#'
#' @param assets_with_hazards Data frame in long format with asset and hazard information
#'   including hazard_type, hazard_indicator, hazard_intensity, scenario_name columns
#'   (from extract_hazard_statistics joined with events)
#' @param damage_factors_df Data frame with damage and cost factors lookup table
#' @return Data frame with original columns plus damage_factor, cost_factor, and business_disruption columns
#' @noRd
join_damage_cost_factors <- function(assets_with_hazards, damage_factors_df) {
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
    join_compound_damage_factors(compound_assets, damage_factors_df)
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

#' Join FloodTIF damage factors using intensity-based matching (internal function)
#'
#' @param flood_assets Data frame with FloodTIF hazard assets
#' @param damage_factors_df Data frame with damage and cost factors lookup table
#' @return Data frame with damage_factor, cost_factor, and business_disruption columns
#' @noRd
join_flood_damage_factors <- function(flood_assets, damage_factors_df) {
  # Add intensity key
  assets_tmp <- flood_assets |>
    dplyr::mutate(intensity_key = as.integer(round(as.numeric(.data$hazard_intensity))))

  factors_tmp <- damage_factors_df |>
    dplyr::filter(.data$hazard_type == "FloodTIF") |>
    dplyr::mutate(intensity_key = as.integer(round(as.numeric(.data$hazard_intensity))))

  # Compute max available intensity key per (hazard_type, hazard_indicator, asset_category)
  max_key_by_group <- factors_tmp |>
    dplyr::group_by(.data$hazard_type, .data$hazard_indicator, .data$asset_category) |>
    dplyr::summarize(
      max_intensity_key = max(.data$intensity_key, na.rm = TRUE),
      .groups = "drop"
    )

  # Join and cap effective intensity key
  assets_tmp <- dplyr::left_join(
    assets_tmp,
    max_key_by_group,
    by = c("hazard_type", "hazard_indicator", "asset_category")
  ) |>
    dplyr::mutate(
      effective_intensity_key = dplyr::if_else(
        !is.na(.data$max_intensity_key) &
          .data$intensity_key > .data$max_intensity_key,
        .data$max_intensity_key,
        .data$intensity_key
      )
    )

  # Join on hazard_type, hazard_indicator, asset_category, effective_intensity_key
  factors_key <- factors_tmp |>
    dplyr::select("hazard_type", "hazard_indicator", "asset_category", "intensity_key",
                  "damage_factor", "cost_factor", "business_disruption")

  flood_merged <- dplyr::left_join(
    assets_tmp,
    factors_key,
    by = dplyr::join_by(
      "hazard_type",
      "hazard_indicator",
      "asset_category",
      "effective_intensity_key" == "intensity_key"
    )
  ) |>
    dplyr::mutate(
      damage_factor = dplyr::coalesce(as.numeric(.data$damage_factor), NA_real_),
      cost_factor = dplyr::coalesce(as.numeric(.data$cost_factor), NA_real_),
      business_disruption = dplyr::coalesce(as.numeric(.data$business_disruption), NA_real_)
    ) |>
    dplyr::select(-dplyr::starts_with(".__"))

  return(flood_merged)
}

#' Join Compound (heat) damage factors using province and GWL matching (internal function)
#'
#' @param compound_assets Data frame with Compound hazard assets
#' @param damage_factors_df Data frame with damage and cost factors lookup table
#' @return Data frame with damage_factor column (cost_factor and business_disruption are NA)
#' @noRd
join_compound_damage_factors <- function(compound_assets, damage_factors_df) {
  # Filter factors for Compound type
  # gwl column in damage_factors_df matches scenario_name from events (column names are lowercased by read function)
  compound_factors <- damage_factors_df |>
    dplyr::filter(.data$hazard_type == "Compound") |>
    dplyr::select("hazard_type", "province", "gwl", "damage_factor")

  # Join on hazard_type, province, and scenario_name = gwl
  # scenario_name is already available in compound_assets from events join
  compound_merged <- dplyr::left_join(
    compound_assets,
    compound_factors,
    by = c("hazard_type", "province", "scenario_name" = "gwl")
  ) |>
    dplyr::mutate(
      cost_factor = NA_real_,
      business_disruption = NA_real_
    )

  return(compound_merged)
}


#' Join Drought damage factors for agriculture assets with crop/province/season matching (internal function)
#'
#' @param drought_assets Data frame with Drought hazard assets including season column from events
#' @param damage_factors_df Data frame with damage and cost factors lookup table
#' @return Data frame with damage_factor column (cost_factor and business_disruption are NA)
#' @noRd
join_drought_damage_factors <- function(drought_assets, damage_factors_df) {
  # Filter damage factors for drought (hazard_type = "drought", hazard_indicator = "SPI3", metric = "mean")
  # Rename season column in damage factors to avoid conflicts
  drought_factors <- damage_factors_df |>
    dplyr::filter(
      .data$hazard_type == "drought",
      .data$hazard_indicator == "SPI3",
      .data$metric == "mean"
    ) |>
    dplyr::mutate(
      intensity_key = round(as.numeric(.data$hazard_intensity), 2)  # Round for matching
    ) |>
    dplyr::select("province", "subtype", "season", "damage_factor", "off_window", "intensity_key") |>
    dplyr::rename(growing_season = "season")  # Rename to avoid conflict with event season

  # Prepare assets: normalize crop subtype (NA/empty → "Other")
  drought_assets_prepared <- drought_assets |>
    dplyr::mutate(
      # Normalize subtype: missing values become "Other"
      asset_subtype_normalized = dplyr::if_else(
        is.na(.data$asset_subtype) | .data$asset_subtype == "",
        "Other",
        .data$asset_subtype
      ),
      # Normalize province for matching: missing values will be matched to "Other"
      province_normalized = dplyr::if_else(
        is.na(.data$province) | .data$province == "",
        "Other",
        .data$province
      ),
      # Cap intensity: < -3 becomes -3, > -1 gets damage_factor = 0
      intensity_capped = dplyr::case_when(
        .data$hazard_intensity < -3 ~ -3,
        .data$hazard_intensity > -1 ~ 999,  # Special marker for no damage
        TRUE ~ .data$hazard_intensity
      ),
      intensity_key = round(.data$intensity_capped, 2)  # Round for matching
    )

  # First attempt: match on specific province
  drought_merged_specific <- dplyr::left_join(
    drought_assets_prepared,
    drought_factors,
    by = c("province_normalized" = "province", "asset_subtype_normalized" = "subtype", "intensity_key"),
    relationship = "many-to-many"
  )

  # Second attempt: for rows that didn't match, try "Other" province
  drought_factors_other <- drought_factors |>
    dplyr::filter(.data$province == "Other")

  # Identify rows that didn't get a match (all damage_factor values are NA)
  unmatched_assets <- drought_merged_specific |>
    dplyr::group_by(.data$asset, .data$event_id) |>
    dplyr::filter(all(is.na(.data$damage_factor))) |>
    dplyr::ungroup() |>
    dplyr::select(-dplyr::any_of(c("subtype", "growing_season", "damage_factor", "off_window"))) |>
    dplyr::distinct()

  # Join unmatched assets with "Other" province
  if (nrow(unmatched_assets) > 0) {
    drought_merged_other <- dplyr::left_join(
      unmatched_assets,
      drought_factors_other |>
        dplyr::select(-"province"),  # Remove province column since we're using "Other"
      by = c("asset_subtype_normalized" = "subtype", "intensity_key"),
      relationship = "many-to-many"
    )

    # Remove unmatched rows from specific matches and combine with "Other" matches
    drought_merged <- drought_merged_specific |>
      dplyr::group_by(.data$asset, .data$event_id) |>
      dplyr::filter(!all(is.na(.data$damage_factor))) |>
      dplyr::ungroup() |>
      dplyr::bind_rows(drought_merged_other)
  } else {
    drought_merged <- drought_merged_specific
  }

  # Now apply season matching logic and intensity capping
  drought_merged <- drought_merged |>
    dplyr::mutate(
      # Ensure numeric types
      damage_factor = as.numeric(.data$damage_factor),
      off_window = as.numeric(.data$off_window),
      # Check if user-selected season matches growing season
      season_match = (.data$season == .data$growing_season),
      # Apply damage factor logic
      damage_factor_final = dplyr::case_when(
        # No damage if intensity > -1
        .data$intensity_capped == 999 ~ 0,
        # Missing damage factor (shouldn't happen but handle gracefully)
        is.na(.data$damage_factor) ~ 0,
        # On-season: use full damage factor
        .data$season_match ~ .data$damage_factor,
        # Off-season: multiply by off_window
        !.data$season_match ~ .data$damage_factor * .data$off_window,
        # Default
        TRUE ~ 0
      ),
      # Set cost_factor and business_disruption to NA for drought
      cost_factor = NA_real_,
      business_disruption = NA_real_
    ) |>
    # Clean up temporary columns
    dplyr::select(
      -dplyr::any_of(c(
        "asset_subtype_normalized",
        "province_normalized",
        "intensity_capped",
        "intensity_key",
        "season_match",
        "growing_season",  # Growing season from damage factors (not needed)
        "off_window",
        "damage_factor"  # Old damage_factor from join
      ))
    ) |>
    dplyr::rename(damage_factor = "damage_factor_final")

  # Filter to agriculture assets only (drought only affects agriculture)
  drought_merged <- drought_merged |>
    dplyr::filter(.data$asset_category == "agriculture")

  return(drought_merged)
}