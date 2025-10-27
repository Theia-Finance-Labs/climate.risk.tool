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

  # Combine results
  merged <- dplyr::bind_rows(flood_merged, compound_merged)

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
    dplyr::mutate(.__intensity_key__. = as.integer(round(as.numeric(.data$hazard_intensity))))

  factors_tmp <- damage_factors_df |>
    dplyr::filter(.data$hazard_type == "FloodTIF") |>
    dplyr::mutate(.__intensity_key__. = as.integer(round(as.numeric(.data$hazard_intensity))))

  # Separate agriculture assets (need asset_subtype matching) from others
  assets_agriculture <- assets_tmp |>
    dplyr::filter(.data$asset_category == "agriculture")
  
  assets_non_agriculture <- assets_tmp |>
    dplyr::filter(.data$asset_category != "agriculture")

  # Process non-agriculture assets (commercial building, industrial building)
  flood_merged_non_ag <- NULL
  if (nrow(assets_non_agriculture) > 0) {
    # Compute max available intensity key per (hazard_type, hazard_indicator, asset_category)
    max_key_by_group <- factors_tmp |>
      dplyr::filter(.data$asset_category != "agriculture") |>
      dplyr::group_by(.data$hazard_type, .data$hazard_indicator, .data$asset_category) |>
      dplyr::summarize(
        .__max_intensity_key__. = max(.data$.__intensity_key__., na.rm = TRUE),
        .groups = "drop"
      )

    # Join and cap effective intensity key
    assets_non_agriculture <- dplyr::left_join(
      assets_non_agriculture,
      max_key_by_group,
      by = c("hazard_type", "hazard_indicator", "asset_category")
    ) |>
      dplyr::mutate(
        .__effective_intensity_key__. = dplyr::if_else(
          !is.na(.data$.__max_intensity_key__.) &
            .data$.__intensity_key__. > .data$.__max_intensity_key__.,
          .data$.__max_intensity_key__.,
          .data$.__intensity_key__.
        )
      )

    # Join on hazard_type, hazard_indicator, asset_category, effective_intensity_key
    factors_key_non_ag <- factors_tmp |>
      dplyr::filter(.data$asset_category != "agriculture") |>
      dplyr::select("hazard_type", "hazard_indicator", "asset_category", ".__intensity_key__.",
                    "damage_factor", "cost_factor", "business_disruption")

    flood_merged_non_ag <- dplyr::left_join(
      assets_non_agriculture,
      factors_key_non_ag,
      by = dplyr::join_by(
        "hazard_type",
        "hazard_indicator",
        "asset_category",
        ".__effective_intensity_key__." == ".__intensity_key__."
      )
    ) |>
      dplyr::mutate(
        damage_factor = dplyr::coalesce(as.numeric(.data$damage_factor), NA_real_),
        cost_factor = dplyr::coalesce(as.numeric(.data$cost_factor), NA_real_),
        business_disruption = dplyr::coalesce(as.numeric(.data$business_disruption), NA_real_)
      ) |>
      dplyr::select(-dplyr::starts_with(".__"))
  }

  # Process agriculture assets (need asset_subtype matching)
  flood_merged_ag <- NULL
  if (nrow(assets_agriculture) > 0) {
    # Compute max available intensity key per (hazard_type, hazard_indicator, asset_category, asset_subtype)
    max_key_by_group_ag <- factors_tmp |>
      dplyr::filter(.data$asset_category == "agriculture") |>
      dplyr::group_by(.data$hazard_type, .data$hazard_indicator, .data$asset_category, .data$asset_subtype) |>
      dplyr::summarize(
        .__max_intensity_key__. = max(.data$.__intensity_key__., na.rm = TRUE),
        .groups = "drop"
      )

    # Join and cap effective intensity key
    assets_agriculture <- dplyr::left_join(
      assets_agriculture,
      max_key_by_group_ag,
      by = c("hazard_type", "hazard_indicator", "asset_category", "asset_subtype")
    ) |>
      dplyr::mutate(
        .__effective_intensity_key__. = dplyr::if_else(
          !is.na(.data$.__max_intensity_key__.) &
            .data$.__intensity_key__. > .data$.__max_intensity_key__.,
          .data$.__max_intensity_key__.,
          .data$.__intensity_key__.
        )
      )

    # Join on hazard_type, hazard_indicator, asset_category, asset_subtype, effective_intensity_key
    factors_key_ag <- factors_tmp |>
      dplyr::filter(.data$asset_category == "agriculture") |>
      dplyr::select("hazard_type", "hazard_indicator", "asset_category", "asset_subtype", ".__intensity_key__.",
                    "damage_factor", "cost_factor", "business_disruption")

    flood_merged_ag <- dplyr::left_join(
      assets_agriculture,
      factors_key_ag,
      by = dplyr::join_by(
        "hazard_type",
        "hazard_indicator",
        "asset_category",
        "asset_subtype",
        ".__effective_intensity_key__." == ".__intensity_key__."
      )
    ) |>
      dplyr::mutate(
        damage_factor = dplyr::coalesce(as.numeric(.data$damage_factor), NA_real_),
        cost_factor = dplyr::coalesce(as.numeric(.data$cost_factor), NA_real_),
        business_disruption = dplyr::coalesce(as.numeric(.data$business_disruption), NA_real_)
      ) |>
      dplyr::select(-dplyr::starts_with(".__"))
  }

  # Combine agriculture and non-agriculture results
  flood_merged <- dplyr::bind_rows(flood_merged_non_ag, flood_merged_ag)

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
