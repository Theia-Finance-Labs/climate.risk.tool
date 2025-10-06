#' Join damage and cost factors based on hazard type, intensity and asset category (internal function)
#'
#' @param assets_with_hazards Data frame in long format with asset and hazard information
#'   including hazard_type, hazard_intensity columns (from extract_hazard_statistics)
#' @param damage_factors_df Data frame with damage and cost factors lookup table
#' @return Data frame with original columns plus damage_factor and cost_factor columns
#' @noRd
join_damage_cost_factors <- function(assets_with_hazards, damage_factors_df) {
  assets_tmp <- assets_with_hazards |>
    dplyr::mutate(.__intensity_key__. = as.integer(round(as.numeric(.data$hazard_intensity))))

  factors_tmp <- damage_factors_df |>
    dplyr::mutate(.__intensity_key__. = as.integer(round(as.numeric(.data$hazard_intensity))))

  # Compute max available intensity key per (hazard_type, asset_category)
  max_key_by_group <- factors_tmp |>
    dplyr::group_by(.data$hazard_type, .data$asset_category) |>
    dplyr::summarize(
      .__max_intensity_key__. = max(.data$.__intensity_key__., na.rm = TRUE),
      .groups = "drop"
    )

  # Attach group max to assets and cap effective key to the group's max
  assets_tmp <- dplyr::left_join(
    assets_tmp,
    max_key_by_group,
    by = c("hazard_type", "asset_category")
  )

  # Effective key with capping
  assets_tmp <- assets_tmp |>
    dplyr::mutate(
      .__effective_intensity_key__. = dplyr::if_else(
        !is.na(.data$.__max_intensity_key__.) &
          .data$.__intensity_key__. > .data$.__max_intensity_key__.,
        .data$.__max_intensity_key__.,
        .data$.__intensity_key__.
      )
    )

  factors_key_cols <- c(
    "hazard_type", "asset_category", ".__intensity_key__.",
    "damage_factor", "cost_factor", "business_disruption"
  )
  factors_key <- factors_tmp |>
    dplyr::select(dplyr::all_of(factors_key_cols))

  merged <- dplyr::left_join(
    assets_tmp,
    factors_key,
    by = dplyr::join_by(
      "hazard_type",
      "asset_category",
      ".__effective_intensity_key__." == ".__intensity_key__."
    )
  )

  merged <- merged |>
    dplyr::mutate(
      damage_factor = dplyr::coalesce(as.numeric(.data$damage_factor), NA_real_),
      cost_factor = dplyr::coalesce(as.numeric(.data$cost_factor), NA_real_),
      business_disruption = dplyr::coalesce(as.numeric(.data$business_disruption), NA_real_)
    ) |>
    dplyr::select(
      -dplyr::starts_with(".__")
    )

  return(merged)
}
