
#' Join damage and cost factors based on hazard type, intensity and asset category (internal function)
#'
#' @param assets_with_hazards Data frame in long format with asset and hazard information
#'   including hazard_type, hazard_intensity columns (from extract_hazard_statistics)
#' @param damage_factors_df Data frame with damage and cost factors lookup table
#' @return Data frame with original columns plus damage_factor and cost_factor columns
#' @noRd
join_damage_cost_factors <- function(assets_with_hazards, damage_factors_df) {
  assets_tmp <- assets_with_hazards
  assets_tmp$.__intensity_key__. <- as.integer(round(as.numeric(assets_tmp$hazard_intensity)))

  factors_tmp <- damage_factors_df
  factors_tmp$.__intensity_key__. <- as.integer(round(as.numeric(factors_tmp$hazard_intensity)))

  # Compute max available intensity key per (hazard_type, asset_category)
  max_key_by_group <- stats::aggregate(
    .__intensity_key__. ~ hazard_type + asset_category,
    data = factors_tmp,
    FUN = max
  )
  names(max_key_by_group)[names(max_key_by_group) == ".__intensity_key__."] <- ".__max_intensity_key__."

  # Attach group max to assets and cap effective key to the group's max
  assets_tmp <- merge(
    assets_tmp,
    max_key_by_group,
    by = c("hazard_type", "asset_category"),
    all.x = TRUE,
    sort = FALSE
  )

  # Effective key with capping
  assets_tmp$.__effective_intensity_key__. <- ifelse(
    !is.na(assets_tmp$.__max_intensity_key__.) &
      assets_tmp$.__intensity_key__. > assets_tmp$.__max_intensity_key__.,
    assets_tmp$.__max_intensity_key__.,
    assets_tmp$.__intensity_key__.
  )

  factors_key_cols <- c("hazard_type", "asset_category", ".__intensity_key__.",
                        "damage_factor", "cost_factor", "business_disruption")
  factors_key <- factors_tmp[, factors_key_cols, drop = FALSE]

  merged <- merge(
    assets_tmp,
    factors_key,
    by.x = c("hazard_type", "asset_category", ".__effective_intensity_key__."),
    by.y = c("hazard_type", "asset_category", ".__intensity_key__."),
    all.x = TRUE,
    sort = FALSE
  )

  if (!"damage_factor" %in% names(merged)) merged$damage_factor <- NA_real_
  if (!"cost_factor" %in% names(merged)) merged$cost_factor <- NA_real_
  if (!"business_disruption" %in% names(merged)) merged$business_disruption <- NA_real_

  merged$damage_factor <- as.numeric(merged$damage_factor)
  merged$cost_factor <- as.numeric(merged$cost_factor)
  merged$business_disruption <- as.numeric(merged$business_disruption)

  # Clean helper columns
  merged$.__intensity_key__. <- NULL
  merged$.__max_intensity_key__. <- NULL
  merged$.__effective_intensity_key__. <- NULL

  return(merged)
}
