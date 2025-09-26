#' Join damage and cost factors based on hazard type, intensity and asset category
#'
#' @title Map hazard intensity to damage and cost factors
#' @description Joins assets with damage and cost factors from a lookup table based on
#'   hazard_type, rounded hazard intensity values and asset category. Hazard intensity 
#'   is rounded to the nearest integer for matching. Expects long format input with
#'   hazard_type, hazard_intensity columns.
#' @param assets_with_hazards Data frame in long format with asset and hazard information
#'   including hazard_type, hazard_intensity columns (from summarize_hazards)
#' @param damage_factors_df Data frame with damage and cost factors lookup table
#' @return Data frame with original columns plus damage_factor and cost_factor columns
#' @examples
#' \dontrun{
#' base_dir <- system.file("tests_data", package = "climate.risk.tool")
#' assets <- read_inputs(base_dir)$assets
#' hazards <- load_hazards(file.path(base_dir, "hazards"))
#' assets_geo <- geolocate_assets(assets, hazards, base_dir)
#' assets_hazards <- cutout_hazards(assets_geo, hazards)
#' assets_long <- summarize_hazards(assets_hazards)
#' damage_factors <- read_damage_cost_factors(base_dir)
#' assets_with_factors <- join_damage_cost_factors(assets_long, damage_factors)
#' }
#' @export
join_damage_cost_factors <- function(assets_with_hazards, damage_factors_df) {
  message("🔗 [join_damage_cost_factors] Starting damage and cost factor joining for ", nrow(assets_with_hazards), " asset-hazard combinations...")

  if (!is.data.frame(assets_with_hazards)) {
    stop("assets_with_hazards must be a data.frame")
  }

  if (!is.data.frame(damage_factors_df)) {
    stop("damage_factors_df must be a data.frame")
  }

  # Check required columns in assets_with_hazards
  required_asset_cols <- c("asset", "asset_category", "hazard_type", "hazard_intensity")
  missing_asset_cols <- setdiff(required_asset_cols, names(assets_with_hazards))
  if (length(missing_asset_cols) > 0) {
    stop("Missing required columns in assets_with_hazards: ", paste(missing_asset_cols, collapse = ", "))
  }

  # Check required columns in damage_factors_df
  required_factor_cols <- c("hazard_type", "hazard_intensity", "asset_category", "damage_factor", "cost_factor")
  missing_factor_cols <- setdiff(required_factor_cols, names(damage_factors_df))
  if (length(missing_factor_cols) > 0) {
    stop("Missing required columns in damage_factors_df: ", paste(missing_factor_cols, collapse = ", "))
  }

  # Round hazard intensity to nearest integer for matching
  assets_with_hazards$hazard_intensity_rounded <- round(assets_with_hazards$hazard_intensity)
  damage_factors_df$hazard_intensity_rounded <- round(damage_factors_df$hazard_intensity)

  # Perform the join on hazard_type, hazard_intensity_rounded, and asset_category
  result <- dplyr::left_join(
    assets_with_hazards,
    damage_factors_df[, c("hazard_type", "hazard_intensity_rounded", "asset_category", "damage_factor", "cost_factor")],
    by = c("hazard_type", "hazard_intensity_rounded", "asset_category")
  )

  # Clean up temporary column
  result$hazard_intensity_rounded <- NULL

  # Count successful matches
  matches_found <- sum(!is.na(result$damage_factor))
  total_rows <- nrow(result)

  message("✅ [join_damage_cost_factors] Factor joining completed for ", total_rows, " asset-hazard combinations (", matches_found, " matches found)")

  return(result)
}
