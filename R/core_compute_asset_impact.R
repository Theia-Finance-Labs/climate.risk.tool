#' Compute Asset Impact
#'
#' Calculates the final climate impact on assets by updating share of economic activity
#' and removing working columns. This is the final step before scenario building.
#'
#' @title Compute Asset Impact
#' @description Computes the total climate impact on each asset by combining damage factors,
#'   acute shocks, and chronic shocks. Updates the share_of_economic_activity to reflect
#'   reduced economic productivity due to climate impacts. Removes all working columns
#'   to prepare for scenario analysis.
#' @param assets_with_shocks data.frame. Asset data with damage factors, acute_shock, and chronic_shock columns
#' @return data.frame with updated share_of_economic_activity and working columns removed
#' @examples
#' \dontrun{
#' assets <- data.frame(company = "A", damage_factor = 0.2, acute_shock = 0.05, chronic_shock = 0.02)
#' result <- compute_asset_impact(assets)
#' }
#' @export
compute_asset_impact <- function(assets_with_shocks) {
  # Validate inputs
  if (!is.data.frame(assets_with_shocks)) {
    stop("assets_with_shocks must be a data.frame")
  }
  
  # Check for required columns
  required_cols <- c("share_of_economic_activity", "damage_factor", "acute_shock", "chronic_shock")
  missing_cols <- setdiff(required_cols, names(assets_with_shocks))
  if (length(missing_cols) > 0) {
    stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
  }
  
  # Make a copy to avoid modifying the input
  result <- assets_with_shocks
  
  # Store original share values for calculation
  original_share <- result$share_of_economic_activity
  
  # Compute total climate impact
  # Impact formula: combines damage factor, acute shock, and chronic shock
  # Total impact = damage_factor + acute_shock + chronic_shock
  # But ensure impacts don't compound unrealistically
  
  damage_impact <- pmax(0, pmin(1, result$damage_factor))  # Bound between 0 and 1
  acute_impact <- pmax(0, pmin(1, result$acute_shock))     # Bound between 0 and 1  
  chronic_impact <- pmax(0, pmin(1, result$chronic_shock)) # Bound between 0 and 1
  
  # Combine impacts using a formula that prevents over-amplification
  # Use multiplicative approach for more realistic compounding
  # New share = original * (1 - damage) * (1 - acute) * (1 - chronic)
  damage_retention <- 1 - damage_impact
  acute_retention <- 1 - acute_impact
  chronic_retention <- 1 - chronic_impact
  
  # Calculate new share of economic activity
  result$share_of_economic_activity <- original_share * 
                                      damage_retention * 
                                      acute_retention * 
                                      chronic_retention
  
  # Ensure shares remain within valid bounds [0, 1]
  result$share_of_economic_activity <- pmax(0, pmin(1, result$share_of_economic_activity))
  
  # Remove all working columns as specified in the contract
  # Define working columns to remove
  working_cols <- c(
    # All hazard columns
    grep("^hazard_", names(result), value = TRUE),
    # Specific working columns
    "damage_factor", "cost_factor", "acute_shock", "chronic_shock",
    "geometry", "centroid"
  )
  
  # Remove working columns that exist, but never remove essential columns
  essential_cols <- c("asset", "company", "share_of_economic_activity")
  existing_working_cols <- intersect(working_cols, names(result))
  # Ensure we never accidentally remove essential columns
  existing_working_cols <- setdiff(existing_working_cols, essential_cols)
  
  if (length(existing_working_cols) > 0) {
    result <- result[, !names(result) %in% existing_working_cols, drop = FALSE]
  }
  
  # Validate the result - ensure all essential columns are present
  missing_essential <- setdiff(essential_cols, names(result))
  if (length(missing_essential) > 0) {
    stop(paste("Essential columns were unexpectedly removed:", paste(missing_essential, collapse = ", ")))
  }
  
  if (!is.numeric(result$share_of_economic_activity)) {
    stop("share_of_economic_activity is not numeric")
  }
  
  if (any(is.na(result$share_of_economic_activity))) {
    stop("share_of_economic_activity contains NA values")
  }
  
  # Ensure all values are within valid range
  if (any(result$share_of_economic_activity < 0 | result$share_of_economic_activity > 1)) {
    stop("share_of_economic_activity values are outside valid range [0, 1]")
  }
  
  return(result)
}
