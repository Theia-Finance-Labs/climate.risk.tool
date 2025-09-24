#' Build Scenarios from Baseline and Shock Data
#'
#' Concatenates baseline and shock datasets into a long format with scenario labels.
#' Creates an ordered factor for scenarios with baseline < shock ordering.
#'
#' @title Build Scenarios
#' @description Combines baseline and shock asset data into a single dataset with 
#'   scenario labels. Preserves all original columns and adds a 'scenario' column 
#'   as an ordered factor.
#' @param baseline_assets data.frame. Baseline asset data
#' @param shock_assets data.frame. Shocked asset data (same structure as baseline)
#' @return data.frame with all original columns plus 'scenario' column (ordered factor)
#' @examples
#' \dontrun{
#' baseline <- data.frame(company = "A", asset = "A1", value = 100)
#' shock <- data.frame(company = "A", asset = "A1", value = 90)
#' scenarios <- build_scenarios(baseline, shock)
#' }
#' @export
build_scenarios <- function(baseline_assets, shock_assets) {
  # Validate inputs
  if (!is.data.frame(baseline_assets) || !is.data.frame(shock_assets)) {
    stop("Both baseline_assets and shock_assets must be data.frames")
  }
  
  # Check that both datasets have the same columns
  baseline_cols <- names(baseline_assets)
  shock_cols <- names(shock_assets)
  
  if (!identical(sort(baseline_cols), sort(shock_cols))) {
    stop("baseline_assets and shock_assets must have identical column names")
  }
  
  # Ensure column order matches
  shock_assets <- shock_assets[, baseline_cols, drop = FALSE]
  
  # Add scenario labels
  baseline_with_scenario <- baseline_assets
  baseline_with_scenario$scenario <- "baseline"
  
  shock_with_scenario <- shock_assets
  shock_with_scenario$scenario <- "shock"
  
  # Combine datasets
  combined <- rbind(baseline_with_scenario, shock_with_scenario)
  
  # Convert scenario to ordered factor with baseline < shock
  combined$scenario <- factor(combined$scenario, 
                             levels = c("baseline", "shock"), 
                             ordered = TRUE)
  
  # Ensure all columns follow snake_case convention (scenario already does)
  # Note: We don't modify existing column names as they come from upstream functions
  # The snake_case requirement is enforced by the upstream data processing pipeline
  
  return(combined)
}
