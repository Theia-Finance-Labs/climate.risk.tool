#' Compute Shock Yearly Trajectories for Assets
#'
#' @title Compute Shock Yearly Trajectories
#' @description Applies climate shocks to baseline yearly trajectories by splitting events
#'   into acute and chronic categories and applying them sequentially. Acute shocks are
#'   applied first, followed by chronic shocks to the resulting trajectories.
#' @param yearly_baseline_profits data.frame with columns: asset, company, year, baseline_revenue, baseline_profit
#' @param assets_with_factors data.frame with hazard data and damage/cost factors
#' @param events data.frame with columns: event_id, hazard_type, scenario, event_year (or NA), chronic (logical)
#' @param start_year numeric. Starting year for projections (default: 2025)
#' @return data.frame with columns: asset, company, year, shocked_revenue, shocked_profit
#' @examples
#' \dontrun{
#' yearly_baseline <- data.frame(
#'   asset = c("A1", "A1"),
#'   company = c("C1", "C1"),
#'   year = c(2025, 2030),
#'   baseline_revenue = c(1000, 1200),
#'   baseline_profit = c(100, 120)
#' )
#' assets_factors <- data.frame(
#'   asset_id = "A1", hazard_type = "flood", hazard_intensity = 5,
#'   damage_factor = 0.1, cost_factor = 1000
#' )
#' events <- data.frame(
#'   event_id = "ev1", hazard_type = "flood", scenario = "rcp85",
#'   event_year = 2030, chronic = FALSE
#' )
#' result <- compute_shock_trajectories(yearly_baseline, assets_factors, events)
#' }
#' @export
compute_shock_trajectories <- function(
    yearly_baseline_profits,
    assets_with_factors,
    events,
    start_year = 2025) {
  # Validate inputs
  if (!is.data.frame(yearly_baseline_profits) || nrow(yearly_baseline_profits) == 0) {
    stop("yearly_baseline_profits must be a non-empty data.frame")
  }
  if (!is.data.frame(assets_with_factors) || nrow(assets_with_factors) == 0) {
    stop("assets_with_factors must be a non-empty data.frame")
  }
  if (!is.data.frame(events) || nrow(events) == 0) {
    stop("events must be a non-empty data.frame")
  }

  required_baseline_cols <- c("asset", "company", "year", "baseline_revenue", "baseline_profit")
  if (!all(required_baseline_cols %in% names(yearly_baseline_profits))) {
    stop(
      "yearly_baseline_profits missing required columns: ",
      paste(setdiff(required_baseline_cols, names(yearly_baseline_profits)), collapse = ", ")
    )
  }

  required_event_cols <- c("event_id", "hazard_type", "hazard_name", "event_year", "chronic")
  if (!all(required_event_cols %in% names(events))) {
    stop(
      "events missing required columns: ",
      paste(setdiff(required_event_cols, names(events)), collapse = ", ")
    )
  }

  # Ensure hazard_name column exists in assets_with_factors
  if (!"hazard_name" %in% names(assets_with_factors)) {
    stop("assets_with_factors must contain 'hazard_name' column")
  }

  # Filter assets_with_factors to only the hazards referenced in events
  relevant_hazards <- unique(events$hazard_name)
  filtered_assets <- assets_with_factors[assets_with_factors$hazard_name %in% relevant_hazards, , drop = FALSE]

  if (nrow(filtered_assets) == 0) {
    stop("No matching hazard_name entries found in assets_with_factors for provided events")
  }

  # Split events into acute and chronic dataframes
  acute_events <- events[!isTRUE(events$chronic), ]
  chronic_events <- events[isTRUE(events$chronic), ]

  # Start with baseline trajectories
  current_trajectories <- yearly_baseline_profits

  # Apply acute shocks first
  if (nrow(acute_events) > 0) {
    current_trajectories <- apply_acute_shock_yearly(
      current_trajectories,
      filtered_assets,
      acute_events
    )
  }

  # Apply chronic shocks second
  if (nrow(chronic_events) > 0) {
    current_trajectories <- apply_chronic_shock_yearly(
      current_trajectories,
      filtered_assets,
      chronic_events
    )
  }

  # Return final trajectories with shocked values
  return(current_trajectories)
}
