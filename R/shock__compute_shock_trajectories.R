#' Compute Shock Yearly Trajectories and Concatenate with Baseline
#'
#' @title Compute Shock Yearly Trajectories
#' @description Applies climate shocks to baseline yearly trajectories through a clear sequence:
#'   1. Apply acute revenue shocks
#'   2. Apply chronic revenue shocks
#'   3. Compute profits from shocked revenue
#'   4. Apply acute profit shocks
#'   5. Concatenate with baseline scenarios
#'   Returns a single dataframe with both baseline and shock scenarios ready for downstream analysis.
#' @param yearly_baseline_profits data.frame with columns: asset, company, year, revenue, profit
#' @param assets_with_factors data.frame with hazard data and damage/cost factors
#' @param events data.frame with columns: event_id, hazard_type, hazard_name, event_year (or NA), chronic (logical)
#' @param net_profit_margin numeric. Net profit margin for computing profits from shocked revenue (default: 0.1)
#' @param start_year numeric. Starting year for projections (default: 2025)
#' @return data.frame with columns: asset, company, year, scenario, revenue, profit
#'   where scenario is either "baseline" or "shock"
#' @examples
#' \dontrun{
#' yearly_baseline <- data.frame(
#'   asset = c("A1", "A1"),
#'   company = c("C1", "C1"),
#'   year = c(2025, 2030),
#'   revenue = c(1000, 1200),
#'   profit = c(100, 120)
#' )
#' assets_factors <- data.frame(
#'   asset = "A1", hazard_name = "flood__global_rcp85_h100glob_brazil",
#'   damage_factor = 0.1, cost_factor = 1000
#' )
#' events <- data.frame(
#'   event_id = "ev1", hazard_type = "flood",
#'   hazard_name = "flood__global_rcp85_h100glob_brazil",
#'   event_year = 2030, chronic = FALSE
#' )
#' result <- compute_shock_trajectories(yearly_baseline, assets_factors, events)
#' # result contains both baseline and shock scenarios
#' }
#' @export
compute_shock_trajectories <- function(
    yearly_baseline_profits,
    assets_with_factors,
    events,
    net_profit_margin = 0.1,
    start_year = 2025) {
  # Filter assets_with_factors to only the hazards referenced in events
  relevant_hazards <- unique(events$hazard_name)
  filtered_assets <- assets_with_factors[assets_with_factors$hazard_name %in% relevant_hazards, , drop = FALSE]

  if (nrow(filtered_assets) == 0) {
    stop("No matching hazard_name entries found in assets_with_factors for provided events")
  }

  # Split events into acute and chronic dataframes
  acute_events <- events[!isTRUE(events$chronic), ]
  chronic_events <- events[isTRUE(events$chronic), ]

  # ============================================================================
  # SHOCK SEQUENCE: Apply shocks in the correct order
  # ============================================================================

  # Start with baseline trajectories (keeping only revenue for shock application)
  current_trajectories <- yearly_baseline_profits[, c("asset", "company", "year", "revenue")]

  # STEP 1: Apply acute revenue shocks
  if (nrow(acute_events) > 0) {
    current_trajectories <- apply_acute_revenue_shock(
      current_trajectories,
      filtered_assets,
      acute_events
    )
  } else {
    # No acute events, just convert revenue to shocked_revenue
    current_trajectories <- current_trajectories[, c("asset", "company", "year", "revenue")]
  }

  # STEP 2: Apply chronic revenue shocks
  if (nrow(chronic_events) > 0) {
    current_trajectories <- apply_chronic_revenue_shock(
      current_trajectories,
      filtered_assets,
      chronic_events
    )
  }
  # At this point: current_trajectories has columns: asset, company, year, revenue

  # STEP 3: Compute profits from shocked revenue
  current_trajectories <- compute_profits_from_revenue(
    current_trajectories,
    net_profit_margin = net_profit_margin
  )
  # At this point: current_trajectories has columns: asset, company, year, revenue, profit

  # STEP 4: Apply acute profit shocks
  if (nrow(acute_events) > 0) {
    current_trajectories <- apply_acute_profit_shock(
      current_trajectories,
      filtered_assets,
      acute_events
    )
  }

  return(current_trajectories)
}
