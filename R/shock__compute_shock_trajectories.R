#' Compute Shock Yearly Trajectories and Concatenate with Baseline
#'
#' @title Compute Shock Yearly Trajectories
#' @description Applies climate shocks to baseline yearly trajectories through a clear sequence:
#'   1. Apply acute revenue shocks
#'   2. Compute profits from shocked revenue
#'   3. Apply acute profit shocks
#'   Returns a single dataframe with shock scenarios ready for downstream analysis.
#' @param yearly_baseline_profits tibble with columns: asset, company, year, revenue, profit
#' @param assets_with_factors tibble with hazard data and damage/cost factors
#' @param events tibble with columns: event_id, hazard_type, hazard_name, scenario_name, hazard_return_period, event_year (or NA)
#' @param net_profit_margin numeric. Net profit margin for computing profits from shocked revenue (default: 0.1)
#' @param start_year numeric. Starting year for projections (default: 2025)
#' @return tibble with columns: asset, company, year, revenue, profit
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
#'   event_year = 2030
#' )
#' result <- compute_shock_trajectories(yearly_baseline, assets_factors, events)
#' }
#' @export
compute_shock_trajectories <- function(
  yearly_baseline_profits,
  assets_with_factors,
  events,
  net_profit_margin = 0.1,
  start_year = 2025
) {
  # Filter assets_with_factors to only the events referenced
  # Use event_id for matching (more reliable than hazard_name, especially for multi-indicator hazards like Fire)
  relevant_event_ids <- events |>
    dplyr::distinct(.data$event_id) |>
    dplyr::pull(.data$event_id)

  filtered_assets <- assets_with_factors |>
    dplyr::filter(.data$event_id %in% relevant_event_ids)

  if (nrow(filtered_assets) == 0) {
    stop("No matching event_id entries found in assets_with_factors for provided events")
  }

  # ============================================================================
  # SHOCK SEQUENCE: Apply shocks in the correct order
  # ============================================================================

  # Start with baseline trajectories (keeping only revenue for shock application)
  current_trajectories <- yearly_baseline_profits |>
    dplyr::select("asset", "company", "year", "revenue")

  # STEP 1: Apply acute revenue shocks
  if (nrow(events) > 0) {
    current_trajectories <- apply_acute_revenue_shock(
      current_trajectories,
      filtered_assets,
      events
    )
  } else {
    # No events, just keep the revenue columns
    current_trajectories <- current_trajectories |>
      dplyr::select("asset", "company", "year", "revenue")
  }

  # STEP 2: Compute profits from shocked revenue
  current_trajectories <- compute_profits_from_revenue(
    current_trajectories,
    net_profit_margin = net_profit_margin
  )
  # At this point: current_trajectories has columns: asset, company, year, revenue, profit

  # STEP 3: Apply acute profit shocks
  if (nrow(events) > 0) {
    current_trajectories <- apply_acute_profit_shock(
      current_trajectories,
      filtered_assets,
      events
    )
  }

  return(current_trajectories)
}
