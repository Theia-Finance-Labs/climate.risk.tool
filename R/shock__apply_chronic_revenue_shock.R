#' Apply Chronic Shock to Yearly Revenue Trajectories (Placeholder)
#'
#' @title Apply Chronic Revenue Shock (Placeholder)
#' @description Placeholder function that passes through baseline revenue as shocked revenue.
#'   This is a temporary implementation that maintains the expected output structure
#'   while the actual shock calculation logic is being developed. NOTE: This function
#'   only affects REVENUE. Profit is computed separately using compute_profits_from_revenue().
#' @param yearly_trajectories tibble with columns: asset, company, year, revenue
#' @param assets_factors tibble with hazard data and damage factors (currently unused)
#' @param chronic_events tibble with chronic event information (currently unused)
#' @return tibble with columns: asset, company, year, shocked_revenue
#' @examples
#' \dontrun{
#' yearly_trajectories <- data.frame(
#'   asset = c("A1", "A1"),
#'   company = c("C1", "C1"),
#'   year = c(2025, 2030),
#'   revenue = c(1000, 1200),
#'   profit = c(100, 120)
#' )
#' assets_factors <- data.frame(asset = "A1", hazard_type = "temperature", damage_factor = 0.05)
#' chronic_events <- data.frame(event_id = "e1", hazard_type = "temperature", chronic = TRUE)
#' # Apply revenue shock (returns only shocked_revenue, not profit)
#' result <- apply_chronic_revenue_shock(yearly_trajectories, assets_factors, chronic_events)
#' }
#' @export
apply_chronic_revenue_shock <- function(
  yearly_trajectories,
  assets_factors,
  chronic_events
) {
  # Sort events by event_id to ensure consistent processing order
  chronic_events <- chronic_events |>
    dplyr::arrange(.data$event_id)

  # PLACEHOLDER IMPLEMENTATION:
  # For now, just pass through revenue values unchanged
  # This maintains the expected output structure while preserving the interface

  result <- yearly_trajectories |>
    # Return only the required columns (revenue only, not profit)
    dplyr::select("asset", "company", "year", "revenue")

  return(result)
}
