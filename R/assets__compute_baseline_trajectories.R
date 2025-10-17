#' Compute Baseline Yearly Trajectories for Assets
#'
#' @title Compute Baseline Yearly Trajectories
#' @description Computes baseline yearly revenue and profit trajectories for assets
#'   from baseline assets and company data. This is the first step in the yearly
#'   trajectory approach, creating baseline projections before shock application.
#' @param baseline_assets tibble with columns: asset, company, share_of_economic_activity
#' @param companies tibble with columns: company, revenues
#' @param growth_rate numeric. Annual growth rate for revenue trajectories (default: 0.02)
#' @param net_profit_margin numeric. Net profit margin to apply (default: 0.1)
#' @param start_year numeric. Starting year for projections (default: 2025)
#' @param end_year numeric. Ending year for projections (default: 2050)
#' @return tibble with columns: asset, company, year, revenue, profit
#' @examples
#' \dontrun{
#' baseline_assets <- data.frame(
#'   asset = c("A1", "A2"),
#'   company = c("C1", "C1"),
#'   share_of_economic_activity = c(0.6, 0.4)
#' )
#' companies <- data.frame(company = "C1", revenues = 1000)
#' result <- compute_baseline_trajectories(baseline_assets, companies)
#' }
#' @export
compute_baseline_trajectories <- function(
  baseline_assets, # data.frame with columns: asset, company, share_of_economic_activity
  companies, # data.frame with columns: company, revenues
  growth_rate = 0.02,
  net_profit_margin = 0.1,
  start_year = 2025,
  end_year = 2050
) {
  # Step 1: Compute yearly revenue trajectories
  yearly_revenue <- compute_yearly_baseline_revenue(
    baseline_assets, companies, growth_rate, start_year, end_year
  )

  # Step 2: Compute profit from revenue using scenario-agnostic function
  yearly_trajectories <- compute_profits_from_revenue(
    yearly_revenue,
    net_profit_margin = net_profit_margin
  )

  return(yearly_trajectories)
}
