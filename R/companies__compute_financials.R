#' Compute company-level financial results from yearly trajectories
#'
#' @title Compute Company Financials
#' @description Takes yearly company trajectories and computes final company-level
#'   NPV, PD, and Expected Loss metrics. Aggregates yearly data and applies risk models.
#'   This is the main function for company-level financial analysis.
#' @param company_yearly_trajectories tibble with columns: company, year, scenario, total_revenue, total_profit, total_discounted_profit, total_discounted_net_profit
#' @param assets_discounted_yearly tibble with yearly asset data for asset-level results
#' @param discount_rate numeric. Discount rate (used for consistency, but discounting already applied)
#' @return tibble with company financial results
#' @examples
#' \dontrun{
#' company_yearly <- data.frame(
#'   company = c("C1", "C1"),
#'   year = c(2025, 2026),
#'   scenario = c("baseline", "baseline"),
#'   total_revenue = c(1000, 1020),
#'   total_profit = c(100, 102),
#'   total_discounted_profit = c(100, 97.14),
#'   total_discounted_net_profit = c(100, 97.14)
#' )
#' assets_yearly <- data.frame(
#'   asset = c("A1", "A1"),
#'   company = c("C1", "C1"),
#'   year = c(2025, 2026),
#'   scenario = c("baseline", "baseline"),
#'   revenue = c(1000, 1020),
#'   profit = c(100, 102),
#'   discounted_profit = c(100, 97.14),
#'   discounted_net_profit = c(100, 97.14)
#' )
#' result <- compute_companies_financials(company_yearly, assets_yearly)
#' }
#' @export
compute_companies_financials <- function(
  companies,
  company_yearly_trajectories,
  assets_discounted_yearly,
  discount_rate = 0.05
) {
  # Step 1: Compute company NPV from yearly trajectories
  companies_npv <- compute_company_npv(company_yearly_trajectories)
  companies_with_financial_data <- dplyr::left_join(companies, companies_npv, by = "company")

  # Step 3: Apply company-level risk models
  companies_pd <- compute_pd_merton(companies_with_financial_data)
  companies_el <- compute_expected_loss(companies_pd)

  # Step 4: Format final results
  companies_results <- gather_and_pivot_results(companies_el)

  return(companies_results)
}
