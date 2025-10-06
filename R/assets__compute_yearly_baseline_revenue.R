#' Compute Yearly Baseline Revenue Trajectories for Assets
#'
#' @title Compute Yearly Baseline Revenue Trajectories
#' @description Computes yearly baseline revenue trajectories for assets from 2025 onwards.
#'   Starting with 2025 revenues = company_revenue * share_of_economic_activity,
#'   then applying growth rate for subsequent years: 2026 = 2025 * (1 + growth_rate), etc.
#' @param baseline_assets data.frame with columns: asset, company, share_of_economic_activity
#' @param companies data.frame with columns: company, revenues
#' @param growth_rate numeric. Annual growth rate (default: 0.02)
#' @param start_year numeric. Starting year for projections (default: 2025)
#' @param end_year numeric. Ending year for projections (default: 2050)
#' @return data.frame with columns: asset, company, year, revenue
#' @examples
#' \dontrun{
#' baseline_assets <- data.frame(
#'   asset = c("A1", "A2"),
#'   company = c("C1", "C1"),
#'   share_of_economic_activity = c(0.6, 0.4)
#' )
#' companies <- data.frame(
#'   company = "C1",
#'   revenues = 1000
#' )
#' result <- compute_yearly_baseline_revenue(baseline_assets, companies, 0.02)
#' }
#' @export
compute_yearly_baseline_revenue <- function(
    baseline_assets,
    companies,
    growth_rate = 0.02,
    start_year = 2025,
    end_year = 2050) {
  # Join assets with company data
  assets_with_companies <- dplyr::left_join(baseline_assets, companies, by = "company")

  # Check for any failed joins
  if (any(is.na(assets_with_companies$revenues))) {
    stop("Join failed: some assets could not be matched to companies")
  }

  # Calculate 2025 baseline revenue for each asset
  # Formula: company_revenue * share_of_economic_activity
  assets_with_companies <- assets_with_companies |>
    dplyr::mutate(revenue_2025 = .data$revenues * .data$share_of_economic_activity)

  # Generate yearly trajectories
  years <- start_year:end_year

  # Create expanded data frame with one row per asset-year combination
  # First expand each asset to have one row per year
  result <- assets_with_companies |>
    dplyr::mutate(year = list(years)) |>
    tidyr::unnest("year") |>
    # Apply growth rate: revenue_year = revenue_2025 * (1 + growth_rate)^(year - 2025)
    dplyr::mutate(revenue = .data$revenue_2025 * (1 + growth_rate)^(.data$year - start_year)) |>
    dplyr::select("asset", "company", "year", "revenue")


  return(result)
}
