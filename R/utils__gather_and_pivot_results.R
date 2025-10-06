#' Gather and Pivot Results
#'
#' Transforms scenario-based results into wide format for reporting.
#' Creates separate pivoted tables for assets and companies.
#'
#' @title Gather and Pivot Results
#' @description Pivots long-format scenario data into wide format suitable for reporting.
#'   Creates separate tables for assets (with NPV columns) and companies
#'   (with NPV, PD, and Expected Loss columns) by scenario.
#' @param assets_discounted tibble. Asset data with discounted_net_profit by scenario
#' @param companies_expected_loss tibble. Company data with NPV, PD, and expected_loss by scenario
#' @return tibble with pivoted company data
#' @examples
#' \dontrun{
#' assets <- data.frame(company = "A", scenario = "baseline", discounted_net_profit = 1000)
#' companies <- data.frame(
#'   company = "A", scenario = "baseline", npv = 1000,
#'   merton_pd = 0.1, expected_loss = 100
#' )
#' result <- gather_and_pivot_results(assets, companies)
#' }
#' @export
gather_and_pivot_results <- function(companies_expected_loss) {
  # Pivot companies data
  companies_baseline <- companies_expected_loss |>
    dplyr::filter(.data$scenario == "baseline")
  companies_shock <- companies_expected_loss |>
    dplyr::filter(.data$scenario == "shock")

  if (nrow(companies_baseline) > 0 && nrow(companies_shock) > 0) {
    # Extract baseline values
    companies_baseline_vals <- companies_baseline |>
      dplyr::select(.data$company, .data$npv, .data$merton_pd, .data$expected_loss) |>
      dplyr::rename(
        NPV_baseline = .data$npv,
        PD_baseline = .data$merton_pd,
        Expected_loss_baseline = .data$expected_loss
      )

    # Extract shock values
    companies_shock_vals <- companies_shock |>
      dplyr::select(.data$company, .data$npv, .data$merton_pd, .data$expected_loss) |>
      dplyr::rename(
        NPV_shock = .data$npv,
        PD_shock = .data$merton_pd,
        Expected_loss_shock = .data$expected_loss
      )

    # Join baseline and shock data
    companies_pivoted <- dplyr::full_join(companies_baseline_vals, companies_shock_vals, by = "company")
  } else {
    # Handle case where one scenario is missing
    companies_pivoted <- tibble::tibble(
      company = character(0),
      NPV_baseline = numeric(0),
      NPV_shock = numeric(0),
      PD_baseline = numeric(0),
      PD_shock = numeric(0),
      Expected_loss_baseline = numeric(0),
      Expected_loss_shock = numeric(0)
    )
  }


  return(companies_pivoted)
}
