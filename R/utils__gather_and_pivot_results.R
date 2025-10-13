#' Gather and Pivot Results
#'
#' Transforms scenario-based results into wide format for reporting.
#' Creates separate pivoted tables for assets and companies.
#'
#' @title Gather and Pivot Results
#' @description Pivots long-format scenario data into wide format suitable for reporting.
#'   Creates separate tables for assets (with NPV columns) and companies
#'   (with NPV, PD, and Expected Loss columns) by scenario. Also calculates percentage
#'   changes from baseline to shock for NPV and Expected Loss.
#' @param companies_expected_loss tibble. Company data with NPV, PD, and expected_loss by scenario
#' @return tibble with pivoted company data including NPV_change_pct and Expected_loss_change_pct
#' @examples
#' \dontrun{
#' companies <- data.frame(
#'   company = c("A", "A"),
#'   scenario = c("baseline", "shock"),
#'   npv = c(1000, 950),
#'   merton_pd = c(0.1, 0.15),
#'   expected_loss = c(100, 120)
#' )
#' result <- gather_and_pivot_results(companies)
#' # Result includes NPV_change_pct = -5% and Expected_loss_change_pct = 20%
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
      dplyr::select("company", "npv", "merton_pd", "expected_loss") |>
      dplyr::rename(
        NPV_baseline = "npv",
        PD_baseline = "merton_pd",
        Expected_loss_baseline = "expected_loss"
      )

    # Extract shock values
    companies_shock_vals <- companies_shock |>
      dplyr::select("company", "npv", "merton_pd", "expected_loss") |>
      dplyr::rename(
        NPV_shock = "npv",
        PD_shock = "merton_pd",
        Expected_loss_shock = "expected_loss"
      )

    # Join baseline and shock data
    companies_pivoted <- dplyr::full_join(companies_baseline_vals, companies_shock_vals, by = "company")
    
    # Calculate percentage changes
    companies_pivoted <- companies_pivoted |>
      dplyr::mutate(
        NPV_change_pct = dplyr::if_else(
          .data$NPV_baseline == 0 | is.na(.data$NPV_baseline),
          NA_real_,
          (.data$NPV_shock - .data$NPV_baseline) / .data$NPV_baseline * 100
        ),
        Expected_loss_change_pct = dplyr::if_else(
          .data$Expected_loss_baseline == 0 | is.na(.data$Expected_loss_baseline),
          NA_real_,
          (.data$Expected_loss_shock - .data$Expected_loss_baseline) / .data$Expected_loss_baseline * 100
        )
      )
  } else {
    # Handle case where one scenario is missing
    companies_pivoted <- tibble::tibble(
      company = character(0),
      NPV_baseline = numeric(0),
      NPV_shock = numeric(0),
      PD_baseline = numeric(0),
      PD_shock = numeric(0),
      Expected_loss_baseline = numeric(0),
      Expected_loss_shock = numeric(0),
      NPV_change_pct = numeric(0),
      Expected_loss_change_pct = numeric(0)
    )
  }


  return(companies_pivoted)
}
