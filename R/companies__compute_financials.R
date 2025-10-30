#' Compute company-level financial results from yearly trajectories
#'
#' @title Compute Company Financials
#' @description Takes yearly company trajectories and computes final company-level
#'   NPV, PD, and Expected Loss metrics. Aggregates yearly data and applies risk models.
#'   This is the main function for company-level financial analysis.
#' @param companies tibble with company data including NPV and debt information
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


#' Compute Company NPV from Yearly Trajectories
#'
#' Aggregates yearly company trajectories to company-level NPV by scenario.
#' Computes Net Present Value (NPV) for each company in each scenario by summing yearly discounted profits.
#'
#' @title Compute Company NPV from Yearly Data
#' @description Aggregates yearly discounted net profits from company trajectories to compute company-level NPV.
#'   Groups by company and scenario, summing total_discounted_net_profit values across years.
#' @param company_yearly_data tibble. Company yearly trajectories with total_discounted_net_profit column
#' @return tibble with columns: company, scenario, npv
#' @examples
#' \dontrun{
#' company_yearly <- data.frame(
#'   company = c("C1", "C1"),
#'   year = c(2025, 2026),
#'   scenario = c("baseline", "baseline"),
#'   total_discounted_net_profit = c(100, 97)
#' )
#' result <- compute_company_npv(company_yearly)
#' }
#' @export
compute_company_npv <- function(company_yearly_data) {
  # Aggregate discounted net profits by company and scenario
  # Sum across years for each company-scenario combination
  company_npv_data <- company_yearly_data |>
    dplyr::group_by(.data$company, .data$scenario) |>
    dplyr::summarize(
      npv = sum(.data$total_discounted_net_profit, na.rm = TRUE),
      .groups = "drop"
    )

  # Ensure proper column types and format
  scenario_col <- company_yearly_data |> dplyr::pull(.data$scenario)
  company_npv_data <- company_npv_data |>
    dplyr::mutate(
      company = as.character(.data$company),
      scenario = {
        if (is.factor(scenario_col)) {
          if (is.ordered(scenario_col)) {
            factor(.data$scenario,
              levels = levels(scenario_col),
              ordered = TRUE
            )
          } else {
            factor(.data$scenario,
              levels = levels(scenario_col)
            )
          }
        } else {
          .data$scenario
        }
      }
    ) |>
    dplyr::arrange(.data$company, .data$scenario)

  return(company_npv_data)
}


#' Compute Company Probability of Default using Merton Model
#'
#' Calculates probability of default for each company using the Merton model.
#' Uses company NPV, debt, and volatility data.
#'
#' @title Compute Company PD (Merton)
#' @description Computes probability of default using Merton model approach.
#'   Uses company NPV as proxy for asset value, along with debt and volatility.
#'   Returns PD values in the range 0 to 1.
#' @param companies_npv tibble. Company NPV data with company, scenario, npv columns
#' @return tibble with all original columns plus 'merton_pd' column
#' @examples
#' \dontrun{
#' companies <- data.frame(company = "A", scenario = "baseline", npv = 1000)
#' result <- compute_pd_merton(companies)
#' }
#' @importFrom stats pnorm
#' @export
compute_pd_merton <- function(companies_npv) {
  # Make a copy to avoid modifying the input
  result_with_financials <- companies_npv

  # Use NPV as part of asset value proxy: V = debt + npv
  result <- result_with_financials |>
    dplyr::mutate(
      V = pmax(.data$debt + .data$npv, 1), # avoid non-positive
      D = pmax(.data$debt, 1), # avoid non-positive
      sigma = pmax(.data$volatility, 1e-8), # avoid zero
      T = pmax(.data$term, 1), # default Term = 1
      r = 0.02, # Risk-free rate hard-coded at 2%
      # Excel-equivalent:
      # d1 = ( ln(V/D) + (r + 0.5*sigma^2)*T ) / (sigma*sqrt(T))
      # PD = N( -(d1 - sigma*sqrt(T)) )
      d1 = (log(.data$V / .data$D) + (.data$r + 0.5 * .data$sigma^2) * .data$T) / (.data$sigma * sqrt(.data$T)),
      merton_pd = pnorm(-(.data$d1 - .data$sigma * sqrt(.data$T))),
      # Ensure PD is in [0, 1] range
      merton_pd = pmax(0, pmin(1, .data$merton_pd))
    ) |>
    dplyr::select(-"V", -"D", -"sigma", -"T", -"r", -"d1")

  return(result)
}

#' Compute Expected Loss
#'
#' Calculates expected loss for each company using the formula: EL = LGD * Loan_Size * PD.
#' Uses company-specific LGD and loan size data along with computed Merton PD.
#'
#' @title Compute Expected Loss
#' @description Computes expected loss using the standard credit risk formula:
#'   Expected Loss = Loss Given Default * Loan Size * Probability of Default
#'   Joins company PD data with LGD and loan size information.
#' @param companies_pd tibble. Company PD data with company, scenario, merton_pd columns
#' @return tibble with all original columns plus 'expected_loss' column
#' @examples
#' \dontrun{
#' companies <- data.frame(company = "A", scenario = "baseline", merton_pd = 0.1)
#' result <- compute_expected_loss(companies)
#' }
#' @export
compute_expected_loss <- function(companies_pd) {
  # Make a copy to avoid modifying the input
  result <- companies_pd

  # Calculate Expected Loss: EL = LGD * Loan_Size * PD
  # lgd and loan_size should already be in the input dataframe
  result <- result |>
    dplyr::mutate(expected_loss = .data$lgd * .data$loan_size * .data$merton_pd)


  return(result)
}
