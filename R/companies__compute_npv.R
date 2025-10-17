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
