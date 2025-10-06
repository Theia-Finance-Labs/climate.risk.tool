#' Compute Company-Level Yearly Trajectories
#'
#' @title Compute Company-Level Yearly Trajectories
#' @description Aggregates asset-level yearly trajectories to company-level yearly trajectories.
#'   This provides yearly revenue and profit trajectories at the company level for detailed analysis.
#' @param yearly_discounted_df tibble with columns: asset, company, year, scenario, revenue, profit, discounted_profit, discounted_net_profit
#' @return tibble with columns: company, year, scenario, total_revenue, total_profit, total_discounted_profit, total_discounted_net_profit
#' @examples
#' \dontrun{
#' yearly_data <- data.frame(
#'   asset = c("A1", "A1", "A2", "A2"),
#'   company = c("C1", "C1", "C1", "C1"),
#'   year = c(2025, 2025, 2025, 2025),
#'   scenario = c("baseline", "baseline", "baseline", "baseline"),
#'   revenue = c(600, 600, 400, 400),
#'   profit = c(60, 60, 40, 40),
#'   discounted_profit = c(60, 60, 40, 40),
#'   discounted_net_profit = c(60, 60, 40, 40)
#' )
#' result <- aggregate_assets_to_company(yearly_data)
#' }
#' @export
aggregate_assets_to_company <- function(yearly_discounted_df) {
  # Aggregate by company, year, and scenario using dplyr
  result <- yearly_discounted_df |>
    dplyr::group_by(.data$company, .data$year, .data$scenario) |>
    dplyr::summarize(
      total_revenue = sum(.data$revenue, na.rm = TRUE),
      total_profit = sum(.data$profit, na.rm = TRUE),
      total_discounted_profit = sum(.data$discounted_profit, na.rm = TRUE),
      total_discounted_net_profit = sum(.data$discounted_net_profit, na.rm = TRUE),
      .groups = "drop"
    )


  # Sort by company, scenario, and year for consistency
  result <- result |>
    dplyr::arrange(.data$company, .data$scenario, .data$year)

  return(result)
}
