#' Discount Yearly Profit Trajectories
#'
#' @title Discount Yearly Profit Trajectories
#' @description Applies present value discounting to yearly profit trajectories.
#'   Formula: discounted_profit = profit / (1 + discount_rate)^(year - base_year)
#' @param yearly_scenarios_df tibble with columns: asset, company, year, scenario, revenue, profit
#' @param discount_rate numeric. Discount rate for NPV calculation (default: 0.05)
#' @param base_year numeric. Base year for discounting (default: NULL, uses minimum year in data)
#' @return tibble with all original columns plus 'discounted_profit' column
#' @examples
#' \dontrun{
#' scenarios <- data.frame(
#'   asset = c("A1", "A1"),
#'   company = c("C1", "C1"),
#'   year = c(2025, 2030),
#'   scenario = c("baseline", "baseline"),
#'   revenue = c(1000, 1200),
#'   profit = c(100, 120)
#' )
#' # Uses minimum year (2025) as base year automatically
#' result <- discount_yearly_profits(scenarios, 0.05)
#' # Or specify base year explicitly
#' result2 <- discount_yearly_profits(scenarios, 0.05, base_year = 2025)
#' }
#' @export
discount_yearly_profits <- function(
  yearly_scenarios_df,
  discount_rate = 0.05,
  base_year = NULL
) {
  # Make a copy to avoid modifying the input
  result <- yearly_scenarios_df

  # Use minimum year as base year if not provided
  if (is.null(base_year)) {
    year_col <- result |> dplyr::pull(.data$year)
    base_year <- min(year_col, na.rm = TRUE)
  }

  # Calculate discounted profit
  # Formula: discounted_profit = profit / (1 + discount_rate)^(year - base_year)
  result <- result |>
    dplyr::mutate(
      years_from_base = .data$year - base_year,
      discount_factor = (1 + discount_rate)^.data$years_from_base,
      discounted_profit = .data$profit / .data$discount_factor,
      discounted_net_profit = .data$discounted_profit
    ) |>
    dplyr::select(-"years_from_base", -"discount_factor")

  return(result)
}
