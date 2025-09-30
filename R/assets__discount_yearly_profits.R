#' Discount Yearly Profit Trajectories
#'
#' @title Discount Yearly Profit Trajectories
#' @description Applies present value discounting to yearly profit trajectories.
#'   Formula: discounted_profit = profit / (1 + discount_rate)^(year - base_year)
#' @param yearly_scenarios_df data.frame with columns: asset, company, year, scenario, revenue, profit
#' @param discount_rate numeric. Discount rate for NPV calculation (default: 0.05)
#' @param base_year numeric. Base year for discounting (default: 2025)
#' @return data.frame with all original columns plus 'discounted_profit' column
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
#' result <- discount_yearly_profits(scenarios, 0.05)
#' }
#' @export
discount_yearly_profits <- function(
    yearly_scenarios_df,
    discount_rate = 0.05,
    base_year = 2025) {
  # Validate inputs
  if (!is.data.frame(yearly_scenarios_df) || nrow(yearly_scenarios_df) == 0) {
    stop("yearly_scenarios_df must be a non-empty data.frame")
  }
  if (!is.numeric(discount_rate) || length(discount_rate) != 1) {
    stop("discount_rate must be a single numeric value")
  }
  if (!is.numeric(base_year) || length(base_year) != 1) {
    stop("base_year must be a single numeric value")
  }

  # Check required columns
  required_cols <- c("asset", "company", "year", "scenario", "revenue", "profit")
  missing_cols <- setdiff(required_cols, names(yearly_scenarios_df))
  if (length(missing_cols) > 0) {
    stop(paste(
      "Missing required columns in yearly_scenarios_df:",
      paste(missing_cols, collapse = ", ")
    ))
  }

  # Make a copy to avoid modifying the input
  result <- yearly_scenarios_df

  # Calculate discounted profit
  # Formula: discounted_profit = profit / (1 + discount_rate)^(year - base_year)
  years_from_base <- result$year - base_year
  discount_factor <- (1 + discount_rate)^years_from_base

  result$discounted_profit <- result$profit / discount_factor

  # Also create discounted_net_profit column for compatibility with downstream functions
  result$discounted_net_profit <- result$discounted_profit

  # Validate the result
  if (!is.numeric(result$discounted_profit)) {
    stop("Calculated discounted_profit is not numeric")
  }

  if (any(is.na(result$discounted_profit))) {
    stop("Calculated discounted_profit contains NA values")
  }

  # Allow negative profits (losses) - do not force non-negative
  # Profits can be negative when companies have losses

  return(result)
}
