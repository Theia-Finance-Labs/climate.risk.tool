#' Compute Profit from Revenue (Scenario-Agnostic)
#'
#' @title Compute Profit from Revenue
#' @description Generic function to compute profit from revenue by applying net profit margin.
#'   Works with any column names (baseline_revenue, shocked_revenue, or custom names).
#'   Formula: profit = revenue * net_profit_margin
#' @param yearly_revenue_df data.frame containing revenue data
#' @param revenue_col character. Name of the revenue column to use (e.g., "baseline_revenue", "shocked_revenue")
#' @param profit_col character. Name of the profit column to create (e.g., "baseline_profit", "shocked_profit")
#' @param net_profit_margin numeric. Net profit margin to apply (default: 0.1)
#' @return data.frame with added profit column
#' @examples
#' \dontrun{
#' # With baseline columns
#' baseline_revenue <- data.frame(
#'   asset = c("A1", "A1"),
#'   company = c("C1", "C1"),
#'   year = c(2025, 2026),
#'   baseline_revenue = c(1000, 1020)
#' )
#' result <- compute_profits_from_revenue(
#'   baseline_revenue,
#'   revenue_col = "baseline_revenue",
#'   profit_col = "baseline_profit",
#'   net_profit_margin = 0.1
#' )
#'
#' # With shocked columns
#' shocked_revenue <- data.frame(
#'   asset = c("A1", "A1"),
#'   company = c("C1", "C1"),
#'   year = c(2025, 2026),
#'   shocked_revenue = c(950, 970)
#' )
#' result <- compute_profits_from_revenue(
#'   shocked_revenue,
#'   revenue_col = "shocked_revenue",
#'   profit_col = "shocked_profit",
#'   net_profit_margin = 0.15
#' )
#' }
#' @export
compute_profits_from_revenue <- function(
    yearly_revenue_df,
    revenue_col,
    profit_col,
    net_profit_margin = 0.1) {
  # Validate inputs
  if (!is.data.frame(yearly_revenue_df) || nrow(yearly_revenue_df) == 0) {
    stop("yearly_revenue_df must be a non-empty data.frame")
  }
  if (!is.character(revenue_col) || length(revenue_col) != 1) {
    stop("revenue_col must be a single character value")
  }
  if (!is.character(profit_col) || length(profit_col) != 1) {
    stop("profit_col must be a single character value")
  }
  if (!is.numeric(net_profit_margin) || length(net_profit_margin) != 1) {
    stop("net_profit_margin must be a single numeric value")
  }

  # Check that revenue column exists
  if (!revenue_col %in% names(yearly_revenue_df)) {
    stop(paste0("revenue column '", revenue_col, "' not found in yearly_revenue_df"))
  }

  # Make a copy to avoid modifying the input
  result <- yearly_revenue_df

  # Calculate profit from revenue
  # Formula: profit = revenue * net_profit_margin
  revenue_values <- result[[revenue_col]]
  
  # Validate revenue values
  if (!is.numeric(revenue_values)) {
    stop(paste0("revenue column '", revenue_col, "' must be numeric"))
  }

  # Ensure revenue is non-negative before calculation
  revenue_values <- pmax(0, revenue_values)
  
  # Calculate profit
  result[[profit_col]] <- revenue_values * net_profit_margin

  # Ensure profit is non-negative
  result[[profit_col]] <- pmax(0, result[[profit_col]])

  # Validate the result
  if (any(is.na(result[[profit_col]]))) {
    stop(paste0("Calculated ", profit_col, " contains NA values"))
  }

  return(result)
}

