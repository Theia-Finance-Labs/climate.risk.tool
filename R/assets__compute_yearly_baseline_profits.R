#' Compute Yearly Baseline Profit Trajectories from Revenue
#'
#' @title Compute Yearly Baseline Profit Trajectories
#' @description Computes yearly baseline profit trajectories from revenue trajectories
#'   by applying net profit margin: profit = revenue * net_profit_margin
#' @param yearly_revenue_df data.frame with columns: asset, company, year, baseline_revenue
#' @param net_profit_margin numeric. Net profit margin to apply (default: 0.1)
#' @return data.frame with columns: asset, company, year, baseline_revenue, baseline_profit
#' @examples
#' \dontrun{
#' yearly_revenue <- data.frame(
#'   asset = c("A1", "A1"),
#'   company = c("C1", "C1"),
#'   year = c(2025, 2026),
#'   baseline_revenue = c(1000, 1020)
#' )
#' result <- compute_yearly_baseline_profits(yearly_revenue, 0.1)
#' }
#' @export
compute_yearly_baseline_profits <- function(
    yearly_revenue_df,
    net_profit_margin = 0.1) {
  # Validate inputs
  if (!is.data.frame(yearly_revenue_df) || nrow(yearly_revenue_df) == 0) {
    stop("yearly_revenue_df must be a non-empty data.frame")
  }
  if (!is.numeric(net_profit_margin) || length(net_profit_margin) != 1) {
    stop("net_profit_margin must be a single numeric value")
  }

  # Check required columns
  required_cols <- c("asset", "company", "year", "baseline_revenue")
  missing_cols <- setdiff(required_cols, names(yearly_revenue_df))
  if (length(missing_cols) > 0) {
    stop(paste(
      "Missing required columns in yearly_revenue_df:",
      paste(missing_cols, collapse = ", ")
    ))
  }

  # Make a copy to avoid modifying the input
  result <- yearly_revenue_df

  # Calculate baseline profit
  # Formula: baseline_profit = baseline_revenue * net_profit_margin
  result$baseline_profit <- result$baseline_revenue * net_profit_margin

  # Validate the result
  if (!is.numeric(result$baseline_profit)) {
    stop("Calculated baseline_profit is not numeric")
  }

  if (any(is.na(result$baseline_profit))) {
    stop("Calculated baseline_profit contains NA values")
  }

  # Ensure profit is non-negative
  result$baseline_profit <- pmax(0, result$baseline_profit)

  return(result)
}
