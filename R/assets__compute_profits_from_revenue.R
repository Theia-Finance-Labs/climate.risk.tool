#' Compute Profit from Revenue (Scenario-Agnostic)
#'
#' @title Compute Profit from Revenue
#' @description Generic function to compute profit from revenue by applying net profit margin.
#'   Formula: profit = revenue * net_profit_margin
#' @param yearly_revenue_df tibble containing revenue data
#' @param net_profit_margin numeric. Net profit margin to apply (default: 0.1)
#' @return tibble with added profit column
#' @examples
#' \dontrun{
#' # With baseline columns
#' baseline_revenue <- data.frame(
#'   asset = c("A1", "A1"),
#'   company = c("C1", "C1"),
#'   year = c(2025, 2026),
#'   revenue = c(1000, 1020)
#' )
#' result <- compute_profits_from_revenue(
#'   revenue,
#'   net_profit_margin = 0.1
#' )
#'
#' # With shocked columns
#' shocked_revenue <- data.frame(
#'   asset = c("A1", "A1"),
#'   company = c("C1", "C1"),
#'   year = c(2025, 2026),
#'   revenue = c(950, 970)
#' )
#' result <- compute_profits_from_revenue(
#'   shocked_revenue,
#'   net_profit_margin = 0.15
#' )
#' }
#' @export
compute_profits_from_revenue <- function(
  yearly_revenue_df,
  net_profit_margin = 0.1
) {
  # Make a copy to avoid modifying the input
  result <- yearly_revenue_df

  # Calculate profit from revenue
  # Formula: profit = revenue * net_profit_margin
  result <- result |>
    dplyr::mutate(profit = .data$revenue * net_profit_margin)


  return(result)
}
