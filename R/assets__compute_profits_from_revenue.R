#' Compute Profit from Revenue (Scenario-Agnostic)
#'
#' @title Compute Profit from Revenue
#' @description Generic function to compute profit from revenue by applying company-specific net profit margin.
#'   Formula: profit = revenue * net_profit_margin (from company data)
#' @param yearly_revenue_df tibble containing revenue data with `company` column
#' @param companies tibble containing company data with `company` and `net_profit_margin` columns
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
#' companies <- data.frame(
#'   company = "C1",
#'   net_profit_margin = 0.1
#' )
#' result <- compute_profits_from_revenue(
#'   baseline_revenue,
#'   companies
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
#'   companies
#' )
#' }
#' @export
compute_profits_from_revenue <- function(
  yearly_revenue_df,
  companies
) {

  # Join with companies to get company-specific net profit margin
  result <- yearly_revenue_df |>
    dplyr::left_join(
      companies |> dplyr::select("company", "net_profit_margin"),
      by = "company"
    )

  # Check for missing net_profit_margin
  missing_npm <- is.na(result$net_profit_margin)
  if (any(missing_npm)) {
    missing_companies <- unique(result$company[missing_npm])
    stop("Missing net_profit_margin for companies: ", paste(missing_companies, collapse = ", "))
  }

  # Calculate profit from revenue using company-specific margin
  # Formula: profit = revenue * net_profit_margin
  result <- result |>
    dplyr::mutate(profit = .data$revenue * .data$net_profit_margin) |>
    dplyr::select(-"net_profit_margin")  # Remove the joined column

  return(result)
}
