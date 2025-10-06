#' Compute Company Probability of Default using Merton Model
#'
#' Calculates probability of default for each company using the Merton model.
#' Uses company NPV, debt, and volatility data.
#'
#' @title Compute Company PD (Merton)
#' @description Computes probability of default using Merton model approach.
#'   Uses company NPV as proxy for asset value, along with debt and volatility.
#'   Returns PD values in the range [0, 1].
#' @param companies_npv data.frame. Company NPV data with company, scenario, npv columns
#' @return data.frame with all original columns plus 'merton_pd' column
#' @examples
#' \dontrun{
#' companies <- data.frame(company = "A", scenario = "baseline", npv = 1000)
#' result <- compute_pd_merton(companies)
#' }
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
    dplyr::select(-.data$V, -.data$D, -.data$sigma, -.data$T, -.data$r, -.data$d1)

  return(result)
}
