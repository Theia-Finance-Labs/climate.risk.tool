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
  V <- pmax(result_with_financials$debt + result_with_financials$npv, 1)   # avoid non-positive
  D <- pmax(result_with_financials$debt, 1)                                # avoid non-positive
  sigma <- pmax(result_with_financials$volatility, 1e-8)                   # avoid zero
  T <- pmax(result_with_financials$term, 1)                                # default Term = 1

  # Risk-free rate hard-coded at 2%,needs to be coded into parameters
  r <- 0.02

  # Excel-equivalent:
  # d1 = ( ln(V/D) + (r + 0.5*sigma^2)*T ) / (sigma*sqrt(T))
  # PD = N( -(d1 - sigma*sqrt(T)) )
  d1 <- (log(V / D) + (r + 0.5 * sigma^2) * T) / (sigma * sqrt(T))
  result_with_financials$merton_pd <- pnorm(-(d1 - sigma * sqrt(T)))

  # Update result with the calculated values
  result <- result_with_financials

  # Ensure PD is in [0, 1] range
  result$merton_pd <- pmax(0, pmin(1, result$merton_pd))

  return(result)
}
