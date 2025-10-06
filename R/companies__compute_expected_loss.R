#' Compute Expected Loss
#'
#' Calculates expected loss for each company using the formula: EL = LGD * Loan_Size * PD.
#' Uses company-specific LGD and loan size data along with computed Merton PD.
#'
#' @title Compute Expected Loss
#' @description Computes expected loss using the standard credit risk formula:
#'   Expected Loss = Loss Given Default * Loan Size * Probability of Default
#'   Joins company PD data with LGD and loan size information.
#' @param companies_pd data.frame. Company PD data with company, scenario, merton_pd columns
#' @return data.frame with all original columns plus 'expected_loss' column
#' @examples
#' \dontrun{
#' companies <- data.frame(company = "A", scenario = "baseline", merton_pd = 0.1)
#' result <- compute_expected_loss(companies)
#' }
#' @export
compute_expected_loss <- function(companies_pd) {
  # Make a copy to avoid modifying the input
  result <- companies_pd

  # Calculate Expected Loss: EL = LGD * Loan_Size * PD
  # lgd and loan_size should already be in the input dataframe
  result <- result |>
    dplyr::mutate(expected_loss = .data$lgd * .data$loan_size * .data$merton_pd)


  return(result)
}
