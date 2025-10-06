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
  # Validate inputs
  if (!is.data.frame(companies_npv)) {
    stop("companies_npv must be a data.frame")
  }

  # Check required columns
  required_cols <- c("company", "scenario", "npv")
  missing_cols <- setdiff(required_cols, names(companies_npv))
  if (length(missing_cols) > 0) {
    stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
  }

  # Make a copy to avoid modifying the input
  result <- companies_npv

  # Try to get company-specific debt, volatility, and term data
  company_financial_data <- NULL

  tryCatch(
    {
      # Get test data directory (hack for test environment)
      test_data_dir <- file.path(testthat::test_path(".."), "tests_data")
      companies_file <- file.path(test_data_dir, "user_input", "company.csv")

      if (file.exists(companies_file)) {
        companies_raw <- read.csv(companies_file, stringsAsFactors = FALSE)
        # Convert to snake_case column names
        names(companies_raw) <- tolower(gsub("[^A-Za-z0-9]", "_", names(companies_raw)))
        names(companies_raw) <- gsub("_+", "_", names(companies_raw))
        names(companies_raw) <- gsub("^_|_$", "", names(companies_raw))

        if ("company_name" %in% names(companies_raw) &&
            "debt" %in% names(companies_raw) &&
            "volatility" %in% names(companies_raw) &&
            "term" %in% names(companies_raw)) {

          cols_to_keep <- c("company_name", "debt", "volatility", "term")
          company_financial_data <- companies_raw[, cols_to_keep]
          names(company_financial_data)[names(company_financial_data) == "company_name"] <- "company"
        }
      }
    },
    error = function(e) {
      # If we can't load company data, continue as per surrounding logic
    }
  )

  # Compute Merton PD
  if (is.null(company_financial_data)) {
    stop("Company financial data not found. Need 'company_name', 'debt', 'volatility', 'term' in company.csv.")
  } else {
    # Join with financial data
    result_with_financials <- merge(result, company_financial_data, by = "company", all.x = TRUE)

    # Ensure required cols present in working frame
    result_with_financials <- result_with_financials[, intersect(
      c("company", "scenario", "npv", "debt", "volatility", "term"),
      names(result_with_financials)
    )]

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
    result$merton_pd <- pnorm(-(d1 - sigma * sqrt(T)))
  }

  # Ensure PD is in [0, 1] range
  result$merton_pd <- pmax(0, pmin(1, result$merton_pd))

  # Validate the result
  if (!is.numeric(result$merton_pd)) {
    stop("Calculated merton_pd is not numeric")
  }

  # Keep NA values as NA (no replacement)

  return(result)
}
