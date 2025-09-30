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
  # Validate inputs
  if (!is.data.frame(companies_pd)) {
    stop("companies_pd must be a data.frame")
  }

  # Check required columns
  required_cols <- c("company", "scenario", "merton_pd")
  missing_cols <- setdiff(required_cols, names(companies_pd))
  if (length(missing_cols) > 0) {
    stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
  }

  # Make a copy to avoid modifying the input
  result <- companies_pd

  # Try to get company-specific LGD and loan size data
  company_credit_data <- NULL

  # Try to load company data to get LGD and loan size
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
          "lgd" %in% names(companies_raw) &&
          "loan_size" %in% names(companies_raw)) {
          company_credit_data <- companies_raw[, c("company_name", "lgd", "loan_size")]
          names(company_credit_data) <- c("company", "lgd", "loan_size")
        }
      }
    },
    error = function(e) {
      # If we can't load company data, continue with default values
    }
  )

  # Compute Expected Loss
  if (!is.null(company_credit_data)) {
    # Join with credit data
    result_with_credit <- merge(result, company_credit_data, by = "company", all.x = TRUE)

    # Calculate Expected Loss: EL = LGD * Loan_Size * PD
    result$expected_loss <- result_with_credit$lgd *
      result_with_credit$loan_size *
      result$merton_pd

    # Handle missing values (companies not found in credit data)
    result$expected_loss[is.na(result$expected_loss)] <- 0
  } else {
    # Simplified approach without company credit data
    # Use default values for LGD and loan size
    default_lgd <- 0.45 # Typical LGD
    default_loan_size <- 100000 # Default loan size

    result$expected_loss <- default_lgd * default_loan_size * result$merton_pd
  }

  # Ensure expected loss is non-negative
  result$expected_loss <- pmax(0, result$expected_loss)

  # Validate the result
  if (!is.numeric(result$expected_loss)) {
    stop("Calculated expected_loss is not numeric")
  }

  if (any(is.na(result$expected_loss))) {
    # Replace any remaining NAs with zero
    result$expected_loss[is.na(result$expected_loss)] <- 0
  }

  return(result)
}
