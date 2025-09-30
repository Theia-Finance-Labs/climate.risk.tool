#' Compute Company NPV from Yearly Trajectories
#'
#' Aggregates yearly company trajectories to company-level NPV by scenario.
#' Computes Net Present Value (NPV) for each company in each scenario by summing yearly discounted profits.
#'
#' @title Compute Company NPV from Yearly Data
#' @description Aggregates yearly discounted net profits from company trajectories to compute company-level NPV.
#'   Can work with either company yearly trajectories or asset yearly data.
#'   Groups by company and scenario, summing discounted_net_profit or total_discounted_net_profit values across years.
#' @param yearly_data data.frame. Either company yearly trajectories or asset yearly data with discounted profit columns
#' @return data.frame with columns: company, scenario, npv
#' @examples
#' \dontrun{
#' # With company yearly data
#' company_yearly <- data.frame(
#'   company = c("C1", "C1"),
#'   year = c(2025, 2026),
#'   scenario = c("baseline", "baseline"),
#'   total_discounted_net_profit = c(100, 97)
#' )
#' result <- compute_company_npv(company_yearly)
#'
#' # With asset yearly data
#' asset_yearly <- data.frame(
#'   asset = c("A1", "A1"),
#'   company = c("C1", "C1"),
#'   year = c(2025, 2026),
#'   scenario = c("baseline", "baseline"),
#'   discounted_net_profit = c(100, 97)
#' )
#' result <- compute_company_npv(asset_yearly)
#' }
#' @export
compute_company_npv <- function(yearly_data) {
  # Validate inputs
  if (!is.data.frame(yearly_data)) {
    stop("yearly_data must be a data.frame")
  }

  # Determine data type and required columns based on available columns
  has_total_discounted <- "total_discounted_net_profit" %in% names(yearly_data)
  has_asset_discounted <- "discounted_net_profit" %in% names(yearly_data)

  if (has_total_discounted) {
    # Company yearly trajectories
    required_cols <- c("company", "scenario", "total_discounted_net_profit")
    profit_col <- "total_discounted_net_profit"
  } else if (has_asset_discounted) {
    # Asset yearly trajectories - need to aggregate to company level
    required_cols <- c("company", "scenario", "discounted_net_profit")
    profit_col <- "discounted_net_profit"
  } else {
    stop("yearly_data must contain either 'total_discounted_net_profit' (company data) or 'discounted_net_profit' (asset data)")
  }

  missing_cols <- setdiff(required_cols, names(yearly_data))
  if (length(missing_cols) > 0) {
    stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
  }

  # Aggregate discounted net profits by company and scenario
  # Sum across years for each company-scenario combination
  formula_str <- paste(profit_col, "~ company + scenario")
  result <- stats::aggregate(
    as.formula(formula_str),
    data = yearly_data,
    FUN = sum,
    na.rm = TRUE
  )

  # Rename the aggregated column to npv (snake_case preferred)
  names(result)[names(result) == profit_col] <- "npv"

  # Ensure proper column types
  result$company <- as.character(result$company)

  # Preserve scenario as ordered factor if it was one
  if (is.factor(yearly_data$scenario)) {
    if (is.ordered(yearly_data$scenario)) {
      result$scenario <- factor(result$scenario,
        levels = levels(yearly_data$scenario),
        ordered = TRUE
      )
    } else {
      result$scenario <- factor(result$scenario,
        levels = levels(yearly_data$scenario)
      )
    }
  }

  # Ensure npv is numeric
  if (!is.numeric(result$npv)) {
    stop("Calculated NPV is not numeric")
  }

  # Sort by company and scenario for consistent output
  result <- result[order(result$company, result$scenario), ]

  # Reset row names
  rownames(result) <- NULL

  return(result)
}
