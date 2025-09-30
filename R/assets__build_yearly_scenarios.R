#' Build Yearly Scenarios from Baseline and Shocked Trajectories
#'
#' @title Build Yearly Scenarios
#' @description Concatenates baseline and shocked yearly trajectories into a single dataframe
#'   with a scenario column indicating whether each row is "baseline" or "shocked".
#' @param yearly_baseline_df data.frame with columns: asset, company, year, baseline_revenue, baseline_profit
#' @param yearly_shocked_df data.frame with columns: asset, company, year, shocked_revenue, shocked_profit
#' @return data.frame with columns: asset, company, year, scenario, revenue, profit
#' @examples
#' \dontrun{
#' baseline <- data.frame(
#'   asset = c("A1", "A1"),
#'   company = c("C1", "C1"),
#'   year = c(2025, 2026),
#'   baseline_revenue = c(1000, 1020),
#'   baseline_profit = c(100, 102)
#' )
#' shocked <- data.frame(
#'   asset = c("A1", "A1"),
#'   company = c("C1", "C1"),
#'   year = c(2025, 2026),
#'   shocked_revenue = c(970, 989),
#'   shocked_profit = c(97, 99)
#' )
#' result <- build_yearly_scenarios(baseline, shocked)
#' }
#' @export
build_yearly_scenarios <- function(
    yearly_baseline_df,
    yearly_shocked_df) {
  # Validate inputs
  if (!is.data.frame(yearly_baseline_df) || nrow(yearly_baseline_df) == 0) {
    stop("yearly_baseline_df must be a non-empty data.frame")
  }
  if (!is.data.frame(yearly_shocked_df) || nrow(yearly_shocked_df) == 0) {
    stop("yearly_shocked_df must be a non-empty data.frame")
  }

  # Check required columns for baseline
  required_baseline_cols <- c("asset", "company", "year", "baseline_revenue", "baseline_profit")
  missing_baseline_cols <- setdiff(required_baseline_cols, names(yearly_baseline_df))
  if (length(missing_baseline_cols) > 0) {
    stop(paste(
      "Missing required columns in yearly_baseline_df:",
      paste(missing_baseline_cols, collapse = ", ")
    ))
  }

  # Check required columns for shocked
  required_shocked_cols <- c("asset", "company", "year", "shocked_revenue", "shocked_profit")
  missing_shocked_cols <- setdiff(required_shocked_cols, names(yearly_shocked_df))
  if (length(missing_shocked_cols) > 0) {
    stop(paste(
      "Missing required columns in yearly_shocked_df:",
      paste(missing_shocked_cols, collapse = ", ")
    ))
  }

  # Prepare baseline scenario data
  baseline_scenario <- yearly_baseline_df[, c("asset", "company", "year", "baseline_revenue", "baseline_profit"), drop = FALSE]
  baseline_scenario$scenario <- "baseline"
  names(baseline_scenario)[names(baseline_scenario) == "baseline_revenue"] <- "revenue"
  names(baseline_scenario)[names(baseline_scenario) == "baseline_profit"] <- "profit"

  # Prepare shocked scenario data
  shocked_scenario <- yearly_shocked_df[, c("asset", "company", "year", "shocked_revenue", "shocked_profit"), drop = FALSE]
  shocked_scenario$scenario <- "shock" # Use "shock" to match downstream expectations
  names(shocked_scenario)[names(shocked_scenario) == "shocked_revenue"] <- "revenue"
  names(shocked_scenario)[names(shocked_scenario) == "shocked_profit"] <- "profit"

  # Combine scenarios
  result <- rbind(baseline_scenario, shocked_scenario)

  # Reorder columns for consistency
  result <- result[, c("asset", "company", "year", "scenario", "revenue", "profit"), drop = FALSE]

  # Validate the result
  if (!is.numeric(result$revenue) || !is.numeric(result$profit)) {
    stop("Revenue and profit columns must be numeric")
  }

  if (any(is.na(result$revenue)) || any(is.na(result$profit))) {
    stop("Revenue and profit columns contain NA values")
  }

  # Ensure values are non-negative
  result$revenue <- pmax(0, result$revenue)
  result$profit <- pmax(0, result$profit)

  # Sort by asset, scenario, and year for consistency
  result <- result[order(result$asset, result$scenario, result$year), ]

  return(result)
}
