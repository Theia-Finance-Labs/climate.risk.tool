#' Build Yearly Scenarios from Baseline and Shocked Trajectories
#'
#' @title Build Yearly Scenarios
#' @description Concatenates baseline and shocked yearly trajectories into a single dataframe
#'   with a scenario column indicating whether each row is "baseline" or "shocked".
#' @param yearly_baseline_df data.frame with columns: asset, company, year, revenue, profit
#' @param yearly_shocked_df data.frame with columns: asset, company, year, revenue, profit
#' @return data.frame with columns: asset, company, year, scenario, revenue, profit
#' @examples
#' \dontrun{
#' baseline <- data.frame(
#'   asset = c("A1", "A1"),
#'   company = c("C1", "C1"),
#'   year = c(2025, 2026),
#'   revenue = c(1000, 1020),
#'   profit = c(100, 102)
#' )
#' shocked <- data.frame(
#'   asset = c("A1", "A1"),
#'   company = c("C1", "C1"),
#'   year = c(2025, 2026),
#'   revenue = c(970, 989),
#'   profit = c(97, 99)
#' )
#' result <- concatenate_baseline_and_shock(baseline, shocked)
#' }
#' @export
concatenate_baseline_and_shock <- function(
    yearly_baseline_df,
    yearly_shocked_df) {

  # Prepare baseline scenario data
  baseline_scenario <- yearly_baseline_df[, c("asset", "company", "year", "revenue", "profit"), drop = FALSE]
  baseline_scenario$scenario <- "baseline"

  # Prepare shocked scenario data
  shocked_scenario <- yearly_shocked_df[, c("asset", "company", "year", "revenue", "profit"), drop = FALSE]
  shocked_scenario$scenario <- "shock" 

  # Combine scenarios
  result <- rbind(baseline_scenario, shocked_scenario)

  # Reorder columns for consistency
  result <- result[, c("asset", "company", "year", "scenario", "revenue", "profit"), drop = FALSE]

  # Sort by asset, scenario, and year for consistency
  result <- result[order(result$asset, result$scenario, result$year), ]

  return(result)
}
