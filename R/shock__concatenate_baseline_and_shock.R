#' Build Yearly Scenarios from Baseline and Shocked Trajectories
#'
#' @title Build Yearly Scenarios
#' @description Concatenates baseline and shocked yearly trajectories into a single dataframe
#'   with a scenario column indicating whether each row is "baseline" or "shocked".
#' @param yearly_baseline_df tibble with columns: asset, company, year, revenue, profit
#' @param yearly_shocked_df tibble with columns: asset, company, year, revenue, profit
#' @return tibble with columns: asset, company, year, scenario, revenue, profit
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
  yearly_shocked_df
) {
  # Prepare baseline scenario data
  baseline_scenario <- yearly_baseline_df |>
    dplyr::select("asset", "company", "year", "revenue", "profit") |>
    dplyr::mutate(scenario = "baseline")

  # Prepare shocked scenario data
  shocked_scenario <- yearly_shocked_df |>
    dplyr::select("asset", "company", "year", "revenue", "profit") |>
    dplyr::mutate(scenario = "shock")

  # Combine scenarios and reorder columns for consistency
  result <- dplyr::bind_rows(baseline_scenario, shocked_scenario) |>
    dplyr::select("asset", "company", "year", "scenario", "revenue", "profit") |>
    dplyr::arrange("asset", "scenario", "year")

  return(result)
}
