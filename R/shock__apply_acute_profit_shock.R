#' Apply Acute Shock to Yearly Profit Trajectories (Placeholder)
#'
#' @title Apply Acute Shock to Yearly Profit (Placeholder)
#' @description Placeholder function that passes through profit values unchanged.
#'   This is a temporary implementation that maintains the expected output structure
#'   while the actual shock calculation logic is being developed. Unlike revenue shocks,
#'   profit shocks may involve fixed costs or operational disruptions that aren't
#'   proportional to revenue impacts.
#' @param yearly_trajectories tibble with columns: asset, company, year, revenue, profit
#' @param assets_factors tibble with hazard data and cost factors (currently unused)
#' @param acute_events tibble with acute event information (currently unused)
#' @return tibble with columns: asset, company, year, revenue, profit
#' @examples
#' \dontrun{
#' yearly_trajectories <- data.frame(
#'   asset = c("A1", "A1"),
#'   company = c("C1", "C1"),
#'   year = c(2025, 2030),
#'   revenue = c(1000, 1200),
#'   profit = c(100, 120)
#' )
#' assets_factors <- data.frame(asset = "A1", hazard_type = "flood", cost_factor = 100)
#' acute_events <- data.frame(event_id = "e1", hazard_type = "flood", event_year = 2030, chronic = FALSE)
#' result <- apply_acute_profit_shock(yearly_trajectories, assets_factors, acute_events)
#' }
#' @export
apply_acute_profit_shock <- function(
    yearly_trajectories,
    assets_factors,
    acute_events) {
  # --- APPLY FLOOD ACUTE DAMAGE TO PROFITS ---

  # compute acute damages for floods in assets_factors (non-invasive; only adds a column)
  if (all(c("hazard_type", "damage_factor", "cost_factor") %in% names(assets_factors))) {
    assets_factors <- assets_factors |>
      dplyr::mutate(
        acute_damage = dplyr::case_when(
          tolower(as.character(.data$hazard_type)) == "flood" ~
            as.numeric(.data$damage_factor) * as.numeric(.data$cost_factor),
          TRUE ~ NA_real_
        )
      )
  }

  # Select only floods from events
  flood_events <- acute_events |>
    dplyr::filter(tolower(as.character(.data$hazard_type)) == "flood") |>
    dplyr::filter(!is.na(.data$event_year))

  # Select only floods from assets_factors
  assets_flood <- assets_factors |>
    dplyr::filter(tolower(as.character(.data$hazard_type)) == "flood")

  # Merge by hazard_name to attach event_year to each asset-hazard
  # -> (asset, hazard_name, event_year, acute_damage)
  if (nrow(assets_flood) > 0 && nrow(flood_events) > 0) {
    shock_map <- dplyr::inner_join(
      assets_flood |>
        dplyr::select(.data$asset, .data$hazard_name, .data$acute_damage),
      flood_events |>
        dplyr::select(.data$hazard_name, .data$event_year),
      by = "hazard_name"
    )

    # Sum acute_damage per (asset, event_year) in case multiple hazards per asset-year
    if (nrow(shock_map) > 0) {
      shocks_by_asset_year <- shock_map |>
        dplyr::group_by(.data$asset, .data$event_year) |>
        dplyr::summarize(
          acute_damage = sum(as.numeric(.data$acute_damage), na.rm = TRUE),
          .groups = "drop"
        )
    } else {
      shocks_by_asset_year <- tibble::tibble(asset = character(0), event_year = integer(0), acute_damage = numeric(0))
    }
  } else {
    shocks_by_asset_year <- tibble::tibble(asset = character(0), event_year = integer(0), acute_damage = numeric(0))
  }

  # Start from trajectories
  result <- yearly_trajectories

  # Attach shock (by asset + year == event_year), then deduct from profit
  if (nrow(shocks_by_asset_year) > 0) {
    shock_data <- shocks_by_asset_year |>
      dplyr::rename(year = .data$event_year, acute_damage_to_apply = .data$acute_damage)
    result <- dplyr::left_join(result, shock_data, by = c("asset", "year"))

    # Deduct (do not alter revenue here as per request)
    result <- result |>
      dplyr::mutate(
        profit = as.numeric(.data$profit) -
          dplyr::if_else(is.na(.data$acute_damage_to_apply), 0, as.numeric(.data$acute_damage_to_apply))
      ) |>
      dplyr::select(-.data$acute_damage_to_apply)
  }


  return(result)
}
