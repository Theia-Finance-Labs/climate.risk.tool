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
  # --- APPLY ACUTE DAMAGE TO PROFITS BY EVENT ---

  # Initialize empty results
  shocks_by_asset_year <- tibble::tibble(
    asset = character(0), 
    event_year = integer(0), 
    acute_damage = numeric(0)
  )

  # Process each event and check its hazard type
  for (i in seq_len(nrow(acute_events))) {
    event <- acute_events[i, ]
    hazard_type <- event$hazard_type
    event_year <- event$event_year

    if (hazard_type %in% c("FloodTIF", "Flood")) {
      # Flood events: calculate acute damage as damage_factor * cost_factor
      assets_flood <- assets_factors |>
        dplyr::filter(.data$hazard_type %in% c("FloodTIF", "Flood")) |>
        dplyr::filter(.data$hazard_name == event$hazard_name) |>
        dplyr::mutate(
          acute_damage = as.numeric(.data$damage_factor) * as.numeric(.data$cost_factor)
        )

      # Create shock data for this event
      if (nrow(assets_flood) > 0) {
        flood_shocks <- assets_flood |>
          dplyr::select("asset", "acute_damage") |>
          dplyr::mutate(event_year = event_year)
        
        shocks_by_asset_year <- dplyr::bind_rows(shocks_by_asset_year, flood_shocks)
      }
    } else if (hazard_type == "drought") {
      # Drought events: TODO - implement drought-specific logic
      # For now, no damage applied for drought events
    } else {
      # Other hazard types: TODO - implement as needed
      # For now, no damage applied for other hazard types
    }
  }

  # Sum acute_damage per (asset, event_year) in case multiple events affect same asset-year
  if (nrow(shocks_by_asset_year) > 0) {
    shocks_by_asset_year <- shocks_by_asset_year |>
      dplyr::group_by(.data$asset, .data$event_year) |>
      dplyr::summarize(
        acute_damage = sum(.data$acute_damage, na.rm = TRUE),
        .groups = "drop"
      )
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
      dplyr::select(-"acute_damage_to_apply")
  }


  return(result)
}
