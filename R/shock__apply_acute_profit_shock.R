#' Apply Acute Shock to Yearly Profit Trajectories
#'
#' @title Apply Acute Shock to Yearly Profit
#' @description Applies profit shocks from acute climate events based on hazard configuration
#'   equations. Profit shocks are computed per event and aggregated by asset-year.
#' @param yearly_trajectories tibble with columns: asset, company, year, revenue, profit
#' @param assets_factors tibble with hazard data and mapping factors
#' @param acute_events tibble with acute event information including event_id, hazard_type, event_year
#' @param hazard_configs Named list from load_hazards_and_inventory()$configs
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
#' assets_factors <- data.frame(asset = "A1", hazard_type = "Flood", cost_factor = 100)
#' acute_events <- data.frame(event_id = "e1", hazard_type = "Flood", event_year = 2030)
#' hazard_configs <- list(
#'   Flood = list(shocks = list())
#' )
#' result <- apply_acute_profit_shock(yearly_trajectories, assets_factors, acute_events, hazard_configs)
#' }
#' @export
apply_acute_profit_shock <- function(
  yearly_trajectories,
  assets_factors,
  acute_events,
  hazard_configs
) {
  if (is.null(hazard_configs) || length(hazard_configs) == 0) {
    stop("hazard_configs is required to apply profit shocks")
  }

  # Initialize empty results
  shocks_by_asset_year <- tibble::tibble(
    asset = character(0),
    event_year = integer(0),
    acute_damage = numeric(0)
  )

  # Sort events by event_id to ensure consistent processing order
  acute_events <- acute_events |>
    dplyr::arrange(.data$event_id)

  # Process each event and compute damage
  for (i in seq_len(nrow(acute_events))) {
    event <- acute_events[i, ]
    hazard_type <- as.character(event$hazard_type)
    hazard_config <- hazard_configs[[hazard_type]]
    if (is.null(hazard_config)) {
      next
    }

    event_assets <- assets_factors |>
      dplyr::filter(
        .data$hazard_type == hazard_type,
        .data$event_id == event$event_id
      )

    if (nrow(event_assets) == 0) {
      next
    }

    event_assets <- event_assets |>
      dplyr::mutate(event_year = as.numeric(event$event_year))

    # Join with yearly_trajectories to get baseline revenue/profit for the event year
    event_assets <- event_assets |>
      dplyr::left_join(
        yearly_trajectories |>
          dplyr::select("asset", "year", "revenue", "profit"),
        by = c("asset" = "asset", "event_year" = "year")
      )

    event_damage <- evaluate_hazard_shock(
      assets_event = event_assets,
      hazard_config = hazard_config,
      shock_type = "profit",
      combine = "sum"
    )

    if (nrow(event_damage) > 0) {
      event_damage <- event_damage |>
        dplyr::mutate(event_year = as.numeric(event$event_year)) |>
        dplyr::rename(acute_damage = "shock_value")

      shocks_by_asset_year <- dplyr::bind_rows(shocks_by_asset_year, event_damage)
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
      dplyr::rename(year = "event_year", acute_damage_to_apply = "acute_damage")
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
