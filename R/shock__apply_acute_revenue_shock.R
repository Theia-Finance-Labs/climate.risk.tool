#' Apply Acute Shock to Yearly Revenue Trajectories (Placeholder)
#'
#' @title Apply Acute Revenue Shock (Placeholder)
#' @description Placeholder function that passes through baseline revenue as shocked revenue.
#'   This is a temporary implementation that maintains the expected output structure
#'   while the actual shock calculation logic is being developed. NOTE: This function
#'   only affects REVENUE. Profit is computed separately using compute_profits_from_revenue().
#' @param yearly_trajectories tibble with columns: asset, company, year, revenue
#' @param assets_factors tibble with hazard data and damage factors (currently unused)
#' @param acute_events tibble with acute event information (currently unused)
#' @return tibble with columns: asset, company, year, revenue
#' @examples
#' \dontrun{
#' yearly_trajectories <- data.frame(
#'   asset = c("A1", "A1"),
#'   company = c("C1", "C1"),
#'   year = c(2025, 2030),
#'   revenue = c(1000, 1200),
#'   profit = c(100, 120)
#' )
#' assets_factors <- data.frame(asset = "A1", hazard_type = "flood", damage_factor = 0.1)
#' acute_events <- data.frame(event_id = "e1", hazard_type = "flood", event_year = 2030, chronic = FALSE)
#' # Apply revenue shock (returns only revenue, not profit)
#' result <- apply_acute_revenue_shock(yearly_trajectories, assets_factors, acute_events)
#' }
#' @export
apply_acute_revenue_shock <- function(
    yearly_trajectories,
    assets_factors,
    acute_events) {
  # --- Build flood disruption map (asset, event_year -> total disruption days) ---

  # Keep only FLOOD rows from here. Additional hazards have to be integrated seperately
  flood_events <- acute_events |>
    dplyr::filter(tolower(as.character(.data$hazard_type)) == "flood")

  assets_flood <- assets_factors |>
    dplyr::filter(tolower(as.character(.data$hazard_type)) == "flood")

  # Map by hazard_name, then sum per (asset, event_year)
  if (nrow(assets_flood) > 0 && nrow(flood_events) > 0) {
    shock_map <- dplyr::inner_join(
      assets_flood |>
        dplyr::select(.data$asset, .data$hazard_name, .data$business_disruption),
      flood_events |>
        dplyr::select(.data$hazard_name, .data$event_year),
      by = "hazard_name"
    )

    if (nrow(shock_map) > 0) {
      shocks_by_asset_year <- shock_map |>
        dplyr::group_by(.data$asset, .data$event_year) |>
        dplyr::summarize(
          business_disruption = sum(as.numeric(.data$business_disruption), na.rm = TRUE),
          .groups = "drop"
        )
      # Cap summed disruption into [0, 365]
      shocks_by_asset_year <- shocks_by_asset_year |>
        dplyr::mutate(business_disruption = pmax(0, pmin(365, as.numeric(.data$business_disruption))))
    } else {
      shocks_by_asset_year <- tibble::tibble(asset = character(0), event_year = integer(0), business_disruption = numeric(0))
    }
  } else {
    shocks_by_asset_year <- tibble::tibble(asset = character(0), event_year = integer(0), business_disruption = numeric(0))
  }


  # Start with the baseline trajectories
  result <- yearly_trajectories

  if (nrow(shocks_by_asset_year) > 0) {
    disruption_data <- shocks_by_asset_year |>
      dplyr::rename(year = .data$event_year, disruption_days = .data$business_disruption)
    result <- dplyr::left_join(result, disruption_data, by = c("asset", "year"))

    # Apply formula only where disruption is present
    result <- result |>
      dplyr::mutate(
        revenue = dplyr::if_else(
          is.na(.data$disruption_days),
          .data$revenue,
          as.numeric(.data$revenue) * (1 - as.numeric(.data$disruption_days) / 365)
        )
      ) |>
      dplyr::select(-.data$disruption_days)
  }


  return(result)
}
