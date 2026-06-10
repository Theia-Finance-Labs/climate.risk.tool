#' Apply Acute Revenue Shock
#'
#' @title Apply Acute Revenue Shock
#' @description Applies revenue shocks from acute climate events in event_id order.
#'   Shock equations are defined in hazard configuration YAML files.
#'   NOTE: This function only affects REVENUE. Profit is computed separately using compute_profits_from_revenue().
#' @param yearly_trajectories tibble with columns: asset, company, year, revenue
#' @param assets_factors tibble with hazard data and mapping factors including event_id, hazard_type
#' @param acute_events tibble with acute event information including event_id, hazard_type, event_year
#' @param hazard_configs Named list from load_hazards_and_inventory()$configs
#' @return tibble with columns: asset, company, year, revenue (with shocks applied)
#' @examples
#' \dontrun{
#' yearly_trajectories <- data.frame(
#'   asset = c("A1", "A1"),
#'   company = c("C1", "C1"),
#'   year = c(2025, 2030),
#'   revenue = c(1000, 1200)
#' )
#' assets_factors <- data.frame(
#'   asset = "A1", hazard_type = "Flood", event_id = "event_1",
#'   damage_factor = 0.1, business_disruption = 10
#' )
#' acute_events <- data.frame(
#'   event_id = "event_1", hazard_type = "Flood", event_year = 2030
#' )
#' hazard_configs <- list(
#'   Flood = list(shocks = list())
#' )
#' result <- apply_acute_revenue_shock(
#'   yearly_trajectories, assets_factors, acute_events, hazard_configs
#' )
#' }
#' @export
apply_acute_revenue_shock <- function(
  yearly_trajectories,
  assets_factors,
  acute_events,
  hazard_configs
) {
  if (is.null(hazard_configs) || length(hazard_configs) == 0) {
    stop("hazard_configs is required to apply revenue shocks")
  }

  # Sort events by event_id to apply shocks in order
  acute_events <- acute_events |>
    dplyr::arrange(.data$event_id)

  # Start with baseline revenue
  result <- yearly_trajectories

  # Loop through events in event_id order
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
      ) |>
      dplyr::mutate(event_year = as.numeric(event$event_year))

    if (nrow(event_assets) == 0) {
      next
    }

    event_assets <- event_assets |>
      dplyr::left_join(
        result |>
          dplyr::select("asset", "year", "revenue"),
        by = c("asset" = "asset", "event_year" = "year")
      )

    shock_values <- evaluate_hazard_shock(
      assets_event = event_assets,
      hazard_config = hazard_config,
      shock_type = "revenue",
      combine = "error"
    )

    if (nrow(shock_values) == 0) {
      next
    }

    if (!"spatial_multiplier" %in% names(event_assets)) {
      event_assets$spatial_multiplier <- 1
    }

    # Scale the shock delta using spatial_multiplier:
    # adjusted = baseline + multiplier * (shock - baseline)
    asset_modifiers <- event_assets |>
      dplyr::group_by(.data$asset) |>
      dplyr::summarise(
        baseline_revenue = first_non_missing(.data$revenue),
        spatial_multiplier = first_non_missing(.data$spatial_multiplier),
        .groups = "drop"
      ) |>
      dplyr::mutate(
        spatial_multiplier = dplyr::coalesce(as.numeric(.data$spatial_multiplier), 1)
      )

    shock_values <- shock_values |>
      dplyr::left_join(asset_modifiers, by = "asset") |>
      dplyr::mutate(
        shock_value = dplyr::if_else(
          !is.na(.data$baseline_revenue),
          as.numeric(.data$baseline_revenue) +
            as.numeric(.data$spatial_multiplier) * (as.numeric(.data$shock_value) - as.numeric(.data$baseline_revenue)),
          as.numeric(.data$shock_value)
        )
      ) |>
      dplyr::select("asset", "shock_value")

    shock_values <- shock_values |>
      dplyr::mutate(year = as.numeric(event$event_year))

    result <- result |>
      dplyr::left_join(shock_values, by = c("asset", "year")) |>
      dplyr::mutate(
        revenue = dplyr::if_else(
          is.na(.data$shock_value),
          .data$revenue,
          .data$shock_value
        )
      ) |>
      dplyr::select(-"shock_value")
  }

  return(result)
}
