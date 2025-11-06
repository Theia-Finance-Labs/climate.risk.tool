#' Apply Acute Revenue Shock
#'
#' @title Apply Acute Revenue Shock
#' @description Applies revenue shocks from acute climate events in event_id order.
#'   Supports Flood (business disruption), Heat/heat (labor productivity loss via Cobb-Douglas),
#'   and Drought (crop damage for agriculture). Multiple shocks in the same year are applied sequentially by event_id.
#'   NOTE: This function only affects REVENUE. Profit is computed separately using compute_profits_from_revenue().
#' @param yearly_trajectories tibble with columns: asset, company, year, revenue
#' @param assets_factors tibble with hazard data and damage factors including event_id, hazard_type
#' @param acute_events tibble with acute event information including event_id, hazard_type, event_year
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
#' result <- apply_acute_revenue_shock(yearly_trajectories, assets_factors, acute_events)
#' }
#' @export
apply_acute_revenue_shock <- function(
  yearly_trajectories,
  assets_factors,
  acute_events
) {
  # Sort events by event_id to apply shocks in order
  acute_events <- acute_events |>
    dplyr::arrange(.data$event_id)

  # Start with baseline revenue
  result <- yearly_trajectories

  # Loop through events in event_id order
  for (i in seq_len(nrow(acute_events))) {
    event <- acute_events[i, ]
    hazard_type <- as.character(event$hazard_type)

    if (hazard_type == "Flood") {
      result <- apply_flood_shock(result, event, assets_factors)
    } else if (hazard_type == "Heat") {
      result <- apply_compound_shock(result, event, assets_factors)
    } else if (hazard_type == "Drought") {
      result <- apply_drought_shock(result, event, assets_factors)
    } else if (hazard_type == "Fire") {
      result <- apply_fire_revenue_shock(result, event, assets_factors)
    }
    # Other hazard types: no action (extend later as needed)
  }

  return(result)
}

#' Apply Flood shock to revenue trajectories (internal function)
#'
#' @param yearly_trajectories tibble with columns: asset, company, year, revenue
#' @param event Single event row with event_id, event_year, hazard_type
#' @param assets_factors tibble with hazard data and damage factors
#' @return tibble with revenue adjusted for Flood business disruption
#' @noRd
apply_flood_shock <- function(yearly_trajectories, event, assets_factors) {
  # Filter Flood assets for this specific event (by event_id)
  flood_assets <- assets_factors |>
    dplyr::filter(
      .data$hazard_type == "Flood",
      .data$event_id == event$event_id
    )

  if (nrow(flood_assets) == 0) {
    return(yearly_trajectories)
  }

  # Separate agriculture from non-agriculture assets
  agriculture_assets <- flood_assets |>
    dplyr::filter(.data$asset_category == "agriculture")

  non_agriculture_assets <- flood_assets |>
    dplyr::filter(.data$asset_category != "agriculture")

  # Process non-agriculture assets (commercial building, industrial building)
  # Only business disruption applies
  result <- yearly_trajectories

  if (nrow(non_agriculture_assets) > 0) {
    disruption_map <- non_agriculture_assets |>
      dplyr::group_by(.data$asset) |>
      dplyr::summarize(
        business_disruption = sum(as.numeric(.data$business_disruption), na.rm = TRUE),
        .groups = "drop"
      ) |>
      dplyr::mutate(
        year = event$event_year,
        disruption_days = pmax(0, pmin(365, .data$business_disruption))
      )

    # Join and apply: revenue * (1 - disruption_days/365)
    result <- dplyr::left_join(result, disruption_map, by = c("asset", "year")) |>
      dplyr::mutate(
        revenue = dplyr::if_else(
          is.na(.data$disruption_days),
          .data$revenue,
          as.numeric(.data$revenue) * (1 - as.numeric(.data$disruption_days) / 365)
        )
      ) |>
      dplyr::select(-dplyr::any_of(c("disruption_days", "business_disruption")))
  }

  # Process agriculture assets
  # Apply damage factor THEN business disruption
  if (nrow(agriculture_assets) > 0) {
    agriculture_map <- agriculture_assets |>
      dplyr::group_by(.data$asset) |>
      dplyr::summarize(
        damage_factor = sum(as.numeric(.data$damage_factor), na.rm = TRUE),
        business_disruption = sum(as.numeric(.data$business_disruption), na.rm = TRUE),
        .groups = "drop"
      ) |>
      dplyr::mutate(
        year = event$event_year,
        disruption_days = pmax(0, pmin(365, .data$business_disruption))
      )

    # Join and apply:
    # Step 1: Apply damage factor: revenue * (1 - damage_factor)
    # Step 2: Apply business disruption: revenue * (1 - disruption_days/365)
    result <- dplyr::left_join(result, agriculture_map, by = c("asset", "year")) |>
      dplyr::mutate(
        revenue = dplyr::if_else(
          is.na(.data$damage_factor),
          .data$revenue,
          {
            # Step 1: Apply damage factor
            rev_after_damage <- as.numeric(.data$revenue) * (1 - as.numeric(.data$damage_factor))
            # Step 2: Apply business disruption
            rev_after_disruption <- rev_after_damage * (1 - as.numeric(.data$disruption_days) / 365)
            rev_after_disruption
          }
        )
      ) |>
      dplyr::select(-dplyr::any_of(c("damage_factor", "disruption_days", "business_disruption")))
  }

  return(result)
}

#' Apply Heat (heat) shock to revenue trajectories using Cobb-Douglas (internal function)
#'
#' @param yearly_trajectories tibble with columns: asset, company, year, revenue
#' @param event Single event row with event_id, event_year, hazard_type
#' @param assets_factors tibble with hazard data and damage factors
#' @return tibble with revenue adjusted for Heat labor productivity loss
#' @noRd
apply_compound_shock <- function(yearly_trajectories, event, assets_factors) {
  # Cobb-Douglas parameters (hardcoded from CD_inputs.csv)
  cd_params <- list(
    L0 = 339.2285,
    K0 = 87025023,
    E0 = 43.99034,
    lnA = 2.398,
    B1 = 0.602, # elasticity on ln(K)
    B2 = 0.455, # elasticity on ln(L)
    B3 = 0.147 # elasticity on ln(E)
  )

  # Calculate baseline output Y_base
  Y_base <- exp(cd_params$lnA +
    cd_params$B1 * log(cd_params$K0) +
    cd_params$B2 * log(cd_params$L0) +
    cd_params$B3 * log(cd_params$E0))


  # Filter Heat assets for this specific event (by event_id)
  compound_assets <- assets_factors |>
    dplyr::filter(
      .data$hazard_type == "Heat",
      .data$event_id == event$event_id
    )

  if (nrow(compound_assets) == 0) {
    return(yearly_trajectories)
  }

  # Calculate weighted LP loss per asset
  compound_effects <- compound_assets |>
    dplyr::mutate(
      # hazard_intensity = days with extreme heat
      # damage_factor = raw labor productivity loss (negative value from damage_and_cost_factors.csv)
      weighted_lp_loss = (as.numeric(.data$hazard_intensity) / 365) * as.numeric(.data$damage_factor)
    ) |>
    dplyr::group_by(.data$asset) |>
    dplyr::summarize(weighted_lp_loss = sum(.data$weighted_lp_loss, na.rm = TRUE), .groups = "drop") |>
    dplyr::mutate(
      year = event$event_year,
      # Adjust labor input: L_adj = L0 * (1 + weighted_lp_loss)
      # Note: weighted_lp_loss is negative, so this reduces L
      L_adjusted = cd_params$L0 * (1 + .data$weighted_lp_loss),
      # Calculate shocked output
      Y_shock = exp(cd_params$lnA +
        cd_params$B1 * log(cd_params$K0) +
        cd_params$B2 * log(.data$L_adjusted) +
        cd_params$B3 * log(cd_params$E0)),
      # Calculate relative change
      change = ((.data$Y_shock / Y_base) - 1)
    ) |>
    dplyr::select("asset", "year", "change")

  # Join and apply: revenue * (1 + change)
  result <- dplyr::left_join(yearly_trajectories, compound_effects, by = c("asset", "year")) |>
    dplyr::mutate(
      revenue = dplyr::if_else(
        is.na(.data$change),
        .data$revenue,
        as.numeric(.data$revenue) * (1 + as.numeric(.data$change))
      )
    ) |>
    dplyr::select(-"change")

  return(result)
}

#' Apply Drought shock to agriculture revenue trajectories (internal function)
#'
#' @param yearly_trajectories tibble with columns: asset, company, year, revenue
#' @param event Single event row with event_id, event_year, hazard_type
#' @param assets_factors tibble with hazard data and damage factors
#' @return tibble with revenue adjusted for Drought damage to agriculture
#' @noRd
apply_drought_shock <- function(yearly_trajectories, event, assets_factors) {
  # Filter Drought assets for this specific event (by event_id)
  # Drought only affects agriculture assets
  drought_assets <- assets_factors |>
    dplyr::filter(
      .data$hazard_type == "Drought",
      .data$event_id == event$event_id,
      .data$asset_category == "agriculture"
    )

  if (nrow(drought_assets) == 0) {
    return(yearly_trajectories)
  }

  # Aggregate damage factors by asset (in case there are multiple rows per asset)
  drought_map <- drought_assets |>
    dplyr::group_by(.data$asset) |>
    dplyr::summarize(
      damage_factor = mean(as.numeric(.data$damage_factor), na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(year = event$event_year)

  # Join and apply: revenue * (1 - damage_factor)
  result <- dplyr::left_join(yearly_trajectories, drought_map, by = c("asset", "year")) |>
    dplyr::mutate(
      revenue = dplyr::if_else(
        is.na(.data$damage_factor),
        .data$revenue,
        as.numeric(.data$revenue) * (1 - as.numeric(.data$damage_factor))
      )
    ) |>
    dplyr::select(-"damage_factor")

  return(result)
}

#' Apply Fire shock to revenue trajectories (agriculture only) (internal function)
#'
#' @description
#' Fire affects agriculture revenues using the formula:
#' Fire Damage = land_cover_risk × damage_factor(FWI) × (days_danger_total / 365)
#' revenue_shocked = revenue × (1 - Fire Damage)
#'
#' @param yearly_trajectories tibble with columns: asset, company, year, revenue
#' @param event Single event row with event_id, event_year, hazard_type
#' @param assets_factors tibble with hazard data and damage factors
#' @return tibble with revenue adjusted for Fire damage to agriculture
#' @noRd
apply_fire_revenue_shock <- function(yearly_trajectories, event, assets_factors) {

  # Filter Fire assets for agriculture only
  # Fire affects agriculture through revenue shock
  fire_assets <- assets_factors |>
    dplyr::filter(
      .data$hazard_type == "Fire",
      .data$event_id == event$event_id,
      .data$asset_category == "agriculture"
    )

  if (nrow(fire_assets) == 0) {
    return(yearly_trajectories)
  }

  # Calculate full fire damage formula using components
  # Fire Damage = land_cover_risk × damage_factor(FWI) × (days_danger_total / 365)
  fire_damage_map <- fire_assets |>
    dplyr::select("asset", "land_cover_risk", "damage_factor", "days_danger_total") |>
    dplyr::mutate(
      year = event$event_year,
      # Calculate fire damage percentage
      fire_damage_pct = as.numeric(.data$land_cover_risk) * 
                       as.numeric(.data$damage_factor) * 
                       (as.numeric(.data$days_danger_total) / 365)
    ) |>
    dplyr::select("asset", "year", "fire_damage_pct")

  # Join and apply: revenue * (1 - fire_damage_pct)
  result <- dplyr::left_join(yearly_trajectories, fire_damage_map, by = c("asset", "year")) |>
    dplyr::mutate(
      revenue = dplyr::if_else(
        is.na(.data$fire_damage_pct),
        .data$revenue,
        as.numeric(.data$revenue) * (1 - as.numeric(.data$fire_damage_pct))
      )
    ) |>
    dplyr::select(-"fire_damage_pct")

  return(result)
}
