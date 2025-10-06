#' Apply Acute Shock to Yearly Revenue Trajectories (Placeholder)
#'
#' @title Apply Acute Revenue Shock (Placeholder)
#' @description Placeholder function that passes through baseline revenue as shocked revenue.
#'   This is a temporary implementation that maintains the expected output structure
#'   while the actual shock calculation logic is being developed. NOTE: This function
#'   only affects REVENUE. Profit is computed separately using compute_profits_from_revenue().
#' @param yearly_trajectories data.frame with columns: asset, company, year, revenue
#' @param assets_factors data.frame with hazard data and damage factors (currently unused)
#' @param acute_events data.frame with acute event information (currently unused)
#' @return data.frame with columns: asset, company, year, revenue
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
    acute_events
) {

  # --- Build flood disruption map (asset, event_year -> total disruption days) ---

  # Keep only FLOOD rows from here. Additional hazards have to be integrated seperately
  flood_events <- acute_events[tolower(as.character(acute_events$hazard_type)) == "flood", , drop = FALSE]

  assets_flood <- assets_factors[tolower(as.character(assets_factors$hazard_type)) == "flood", , drop = FALSE]

  # Map by hazard_name, then sum per (asset, event_year)
  if (nrow(assets_flood) > 0 && nrow(flood_events) > 0) {
    shock_map <- merge(
      assets_flood[, c("asset", "hazard_name", "business_disruption"), drop = FALSE],
      flood_events[, c("hazard_name", "event_year"), drop = FALSE],
      by = "hazard_name",
      all = FALSE
    )

    # Remove rows with NA in key columns before aggregating
    shock_map <- shock_map[!is.na(shock_map$asset) & !is.na(shock_map$event_year), , drop = FALSE]
    
    if (nrow(shock_map) > 0) {
      shocks_by_asset_year <- stats::aggregate(
        business_disruption ~ asset + event_year,
        data = shock_map,
        FUN = function(x) sum(as.numeric(x), na.rm = TRUE)
      )
      # Cap summed disruption into [0, 365]
      shocks_by_asset_year$business_disruption <- pmax(0, pmin(365, as.numeric(shocks_by_asset_year$business_disruption)))
    } else {
      shocks_by_asset_year <- data.frame(asset = character(0), event_year = integer(0), business_disruption = numeric(0))
    }
  } else {
    shocks_by_asset_year <- data.frame(asset = character(0), event_year = integer(0), business_disruption = numeric(0))
  }


  # Start with the baseline trajectories
  result <- yearly_trajectories
  
  if (nrow(shocks_by_asset_year) > 0) {
    result <- merge(
      result,
      setNames(shocks_by_asset_year, c("asset", "year", "disruption_days")),
      by = c("asset", "year"),
      all.x = TRUE,
      sort = FALSE
    )

    # Apply formula only where disruption is present
    result$revenue <- ifelse(
      is.na(result$disruption_days),
      result$revenue,
      as.numeric(result$revenue) * (1 - as.numeric(result$disruption_days) / 365)
    )

    # Drop helper
    result$disruption_days <- NULL
  }


  return(result)
}
