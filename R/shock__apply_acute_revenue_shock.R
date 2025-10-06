#' Apply Acute Shock to Yearly Revenue Trajectories (Placeholder)
#'
#' @title Apply Acute Revenue Shock (Placeholder)
#' @description Placeholder function that passes through baseline revenue as shocked revenue.
#'   This is a temporary implementation that maintains the expected output structure
#'   while the actual shock calculation logic is being developed. NOTE: This function
#'   only affects REVENUE. Profit is computed separately using compute_profits_from_revenue().
#' @param yearly_trajectories data.frame with columns: asset, company, year, baseline_revenue (or shocked_revenue if already processed)
#' @param assets_factors data.frame with hazard data and damage factors (currently unused)
#' @param acute_events data.frame with acute event information (currently unused)
#' @return data.frame with columns: asset, company, year, shocked_revenue
#' @examples
#' \dontrun{
#' yearly_trajectories <- data.frame(
#'   asset = c("A1", "A1"),
#'   company = c("C1", "C1"),
#'   year = c(2025, 2030),
#'   baseline_revenue = c(1000, 1200),
#'   baseline_profit = c(100, 120)
#' )
#' assets_factors <- data.frame(asset = "A1", hazard_type = "flood", damage_factor = 0.1)
#' acute_events <- data.frame(event_id = "e1", hazard_type = "flood", event_year = 2030, chronic = FALSE)
#' # Apply revenue shock (returns only shocked_revenue, not profit)
#' result <- apply_acute_revenue_shock(yearly_trajectories, assets_factors, acute_events)
#' }
#' @export
apply_acute_revenue_shock <- function(
    yearly_trajectories,
    assets_factors,
    acute_events
) {
  # Validate inputs
  if (!is.data.frame(yearly_trajectories) || nrow(yearly_trajectories) == 0) {
    stop("yearly_trajectories must be a non-empty data.frame")
  }
  if (!is.data.frame(assets_factors) || nrow(assets_factors) == 0) {
    stop("assets_factors must be a non-empty data.frame")
  }
  if (!is.data.frame(acute_events) || nrow(acute_events) == 0) {
    stop("acute_events must be a non-empty data.frame")
  }

  # Required columns in trajectories
  required_base_cols <- c("asset", "company", "year")
  missing_base <- setdiff(required_base_cols, names(yearly_trajectories))
  if (length(missing_base) > 0) {
    stop(paste("Missing required columns in yearly_trajectories:",
               paste(missing_base, collapse = ", ")))
  }

  # We need a revenue source: prefer baseline_revenue; if absent, allow shocked_revenue as the baseline source
  if (!("baseline_revenue" %in% names(yearly_trajectories)) &&
      !("shocked_revenue" %in% names(yearly_trajectories))) {
    stop("yearly_trajectories must have either 'baseline_revenue' or 'shocked_revenue'")
  }

  # Prepare base revenue column (numeric)
  result <- yearly_trajectories
  if ("baseline_revenue" %in% names(result)) {
    base_rev <- as.numeric(result$baseline_revenue)
  } else {
    base_rev <- as.numeric(result$shocked_revenue)
  }
  if (!is.numeric(base_rev)) stop("Revenue column must be numeric")

  # --- Build flood disruption map (asset, event_year -> total disruption days) ---
  # Required columns in assets_factors and acute_events
  req_af_cols <- c("asset", "hazard_type", "hazard_name", "business_disruption")
  req_events_cols <- c("hazard_type", "hazard_name", "event_year")

  if (!all(req_af_cols %in% names(assets_factors))) {
    stop("assets_factors missing required columns: ",
         paste(setdiff(req_af_cols, names(assets_factors)), collapse = ", "))
  }
  if (!all(req_events_cols %in% names(acute_events))) {
    stop("acute_events missing required columns: ",
         paste(setdiff(req_events_cols, names(acute_events)), collapse = ", "))
  }

  # Keep only FLOOD rows from here. Additional hazards have to be integrated seperately
  flood_events <- acute_events[tolower(as.character(acute_events$hazard_type)) == "flood", , drop = FALSE]
  flood_events <- flood_events[!is.na(flood_events$event_year), , drop = FALSE]

  assets_flood <- assets_factors[tolower(as.character(assets_factors$hazard_type)) == "flood", , drop = FALSE]

  # Map by hazard_name, then sum per (asset, event_year)
  if (nrow(assets_flood) > 0 && nrow(flood_events) > 0) {
    shock_map <- merge(
      assets_flood[, c("asset", "hazard_name", "business_disruption"), drop = FALSE],
      flood_events[, c("hazard_name", "event_year"), drop = FALSE],
      by = "hazard_name",
      all = FALSE
    )

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

  # --- Apply disruption to revenue for matching (asset, year == event_year) ---
  result$shocked_revenue <- base_rev

  if (nrow(shocks_by_asset_year) > 0) {
    result <- merge(
      result,
      setNames(shocks_by_asset_year, c("asset", "year", "disruption_days")),
      by = c("asset", "year"),
      all.x = TRUE,
      sort = FALSE
    )

    # Apply formula only where disruption is present
    result$shocked_revenue <- ifelse(
      is.na(result$disruption_days),
      result$shocked_revenue,
      as.numeric(result$shocked_revenue) * (1 - as.numeric(result$disruption_days) / 365)
    )

    # Drop helper
    result$disruption_days <- NULL
  }

  # Ensure non-negative & numeric
  result$shocked_revenue <- as.numeric(result$shocked_revenue)
  result$shocked_revenue <- pmax(0, result$shocked_revenue)

  # Return only required columns
  result <- result[, c("asset", "company", "year", "shocked_revenue"), drop = FALSE]

  # Validate structure
  if (!is.numeric(result$shocked_revenue)) {
    stop("Calculated shocked_revenue is not numeric")
  }

  return(result)
}
