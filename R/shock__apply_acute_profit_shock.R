#' Apply Acute Shock to Yearly Profit Trajectories (Placeholder)
#'
#' @title Apply Acute Shock to Yearly Profit (Placeholder)
#' @description Placeholder function that passes through profit values unchanged.
#'   This is a temporary implementation that maintains the expected output structure
#'   while the actual shock calculation logic is being developed. Unlike revenue shocks,
#'   profit shocks may involve fixed costs or operational disruptions that aren't
#'   proportional to revenue impacts.
#' @param yearly_trajectories data.frame with columns: asset, company, year, shocked_revenue, shocked_profit
#' @param assets_factors data.frame with hazard data and cost factors (currently unused)
#' @param acute_events data.frame with acute event information (currently unused)
#' @return data.frame with columns: asset, company, year, shocked_revenue, shocked_profit
#' @examples
#' \dontrun{
#' yearly_trajectories <- data.frame(
#'   asset = c("A1", "A1"),
#'   company = c("C1", "C1"),
#'   year = c(2025, 2030),
#'   shocked_revenue = c(1000, 1200),
#'   shocked_profit = c(100, 120)
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

  # Check required columns in trajectory data
  required_trajectory_cols <- c("asset", "company", "year", "shocked_revenue", "shocked_profit")
  missing_trajectory_cols <- setdiff(required_trajectory_cols, names(yearly_trajectories))
  if (length(missing_trajectory_cols) > 0) {
    stop(paste(
      "Missing required columns in yearly_trajectories:",
      paste(missing_trajectory_cols, collapse = ", ")
    ))
  }

  #compute acute damages for floods in assets_factors (non-invasive; only adds a column)
  if (all(c("hazard_type", "damage_factor", "cost_factor") %in% names(assets_factors))) {
    assets_factors$acute_damage <- NA_real_
    flood_idx <- tolower(as.character(assets_factors$hazard_type)) == "flood"
    assets_factors$acute_damage[flood_idx] <-
      as.numeric(assets_factors$damage_factor[flood_idx]) *
      as.numeric(assets_factors$cost_factor[flood_idx])
  }

  # --- APPLY FLOOD ACUTE DAMAGE TO PROFITS ---
  # Requirements for mapping
  req_af_cols <- c("asset", "hazard_type", "hazard_name", "acute_damage")
  req_events_cols <- c("hazard_type", "hazard_name", "event_year")

  if (!all(req_af_cols %in% names(assets_factors))) {
    stop("assets_factors missing required columns: ",
         paste(setdiff(req_af_cols, names(assets_factors)), collapse = ", "))
  }
  if (!all(req_events_cols %in% names(acute_events))) {
    stop("acute_events missing required columns: ",
         paste(setdiff(req_events_cols, names(acute_events)), collapse = ", "))
  }

  # Select only floods from events
  flood_events <- acute_events[tolower(as.character(acute_events$hazard_type)) == "flood", , drop = FALSE]
  flood_events <- flood_events[!is.na(flood_events$event_year), , drop = FALSE]

  # Select only floods from assets_factors
  assets_flood <- assets_factors[tolower(as.character(assets_factors$hazard_type)) == "flood", , drop = FALSE]

  # Merge by hazard_name to attach event_year to each asset-hazard
  # -> (asset, hazard_name, event_year, acute_damage)
  if (nrow(assets_flood) > 0 && nrow(flood_events) > 0) {
    shock_map <- merge(
      assets_flood[, c("asset", "hazard_name", "acute_damage"), drop = FALSE],
      flood_events[, c("hazard_name", "event_year"), drop = FALSE],
      by = "hazard_name",
      all = FALSE
    )

    # Sum acute_damage per (asset, event_year) in case multiple hazards per asset-year
    if (nrow(shock_map) > 0) {
      shocks_by_asset_year <- stats::aggregate(
        acute_damage ~ asset + event_year,
        data = shock_map,
        FUN = function(x) sum(as.numeric(x), na.rm = TRUE)
      )
    } else {
      shocks_by_asset_year <- data.frame(asset = character(0), event_year = integer(0), acute_damage = numeric(0))
    }
  } else {
    shocks_by_asset_year <- data.frame(asset = character(0), event_year = integer(0), acute_damage = numeric(0))
  }

  # Start from trajectories
  result <- yearly_trajectories

  # Attach shock (by asset + year == event_year), then deduct from shocked_profit
  if (nrow(shocks_by_asset_year) > 0) {
    result <- merge(
      result,
      setNames(shocks_by_asset_year, c("asset", "year", "acute_damage_to_apply")),
      by = c("asset", "year"),
      all.x = TRUE,
      sort = FALSE
    )

    # Deduct (do not alter revenue here as per request)
    result$shocked_profit <- as.numeric(result$shocked_profit) -
      ifelse(is.na(result$acute_damage_to_apply), 0, as.numeric(result$acute_damage_to_apply))

    # Drop helper column
    result$acute_damage_to_apply <- NULL
  }

  # Ensure shocked values are non-negative
  result$shocked_profit <- pmax(0, result$shocked_profit)
  result$shocked_revenue <- pmax(0, result$shocked_revenue)

  # Return only the required columns
  result <- result[, c("asset", "company", "year", "shocked_revenue", "shocked_profit"), drop = FALSE]

  # Validate the result structure
  if (!is.numeric(result$shocked_profit) || !is.numeric(result$shocked_revenue)) {
    stop("Shocked values are not numeric")
  }

  return(result)
}

