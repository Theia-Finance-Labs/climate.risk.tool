#' Compute hazard events for assets (long format)
#'
#' @description Given assets, hazards and a list of events, compute per-asset
#' hazard intensities and flags per event and return a long format table. This
#' isolates the hazard application from financial computations.
#'
#' @param assets data.frame of assets (as read by read_assets())
#' @param hazards named list of SpatRaster from load_hazards()
#' @param areas list with municipalities and provinces used by geolocate_assets()
#' @param events data.frame with columns: event_id, hazard_type, scenario, event_year (or NA), chronic (logical)
#' @param damage_factors character path to CSV with damage/cost factors
#' @param verbose logical
#' @return data.frame with columns: asset_id, event_id, hazard_key, event_year, chronic, share_of_economic_activity
#'   representing the post-shock share for this event only. The baseline value is in the input assets.
#' @export
compute_hazard_events <- function(assets, hazards, areas, events, damage_factors, verbose = TRUE) {
  if (!is.data.frame(assets) || nrow(assets) == 0) stop("assets must be data.frame")
  if (!is.list(hazards) || length(hazards) == 0) stop("hazards must be list")
  if (!is.list(areas) || !all(c("municipalities", "provinces") %in% names(areas))) stop("areas invalid")
  if (!is.data.frame(events)) stop("events must be data.frame")
  required_cols <- c("event_id", "hazard_type", "scenario", "event_year", "chronic")
  if (!all(required_cols %in% names(events))) stop("events missing columns")
  # Construct hazard_key consistently for internal diagnostics
  events$hazard_key <- paste0(events$hazard_type, "__", events$scenario)

  assets_geo <- geolocate_assets(assets, hazards, areas$municipalities, areas$provinces)
  assets_cut <- cutout_hazards(assets_geo, hazards)
  assets_mean <- summarize_hazards(assets_cut)
  assets_factors <- join_damage_cost_factors(assets_mean, damage_factors)

  # For each event, compute acute/chronic and resulting share using existing helpers
  res_list <- lapply(seq_len(nrow(events)), function(i) {
    ev <- events[i, ]
    df <- assets_factors
    # Keep only columns related to the selected hazard key when joining factors downstream
    selected_key <- paste0(ev$hazard_type, "__", ev$scenario)
    hazard_cols <- grep("^hazard_.*|__.*", names(df), value = TRUE)
    # Also consider raw hazard names present before summarization suffixes
    raw_cols <- grep(selected_key, names(df), fixed = TRUE, value = TRUE)
    if (length(raw_cols) > 0) {
      not_sel <- setdiff(hazard_cols, raw_cols)
      if (length(not_sel) > 0) {
        for (col in not_sel) df[[col]] <- NA_real_
      }
    }
    # Optional: filter hazard columns to the selected hazard_type if present
    # If hazard columns include names like hazard_type__scenario_mean we could refine; for now we proceed generically
    # Apply acute if not chronic NA year provided
    if (!isTRUE(ev$chronic)) {
      df <- apply_acute_shock(df, shock_year = as.integer(ev$event_year))
      df$chronic_shock <- 0
    } else {
      df$acute_shock <- 0
      df <- apply_chronic_shock(df)
    }
    df2 <- compute_asset_impact(df)
    
    # Validate that required columns exist before selection
    required_cols <- c("asset", "company", "share_of_economic_activity")
    missing_cols <- setdiff(required_cols, names(df2))
    if (length(missing_cols) > 0) {
      stop(paste("Missing required columns after compute_asset_impact:", paste(missing_cols, collapse = ", ")))
    }
    
    out <- df2[, required_cols, drop = FALSE]
    out$event_id <- as.character(ev$event_id)
    out$hazard_type <- as.character(ev$hazard_type)
    out$scenario <- as.character(ev$scenario)
    out$hazard_key <- as.character(selected_key)
    out$event_year <- if (isTRUE(ev$chronic)) NA_integer_ else as.integer(ev$event_year)
    out$chronic <- isTRUE(ev$chronic)
    out
  })
  result <- do.call(rbind, res_list)
  result
}


