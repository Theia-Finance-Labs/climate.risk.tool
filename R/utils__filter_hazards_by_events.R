#' Filter hazards by events with NC ensemble handling
#'
#' @title Filter hazard rasters to match event requirements
#' @description Filters a list of hazard rasters to only those referenced by events.
#'   For NC hazards, only the 'mean' ensemble is loaded by default, so filtering
#'   matches the base hazard name without ensemble suffix.
#'
#' @param hazards Named list of SpatRaster objects (from load_hazards_and_inventory())
#' @param events Data frame with event specifications including hazard_name column
#'
#' @return Named list of filtered SpatRaster objects
#'
#' @details
#' The function handles two types of hazard filtering:
#' 
#' **TIF hazards**: Exact name matching
#' - If event specifies "flood__rcp85_h10glob", returns exactly that raster
#' 
#' **NC hazards**: Base name matching
#' - If event specifies "Drought__CDD__GWL=present__RP=5" (base event), returns the mean ensemble:
#'   - Drought__CDD__GWL=present__RP=5 (loaded as mean ensemble by default)
#' - If event already specifies "__ensemble=mean", strips the ensemble suffix and matches base name
#' 
#' This simplified approach avoids loading multiple ensemble variants and focuses on
#' the mean ensemble as the representative value for each hazard scenario.
#'
#' @examples
#' \dontrun{
#' # Load hazards (gets NC hazards with mean ensemble only)
#' result <- load_hazards_and_inventory(hazards_dir, aggregate_factor = 1L)
#' hazards <- c(result$hazards$tif, result$hazards$nc)
#' 
#' # Define events (base event names without ensemble suffix)
#' events <- data.frame(
#'   hazard_name = c("Drought__CDD__GWL=present__RP=5",
#'                   "Compound__FWI__GWL=2__RP=10"),
#'   event_year = c(2030, 2040),
#'   chronic = c(FALSE, FALSE)
#' )
#' 
#' # Filter: matches 2 events -> 2 hazard rasters (mean ensemble only)
#' filtered_hazards <- filter_hazards_by_events(hazards, events)
#' }
#' @export
filter_hazards_by_events <- function(hazards, events) {
  
  if (!tibble::is_tibble(events) && !is.data.frame(events)) {
    return(hazards)
  }
  
  available_names <- names(hazards)
  desired_names <- events |>
    dplyr::distinct(.data$hazard_name) |>
    dplyr::pull(.data$hazard_name) |>
    as.character() |>
    unique()
  
  # Exact matches (for TIF hazards and NC hazards with mean ensemble)
  exact_matches <- available_names[available_names %in% desired_names]
  
  # Pattern matches for NC hazards (base event name matches)
  # Since we only load mean ensemble, we match base names directly
  pattern_matches <- character()
  for (desired in desired_names) {
    # If desired name contains __ensemble=, strip it to get base event
    if (grepl("__ensemble=", desired)) {
      # Remove ensemble suffix to get base event
      base_event <- sub("__ensemble=.*$", "", desired)
      # Match the base event name (which represents mean ensemble)
      if (base_event %in% available_names) {
        pattern_matches <- c(pattern_matches, base_event)
      }
    } else {
      # Match the base event name directly (represents mean ensemble)
      if (desired %in% available_names) {
        pattern_matches <- c(pattern_matches, desired)
      }
    }
  }
  
  # Combine exact and pattern matches
  selected_names <- unique(c(exact_matches, pattern_matches))
  filtered_hazards <- hazards[selected_names]
  
  message("[filter_hazards_by_events] Filtered hazards: ", length(selected_names), 
          " hazard layers selected from ", length(available_names), 
          " available (", length(desired_names), " events requested)")
  
  return(filtered_hazards)
}

