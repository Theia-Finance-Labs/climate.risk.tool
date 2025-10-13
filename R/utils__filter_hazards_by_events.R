#' Filter hazards by events with NC ensemble expansion
#'
#' @title Filter hazard rasters to match event requirements
#' @description Filters a list of hazard rasters to only those referenced by events.
#'   For NC hazards with ensemble dimensions, automatically expands to include ALL
#'   ensemble variants (mean, median, p10, p90, etc.) to enable complete statistics
#'   extraction.
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
#' **NC hazards**: Ensemble expansion
#' - If event specifies "Drought__CDD__GWL=present__RP=5" (base event), returns ALL variants:
#'   - Drought__CDD__GWL=present__RP=5__ensemble=mean
#'   - Drought__CDD__GWL=present__RP=5__ensemble=median
#'   - Drought__CDD__GWL=present__RP=5__ensemble=p10
#'   - Drought__CDD__GWL=present__RP=5__ensemble=p90
#' - If event already specifies "__ensemble=mean", still expands to ALL variants
#' 
#' This expansion ensures that the extraction function can populate all statistic
#' columns (hazard_mean, hazard_median, hazard_p10, hazard_p90, etc.) from the
#' pre-computed NC ensemble layers.
#'
#' @examples
#' \dontrun{
#' # Load hazards (gets 800 NC hazards with ensemble variants)
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
#' # Filter: expands 2 events -> 8 hazard rasters (2 × 4 ensemble variants)
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
  
  # Exact matches (for TIF hazards)
  exact_matches <- available_names[available_names %in% desired_names]
  
  # Pattern matches for NC hazards (base event name matches, different ensemble values)
  # For each desired name, also match names that start with the desired name + "__ensemble="
  # We ALWAYS expand to all ensemble variants to get complete statistics
  pattern_matches <- character()
  for (desired in desired_names) {
    # If desired name contains __ensemble=, strip it to get base event
    if (grepl("__ensemble=", desired)) {
      # Remove ensemble suffix to get base event
      base_event <- sub("__ensemble=.*$", "", desired)
      # Match ALL ensemble variants for this base event
      pattern <- paste0("^", gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", base_event), "__ensemble=")
      matches <- grep(pattern, available_names, value = TRUE)
      pattern_matches <- c(pattern_matches, matches)
    } else {
      # Match all ensemble variants for this base event
      pattern <- paste0("^", gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", desired), "__ensemble=")
      matches <- grep(pattern, available_names, value = TRUE)
      pattern_matches <- c(pattern_matches, matches)
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

