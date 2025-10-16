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
#' @param raster_mapping Tibble mapping unified hazard_name to internal raster keys (optional)
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
filter_hazards_by_events <- function(hazards, events, raster_mapping = NULL) {
  if (!tibble::is_tibble(events) && !is.data.frame(events)) {
    return(hazards)
  }
  
  available_names <- names(hazards)
  desired_names <- events |>
    dplyr::distinct(.data$hazard_name) |>
    dplyr::pull(.data$hazard_name) |>
    as.character() |>
    unique()
  
  # Separate TIF hazards (old format) from NC hazards (new format)
  # TIF hazards use old format: hazard_type__scenario_code_hreturn_periodglob
  # NC hazards use new format: hazard_type__hazard_indicator__GWL=X__RP=Y__ensemble=Z
  tif_pattern <- "_h[0-9]+glob$"
  tif_hazards <- available_names[grepl(tif_pattern, available_names)]
  nc_hazards <- available_names[!grepl(tif_pattern, available_names)]
  
  # For TIF hazards: use raster_mapping to convert unified names to old format keys
  tif_matches <- character()
  if (!is.null(raster_mapping) && nrow(raster_mapping) > 0) {
    tif_mapping_subset <- raster_mapping |>
      dplyr::filter(.data$source == "tif", .data$hazard_name %in% desired_names)
    
    if (nrow(tif_mapping_subset) > 0) {
      tif_matches <- tif_mapping_subset |>
        dplyr::pull(.data$raster_key) |>
        unique()
      
      # Only keep matches that exist in available hazards
      tif_matches <- tif_matches[tif_matches %in% tif_hazards]
    }
  } else {
    # Fallback: include all TIF hazards if no mapping provided
    tif_matches <- tif_hazards
  }
  
  # Exact matches (for NC hazards with exact names)
  exact_matches <- nc_hazards[nc_hazards %in% desired_names]
  
  # Pattern matches for NC hazards (base event name matches, different ensemble values)
  # For each desired name, also match names that start with the desired name + "__ensemble="
  pattern_matches <- character()
  for (desired in desired_names) {
    # If desired name contains __ensemble=, strip it to get base event
    if (grepl("__ensemble=", desired)) {
      # Remove ensemble suffix to get base event
      base_event <- sub("__ensemble=.*$", "", desired)
      # Match ALL ensemble variants for this base event in NC hazards
      pattern <- paste0("^", gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", base_event), "__ensemble=")
      matches <- grep(pattern, nc_hazards, value = TRUE)
      pattern_matches <- c(pattern_matches, matches)
    } else {
      # Match all ensemble variants for this base event in NC hazards
      pattern <- paste0("^", gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", desired), "__ensemble=")
      matches <- grep(pattern, nc_hazards, value = TRUE)
      pattern_matches <- c(pattern_matches, matches)
    }
  }
  
  # Combine TIF, exact matches, and pattern matches
  selected_names <- unique(c(tif_matches, exact_matches, pattern_matches))
  filtered_hazards <- hazards[selected_names]
  
  message("[filter_hazards_by_events] Filtered hazards: ", length(selected_names), 
          " hazard layers selected from ", length(available_names), 
          " available (", length(desired_names), " events requested)")
  
  return(filtered_hazards)
}

