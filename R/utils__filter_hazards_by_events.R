#' Filter hazards by events with NC ensemble handling
#'
#' @title Filter hazard rasters to match event requirements
#' @description Filters a list of hazard rasters to only those referenced by events.
#'   For NC hazards, only the 'mean' ensemble is loaded by default, so filtering
#'   matches the base hazard name without ensemble suffix.
#'   For multi-indicator hazards (Fire), internally expands to load all required indicators.
#'
#' @param hazards Named list of SpatRaster objects (from load_hazards_and_inventory())
#' @param events Data frame with event specifications including hazard_name column
#' @param hazards_inventory Optional. Hazard inventory for multi-indicator expansion. If NULL, assumes single-indicator only.
#' @param hazard_configs Optional. Hazard config registry from load_hazards_and_inventory()$configs
#'
#' @return Named list of filtered SpatRaster objects
#'
#' @details
#' The function handles NetCDF hazard filtering:
#'
#' **NC hazards**: Base name matching
#' - If event specifies "Drought__CDD__GWL=present__RP=5" (base event), returns the mean ensemble:
#'   - Drought__CDD__GWL=present__RP=5__ensemble=mean (loaded as mean ensemble by default)
#' - If event already specifies "__ensemble=mean", strips the ensemble suffix and matches base name
#'
#' This simplified approach avoids loading multiple ensemble variants and focuses on
#' the mean ensemble as the representative value for each hazard scenario.
#'
#' @examples
#' \dontrun{
#' # Load hazards (gets NC hazards with mean ensemble only)
#' result <- load_hazards_and_inventory(hazards_dir, aggregate_factor = 1L)
#' hazards <- result$hazards
#'
#' # Define events (base event names without ensemble suffix)
#' events <- data.frame(
#'   hazard_name = c(
#'     "Drought__CDD__GWL=present__RP=5",
#'     "Compound__FWI__GWL=2__RP=10"
#'   ),
#'   event_year = c(2030, 2040)
#' )
#'
#' # Filter: matches 2 events -> 2 hazard rasters (mean ensemble only)
#' filtered_hazards <- filter_hazards_by_events(hazards, events)
#' }
#' @export
filter_hazards_by_events <- function(hazards, events, hazards_inventory = NULL, hazard_configs = NULL) {
  if (!tibble::is_tibble(events) && !is.data.frame(events)) {
    return(hazards)
  }

  available_names <- names(hazards)

  # For multi-indicator hazards, expand internally to get all required hazard_names
  # This doesn't modify the events dataframe - just gets the list of hazard_names to load
  if (!is.null(hazards_inventory) && "hazard_type" %in% names(events) && !is.null(hazard_configs)) {
    multi_indicator_types <- names(hazard_configs)[
      vapply(names(hazard_configs), function(htype) is_multi_indicator_hazard(hazard_configs, htype), logical(1))
    ]

    # Get hazard_names for single-indicator events (use as-is)
    single_indicator_names <- events |>
      dplyr::filter(!(.data$hazard_type %in% multi_indicator_types)) |>
      dplyr::pull(.data$hazard_name) |>
      as.character() |>
      unique()

    # Get hazard_names for multi-indicator events (expand to all required indicators)
    multi_indicator_events <- events |>
      dplyr::filter(.data$hazard_type %in% multi_indicator_types)

    multi_indicator_names <- character()
    if (nrow(multi_indicator_events) > 0) {
      for (i in seq_len(nrow(multi_indicator_events))) {
        event <- multi_indicator_events[i, ]
        required_indicators <- get_required_indicators(hazard_configs, event$hazard_type)

        for (indicator in required_indicators) {
          matched <- hazards_inventory |>
            dplyr::filter(
              .data$hazard_type == event$hazard_type,
              .data$hazard_indicator == indicator
            )

          if (nrow(matched) == 0) next

          # Handle static vs dynamic indicators
          if (indicator == "land_cover") {
            hazard_name <- matched$hazard_name[1]
          } else {
            event_rp_numeric <- as.numeric(event$return_period)
            exact_match <- matched |>
              dplyr::mutate(rp_numeric = as.numeric(.data$return_period)) |>
              dplyr::filter(
                .data$gwl == event$gwl,
                .data$rp_numeric == event_rp_numeric
              )

            hazard_name <- if (nrow(exact_match) > 0) {
              exact_match$hazard_name[1]
            } else {
              matched$hazard_name[1]
            }
          }

          multi_indicator_names <- c(multi_indicator_names, hazard_name)
        }
      }
    }

    desired_names <- unique(c(single_indicator_names, multi_indicator_names))
  } else {
    # Fallback: no inventory provided, use hazard_name as-is
    desired_names <- events |>
      dplyr::distinct(.data$hazard_name) |>
      dplyr::pull(.data$hazard_name) |>
      as.character() |>
      unique()
  }

  # Exact matches (for NC hazards with mean ensemble)
  exact_matches <- available_names[available_names %in% desired_names]

  # Pattern matches for NC hazards (base event name matches)
  # Since we only load mean ensemble, we match base names directly
  pattern_matches <- character()
  for (desired in desired_names) {
    desired <- as.character(desired)

    # Normalize requested ensemble (or missing ensemble) to loaded ensemble=mean
    desired_mean <- if (grepl("__ensemble=", desired)) {
      sub("__ensemble=.*$", "__ensemble=mean", desired)
    } else {
      paste0(desired, "__ensemble=mean")
    }

    # Also consider the base event name (without ensemble suffix)
    base_event <- sub("__ensemble=.*$", "", desired)
    base_event_mean <- paste0(base_event, "__ensemble=mean")

    candidates <- unique(c(desired, desired_mean, base_event, base_event_mean))
    matched <- candidates[candidates %in% available_names]

    if (length(matched) > 0) {
      # Prefer the mean-ensemble match if available
      if (desired_mean %in% matched) {
        pattern_matches <- c(pattern_matches, desired_mean)
      } else if (base_event_mean %in% matched) {
        pattern_matches <- c(pattern_matches, base_event_mean)
      } else {
        pattern_matches <- c(pattern_matches, matched[1])
      }
    }
  }

  # Combine exact and pattern matches
  selected_names <- unique(c(exact_matches, pattern_matches))
  filtered_hazards <- hazards[selected_names]

  message(
    "[filter_hazards_by_events] Filtered hazards: ", length(selected_names),
    " hazard layers selected from ", length(available_names),
    " available (", length(desired_names), " events requested)"
  )

  return(filtered_hazards)
}
