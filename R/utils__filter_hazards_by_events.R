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

  available_names <- names(hazards)

  # Use hazard configs to expand to all indicators (not just primary)
  if (!is.null(hazards_inventory) && "hazard_type" %in% names(events) && !is.null(hazard_configs)) {
    desired_names <- character()

    for (i in seq_len(nrow(events))) {
      event <- events[i, ]
      required_indicators <- get_required_indicators(hazard_configs, event$hazard_type)
      if (is.null(required_indicators) || length(required_indicators) == 0) next

      for (indicator in required_indicators) {
        matched <- hazards_inventory |>
          dplyr::filter(
            .data$hazard_type == event$hazard_type,
            .data$hazard_indicator == indicator
          )

        if (nrow(matched) == 0) next

        index_cols <- hazard_configs[[event$hazard_type]]$indicators[[indicator]]$index
        if (length(index_cols) == 0) {
          desired_names <- c(desired_names, matched$hazard_name[1])
          next
        }

        filtered <- matched
        for (idx_col in index_cols) {
          # Handle gwl/scenario_name aliases
          if (!idx_col %in% names(event)) {
            if (idx_col == "gwl" && "scenario_name" %in% names(event)) {
              filtered <- filtered |>
                dplyr::filter(.data$scenario_name == event$scenario_name)
              next
            }
            if (idx_col == "scenario_name" && "gwl" %in% names(event)) {
              filtered <- filtered |>
                dplyr::filter(.data$scenario_name == event$gwl)
              next
            }
            filtered <- filtered[0, ]
            next
          }

          if (idx_col == "return_period") {
            event_rp_numeric <- as.numeric(event$return_period)
            filtered <- filtered |>
              dplyr::mutate(rp_numeric = as.numeric(.data$return_period)) |>
              dplyr::filter(.data$rp_numeric == event_rp_numeric)
          } else {
            filtered <- filtered |>
              dplyr::filter(.data[[idx_col]] == event[[idx_col]])
          }
        }

        if (nrow(filtered) > 0) {
          desired_names <- c(desired_names, filtered$hazard_name)
        }
      }
    }

    desired_names <- unique(desired_names)
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
    # Handle old GWL= naming in desired_names if they come from old event data
    base_event <- sub("__GWL=", "__scenario_name=", base_event)
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
