#' Create event-to-hazard mapping for joining with extracted assets
#'
#' @description Creates a mapping table from hazard_names (with extraction suffixes)
#'   to event information (event_id, event_year, season). For multi-indicator
#'   hazards (Fire), creates multiple rows per event (one per indicator). For
#'   single-indicator hazards, creates one row per event.
#'
#' @details
#' This function is used internally in compute_risk() to create a join table
#' between extracted assets (which have indicator-specific hazard_names) and
#' user-defined events (which have one row per event, not per indicator).
#'
#' For single-indicator hazards (Flood, Heat, Drought):
#'   - Appends extraction_method suffix to hazard_name
#'   - Returns one row per event
#'
#' For multi-indicator hazards (Fire):
#'   - Creates 3 rows per event (land_cover, FWI, days_danger_total)
#'   - Each row gets the appropriate hazard_name with extraction_method suffix
#'   - All 3 rows share the same event_id, event_year, season
#'
#' @param events Tibble. User-defined events (one row per event).
#'   Expected columns: event_id, hazard_type, hazard_name, scenario_name,
#'   hazard_return_period, event_year, season
#'
#' @param hazards_inventory Tibble. Full inventory from load_hazards_and_inventory()
#'   Expected columns: hazard_type, hazard_indicator, scenario_name,
#'   hazard_return_period, hazard_name
#'
#' @param aggregation_method Character. Extraction method for assets (e.g., "mean")
#'
#' @return Tibble with columns: hazard_name (with extraction suffix), event_id,
#'   event_year, season. May have multiple rows per event_id for multi-indicator hazards.
#'
#' @noRd
create_event_hazard_mapping <- function(events, hazards_inventory, aggregation_method) {
  if (is.null(events) || nrow(events) == 0) {
    return(tibble::tibble(
      hazard_name = character(),
      event_id = character(),
      event_year = integer(),
      season = character()
    ))
  }

  config <- get_hazard_type_config()
  multi_indicator_types <- names(config)[sapply(names(config), is_multi_indicator_hazard)]

  # For events that don't have hazard_name yet, look it up from inventory
  # This handles programmatic usage where user only provides hazard_type, scenario, RP
  if (!"hazard_name" %in% names(events)) {
    events_with_hazard_name <- purrr::map_dfr(seq_len(nrow(events)), function(i) {
      event_row <- events[i, ]

      # Get primary indicator for this hazard type
      primary_ind <- config[[event_row$hazard_type]]$primary_indicator

      # Find matching hazard_name in inventory
      matched <- hazards_inventory |>
        dplyr::filter(
          .data$hazard_type == event_row$hazard_type,
          .data$hazard_indicator == primary_ind,
          .data$scenario_name == event_row$scenario_name,
          as.numeric(.data$hazard_return_period) == as.numeric(event_row$hazard_return_period)
        )

      event_row$hazard_name <- if (nrow(matched) > 0) {
        matched$hazard_name[1]
      } else {
        NA_character_
      }

      event_row
    })

    events <- events_with_hazard_name
  }

  # Process single-indicator events
  single_events <- events |>
    dplyr::filter(!(.data$hazard_type %in% multi_indicator_types)) |>
    dplyr::mutate(
      hazard_name = paste0(.data$hazard_name, "__extraction_method=", aggregation_method)
    ) |>
    dplyr::select("hazard_name", "event_id", "event_year")

  # Process multi-indicator events (expand to all indicators)
  multi_events <- events |>
    dplyr::filter(.data$hazard_type %in% multi_indicator_types)

  if (nrow(multi_events) == 0) {
    return(single_events)
  }

  # Expand multi-indicator events
  expanded_multi <- purrr::map_dfr(seq_len(nrow(multi_events)), function(i) {
    event <- multi_events |> dplyr::slice(i)
    required_indicators <- config[[event$hazard_type]]$indicators

    # Create one row per required indicator
    purrr::map_dfr(required_indicators, function(indicator) {
      # Match from inventory to get correct hazard_name for this indicator
      matched <- hazards_inventory |>
        dplyr::filter(
          .data$hazard_type == event$hazard_type,
          .data$hazard_indicator == indicator
        )

      if (nrow(matched) == 0) {
        return(NULL)
      }

      # Determine hazard_name based on indicator type
      if (indicator == "land_cover") {
        # Static indicator: use inventory's scenario/RP
        base_hazard_name <- matched$hazard_name[1]
        extraction_method <- "mode" # land_cover uses mode (categorical)
      } else {
        # Dynamic indicator: match user-selected scenario/RP
        event_rp_numeric <- as.numeric(event$hazard_return_period)
        exact_match <- matched |>
          dplyr::mutate(rp_numeric = as.numeric(.data$hazard_return_period)) |>
          dplyr::filter(
            .data$scenario_name == event$scenario_name,
            .data$rp_numeric == event_rp_numeric
          )

        base_hazard_name <- if (nrow(exact_match) > 0) {
          exact_match$hazard_name[1]
        } else {
          matched$hazard_name[1]
        }
        extraction_method <- aggregation_method
      }

      # Append extraction_method suffix
      hazard_name_with_suffix <- paste0(base_hazard_name, "__extraction_method=", extraction_method)

      tibble::tibble(
        hazard_name = hazard_name_with_suffix,
        event_id = event$event_id,
        event_year = event$event_year
      )
    })
  })

  # Combine single and multi-indicator mappings
  result <- dplyr::bind_rows(single_events, expanded_multi)

  return(result)
}
