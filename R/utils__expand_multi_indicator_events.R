#' Expand multi-indicator hazard events
#'
#' @description For hazards requiring multiple indicators (e.g., Fire needs land_cover,
#'   FWI, days_danger_total), this function takes user-selected events (which only
#'   specify hazard_type, scenario, return_period) and expands them into multiple
#'   internal events, one per required indicator.
#'
#' @details
#' Single-indicator hazards (Flood, Heat, Drought):
#'   - Pass through unchanged
#'   - Already have correct hazard_indicator set
#'
#' Multi-indicator hazards (Fire):
#'   - User selects: Fire, SSP2-4.5, RP=10
#'   - System expands to 3 events:
#'     1. Fire/land_cover (scenario=present, RP=0 - static TIF)
#'     2. Fire/FWI (scenario=SSP2-4.5, RP=10)
#'     3. Fire/days_danger_total (scenario=SSP2-4.5, RP=10)
#'   - All 3 events share same event_id and event_year
#'   - Each gets correct hazard_name from inventory
#'
#' @param events Tibble. User-configured events from mod_hazards_events_server.
#'   Expected columns: event_id, hazard_type, hazard_indicator (primary),
#'   hazard_name (primary), gwl, return_period,
#'   event_year, season
#'
#' @param hazards_inventory Tibble. Full inventory with all indicators from
#'   load_hazards_and_inventory()$inventory.
#'   Expected columns: hazard_type, hazard_indicator, gwl,
#'   return_period, hazard_name, ensemble, source
#'
#' @param hazard_configs Named list from load_hazards_and_inventory()$configs
#'
#' @return Tibble. Expanded events with same structure as input. Multi-indicator
#'   events will have multiple rows per event_id (one per indicator).
#'
#' @noRd
expand_multi_indicator_events <- function(events, hazards_inventory, hazard_configs) {
  if (is.null(events) || nrow(events) == 0) {
    return(events)
  }

  if (!"hazard_type" %in% names(events)) {
    warning("[expand_multi_indicator_events] events missing 'hazard_type' column, returning as-is")
    return(events)
  }

  # Identify which hazard types are multi-indicator
  if (is.null(hazard_configs)) {
    return(events)
  }

  multi_indicator_types <- names(hazard_configs)[
    vapply(names(hazard_configs), function(htype) is_multi_indicator_hazard(hazard_configs, htype), logical(1))
  ]

  message("[expand_multi_indicator_events] Multi-indicator hazard types: ", paste(multi_indicator_types, collapse = ", "))

  # Separate multi-indicator from single-indicator events
  multi_events <- events |>
    dplyr::filter(.data$hazard_type %in% multi_indicator_types)

  single_events <- events |>
    dplyr::filter(!(.data$hazard_type %in% multi_indicator_types))

  if (nrow(multi_events) == 0) {
    message("[expand_multi_indicator_events] No multi-indicator events to expand")
    return(events)
  }

  message("[expand_multi_indicator_events] Expanding ", nrow(multi_events), " multi-indicator events...")

  # Expand each multi-indicator event
  expanded <- purrr::map_dfr(seq_len(nrow(multi_events)), function(i) {
    event <- multi_events |> dplyr::slice(i)
    required_indicators <- get_required_indicators(hazard_configs, event$hazard_type)

    message(
      "  Expanding event ", event$event_id, " (", event$hazard_type, ") into ",
      length(required_indicators), " indicator events"
    )

    # Create one event per required indicator
    purrr::map_dfr(required_indicators, function(indicator) {
      new_event <- event
      new_event$hazard_indicator <- indicator

      # Match from inventory to get correct scenario/RP/hazard_name for this indicator
      matched <- hazards_inventory |>
        dplyr::filter(
          .data$hazard_type == event$hazard_type,
          .data$hazard_indicator == indicator
        )

      if (nrow(matched) == 0) {
        warning(
          "  No inventory entry found for ",
          event$hazard_type, "/", indicator,
          " - skipping this indicator"
        )
        return(NULL)
      }

      # Special handling for static indicators (e.g., land_cover)
      # Static indicators have their own fixed scenario/RP that differs from user selection
      if (indicator == "land_cover") {
        # Use the scenario/RP from inventory for this static indicator
        new_event$gwl <- matched$gwl[1]
        new_event$return_period <- as.numeric(matched$return_period[1])
        new_event$hazard_name <- matched$hazard_name[1]

        message(
          "    ", indicator, ": using static values (scenario=",
          new_event$gwl, ", RP=", new_event$return_period, ")"
        )
      } else {
        # For dynamic indicators (FWI, days_danger_total), use user-selected scenario/RP
        # Convert return period to numeric for comparison and assignment
        event_rp_numeric <- as.numeric(event$return_period)
        # Find exact match in inventory (convert inventory RP to numeric for comparison)
        exact_match <- matched |>
          dplyr::mutate(rp_numeric = as.numeric(.data$return_period)) |>
          dplyr::filter(
            .data$gwl == event$gwl,
            .data$rp_numeric == event_rp_numeric
          )

        if (nrow(exact_match) > 0) {
          new_event$hazard_name <- exact_match$hazard_name[1]
          new_event$return_period <- event_rp_numeric
          message(
            "    ", indicator, ": using user-selected values (scenario=",
            event$gwl, ", RP=", event_rp_numeric, ")"
          )
        } else {
          warning(
            "  No exact match in inventory for ",
            event$hazard_type, "/", indicator,
            " with gwl=", event$gwl,
            ", RP=", event_rp_numeric
          )
          # Fall back to first available for this indicator
          new_event$hazard_name <- matched$hazard_name[1]
          new_event$return_period <- as.numeric(matched$return_period[1])
          message("    ", indicator, ": using fallback hazard_name")
        }
      }

      new_event
    })
  })

  # Ensure return_period is numeric in both single and expanded events before binding
  if (nrow(single_events) > 0) {
    single_events <- single_events |>
      dplyr::mutate(return_period = as.numeric(.data$return_period))
  }

  # Expanded events already have numeric return_period from above

  # Combine single-indicator events with expanded multi-indicator events
  result <- dplyr::bind_rows(single_events, expanded)

  message(
    "[expand_multi_indicator_events] Expanded from ", nrow(events),
    " user events to ", nrow(result), " internal events"
  )

  return(result)
}
