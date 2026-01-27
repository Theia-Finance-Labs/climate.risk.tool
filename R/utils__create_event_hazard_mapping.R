#' Create event-to-indicator mapping for joining with extracted assets
#'
#' @description Creates a mapping table from indicator_key to event information
#'   (event_id, event_year, scenario_name, return_period, season, hazard_name).
#'   For multi-indicator hazards (Fire), creates multiple rows per event
#'   (one per indicator). For single-indicator hazards, creates one row per event.
#'
#' @details
#' This function is used internally in compute_risk() to create a join table
#' between extracted assets (which have indicator_key per raster) and
#' user-defined events (one row per event, not per indicator).
#'
#' @param events Tibble. User-defined events (one row per event).
#'   Expected columns: event_id, hazard_type, scenario_name,
#'   return_period, event_year, season
#'
#' @param hazards_inventory Tibble. Full inventory from load_hazards_and_inventory()
#'   Expected columns: hazard_type, hazard_indicator, scenario_name,
#'   return_period, indicator_key
#'
#' @param hazard_configs Named list from load_hazards_and_inventory()$configs
#'
#' @return Tibble with columns: indicator_key, event_id, event_year,
#'   scenario_name, return_period, season, hazard_name.
#'   May have multiple rows per event_id for multi-indicator hazards.
#'
#' @noRd
create_event_hazard_mapping <- function(events, hazards_inventory, hazard_configs) {
  if (is.null(events) || nrow(events) == 0) {
    return(tibble::tibble(
      indicator_key = character(),
      event_id = character(),
      event_year = integer(),
      scenario_name = character(),
      return_period = numeric(),
      season = character(),
      hazard_name = character()
    ))
  }

  if (is.null(hazard_configs)) {
    return(tibble::tibble(
      indicator_key = character(),
      event_id = character(),
      event_year = integer(),
      scenario_name = character(),
      return_period = numeric(),
      season = character(),
      hazard_name = character()
    ))
  }

  multi_indicator_types <- names(hazard_configs)[
    vapply(names(hazard_configs), function(htype) is_multi_indicator_hazard(hazard_configs, htype), logical(1))
  ]

  # Process single-indicator events
  single_events <- events |>
    dplyr::filter(!(.data$hazard_type %in% multi_indicator_types))

  # Process multi-indicator events (expand to all indicators)
  multi_events <- events |>
    dplyr::filter(.data$hazard_type %in% multi_indicator_types)

  build_event_rows <- function(event_row, indicators) {
    purrr::map_dfr(indicators, function(indicator) {
      matched <- hazards_inventory |>
        dplyr::filter(
          tolower(.data$hazard_type) == tolower(event_row$hazard_type),
          .data$hazard_indicator == indicator
        )
      if (nrow(matched) == 0) {
        return(NULL)
      }

      index_cols <- hazard_configs[[event_row$hazard_type]]$indicators[[indicator]]$index
      filtered <- matched
      if (length(index_cols) > 0) {
        for (idx_col in index_cols) {
          if (!idx_col %in% names(event_row)) {
            if (idx_col == "gwl" && "scenario_name" %in% names(event_row)) {
              filtered <- filtered |>
                dplyr::filter(.data$scenario_name == event_row$scenario_name)
              next
            }
            if (idx_col == "scenario_name" && "gwl" %in% names(event_row)) {
              filtered <- filtered |>
                dplyr::filter(.data$scenario_name == event_row$gwl)
              next
            }
            filtered <- filtered[0, ]
            next
          }
          if (idx_col == "return_period") {
            event_rp_numeric <- as.numeric(event_row$return_period)
            filtered <- filtered |>
              dplyr::mutate(rp_numeric = as.numeric(.data$return_period)) |>
              dplyr::filter(.data$rp_numeric == event_rp_numeric)
          } else {
            filtered <- filtered |>
              dplyr::filter(.data[[idx_col]] == event_row[[idx_col]])
          }
        }
      }
      if (nrow(filtered) == 0) {
        filtered <- matched
      }

      # Start with all columns from event_row to ensure gwl and other indices are preserved
      res_row <- event_row |>
        dplyr::mutate(
          indicator_key = filtered$indicator_key[1],
          return_period = as.numeric(.data$return_period),
          hazard_name = filtered$hazard_name[1],
          season = if ("season" %in% names(event_row)) as.character(.data$season) else NA_character_
        )
      
      # Ensure it's a tibble
      tibble::as_tibble(res_row)
    })
  }

  single_rows <- purrr::map_dfr(seq_len(nrow(single_events)), function(i) {
    event <- single_events |> dplyr::slice(i)
    indicators <- get_required_indicators(hazard_configs, event$hazard_type)
    build_event_rows(event, indicators)
  })

  multi_rows <- purrr::map_dfr(seq_len(nrow(multi_events)), function(i) {
    event <- multi_events |> dplyr::slice(i)
    indicators <- get_required_indicators(hazard_configs, event$hazard_type)
    rows <- build_event_rows(event, indicators)
    
    # For multi-indicator hazards, use the indexing indicator's hazard_name for ALL indicators
    if (nrow(rows) > 0) {
      index_indicator <- get_index_indicator(hazard_configs, event$hazard_type)
      if (!is.null(index_indicator) && length(index_indicator) == 1 && !is.na(index_indicator)) {
        # Find the hazard_name from the indexing indicator
        # indicator_key format: "fire_weather_index__fwi__return_period=100__gwl=3__ensemble=mean"
        # Match at the start of the string
        index_row <- rows |> dplyr::filter(
          grepl(paste0("^", index_indicator, "__"), .data$indicator_key, fixed = FALSE)
        )
        if (nrow(index_row) > 0) {
          indexing_hazard_name <- index_row$hazard_name[1]
          # Apply this hazard_name to all indicators in this event
          rows <- rows |> dplyr::mutate(hazard_name = indexing_hazard_name)
        }
      }
    }
    
    rows
  })

  result <- dplyr::bind_rows(single_rows, multi_rows)
  return(result)
}
