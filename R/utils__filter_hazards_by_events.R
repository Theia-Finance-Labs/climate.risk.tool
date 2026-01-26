#' Filter hazards by events using indicator keys
#'
#' @title Filter hazard rasters to match event requirements
#' @description Filters a list of hazard rasters to only those referenced by events.
#'   Uses hazards_inventory to map event-driven hazard selection to indicator_key,
#'   which is the internal key used to index raster hazards.
#'   For multi-indicator hazards (Fire), internally expands to load all required indicators.
#'
#' @param hazards Named list of SpatRaster objects (from load_hazards_and_inventory())
#' @param events Data frame with event specifications including hazard_type/scenario/return_period
#' @param hazards_inventory Hazard inventory from load_hazards_and_inventory()
#' @param hazard_configs Hazard config registry from load_hazards_and_inventory()$configs
#'
#' @return Named list of filtered SpatRaster objects
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

  if (is.null(hazards_inventory) || nrow(hazards_inventory) == 0) {
    stop("hazards_inventory is required to filter hazards by events.")
  }
  if (is.null(hazard_configs)) {
    stop("hazard_configs is required to filter hazards by events.")
  }

  desired_keys <- character()

  if (!is.null(events) && nrow(events) > 0 && "hazard_type" %in% names(events)) {
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
          desired_keys <- c(desired_keys, matched$indicator_key[1])
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
          desired_keys <- c(desired_keys, filtered$indicator_key)
        }
      }
    }
  }

  desired_keys <- unique(desired_keys)
  filtered_hazards <- hazards[available_names %in% desired_keys]

  message(
    "[filter_hazards_by_events] Filtered hazards: ", length(names(filtered_hazards)),
    " hazard layers selected from ", length(available_names),
    " available (", nrow(events), " events requested)"
  )

  return(filtered_hazards)
}
