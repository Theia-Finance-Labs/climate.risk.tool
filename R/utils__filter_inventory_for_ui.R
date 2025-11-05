#' Filter inventory for UI presentation
#'
#' @description Takes the full hazards inventory (which includes all indicators)
#'   and returns a UI-friendly version that only shows primary indicators,
#'   hiding the multi-indicator complexity from users.
#'
#' @details
#' For single-indicator hazards (Flood, Compound, Drought):
#'   - Returns all rows as-is since primary = only indicator
#'
#' For multi-indicator hazards (Fire with land_cover, FWI, days_danger_total):
#'   - Returns only rows matching the primary indicator (FWI)
#'   - This ensures UI dropdowns show scenario/RP combinations from FWI NetCDF
#'   - Other indicators (land_cover, days_danger_total) are handled internally
#'
#' @param inventory Tibble. Full inventory from load_hazards_and_inventory()$inventory
#'   Expected columns: hazard_type, hazard_indicator, scenario_name,
#'   hazard_return_period, scenario_code, hazard_name, ensemble, source
#'
#' @return Tibble with columns: hazard_type, hazard_indicator (primary only),
#'   scenario_name, hazard_return_period, scenario_code
#'   Rows are deduplicated to show unique combinations.
#'
#' @noRd
filter_inventory_for_ui <- function(inventory) {
  if (is.null(inventory) || nrow(inventory) == 0) {
    return(tibble::tibble(
      hazard_type = character(),
      hazard_indicator = character(),
      scenario_name = character(),
      hazard_return_period = numeric(),
      scenario_code = character()
    ))
  }

  config <- get_hazard_type_config()

  # Get all configured hazard types
  configured_types <- names(config)

  # Filter inventory to only include configured hazard types
  inventory_configured <- inventory |>
    dplyr::filter(.data$hazard_type %in% configured_types)

  if (nrow(inventory_configured) == 0) {
    return(tibble::tibble(
      hazard_type = character(),
      hazard_indicator = character(),
      scenario_name = character(),
      hazard_return_period = numeric(),
      scenario_code = character()
    ))
  }

  # Get unique hazard types actually present in inventory
  present_types <- unique(inventory_configured$hazard_type)
  
  # For each hazard type present in inventory, keep only primary indicator rows
  ui_inventory <- purrr::map_dfr(present_types, function(htype) {
    primary_ind <- config[[htype]]$primary_indicator

    inventory_configured |>
      dplyr::filter(
        .data$hazard_type == htype,
        .data$hazard_indicator == primary_ind
      ) |>
      dplyr::select(
        "hazard_type",
        "hazard_indicator",
        "scenario_name",
        "hazard_return_period",
        "scenario_code"
      ) |>
      dplyr::distinct()
  })

  message("[filter_inventory_for_ui] Filtered to ", nrow(ui_inventory), " UI-visible hazard combinations")

  return(ui_inventory)
}

