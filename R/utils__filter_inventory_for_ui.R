#' Filter inventory for UI presentation
#'
#' @description Takes the full hazards inventory (which includes all indicators)
#'   and returns a UI-friendly version that only shows index indicators,
#'   hiding the multi-indicator complexity from users.
#'
#' @details
#' For single-indicator hazards (Flood, Heat, Drought):
#'   - Returns all rows as-is since index indicator = only indicator
#'
#' For multi-indicator hazards (Fire with land_cover, FWI, days_danger_total):
#'   - Returns only rows matching the index indicator (FWI)
#'   - This ensures UI dropdowns show scenario/RP combinations from FWI NetCDF
#'   - Other indicators (land_cover, days_danger_total) are handled internally
#'
#' @param inventory Tibble. Full inventory from load_hazards_and_inventory()$inventory
#'   Expected columns: hazard_type, hazard_indicator, scenario_name,
#'   return_period, hazard_name, ensemble, source
#' @param hazard_configs Named list from load_hazards_and_inventory()$configs
#'
#' @return Tibble with columns: hazard_type, hazard_indicator (primary only),
#'   scenario_name, return_period
#'   Rows are deduplicated to show unique combinations.
#'
#' @noRd
filter_inventory_for_ui <- function(inventory, hazard_configs) {
  # Get all configured hazard types
  configured_types <- names(hazard_configs)

  # Filter inventory to only include configured hazard types
  inventory_configured <- inventory |>
    dplyr::filter(.data$hazard_type %in% configured_types)

  if (nrow(inventory_configured) == 0) {
    return(tibble::tibble(
      hazard_type = character(),
      hazard_indicator = character(),
      scenario_name = character(),
      return_period = numeric()
    ))
  }

  # Get unique hazard types actually present in inventory
  present_types <- unique(inventory_configured$hazard_type)

  # For each hazard type present in inventory, keep only index indicator rows
  ui_inventory <- purrr::map_dfr(present_types, function(htype) {
    index_ind <- get_index_indicator(hazard_configs, htype)

    inventory_configured |>
      dplyr::filter(
        .data$hazard_type == htype,
        .data$hazard_indicator == index_ind
      ) |>
      dplyr::distinct()
  })

  message("[filter_inventory_for_ui] Filtered to ", nrow(ui_inventory), " UI-visible hazard combinations")

  return(ui_inventory)
}
