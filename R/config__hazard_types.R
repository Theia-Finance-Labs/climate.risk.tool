#' Hazard Type Configuration Registry
#'
#' @description Central configuration defining which indicators each hazard type requires,
#'   which indicator is primary (drives UI dropdowns), and metadata.
#'
#' @details
#' This configuration enables the system to handle both single-indicator hazards
#' (Flood, Heat, Drought) and multi-indicator hazards (Fire) uniformly.
#'
#' For multi-indicator hazards like Fire:
#' - Users select only: Hazard Type, Scenario, Return Period
#' - The primary_indicator determines which NetCDF file's scenario/RP options appear in UI
#' - All required indicators are automatically extracted internally
#'
#' @noRd
NULL

#' Get hazard type configuration
#'
#' @description Returns configuration mapping for all hazard types, specifying
#'   required indicators and which one is primary for UI purposes.
#'
#' @return Named list where each element is a hazard type configuration containing:
#'   - indicators: Character vector of required indicator names
#'   - primary_indicator: Character, the indicator that drives UI scenario/RP dropdowns
#'   - description: Character, human-readable description
#'
#' @noRd
get_hazard_type_config <- function() {
  list(
    Fire = list(
      indicators = c("land_cover", "FWI", "days_danger_total"),
      primary_indicator = "FWI", # FWI and days_danger_total share same scenario/RP
      description = "Fire risk combining land cover, fire weather index, and danger days"
    ),
    Flood = list(
      indicators = c("depth(cm)"),
      primary_indicator = "depth(cm)",
      description = "Flood depth in centimeters (TIF)"
    ),
    FloodNC = list(
      indicators = c("depth(cm)"),
      primary_indicator = "depth(cm)",
      description = "Flood depth in centimeters (NetCDF)"
    ),
    Heat = list(
      indicators = c("HI"), # Heat Index from NetCDF
      primary_indicator = "HI",
      description = "Heat climate hazard"
    ),
    Drought = list(
      indicators = c("SPI3"), # Standardized Precipitation Index 3-month from NetCDF
      primary_indicator = "SPI3",
      description = "Drought hazard"
    )
  )
}

#' Check if hazard type is multi-indicator
#'
#' @description Determines if a hazard type requires multiple indicators for
#'   damage calculation (e.g., Fire needs 3 indicators).
#'
#' @param hazard_type Character. Hazard type name (e.g., "Fire", "Flood")
#' @return Logical. TRUE if hazard requires multiple indicators, FALSE otherwise
#'
#' @noRd
is_multi_indicator_hazard <- function(hazard_type) {
  config <- get_hazard_type_config()
  if (!hazard_type %in% names(config)) {
    return(FALSE)
  }
  length(config[[hazard_type]]$indicators) > 1
}

#' Get primary indicator for a hazard type
#'
#' @description Returns the primary indicator for a hazard type. This indicator
#'   drives the scenario and return period options shown in the UI.
#'
#' @param hazard_type Character. Hazard type name
#' @return Character. Primary indicator name, or NA_character_ if hazard type not found
#'
#' @noRd
get_primary_indicator <- function(hazard_type) {
  config <- get_hazard_type_config()
  if (!hazard_type %in% names(config)) {
    return(NA_character_)
  }
  config[[hazard_type]]$primary_indicator
}

#' Get all required indicators for a hazard type
#'
#' @description Returns all indicators required for a hazard type's damage calculation.
#'
#' @param hazard_type Character. Hazard type name
#' @return Character vector of indicator names, or NULL if hazard type not found
#'
#' @noRd
get_required_indicators <- function(hazard_type) {
  config <- get_hazard_type_config()
  if (!hazard_type %in% names(config)) {
    return(NULL)
  }
  config[[hazard_type]]$indicators
}
