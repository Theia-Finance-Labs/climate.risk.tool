#' Get primary indicator from hazard configs (internal)
#'
#' @param hazard_configs Named list of hazard configs
#' @param hazard_type Character hazard type name
#' @return Character primary indicator or NA_character_
#' @noRd
get_primary_indicator <- function(hazard_configs, hazard_type) {
  if (is.null(hazard_configs) || !hazard_type %in% names(hazard_configs)) {
    return(NA_character_)
  }
  hazard_configs[[hazard_type]]$primary_indicator
}

#' Get index indicator from hazard configs (internal)
#'
#' @param hazard_configs Named list of hazard configs
#' @param hazard_type Character hazard type name
#' @return Character index indicator or NA_character_
#' @noRd
get_index_indicator <- function(hazard_configs, hazard_type) {
  if (is.null(hazard_configs) || !hazard_type %in% names(hazard_configs)) {
    return(NA_character_)
  }
  cfg <- hazard_configs[[hazard_type]]
  if (!is.null(cfg$index_indicator) && nzchar(as.character(cfg$index_indicator))) {
    return(cfg$index_indicator)
  }
  cfg$primary_indicator
}

#' Get required indicators from hazard configs (internal)
#'
#' @param hazard_configs Named list of hazard configs
#' @param hazard_type Character hazard type name
#' @return Character vector of indicator keys or NULL
#' @noRd
get_required_indicators <- function(hazard_configs, hazard_type) {
  if (is.null(hazard_configs) || !hazard_type %in% names(hazard_configs)) {
    return(NULL)
  }
  names(hazard_configs[[hazard_type]]$indicators)
}

#' Check if hazard type is multi-indicator (internal)
#'
#' @param hazard_configs Named list of hazard configs
#' @param hazard_type Character hazard type name
#' @return Logical TRUE if hazard has multiple indicators
#' @noRd
is_multi_indicator_hazard <- function(hazard_configs, hazard_type) {
  indicators <- get_required_indicators(hazard_configs, hazard_type)
  if (is.null(indicators)) {
    return(FALSE)
  }
  length(indicators) > 1
}

#' Get spatial separation scheme for hazard configs (internal)
#'
#' @param hazard_configs Named list of hazard configs
#' @param hazard_type Character hazard type name
#' @return Character spatial separation scheme
#' @noRd
get_hazard_spatial_scheme <- function(hazard_configs, hazard_type) {
  if (is.null(hazard_configs) || !hazard_type %in% names(hazard_configs)) {
    return("adm_regions")
  }

  cfg <- hazard_configs[[hazard_type]]
  scheme <- cfg$spatial_separation_scheme
  if (is.null(scheme) || !nzchar(as.character(scheme))) {
    if (tolower(as.character(hazard_type)) == "flood") {
      return("hydro_regions")
    }
    return("adm_regions")
  }

  scheme <- tolower(as.character(scheme))
  if (!scheme %in% c("adm_regions", "hydro_regions")) {
    return("adm_regions")
  }
  scheme
}
