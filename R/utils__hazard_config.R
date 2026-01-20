#' Read a hazard configuration from YAML (internal)
#'
#' @param file_path Character path to a hazard.yml file
#' @return List with normalized hazard configuration
#' @noRd
read_hazard_config <- function(file_path) {
  if (is.null(file_path) || !file.exists(file_path)) {
    stop("hazard config not found: ", file_path)
  }

  raw_config <- yaml::read_yaml(file_path)
  if (is.null(raw_config) || length(raw_config) == 0) {
    stop("hazard config is empty: ", file_path)
  }

  normalized <- normalize_hazard_config(raw_config, file_path)
  return(normalized)
}

#' Load all hazard configs from a hazards directory (internal)
#'
#' @param hazards_dir Character path to hazards folder containing hazard.yml files
#' @return Named list of hazard configs keyed by hazard name
#' @noRd
load_hazard_configs <- function(hazards_dir) {
  if (is.null(hazards_dir) || !dir.exists(hazards_dir)) {
    stop("hazards_dir does not exist: ", hazards_dir)
  }

  config_files <- list.files(
    hazards_dir,
    pattern = "hazard\\.yml$",
    recursive = TRUE,
    full.names = TRUE
  )

  if (length(config_files) == 0) {
    stop("No hazard.yml files found under: ", hazards_dir)
  }

  registry <- list()
  for (config_path in config_files) {
    config <- read_hazard_config(config_path)
    if (config$name %in% names(registry)) {
      stop("Duplicate hazard name in configs: ", config$name)
    }
    registry[[config$name]] <- config
  }

  return(registry)
}

#' Normalize and validate hazard config structure (internal)
#'
#' @param config List parsed from YAML
#' @param file_path Character path for error context
#' @return Normalized config list
#' @noRd
normalize_hazard_config <- function(config, file_path = NULL) {
  if (is.null(config$name) || !nzchar(as.character(config$name))) {
    stop("hazard config missing name", if (!is.null(file_path)) paste0(": ", file_path) else "")
  }

  if (is.null(config$indicators) || length(config$indicators) == 0) {
    stop("hazard config missing indicators", if (!is.null(file_path)) paste0(": ", file_path) else "")
  }

  indicators <- list()
  for (indicator_key in names(config$indicators)) {
    indicator <- config$indicators[[indicator_key]]
    if (is.null(indicator$file) || !nzchar(as.character(indicator$file))) {
      stop("indicator '", indicator_key, "' missing file", if (!is.null(file_path)) paste0(": ", file_path) else "")
    }

    indicator_source <- if (grepl("\\.nc$", indicator$file, ignore.case = TRUE)) "nc" else "tif"

    indicator_index <- character(0)
    if (!is.null(indicator$index)) {
      indicator_index <- as.character(unlist(indicator$index, use.names = FALSE))
    }

    indicator_fixed <- list()
    if (!is.null(indicator$fixed)) {
      indicator_fixed <- indicator$fixed
    }

    indicator_agg <- indicator$agg
    if (is.null(indicator_agg) || !nzchar(as.character(indicator_agg))) {
      indicator_agg <- "mean"
    }

    indicator_variable <- indicator$variable
    if (is.null(indicator_variable) || !nzchar(as.character(indicator_variable))) {
      indicator_variable <- indicator_key
    }

    indicator_categorical <- FALSE
    if (!is.null(indicator$categorical)) {
      indicator_categorical <- isTRUE(indicator$categorical)
    }

    indicators[[indicator_key]] <- list(
      key = indicator_key,
      file = as.character(indicator$file),
      variable = as.character(indicator_variable),
      index = indicator_index,
      fixed = indicator_fixed,
      agg = as.character(indicator_agg),
      categorical = indicator_categorical,
      source = indicator_source
    )
  }

  mappings <- list()
  if (!is.null(config$mappings) && length(config$mappings) > 0) {
    for (mapping_key in names(config$mappings)) {
      mapping <- config$mappings[[mapping_key]]
      if (is.null(mapping$file) || !nzchar(as.character(mapping$file))) {
        stop("mapping '", mapping_key, "' missing file", if (!is.null(file_path)) paste0(": ", file_path) else "")
      }

      join <- mapping$join
      if (is.null(join)) {
        join <- list()
      }

      on_intensity <- character(0)
      on_hazard <- character(0)
      on_assets <- character(0)
      if (!is.null(join$on_intensity)) on_intensity <- as.character(unlist(join$on_intensity, use.names = FALSE))
      if (!is.null(join$on_hazard)) on_hazard <- as.character(unlist(join$on_hazard, use.names = FALSE))
      if (!is.null(join$on_assets)) on_assets <- as.character(unlist(join$on_assets, use.names = FALSE))

      intensity_match <- mapping$intensity_match
      if (is.null(intensity_match) || !nzchar(as.character(intensity_match))) {
        intensity_match <- "exact"
      }

      mappings[[mapping_key]] <- list(
        key = mapping_key,
        file = as.character(mapping$file),
        intensity_match = as.character(intensity_match),
        join = list(
          on_intensity = on_intensity,
          on_hazard = on_hazard,
          on_assets = on_assets
        )
      )
    }
  }

  primary_indicator <- config$primary_indicator
  if (is.null(primary_indicator) || !nzchar(as.character(primary_indicator))) {
    primary_indicator <- names(indicators)[[1]]
  }
  if (!primary_indicator %in% names(indicators)) {
    stop("primary_indicator '", primary_indicator, "' not found in indicators")
  }

  normalized <- list(
    name = as.character(config$name),
    indicators = indicators,
    mappings = mappings,
    primary_indicator = as.character(primary_indicator),
    path = file_path
  )

  return(normalized)
}

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

