#' Deep-merge list overrides (internal)
#'
#' @param base Base list
#' @param override Override list
#' @return Merged list
#' @noRd
deep_merge_lists <- function(base, override) {
  if (is.null(override)) {
    return(base)
  }
  if (!is.list(base) || !is.list(override)) {
    return(override)
  }

  merged <- base
  override_names <- names(override)
  if (is.null(override_names)) {
    return(override)
  }

  for (item_name in override_names) {
    merged[[item_name]] <- deep_merge_lists(merged[[item_name]], override[[item_name]])
  }

  return(merged)
}

#' Read hazard overrides from YAML (internal)
#'
#' @param hazards_override_path Character path to overrides YAML
#' @return Named list of overrides (empty list if not found)
#' @noRd
read_hazard_overrides <- function(hazards_override_path) {
  if (is.null(hazards_override_path) || !file.exists(hazards_override_path)) {
    return(list())
  }

  overrides <- yaml::read_yaml(hazards_override_path)
  if (is.null(overrides) || length(overrides) == 0) {
    return(list())
  }
  if (!is.list(overrides) || is.null(names(overrides))) {
    stop("hazard overrides must be a named list: ", hazards_override_path)
  }

  return(overrides)
}

#' Read a hazard configuration from YAML (internal)
#'
#' @param file_path Character path to a hazard config YAML file
#' @param hazard_name Character hazard name (derived from filename)
#' @param override_config Optional list with overrides for this hazard
#' @return List with normalized hazard configuration
#' @noRd
read_hazard_config <- function(file_path, hazard_name, override_config = NULL) {
  if (is.null(file_path) || !file.exists(file_path)) {
    stop("hazard config not found: ", file_path)
  }
  if (is.null(hazard_name) || !nzchar(as.character(hazard_name))) {
    stop("hazard name is required for config: ", file_path)
  }

  raw_config <- yaml::read_yaml(file_path)
  if (is.null(raw_config) || length(raw_config) == 0) {
    stop("hazard config missing indicators: ", file_path)
  }

  if (!is.null(override_config)) {
    raw_config <- deep_merge_lists(raw_config, override_config)
  }

  normalized <- normalize_hazard_config(raw_config, hazard_name, file_path)
  return(normalized)
}

#' Load all hazard configs from a hazards directory (internal)
#'
#' @param hazards_dir Character path to hazards/config folder containing hazard YAML files
#' @param hazards_override_path Optional path to a config_overrides.yml file.
#'   When NULL, defaults to hazards_dir/config_overrides.yml. Missing files are ignored.
#' @return Named list of hazard configs keyed by hazard name
#' @noRd
load_hazard_configs <- function(hazards_dir, hazards_override_path = NULL) {
  if (is.null(hazards_dir) || !dir.exists(hazards_dir)) {
    stop("hazards_dir does not exist: ", hazards_dir)
  }

  if (is.null(hazards_override_path)) {
    hazards_override_path <- file.path(hazards_dir, "config_overrides.yml")
  }
  overrides <- read_hazard_overrides(hazards_override_path)

  config_files <- list.files(
    hazards_dir,
    pattern = "\\.yml$",
    recursive = FALSE,
    full.names = TRUE
  )
  config_files <- config_files[basename(config_files) != "config_overrides.yml"]

  if (length(config_files) == 0) {
    stop("No hazard config .yml files found under: ", hazards_dir)
  }

  registry <- list()
  for (config_path in config_files) {
    hazard_name <- tools::file_path_sans_ext(basename(config_path))
    override_config <- NULL
    # Use case-insensitive matching for hazard names in overrides
    matching_override_name <- names(overrides)[tolower(names(overrides)) == tolower(hazard_name)]
    if (length(matching_override_name) > 0) {
      override_config <- overrides[[matching_override_name[1]]]
    }

    config <- read_hazard_config(config_path, hazard_name, override_config = override_config)
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
#' @param hazard_name Character hazard name from filename
#' @param file_path Character path for error context
#' @return Normalized config list
#' @noRd
normalize_hazard_config <- function(config, hazard_name, file_path = NULL) {
  if (is.null(hazard_name) || !nzchar(as.character(hazard_name))) {
    stop("hazard config missing name", if (!is.null(file_path)) paste0(": ", file_path) else "")
  }
  if (!is.null(config$name) && nzchar(as.character(config$name)) && !identical(as.character(config$name), as.character(hazard_name))) {
    stop(
      "hazard config name does not match filename (",
      config$name, " vs ", hazard_name, ")",
      if (!is.null(file_path)) paste0(": ", file_path) else ""
    )
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
    name = as.character(hazard_name),
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

