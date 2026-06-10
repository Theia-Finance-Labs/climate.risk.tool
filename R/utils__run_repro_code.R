#' @noRd
format_repro_path <- function(path) {
  gsub("\\", "/", as.character(path), fixed = TRUE)
}

#' @noRd
format_r_scalar <- function(x) {
  if (length(x) == 0 || is.null(x)) {
    return("NULL")
  }

  if (length(x) != 1) {
    stop("format_r_scalar() expects a scalar value")
  }

  if (is.na(x)) {
    if (inherits(x, "integer")) return("NA_integer_")
    if (is.numeric(x)) return("NA_real_")
    if (is.logical(x)) return("NA")
    return("NA_character_")
  }

  if (is.character(x)) {
    return(encodeString(as.character(x), quote = "\""))
  }

  if (inherits(x, "integer")) {
    return(paste0(as.integer(x), "L"))
  }

  if (is.numeric(x)) {
    return(format(as.numeric(x), trim = TRUE, scientific = FALSE, digits = 15))
  }

  if (is.logical(x)) {
    return(if (isTRUE(x)) "TRUE" else "FALSE")
  }

  encodeString(as.character(x), quote = "\"")
}

#' @noRd
format_r_vector <- function(x) {
  if (length(x) == 0) {
    if (inherits(x, "integer")) return("integer()")
    if (is.numeric(x)) return("numeric()")
    if (is.logical(x)) return("logical()")
    return("character()")
  }

  values <- vapply(seq_along(x), function(i) format_r_scalar(x[[i]]), character(1))
  paste0("c(", paste(values, collapse = ", "), ")")
}

#' @noRd
format_events_data_frame_code <- function(events) {
  events_df <- tibble::as_tibble(events)

  columns <- vapply(
    names(events_df),
    function(col) {
      paste0("  ", col, " = ", format_r_vector(events_df[[col]]))
    },
    character(1)
  )

  paste0(
    "events <- data.frame(\n",
    paste(columns, collapse = ",\n"),
    ",\n  stringsAsFactors = FALSE\n",
    ")"
  )
}

#' @noRd
build_run_repro_code <- function(run_spec) {
  if (is.null(run_spec) || !is.list(run_spec)) {
    return("Reproduction code will appear here once the current run inputs are available.")
  }

  base_dir <- run_spec$base_dir
  input_folder <- run_spec$input_folder
  events <- run_spec$events

  if (is.null(base_dir) || !nzchar(as.character(base_dir))) {
    return("Reproduction code unavailable: set a base directory first.")
  }

  if (is.null(input_folder) || !nzchar(as.character(input_folder))) {
    return("Reproduction code unavailable: select an input folder first.")
  }

  if (!is.data.frame(events) || nrow(events) == 0) {
    return("Reproduction code unavailable: add at least one hazard event first.")
  }

  growth_rate <- if (is.null(run_spec$growth_rate)) 0.02 else run_spec$growth_rate
  discount_rate <- if (is.null(run_spec$discount_rate)) 0.05 else run_spec$discount_rate
  risk_free_rate <- if (is.null(run_spec$risk_free_rate)) 0.02 else run_spec$risk_free_rate

  paste(
    c(
      "library(climate.risk.tool)",
      "library(dplyr)",
      "library(sf)",
      "",
      paste0("base_dir <- ", format_r_scalar(format_repro_path(base_dir))),
      paste0("input_folder <- ", format_r_scalar(format_repro_path(input_folder))),
      "",
      "assets <- read_assets(input_folder)",
      "companies <- read_companies(input_folder)",
      "",
      "hazard_data <- load_hazards_and_inventory(",
      "  hazards_dir = file.path(base_dir, \"hazards\", \"config\"),",
      "  hazard_indicators_dir = file.path(base_dir, \"hazards\", \"indicators\"),",
      "  hazards_override_path = file.path(base_dir, \"hazards\", \"config\", \"config_overrides.yml\"),",
      "  aggregate_factor = 1L",
      ")",
      "hazards <- hazard_data$hazards",
      "hazards_inventory <- hazard_data$inventory",
      "hazard_configs <- hazard_data$configs",
      "precomputed_hazards <- read_precomputed_hazards(base_dir, hazard_configs = hazard_configs)",
      "mapping_hazards <- names(hazard_configs)[vapply(",
      "  hazard_configs,",
      "  function(cfg) !is.null(cfg$mappings) && \"cnae_exposure\" %in% names(cfg$mappings),",
      "  logical(1)",
      ")]",
      "if (length(mapping_hazards) == 0) stop(\"Mandatory mapping 'cnae_exposure' not found in hazard configs\")",
      "cnae_exposure <- load_mapping_from_config(base_dir, hazard_configs, mapping_hazards[[1]], \"cnae_exposure\")",
      "",
      "adm1_boundaries <- sf::st_read(",
      "  file.path(base_dir, \"areas\", \"state\", \"geoBoundaries-BRA-ADM1_simplified.geojson\"),",
      "  quiet = TRUE",
      ")",
      "adm2_boundaries <- sf::st_read(",
      "  file.path(base_dir, \"areas\", \"municipality\", \"geoBoundaries-BRA-ADM2_simplified.geojson\"),",
      "  quiet = TRUE",
      ")",
      "",
      format_events_data_frame_code(events),
      "",
      "results <- compute_risk(",
      "  assets = assets,",
      "  companies = companies,",
      "  events = events,",
      "  hazards = hazards,",
      "  hazards_inventory = hazards_inventory,",
      "  precomputed_hazards = precomputed_hazards,",
      "  hazard_configs = hazard_configs,",
      "  hazards_dir = file.path(base_dir, \"hazards\", \"config\"),",
      "  cnae_exposure = cnae_exposure,",
      "  adm1_boundaries = adm1_boundaries,",
      "  adm2_boundaries = adm2_boundaries,",
      "  base_dir = base_dir,",
      "  validate_inputs = TRUE,",
      paste0("  growth_rate = ", format_r_scalar(growth_rate), ","),
      paste0("  discount_rate = ", format_r_scalar(discount_rate), ","),
      paste0("  risk_free_rate = ", format_r_scalar(risk_free_rate), ","),
      "  aggregation_method = \"mean\"",
      ")",
      "",
      "print(names(results))",
      "print(utils::head(results$assets_factors))",
      "print(utils::head(results$companies))",
      "print(utils::head(results$assets_yearly))",
      "print(utils::head(results$companies_yearly))"
    ),
    collapse = "\n"
  )
}
