#' Validate Input Data Coherence
#'
#' @title Validate all input data for coherence and consistency
#' @description Performs comprehensive validation checks on input data including:
#'   - Province names in damage factors match ADM1 boundaries (after normalization)
#'   - Province and municipality names in assets match ADM1/ADM2 boundaries
#'   - Province and municipality names in precomputed hazards match ADM1/ADM2
#'   - CNAE codes in assets exist in reference CNAE file
#'   - Share of economic activity sums to 1 for each company
#' @param assets_df Assets data frame
#' @param companies_df Companies data frame
#' @param damage_factors_df Damage factors data frame
#' @param precomputed_hazards_df Optional precomputed hazards data frame
#' @param cnae_exposure_df CNAE exposure data frame
#' @param adm1_names Character vector of valid ADM1 (province) names (ASCII-normalized)
#' @param adm2_names Character vector of valid ADM2 (municipality) names (ASCII-normalized)
#' @param events_df Optional events data frame to validate hazard-specific coverage
#' @return List with validation results containing `errors` and `warnings` character vectors.
#'   Stops execution if errors are found.
#' @examples
#' \dontrun{
#' # Load boundary names
#' adm1 <- load_adm1_state_names("tests/tests_data")
#' adm2 <- load_adm2_municipality_names("tests/tests_data")
#' # Validate
#' validate_input_coherence(assets, damage_factors, precomputed_hazards, cnae_exposure, adm1, adm2)
#' }
#' @export
validate_input_coherence <- function(
  assets_df,
  companies_df,
  hazards_dir,
  hazard_configs,
  precomputed_hazards_df = NULL,
  adm1_names,
  adm2_names,
  events_df = NULL
) {
  message("[validate_input_coherence] Starting validation checks...")

  base_dir <- if (!is.null(hazards_dir)) {
    dirname(dirname(hazards_dir))
  } else {
    NULL
  }
  geo_mapping <- if (!is.null(base_dir)) {
    load_geo_code_mapping(base_dir)
  } else {
    tibble::tibble()
  }

  validation_results <- list(
    errors = character(),
    warnings = character()
  )

  if (is.null(hazards_dir) || !dir.exists(hazards_dir)) {
    validation_results$errors <- c(validation_results$errors, "hazards_dir does not exist")
  }
  if (is.null(hazard_configs) || length(hazard_configs) == 0) {
    validation_results$errors <- c(validation_results$errors, "hazard_configs is empty")
  }

  validation_results <- validate_required_input_columns(
    assets_df = assets_df,
    companies_df = companies_df,
    validation_results = validation_results
  )

  validation_results <- validate_assets_geography(
    assets_df,
    adm1_names,
    adm2_names,
    validation_results,
    geo_mapping = geo_mapping
  )

  validation_results <- validate_companies_against_assets(
    companies_df = companies_df,
    assets_df = assets_df,
    validation_results = validation_results
  )

  validation_results <- validate_mapping_tables_against_config(
    hazards_dir = hazards_dir,
    hazard_configs = hazard_configs,
    validation_results = validation_results
  )

  if (!is.null(precomputed_hazards_df)) {
    validation_results <- validate_precomputed_hazards_geography(
      precomputed_hazards_df,
      adm1_names,
      adm2_names,
      validation_results,
      assets_df = assets_df,
      events_df = events_df,
      hazard_configs = hazard_configs,
      geo_mapping = geo_mapping
    )
  }

  validation_results <- validate_economic_activity_shares(
    assets_df,
    validation_results
  )

  validation_results <- validate_events_table(
    events_df,
    validation_results
  )

  validation_results <- validate_events_index_columns(
    events_df = events_df,
    hazard_configs = hazard_configs,
    validation_results = validation_results
  )

  n_errors <- length(validation_results$errors)
  n_warnings <- length(validation_results$warnings)

  if (n_warnings > 0) {
    message("[validate_input_coherence] Found ", n_warnings, " warning(s):")
    for (w in validation_results$warnings) {
      message("  WARNING: ", w)
    }
  }

  if (n_errors > 0) {
    error_msg <- paste0(
      "[validate_input_coherence] Found ", n_errors, " validation error(s):\n",
      paste0("  ERROR: ", validation_results$errors, collapse = "\n")
    )
    stop(error_msg)
  }

  if (n_errors == 0 && n_warnings == 0) {
    message("[validate_input_coherence] All validation checks passed!")
  }

  return(validation_results)
}
