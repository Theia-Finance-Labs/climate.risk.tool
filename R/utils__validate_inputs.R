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
#' @param damage_factors_df Damage factors data frame
#' @param precomputed_hazards_df Optional precomputed hazards data frame
#' @param cnae_exposure_df CNAE exposure data frame
#' @param adm1_names Character vector of valid ADM1 (province) names (ASCII-normalized)
#' @param adm2_names Character vector of valid ADM2 (municipality) names (ASCII-normalized)
#' @return List with validation results containing `errors` and `warnings` character vectors.
#'   Stops execution if errors are found.
#' @examples
#' \dontrun{
#' # Load boundary names
#' adm1 <- load_adm1_province_names("tests/tests_data")
#' adm2 <- load_adm2_municipality_names("tests/tests_data")
#' # Validate
#' validate_input_coherence(assets, damage_factors, precomputed_hazards, cnae_exposure, adm1, adm2)
#' }
#' @export
validate_input_coherence <- function(
  assets_df,
  damage_factors_df,
  precomputed_hazards_df = NULL,
  cnae_exposure_df,
  adm1_names,
  adm2_names
) {
  
  message("[validate_input_coherence] Starting validation checks...")
  
  validation_results <- list(
    errors = character(),
    warnings = character()
  )
  
  # ============================================================================
  # VALIDATION CHECK 1: Province names in damage factors
  # ============================================================================
  validation_results <- validate_damage_factors_provinces(
    damage_factors_df,
    adm1_names,
    validation_results
  )
  
  # ============================================================================
  # VALIDATION CHECK 2: Province and municipality names in assets
  # ============================================================================
  validation_results <- validate_assets_geography(
    assets_df,
    adm1_names,
    adm2_names,
    validation_results
  )
  
  # ============================================================================
  # VALIDATION CHECK 3: Province and municipality in precomputed hazards (if provided)
  # ============================================================================
  if (!is.null(precomputed_hazards_df)) {
    validation_results <- validate_precomputed_hazards_geography(
      precomputed_hazards_df,
      adm1_names,
      adm2_names,
      validation_results
    )
  }
  
  # ============================================================================
  # VALIDATION CHECK 4: CNAE codes in assets exist in reference file
  # ============================================================================
  validation_results <- validate_cnae_codes(
    assets_df,
    cnae_exposure_df,
    validation_results
  )
  
  # ============================================================================
  # VALIDATION CHECK 5: Share of economic activity sums to 1 per company
  # ============================================================================
  validation_results <- validate_economic_activity_shares(
    assets_df,
    validation_results
  )
  
  # ============================================================================
  # Report results
  # ============================================================================
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


#' Load ADM1 (province) normalized names from boundaries file
#'
#' @param base_dir Base directory containing areas subdirectory
#' @return Character vector of normalized province names
#' @noRd
load_adm1_province_names <- function(base_dir) {
  province_path <- file.path(base_dir, "areas", "province", "geoBoundaries-BRA-ADM1_simplified.geojson")
  
  if (!file.exists(province_path)) {
    warning("[load_adm1_province_names] Province boundaries not found at: ", province_path)
    return(character(0))
  }
  
  provinces_sf <- sf::st_read(province_path, quiet = TRUE)
  
  # Normalize names same way as in assign_province_to_assets
  province_names <- provinces_sf |>
    dplyr::pull(.data$shapeName) |>
    as.character() |>
    stringi::stri_trans_general("Latin-ASCII")
  
  return(unique(province_names))
}


#' Load ADM2 (municipality) normalized names from boundaries file
#'
#' @param base_dir Base directory containing areas subdirectory
#' @return Character vector of normalized municipality names
#' @noRd
load_adm2_municipality_names <- function(base_dir) {
  municipality_path <- file.path(base_dir, "areas", "municipality", "geoBoundaries-BRA-ADM2_simplified.geojson")
  
  if (!file.exists(municipality_path)) {
    warning("[load_adm2_municipality_names] Municipality boundaries not found at: ", municipality_path)
    return(character(0))
  }
  
  municipalities_sf <- sf::st_read(municipality_path, quiet = TRUE)
  
  # Normalize names same way as in assign_province_to_assets
  municipality_names <- municipalities_sf |>
    dplyr::pull(.data$shapeName) |>
    as.character() |>
    stringi::stri_trans_general("Latin-ASCII")
  
  return(unique(municipality_names))
}


#' Validate damage factors province names against ADM1 boundaries
#'
#' @param damage_factors_df Damage factors data frame
#' @param adm1_names Character vector of valid ADM1 province names
#' @param validation_results List with errors and warnings vectors
#' @return Updated validation_results list
#' @noRd
validate_damage_factors_provinces <- function(damage_factors_df, adm1_names, validation_results) {
  
  if (length(adm1_names) == 0) {
    validation_results$warnings <- c(
      validation_results$warnings,
      "Cannot validate damage factors provinces: ADM1 boundaries not loaded"
    )
    return(validation_results)
  }
  
  # Get unique province names from damage factors (excluding "-" placeholder)
  df_provinces <- damage_factors_df |>
    dplyr::filter(!is.na(.data$province), .data$province != "-") |>
    dplyr::pull(.data$province) |>
    unique()
  
  # Normalize damage factor provinces for comparison
  df_provinces_normalized <- stringi::stri_trans_general(df_provinces, "Latin-ASCII")
  
  # Find mismatches
  invalid_provinces <- df_provinces_normalized[!df_provinces_normalized %in% adm1_names]
  
  if (length(invalid_provinces) > 0) {
    validation_results$errors <- c(
      validation_results$errors,
      paste0(
        "Damage factors contain province names not in ADM1 boundaries (after normalization): ",
        paste(invalid_provinces, collapse = ", ")
      )
    )
  }
  
  return(validation_results)
}


#' Validate assets geography (province and municipality) against ADM1/ADM2 boundaries
#'
#' @param assets_df Assets data frame
#' @param adm1_names Character vector of valid ADM1 province names
#' @param adm2_names Character vector of valid ADM2 municipality names
#' @param validation_results List with errors and warnings vectors
#' @return Updated validation_results list
#' @noRd
validate_assets_geography <- function(assets_df, adm1_names, adm2_names, validation_results) {
  
  # Validate provinces
  if (length(adm1_names) > 0) {
    asset_provinces <- assets_df |>
      dplyr::filter(!is.na(.data$province)) |>
      dplyr::pull(.data$province) |>
      unique()
    
    invalid_provinces <- asset_provinces[!asset_provinces %in% adm1_names]
    
    if (length(invalid_provinces) > 0) {
      validation_results$errors <- c(
        validation_results$errors,
        paste0(
          "Assets contain province names not in ADM1 boundaries: ",
          paste(invalid_provinces, collapse = ", ")
        )
      )
    }
  } else {
    validation_results$warnings <- c(
      validation_results$warnings,
      "Cannot validate asset provinces: ADM1 boundaries not loaded"
    )
  }
  
  # Validate municipalities
  if (length(adm2_names) > 0) {
    asset_municipalities <- assets_df |>
      dplyr::filter(!is.na(.data$municipality)) |>
      dplyr::pull(.data$municipality) |>
      unique()
    
    invalid_municipalities <- asset_municipalities[!asset_municipalities %in% adm2_names]
    
    if (length(invalid_municipalities) > 0) {
      validation_results$errors <- c(
        validation_results$errors,
        paste0(
          "Assets contain municipality names not in ADM2 boundaries: ",
          paste(invalid_municipalities, collapse = ", ")
        )
      )
    }
  } else {
    validation_results$warnings <- c(
      validation_results$warnings,
      "Cannot validate asset municipalities: ADM2 boundaries not loaded"
    )
  }
  
  return(validation_results)
}


#' Validate precomputed hazards geography against ADM1/ADM2 boundaries
#'
#' @param precomputed_hazards_df Precomputed hazards data frame
#' @param adm1_names Character vector of valid ADM1 province names
#' @param adm2_names Character vector of valid ADM2 municipality names
#' @param validation_results List with errors and warnings vectors
#' @return Updated validation_results list
#' @noRd
validate_precomputed_hazards_geography <- function(precomputed_hazards_df, adm1_names, adm2_names, validation_results) {
  
  # Validate provinces (if column exists)
  if ("region" %in% names(precomputed_hazards_df) && length(adm1_names) > 0) {
    # Check adm_level to see if these are provinces
    province_rows <- precomputed_hazards_df |>
      dplyr::filter(.data$adm_level == "ADM1", !is.na(.data$region))
    
    if (nrow(province_rows) > 0) {
      hazard_provinces <- province_rows |>
        dplyr::pull(.data$region) |>
        unique()
      
      invalid_provinces <- hazard_provinces[!hazard_provinces %in% adm1_names]
      
      if (length(invalid_provinces) > 0) {
        validation_results$errors <- c(
          validation_results$errors,
          paste0(
            "Precomputed hazards contain province names not in ADM1 boundaries: ",
            paste(invalid_provinces, collapse = ", ")
          )
        )
      }
    }
  }
  
  # Validate municipalities (if column exists)
  if ("region" %in% names(precomputed_hazards_df) && length(adm2_names) > 0) {
    municipality_rows <- precomputed_hazards_df |>
      dplyr::filter(.data$adm_level == "ADM2", !is.na(.data$region))
    
    if (nrow(municipality_rows) > 0) {
      hazard_municipalities <- municipality_rows |>
        dplyr::pull(.data$region) |>
        unique()
      
      invalid_municipalities <- hazard_municipalities[!hazard_municipalities %in% adm2_names]
      
      if (length(invalid_municipalities) > 0) {
        validation_results$errors <- c(
          validation_results$errors,
          paste0(
            "Precomputed hazards contain municipality names not in ADM2 boundaries: ",
            paste(invalid_municipalities, collapse = ", ")
          )
        )
      }
    }
  }
  
  return(validation_results)
}


#' Validate CNAE codes in assets against reference CNAE file
#'
#' @param assets_df Assets data frame
#' @param cnae_exposure_df CNAE exposure reference data frame
#' @param validation_results List with errors and warnings vectors
#' @return Updated validation_results list
#' @noRd
validate_cnae_codes <- function(assets_df, cnae_exposure_df, validation_results) {
  
  # Get unique CNAE codes from assets (excluding NA)
  asset_cnae_codes <- assets_df |>
    dplyr::filter(!is.na(.data$cnae)) |>
    dplyr::pull(.data$cnae) |>
    unique()
  
  if (length(asset_cnae_codes) == 0) {
    validation_results$warnings <- c(
      validation_results$warnings,
      "No CNAE codes found in assets data"
    )
    return(validation_results)
  }
  
  # Get valid CNAE codes from reference file
  valid_cnae_codes <- cnae_exposure_df |>
    dplyr::pull(.data$cnae) |>
    unique()
  
  # Find invalid codes
  invalid_cnae_codes <- asset_cnae_codes[!asset_cnae_codes %in% valid_cnae_codes]
  
  if (length(invalid_cnae_codes) > 0) {
    validation_results$errors <- c(
      validation_results$errors,
      paste0(
        "Assets contain CNAE codes not in reference file: ",
        paste(invalid_cnae_codes, collapse = ", ")
      )
    )
  }
  
  return(validation_results)
}


#' Validate that share of economic activity sums to 1 for each company
#'
#' @param assets_df Assets data frame
#' @param validation_results List with errors and warnings vectors
#' @return Updated validation_results list
#' @noRd
validate_economic_activity_shares <- function(assets_df, validation_results) {
  
  # Calculate sum of shares per company
  company_shares <- assets_df |>
    dplyr::group_by(.data$company) |>
    dplyr::summarize(
      total_share = sum(.data$share_of_economic_activity, na.rm = TRUE),
      n_assets = dplyr::n(),
      .groups = "drop"
    )
  
  # Allow small tolerance for floating point errors (0.01 = 1%)
  tolerance <- 0.01
  
  # Find companies with invalid shares
  invalid_companies <- company_shares |>
    dplyr::filter(abs(.data$total_share - 1) > tolerance)
  
  if (nrow(invalid_companies) > 0) {
    for (i in seq_len(nrow(invalid_companies))) {
      company_name <- invalid_companies$company[i]
      total_share <- invalid_companies$total_share[i]
      n_assets <- invalid_companies$n_assets[i]
      
      validation_results$errors <- c(
        validation_results$errors,
        paste0(
          "Company '", company_name, "' has ", n_assets, " assets with total share = ",
          round(total_share, 4), " (should be 1.0 ± ", tolerance, ")"
        )
      )
    }
  }
  
  return(validation_results)
}

