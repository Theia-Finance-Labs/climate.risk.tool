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
  companies_df,
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
    c(adm1_names, "Other"),
    validation_results
  )

  # ============================================================================
  # VALIDATION CHECK 1b: Required columns per hazard type in damage factors
  # ============================================================================
  validation_results <- validate_damage_factors_required_fields(
    damage_factors_df = damage_factors_df,
    validation_results = validation_results
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
  # VALIDATION CHECK 2b: Companies coherence (no missing fields, no asset-less companies)
  # ============================================================================
  validation_results <- validate_companies_against_assets(
    companies_df = companies_df,
    assets_df = assets_df,
    validation_results = validation_results
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


#' Validate required non-NA fields per hazard type in damage factors (HARDCODED)
#'
#' Enforces specific required columns per `hazard_type`:
#' - Flood: hazard_intensity, hazard_unit, asset_category, damage_factor,
#'             cost_factor, hazard_indicator, business_disruption
#' - Drought:  hazard_intensity, hazard_unit, asset_category, damage_factor,
#'             hazard_indicator, province, subtype, season, off_window
#' - Compound: gwl, damage_factor, hazard_indicator, province, metric
#'
#' @param damage_factors_df Damage factors data frame
#' @param validation_results List with errors and warnings vectors
#' @return Updated validation_results list
#' @noRd
validate_damage_factors_required_fields <- function(damage_factors_df, validation_results) {
  if (!"hazard_type" %in% names(damage_factors_df)) {
    validation_results$errors <- c(
      validation_results$errors,
      "Damage factors table must contain 'hazard_type' column"
    )
    return(validation_results)
  }

  # Hardcoded mapping of hazard_type -> required columns
  required_by_hazard <- list(
    Flood = c(
      "hazard_intensity", "hazard_unit", "asset_category", "damage_factor",
      "cost_factor", "hazard_indicator", "business_disruption"
    ),
    Drought = c(
      "hazard_intensity", "hazard_unit", "asset_category", "damage_factor",
      "hazard_indicator", "province", "subtype", "season", "off_window"
    ),
    Compound = c(
      "gwl", "damage_factor", "hazard_indicator", "province", "metric"
    )
  )

  # Validate each known hazard type; ignore unknown types for forward-compatibility
  present_types <- intersect(names(required_by_hazard), unique(stats::na.omit(damage_factors_df$hazard_type)))

  for (hz in present_types) {
    hz_rows <- damage_factors_df[damage_factors_df$hazard_type == hz, , drop = FALSE]
    required_cols <- required_by_hazard[[hz]]

    # Check required columns presence first
    missing_columns <- setdiff(required_cols, names(hz_rows))
    if (length(missing_columns) > 0) {
      validation_results$errors <- c(
        validation_results$errors,
        paste0(
          "Damage factors: hazard_type '", hz, "' is missing required column(s): ",
          paste(missing_columns, collapse = ", ")
        )
      )
      next
    }

    # Now ensure non-NA/non-empty values for all rows in required columns
    for (col_name in required_cols) {
      col_vals <- hz_rows[[col_name]]
      is_missing <- is.na(col_vals) | (!is.na(col_vals) & !nzchar(trimws(as.character(col_vals))))

      # Exception: For Flood, cost_factor is not required when asset_category == "agriculture"
      if (hz == "Flood" && col_name == "cost_factor" && "asset_category" %in% names(hz_rows)) {
        exempt_idx <- which(tolower(as.character(hz_rows$asset_category)) == "agriculture")
        if (length(exempt_idx) > 0) {
          # Do not count missing on exempt rows
          is_missing[exempt_idx] <- FALSE
        }
      }
      if (any(is_missing)) {
        missing_idx <- which(is_missing)
        validation_results$errors <- c(
          validation_results$errors,
          paste0(
            "Damage factors: hazard_type '", hz, "' has missing required column '",
            col_name, "' in rows: ", paste(missing_idx, collapse = ", ")
          )
        )
      }
    }
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
  # Flag rows with no geographic information at all
  if (all(c("latitude", "longitude", "municipality", "province") %in% names(assets_df))) {
    no_geo_idx <- which(
      (is.na(assets_df$latitude) | is.null(assets_df$latitude)) &
        (is.na(assets_df$longitude) | is.null(assets_df$longitude)) &
        (is.na(assets_df$municipality) | is.null(assets_df$municipality) | !nzchar(trimws(as.character(assets_df$municipality)))) &
        (is.na(assets_df$province) | is.null(assets_df$province) | !nzchar(trimws(as.character(assets_df$province))))
    )
    if (length(no_geo_idx) > 0) {
      validation_results$errors <- c(
        validation_results$errors,
        paste0("Assets have no geographic information (lat/lon/municipality/province) for rows: ", paste(no_geo_idx, collapse = ", "))
      )
    }
  }

  # Normalize asset text fields to ASCII for comparison
  assets_df <- assets_df |>
    dplyr::mutate(
      municipality = dplyr::if_else(
        !is.na(.data$municipality) & nzchar(trimws(as.character(.data$municipality))),
        stringi::stri_trans_general(as.character(trimws(.data$municipality)), "Latin-ASCII"),
        .data$municipality
      ),
      province = dplyr::if_else(
        !is.na(.data$province) & nzchar(trimws(as.character(.data$province))),
        stringi::stri_trans_general(as.character(trimws(.data$province)), "Latin-ASCII"),
        .data$province
      )
    )
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
  # Check for missing share_of_economic_activity values
  if ("share_of_economic_activity" %in% names(assets_df)) {
    missing_share_rows <- which(is.na(assets_df$share_of_economic_activity))
    if (length(missing_share_rows) > 0) {
      validation_results$errors <- c(
        validation_results$errors,
        paste0("Missing share_of_economic_activity for rows: ", paste(missing_share_rows, collapse = ", "))
      )
    }
  }
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
            round(total_share, 4), " (should be 1.0 \u00b1 ", tolerance, ")"
          )
      )
    }
  }

  return(validation_results)
}


#' Validate companies against assets and check for missing values
#'
#' @param companies_df Companies data frame
#' @param assets_df Assets data frame
#' @param validation_results List with errors and warnings vectors
#' @return Updated validation_results list
#' @noRd
validate_companies_against_assets <- function(companies_df, assets_df, validation_results) {
  # If companies_df is missing or empty, warn and return
  if (missing(companies_df) || is.null(companies_df) || nrow(companies_df) == 0) {
    validation_results$warnings <- c(
      validation_results$warnings,
      "Companies data not provided; skipping company-specific validations"
    )
    return(validation_results)
  }

  # Normalize company name columns if present
  if ("company" %in% names(companies_df)) {
    companies_df$company <- as.character(companies_df$company)
  }
  if ("company" %in% names(assets_df)) {
    assets_df$company <- as.character(assets_df$company)
  }

  # 1) Company owning no assets
  if ("company" %in% names(companies_df) && "company" %in% names(assets_df)) {
    companies_with_assets <- unique(stats::na.omit(assets_df$company))
    companies_listed <- unique(stats::na.omit(companies_df$company))
    assetless_companies <- setdiff(companies_listed, companies_with_assets)
    if (length(assetless_companies) > 0) {
      validation_results$errors <- c(
        validation_results$errors,
        paste0("Companies with no assets: ", paste(assetless_companies, collapse = ", "))
      )
    }
  } else {
    validation_results$warnings <- c(
      validation_results$warnings,
      "Cannot validate companies vs assets: missing 'company' column in one of the datasets"
    )
  }

  # 2) Any missing value on company columns (row-wise)
  na_matrix <- is.na(companies_df)
  if (any(na_matrix)) {
    rows_with_na <- which(rowSums(na_matrix) > 0)
    # Try to include company names if present
    if ("company" %in% names(companies_df)) {
      missing_desc <- paste0(
        "[", rows_with_na, ": ", companies_df$company[rows_with_na], "]"
      )
    } else {
      missing_desc <- as.character(rows_with_na)
    }
    validation_results$errors <- c(
      validation_results$errors,
      paste0("Companies contain missing values in rows: ", paste(missing_desc, collapse = ", "))
    )
  }

  return(validation_results)
}
