#' Validate required input columns for assets and companies
#'
#' @param assets_df Assets data frame
#' @param companies_df Companies data frame
#' @param validation_results List with errors and warnings vectors
#' @return Updated validation_results list
#' @noRd
validate_required_input_columns <- function(assets_df, companies_df, validation_results) {
  catalog <- get_input_columns_catalog()

  if (!is.null(assets_df)) {
    missing_assets <- setdiff(catalog$assets_required, names(assets_df))
    if (length(missing_assets) > 0) {
      validation_results$errors <- c(
        validation_results$errors,
        paste0(
          "Assets table is missing required column(s): ",
          paste(missing_assets, collapse = ", ")
        )
      )
    }
  }

  if (!is.null(companies_df)) {
    missing_companies <- setdiff(catalog$companies_required, names(companies_df))
    if (length(missing_companies) > 0) {
      validation_results$errors <- c(
        validation_results$errors,
        paste0(
          "Companies table is missing required column(s): ",
          paste(missing_companies, collapse = ", ")
        )
      )
    }
  }

  validation_results
}


#' Validate CNAE exposure mapping schema and availability
#'
#' @param cnae_exposure_df CNAE exposure reference data frame
#' @param validation_results List with errors and warnings vectors
#' @return Updated validation_results list
#' @noRd
validate_cnae_exposure_mapping <- function(cnae_exposure_df, validation_results) {
  if (is.null(cnae_exposure_df) || !is.data.frame(cnae_exposure_df) || nrow(cnae_exposure_df) == 0) {
    validation_results$errors <- c(
      validation_results$errors,
      "CNAE exposure mapping is required and must be a non-empty data frame"
    )
    return(validation_results)
  }

  required_cols <- c("cnae", "description")
  missing_cols <- setdiff(required_cols, names(cnae_exposure_df))
  if (length(missing_cols) > 0) {
    validation_results$errors <- c(
      validation_results$errors,
      paste0(
        "CNAE exposure mapping is missing required column(s): ",
        paste(missing_cols, collapse = ", ")
      )
    )
  }

  validation_results
}


#' Validate CNAE codes in assets against reference CNAE file
#'
#' @param assets_df Assets data frame
#' @param cnae_exposure_df CNAE exposure reference data frame
#' @param validation_results List with errors and warnings vectors
#' @return Updated validation_results list
#' @noRd
validate_cnae_codes <- function(assets_df, cnae_exposure_df, validation_results) {
  if (!"cnae" %in% names(assets_df)) {
    return(validation_results)
  }

  # Get unique CNAE codes from assets (excluding NA)
  asset_cnae_codes <- assets_df |>
    dplyr::filter(!is.na(.data$cnae)) |>
    dplyr::pull(.data$cnae) |>
    as.character() |>
    trimws()
  asset_cnae_codes <- asset_cnae_codes[!is.na(asset_cnae_codes) & nzchar(asset_cnae_codes)]
  asset_cnae_codes <- asset_cnae_codes |>
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
    as.character() |>
    trimws()
  valid_cnae_codes <- valid_cnae_codes[!is.na(valid_cnae_codes) & nzchar(valid_cnae_codes)]
  valid_cnae_codes <- valid_cnae_codes |>
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


#' Validate that code-like sector/CNAE values resolve to a full sector label
#'
#' @param assets_df Assets data frame
#' @param cnae_exposure_df CNAE exposure reference data frame
#' @param validation_results List with errors and warnings vectors
#' @return Updated validation_results list
#' @noRd
validate_sector_resolution <- function(assets_df, cnae_exposure_df, validation_results) {
  assets_with_sector <- attach_sector_metadata(assets_df, cnae_exposure_df)

  if (!"sector_code" %in% names(assets_with_sector) || !"sector_name" %in% names(assets_with_sector)) {
    validation_results$errors <- c(
      validation_results$errors,
      "Unable to validate sector resolution: sector metadata columns were not produced"
    )
    return(validation_results)
  }

  unresolved <- assets_with_sector |>
    dplyr::mutate(
      sector_code = trimws(as.character(.data$sector_code)),
      sector_name = trimws(as.character(.data$sector_name))
    ) |>
    dplyr::filter(
      !is.na(.data$sector_code),
      nzchar(.data$sector_code),
      is.na(.data$sector_name) | !nzchar(.data$sector_name)
    )

  if (nrow(unresolved) > 0) {
    row_ref <- if ("asset" %in% names(unresolved)) {
      paste0("asset '", unresolved$asset, "'")
    } else {
      paste0("row ", seq_len(nrow(unresolved)))
    }
    details <- paste0(row_ref, " (code: ", unresolved$sector_code, ")")
    validation_results$errors <- c(
      validation_results$errors,
      paste0(
        "Assets contain unresolved sector codes with no CNAE description: ",
        paste(unique(details), collapse = ", ")
      )
    )
  }

  validation_results
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
  if (!"share_of_economic_activity" %in% names(assets_df)) {
    validation_results$errors <- c(
      validation_results$errors,
      "Assets table is missing required column 'share_of_economic_activity'"
    )
    return(validation_results)
  }

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

  # 2) Any missing value on required company columns (row-wise).
  # Optional columns (fi, growth_rate, …) are excluded so that a user
  # who leaves them blank for some rows does not trigger a validation error.
  optional_company_cols <- c("fi", "growth_rate")
  required_cols_present <- setdiff(names(companies_df), optional_company_cols)
  na_matrix <- is.na(companies_df[, required_cols_present, drop = FALSE])
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
