#' Create color palette for categorical data
#'
#' @param n_categories Integer. Number of categories
#' @param palette_name Character. RColorBrewer palette name (default: "Set3")
#' @return Character vector of colors
#' @noRd
create_color_palette <- function(n_categories, palette_name = "Set3") {
  if (n_categories <= 1) {
    return("#009C3B")
  }

  if (n_categories <= 12) {
    return(RColorBrewer::brewer.pal(min(n_categories, 12), palette_name))
  }

  # For more than 12 categories, interpolate
  base_colors <- RColorBrewer::brewer.pal(12, palette_name)
  grDevices::colorRampPalette(base_colors)(n_categories)
}

#' Prepare profit trajectory data for plotting
#'
#' @param assets_yearly Data frame with yearly asset data
#' @param scenario Character. Scenario name ("baseline" or shock event_id)
#' @param asset_metadata Optional data frame with asset-level metadata (asset, company, share_of_economic_activity, sector_name, etc.)
#' @return Data frame formatted for plotly
#' @noRd
prepare_profit_trajectories <- function(assets_yearly, scenario, asset_metadata = NULL) {
  message("[prepare_profit_trajectories] Called with scenario: ", scenario)
  message("[prepare_profit_trajectories] assets_yearly is null: ", is.null(assets_yearly))

  if (is.null(assets_yearly) || nrow(assets_yearly) == 0) {
    message("[prepare_profit_trajectories] Returning empty data")
    return(tibble::tibble(
      asset = character(),
      year = integer(),
      profit = numeric()
    ))
  }

  message("[prepare_profit_trajectories] assets_yearly nrows: ", nrow(assets_yearly))
  message("[prepare_profit_trajectories] Available scenarios: ", paste(unique(assets_yearly$scenario), collapse = ", "))

  # Filter for the specified scenario
  trajectory_data <- assets_yearly |>
    dplyr::filter(.data$scenario == !!scenario) |>
    dplyr::select(dplyr::any_of(c("asset", "year", "profit", "company"))) |>
    dplyr::arrange(.data$asset, .data$year)

  if (!is.null(asset_metadata) && "asset" %in% names(asset_metadata)) {
    metadata_cols <- asset_metadata |>
      dplyr::select(
        dplyr::any_of(c(
          "asset",
          "company",
          "share_of_economic_activity",
          "sector_name",
          "sector_code",
          "asset_category",
          "asset_subtype"
        ))
      ) |>
      dplyr::distinct()

    trajectory_data <- trajectory_data |>
      dplyr::left_join(metadata_cols, by = "asset", suffix = c("", "_metadata"))

    # Prefer company value from metadata if missing in yearly data
    if ("company_metadata" %in% names(trajectory_data)) {
      trajectory_data <- trajectory_data |>
        dplyr::mutate(
          company = dplyr::coalesce(.data$company, .data$company_metadata)
        ) |>
        dplyr::select(-dplyr::any_of("company_metadata"))
    }
  }

  message("[prepare_profit_trajectories] Filtered data nrows: ", nrow(trajectory_data))
  message("[prepare_profit_trajectories] Unique assets: ", paste(unique(trajectory_data$asset), collapse = ", "))

  trajectory_data
}

#' Compute portfolio summary statistics
#'
#' @param companies_df Data frame with company results
#' @return Data frame with portfolio-level metrics (includes % change)
#' @noRd
compute_portfolio_summary <- function(companies_df) {
  if (is.null(companies_df) || nrow(companies_df) == 0) {
    return(tibble::tibble(
      metric = character(),
      value = numeric(),
      pct_change = numeric()
    ))
  }

  # Check if expected loss columns exist
  has_baseline <- "Expected_loss_baseline" %in% names(companies_df)
  has_shock <- "Expected_loss_shock" %in% names(companies_df)

  if (!has_baseline || !has_shock) {
    # Try to extract from scenario column if pivoted format
    if ("scenario" %in% names(companies_df) && "Expected_loss" %in% names(companies_df)) {
      baseline_sum <- companies_df |>
        dplyr::filter(.data$scenario == "baseline") |>
        dplyr::pull(.data$Expected_loss) |>
        sum(na.rm = TRUE)

      shock_sum <- companies_df |>
        dplyr::filter(.data$scenario != "baseline") |>
        dplyr::pull(.data$Expected_loss) |>
        sum(na.rm = TRUE)

      difference <- shock_sum - baseline_sum
    } else {
      return(tibble::tibble(
        metric = c("Baseline", "Shock", "Difference"),
        value = c(0, 0, 0),
        pct_change = c(NA_real_, NA_real_, NA_real_)
      ))
    }
  } else {
    # Wide format with baseline and shock columns
    baseline_sum <- sum(companies_df$Expected_loss_baseline, na.rm = TRUE)
    shock_sum <- sum(companies_df$Expected_loss_shock, na.rm = TRUE)
    difference <- shock_sum - baseline_sum
  }

  # Calculate percentage change
  pct_change <- if (baseline_sum == 0 || is.na(baseline_sum)) {
    NA_real_
  } else {
    (difference / baseline_sum) * 100
  }

  tibble::tibble(
    metric = c("Baseline", "Shock", "Difference"),
    value = c(baseline_sum, shock_sum, difference),
    pct_change = c(NA_real_, NA_real_, pct_change)
  )
}

#' Attach sector names and codes using CNAE exposure lookup (internal helper)
#'
#' @param df Data frame with optional `sector` and `cnae` columns
#' @param cnae_exposure Optional tibble with columns `cnae` and `description`
#' @return Data frame with `sector_name` and `sector_code` columns populated
#' @noRd
attach_sector_metadata <- function(df, cnae_exposure = NULL) {
  if (is.null(df) || nrow(df) == 0) {
    return(df)
  }

  has_sector <- "sector" %in% names(df)
  has_cnae <- "cnae" %in% names(df)

  df <- df |>
    dplyr::mutate(
      sector_raw = if (has_sector) as.character(.data$sector) else NA_character_,
      cnae_raw = if (has_cnae) as.character(.data$cnae) else NA_character_,
      sector_digits = dplyr::na_if(gsub("[^0-9]", "", .data$sector_raw), ""),
      cnae_digits = dplyr::na_if(gsub("[^0-9]", "", .data$cnae_raw), ""),
      sector_text_candidate = if (has_sector) {
        dplyr::if_else(
          !is.na(.data$sector_raw) & !grepl("^[0-9]+$", .data$sector_raw),
          .data$sector_raw,
          NA_character_
        )
      } else {
        NA_character_
      },
      cnae_text_candidate = if (has_cnae) {
        dplyr::if_else(
          !is.na(.data$cnae_raw) & !grepl("^[0-9]+(\\.[0-9]+)?$", .data$cnae_raw),
          .data$cnae_raw,
          NA_character_
        )
      } else {
        NA_character_
      },
      sector_join_key = dplyr::coalesce(.data$cnae_digits, .data$sector_digits),
      sector_join_numeric = suppressWarnings(as.numeric(.data$sector_join_key))
    )

  if (!is.null(cnae_exposure) && nrow(cnae_exposure) > 0) {
    exposure_prepared <- cnae_exposure |>
      dplyr::mutate(
        cnae_raw = as.character(.data$cnae),
        cnae_digits = dplyr::na_if(gsub("[^0-9]", "", .data$cnae_raw), ""),
        cnae_numeric = suppressWarnings(as.numeric(.data$cnae_digits))
      ) |>
      dplyr::filter(!is.na(.data$cnae_numeric)) |>
      dplyr::select("cnae_numeric", "description") |>
      dplyr::distinct()

    df <- df |>
      dplyr::left_join(
        exposure_prepared,
        by = c("sector_join_numeric" = "cnae_numeric")
      ) |>
      dplyr::rename(sector_description = "description")
  } else {
    df$sector_description <- NA_character_
  }

  df <- df |>
    dplyr::mutate(
      sector_name = dplyr::coalesce(
        .data$sector_description,
        .data$cnae_text_candidate,
        .data$sector_text_candidate
      ),
      sector_code = dplyr::coalesce(
        dplyr::if_else(
          !is.na(.data$sector_raw) & grepl("^[0-9]+$", .data$sector_raw),
          .data$sector_raw,
          NA_character_
        ),
        .data$cnae_digits
      )
    )

  df |>
    dplyr::select(-dplyr::any_of(c(
      "sector_raw",
      "cnae_raw",
      "sector_digits",
      "cnae_digits",
      "sector_text_candidate",
      "cnae_text_candidate",
      "sector_join_key",
      "sector_join_numeric",
      "sector_description"
    )))
}
