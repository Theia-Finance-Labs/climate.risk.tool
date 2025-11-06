#' Create color palette for categorical data
#'
#' @param n_categories Integer. Number of categories
#' @param palette_name Character. RColorBrewer palette name (default: "Set3")
#' @return Character vector of colors
#' @noRd
create_color_palette <- function(n_categories, palette_name = "Set3") {
  if (n_categories <= 1) {
    return("#3498db")
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
#' @return Data frame formatted for plotly
#' @noRd
prepare_profit_trajectories <- function(assets_yearly, scenario) {
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
    dplyr::select("asset", "year", "profit") |>
    dplyr::arrange("asset", "year")

  message("[prepare_profit_trajectories] Filtered data nrows: ", nrow(trajectory_data))
  message("[prepare_profit_trajectories] Unique assets: ", paste(unique(trajectory_data$asset), collapse = ", "))

  trajectory_data
}

#' Compute portfolio summary statistics
#'
#' @param companies_df Data frame with company results
#' @return Data frame with portfolio-level metrics
#' @noRd
compute_portfolio_summary <- function(companies_df) {
  if (is.null(companies_df) || nrow(companies_df) == 0) {
    return(tibble::tibble(
      metric = character(),
      value = numeric()
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
        value = c(0, 0, 0)
      ))
    }
  } else {
    # Wide format with baseline and shock columns
    baseline_sum <- sum(companies_df$Expected_loss_baseline, na.rm = TRUE)
    shock_sum <- sum(companies_df$Expected_loss_shock, na.rm = TRUE)
    difference <- shock_sum - baseline_sum
  }
  
  tibble::tibble(
    metric = c("Baseline", "Shock", "Difference"),
    value = c(baseline_sum, shock_sum, difference)
  )
}

