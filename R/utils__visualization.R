#' Format hover text for asset markers
#'
#' @param asset_name Character. Name of the asset
#' @param hazard_intensity Numeric. Hazard intensity value
#' @param additional_info List. Optional additional information to display
#' @return HTML string for popup
#' @noRd
format_hover_text <- function(asset_name, hazard_intensity, additional_info = NULL) {
  html_text <- paste0(
    "<div style='font-family: Arial, sans-serif; font-size: 12px;'>",
    "<strong>", htmltools::htmlEscape(asset_name), "</strong><br/>",
    "<span style='color: #e74c3c;'><strong>Hazard Intensity:</strong> ",
    round(hazard_intensity, 3), "</span>"
  )
  
  if (!is.null(additional_info)) {
    for (key in names(additional_info)) {
      html_text <- paste0(
        html_text,
        "<br/><strong>", htmltools::htmlEscape(key), ":</strong> ",
        htmltools::htmlEscape(as.character(additional_info[[key]]))
      )
    }
  }
  
  html_text <- paste0(html_text, "</div>")
  html_text
}

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

#' Get municipality centroids for point rendering
#'
#' @param municipality_sf sf object with municipality boundaries
#' @return sf object with centroid points
#' @noRd
get_municipality_centroids <- function(municipality_sf) {
  if (is.null(municipality_sf) || nrow(municipality_sf) == 0) {
    return(NULL)
  }
  
  # Calculate centroids and preserve attributes
  centroids <- sf::st_centroid(municipality_sf)
  centroids
}

#' Extract hazard data by name from hazards list
#'
#' @param hazard_name Character. Name of hazard to extract
#' @param hazards Named list of hazard SpatRaster or data.frame objects
#' @return SpatRaster or data.frame for the specified hazard, or NULL if not found
#' @noRd
extract_hazard_data <- function(hazard_name, hazards) {
  if (is.null(hazards) || length(hazards) == 0) {
    return(NULL)
  }
  
  # Check if hazard exists in the list
  if (hazard_name %in% names(hazards)) {
    return(hazards[[hazard_name]])
  }
  
  return(NULL)
}

#' Determine hazard data type (raster or points)
#'
#' @param hazard_data SpatRaster or data.frame object
#' @return Character. "raster" or "points"
#' @noRd
get_hazard_type <- function(hazard_data) {
  if (inherits(hazard_data, "SpatRaster")) {
    return("raster")
  } else if (is.data.frame(hazard_data)) {
    return("points")
  }
  return("unknown")
}

#' Prepare asset overlay data for mapping
#'
#' @param assets_factors Data frame with asset exposure results
#' @param hazard_name Character. Hazard name to filter for
#' @param adm1_boundaries sf object with province boundaries (optional)
#' @param adm2_boundaries sf object with municipality boundaries (optional)
#' @return List with geolocated_assets, municipality_assets, province_assets
#' @noRd
prepare_asset_overlay <- function(assets_factors, hazard_name, 
                                  adm1_boundaries = NULL, adm2_boundaries = NULL) {
  if (is.null(assets_factors) || nrow(assets_factors) == 0) {
    return(list(
      geolocated = NULL,
      municipalities = NULL,
      provinces = NULL
    ))
  }
  
  # Filter assets for this specific hazard
  hazard_assets <- assets_factors |>
    dplyr::filter(.data$hazard_name == !!hazard_name)
  
  if (nrow(hazard_assets) == 0) {
    return(list(
      geolocated = NULL,
      municipalities = NULL,
      provinces = NULL
    ))
  }
  
  # Separate by matching method
  geolocated <- hazard_assets |>
    dplyr::filter(.data$matching_method == "coordinates")
  
  municipality_matched <- hazard_assets |>
    dplyr::filter(.data$matching_method == "municipality")
  
  province_matched <- hazard_assets |>
    dplyr::filter(.data$matching_method == "province")
  
  # Prepare geolocated assets as sf points
  geolocated_sf <- NULL
  if (nrow(geolocated) > 0 && "lat" %in% names(geolocated) && "lon" %in% names(geolocated)) {
    geolocated_valid <- geolocated |>
      dplyr::filter(!is.na(.data$lat), !is.na(.data$lon))
    
    if (nrow(geolocated_valid) > 0) {
      geolocated_sf <- sf::st_as_sf(
        geolocated_valid,
        coords = c("lon", "lat"),
        crs = 4326
      )
    }
  }
  
  # Prepare municipality data with boundaries
  municipality_sf <- NULL
  if (nrow(municipality_matched) > 0 && !is.null(adm2_boundaries)) {
    # Aggregate assets by municipality
    municipality_summary <- municipality_matched |>
      dplyr::group_by(.data$municipality) |>
      dplyr::summarise(
        n_assets = dplyr::n(),
        asset_list = paste(.data$asset, collapse = ", "),
        avg_hazard_intensity = mean(.data$hazard_intensity, na.rm = TRUE),
        .groups = "drop"
      )
    
    # Join with boundaries
    municipality_sf <- adm2_boundaries |>
      dplyr::inner_join(
        municipality_summary,
        by = c("shapeName" = "municipality")
      )
  }
  
  # Prepare province data with boundaries
  province_sf <- NULL
  if (nrow(province_matched) > 0 && !is.null(adm1_boundaries)) {
    # Aggregate assets by province
    province_summary <- province_matched |>
      dplyr::group_by(.data$province) |>
      dplyr::summarise(
        n_assets = dplyr::n(),
        asset_list = paste(.data$asset, collapse = ", "),
        avg_hazard_intensity = mean(.data$hazard_intensity, na.rm = TRUE),
        .groups = "drop"
      )
    
    # Join with boundaries
    province_sf <- adm1_boundaries |>
      dplyr::inner_join(
        province_summary,
        by = c("shapeName" = "province")
      )
  }
  
  list(
    geolocated = geolocated_sf,
    municipalities = municipality_sf,
    provinces = province_sf
  )
}

