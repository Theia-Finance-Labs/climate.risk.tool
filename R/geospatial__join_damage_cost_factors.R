#' Join damage and cost factors based on hazard type, indicator, intensity and asset category (internal function)
#'
#' @param assets_with_hazards Data frame in long format with asset and hazard information
#'   including hazard_type, hazard_indicator,<｜place▁holder▁no▁118｜> hazard_intensity, scenario_name columns
#'   (from extract_hazard_statistics joined with events)
#' @param damage_factors_df Data frame with damage and cost factors lookup table
#' @param cnae_exposure Optional tibble with CNAE exposure data for sector-based metric selection (columns: cnae, lp_exposure)
#' @return Data frame with original columns plus damage_factor, cost_factor, and business_disruption columns
#' @noRd
join_damage_cost_factors <- function(assets_with_hazards, damage_factors_df, cnae_exposure = NULL) {
  # Separate assets by hazard type
  flood_assets <- assets_with_hazards |>
    dplyr::filter(.data$hazard_type == "FloodTIF")
  compound_assets <- assets_with_hazards |>
    dplyr::filter(.data$hazard_type == "Compound")
  drought_assets <- assets_with_hazards |>
    dplyr::filter(.data$hazard_type == "Drought")    

  # Process each hazard type with its specific logic
  flood_merged <- if (nrow(flood_assets) > 0) {
    join_flood_damage_factors(flood_assets, damage_factors_df)
  } else {
    NULL
  }

  compound_merged <- if (nrow(compound_assets) > 0) {
    join_compound_damage_factors(compound_assets, damage_factors_df, cnae_exposure)
  } else {
    NULL
  }

  drought_merged <- if (nrow(drought_assets) > 0) {
    join_drought_damage_factors(drought_assets, damage_factors_df)
  } else {
    NULL
  }

  # Combine results (filter out NULLs and empty data frames)
  all_results <- list(flood_merged, compound_merged, drought_merged)
  non_empty_results <- all_results[sapply(all_results, function(x) !is.null(x) && nrow(x) > 0)]
  
  if (length(non_empty_results) == 0) {
    stop("No hazard merged with damage and cost factors")
  }
  
  merged <- dplyr::bind_rows(non_empty_results)

  return(merged)
}

#' Join FloodTIF damage factors using closest intensity matching (internal function)
#'
#' @param flood_assets Data frame with FloodTIF hazard assets
#' @param damage_factors_df Data frame with damage and cost factors lookup table
#' @return Data frame with damage_factor, cost_factor, and business_disruption columns
#' @noRd
join_flood_damage_factors <- function(flood_assets, damage_factors_df) {
  # Filter flood damage factors
  flood_factors <- damage_factors_df |>
    dplyr::filter(.data$hazard_type == "FloodTIF") |>
    dplyr::mutate(
      hazard_intensity_num = as.numeric(.data$hazard_intensity)
    ) |>
    dplyr::select("hazard_type", "hazard_indicator", "asset_category", "hazard_intensity_num",
                  "damage_factor", "cost_factor", "business_disruption")

  # Prepare assets with numeric intensity
  flood_assets_prepared <- flood_assets |>
    dplyr::mutate(
      hazard_intensity_num = as.numeric(.data$hazard_intensity)
    )

  # Find closest intensity match for each asset
  result_list <- vector("list", nrow(flood_assets_prepared))
  
  for (i in seq_len(nrow(flood_assets_prepared))) {
    asset_row <- flood_assets_prepared[i, ]
    asset_intensity <- asset_row$hazard_intensity_num
    
    # Find matching factors for this hazard type, indicator, and category
    factors_subset <- flood_factors |>
      dplyr::filter(
        .data$hazard_type == asset_row$hazard_type,
        .data$hazard_indicator == asset_row$hazard_indicator,
        .data$asset_category == asset_row$asset_category
      )
    
    if (nrow(factors_subset) > 0) {
      # Find closest intensity match and cap at maximum available
      factors_subset <- factors_subset |>
        dplyr::mutate(
          intensity_diff = abs(.data$hazard_intensity_num - asset_intensity)
        ) |>
        dplyr::arrange(.data$intensity_diff)
      
      # If asset intensity exceeds max available, use max
      max_intensity <- max(factors_subset$hazard_intensity_num, na.rm = TRUE)
      if (asset_intensity > max_intensity) {
        factors_subset <- factors_subset |>
          dplyr::filter(.data$hazard_intensity_num == max_intensity) |>
          dplyr::slice(1)
      } else {
        factors_subset <- factors_subset |>
          dplyr::slice(1)
      }
      
      result_list[[i]] <- asset_row |>
        dplyr::bind_cols(
          factors_subset |>
            dplyr::select("damage_factor", "cost_factor", "business_disruption")
        )
    } else {
      # No match found - set to NA
      result_list[[i]] <- asset_row |>
        dplyr::mutate(
          damage_factor = NA_real_,
          cost_factor = NA_real_,
          business_disruption = NA_real_
        )
    }
  }
  
  flood_merged <- dplyr::bind_rows(result_list) |>
    dplyr::mutate(
      damage_factor = dplyr::coalesce(as.numeric(.data$damage_factor), NA_real_),
      cost_factor = dplyr::coalesce(as.numeric(.data$cost_factor), NA_real_),
      business_disruption = dplyr::coalesce(as.numeric(.data$business_disruption), NA_real_)
    ) |>
    dplyr::select(-"hazard_intensity_num")
  
  return(flood_merged)
}

#' Join Compound (heat) damage factors using province, GWL, and sector-based metric matching (internal function)
#'
#' @param compound_assets Data frame with Compound hazard assets
#' @param damage_factors_df Data frame with damage and cost factors lookup table
#' @param cnae_exposure Optional tibble with CNAE exposure data (columns: cnae, lp_exposure). If NULL, uses median metric.
#' @return Data frame with damage_factor column (cost_factor and business_disruption are NA)
#' @noRd
join_compound_damage_factors <- function(compound_assets, damage_factors_df, cnae_exposure = NULL) {
  # Determine metric for each asset based on CNAE exposure code
  # If cnae_exposure is provided, use CNAE-based lookup
  # Otherwise, default to median/high fallback
  if (!is.null(cnae_exposure) && nrow(cnae_exposure) > 0) {
    compound_assets_with_metric <- compound_assets |>
      dplyr::left_join(
        cnae_exposure |>
          dplyr::select("cnae", "lp_exposure"),
        by = "cnae"
      ) |>
      dplyr::mutate(
        # Determine metric based on lookup or defaults; only cnae is used
        metric = dplyr::case_when(
          !is.na(.data$lp_exposure) ~ .data$lp_exposure,
          is.na(.data$cnae) & .data$asset_category == "agriculture" ~ "high",
          TRUE ~ "median"
        )
      ) |>
      dplyr::select(-"lp_exposure")
  } else {
    # If no CNAE exposure data provided, use fallback logic based on asset_category
    compound_assets_with_metric <- compound_assets |>
      dplyr::mutate(
        metric = dplyr::if_else(
          is.na(.data$cnae) & .data$asset_category == "agriculture",
          "high",
          "median"
        )
      )
  }

  # Join with damage factors matching on hazard_type, province, gwl, AND metric
  compound_factors <- damage_factors_df |>
    dplyr::filter(.data$hazard_type == "Compound") |>
    dplyr::select("hazard_type", "province", "gwl", "metric", "damage_factor")

  compound_merged <- dplyr::left_join(
    compound_assets_with_metric,
    compound_factors,
    by = c("hazard_type", "province", "scenario_name" = "gwl", "metric")
  ) |>
    dplyr::mutate(
      cost_factor = NA_real_,
      business_disruption = NA_real_
    )

  return(compound_merged)
}


#' Join Drought damage factors for agriculture assets with crop/province/season matching using closest intensity (internal function)
#'
#' Handles crops with multiple growing seasons (e.g., Sugarcane with Winter and Autumn).
#' - If user-selected season matches a growing season: use that season's damage factor
#' - If user-selected season doesn't match any growing season: average all growing seasons'
#'   damage factors and off_windows, then apply: avg_damage_factor * avg_off_window
#' 
#' Province fallback strategy:
#' - First tries to match the asset's specific province
#' - If province not found, uses the first available province that has data for the crop
#' - If no crop data found at all, sets damage_factor = 0 with NA for growing_season and off_window
#'
#' @param drought_assets Data frame with Drought hazard assets including season column from events
#' @param damage_factors_df Data frame with damage and cost factors lookup table
#' @return Data frame with damage_factor, growing_season, off_window, and season columns (cost_factor and business_disruption are NA)
#' @noRd
join_drought_damage_factors <- function(drought_assets, damage_factors_df) {
  # Filter damage factors for drought (hazard_type = "drought", hazard_indicator = "SPI3")
  # Note: metric column was removed from damage factors file
  drought_factors <- damage_factors_df |>
    dplyr::filter(
      .data$hazard_type == "Drought",
      .data$hazard_indicator == "SPI3"
    ) |>
    dplyr::mutate(
      hazard_intensity_num = as.numeric(.data$hazard_intensity),
      damage_factor = as.numeric(.data$damage_factor),
      off_window = as.numeric(.data$off_window)
    ) |>
    dplyr::select("province", "subtype", "season", "damage_factor", "off_window", "hazard_intensity_num") |>
    dplyr::rename(growing_season = "season")  # Rename to avoid conflict with event season

  # Prepare assets: normalize crop subtype and province
  drought_assets_prepared <- drought_assets |>
    dplyr::mutate(
      # Normalize subtype: missing values become "Other"
      asset_subtype_normalized = dplyr::if_else(
        is.na(.data$asset_subtype) | .data$asset_subtype == "",
        "Other",
        .data$asset_subtype
      ),
      # Normalize province for matching: missing values will be matched to "Other"
      province_normalized = dplyr::if_else(
        is.na(.data$province) | .data$province == "",
        "Other",
        .data$province
      ),
      # Use raw intensity for matching (will cap < -3 to -3, but > -1 means no damage)
      intensity_for_match = dplyr::case_when(
        .data$hazard_intensity < -3 ~ -3,
        TRUE ~ as.numeric(.data$hazard_intensity)
      )
    )

  # Process each asset to handle multiple growing seasons
  result_list <- vector("list", nrow(drought_assets_prepared))
  
  for (i in seq_len(nrow(drought_assets_prepared))) {
    asset_row <- drought_assets_prepared[i, ]
    asset_intensity <- asset_row$intensity_for_match
    original_intensity <- as.numeric(drought_assets[i, ]$hazard_intensity)
    user_season <- as.character(asset_row$season)
    
    # Check if intensity > -1 (no damage) - check original intensity
    if (original_intensity > -1) {
      result_list[[i]] <- asset_row |>
        dplyr::mutate(
          damage_factor = 0,
          cost_factor = NA_real_,
          business_disruption = NA_real_,
          growing_season = NA_character_,
          off_window = NA_real_
        )
      next
    }
    
    # First attempt: match on specific province and subtype
    factors_for_crop <- drought_factors |>
      dplyr::filter(
        .data$province == asset_row$province_normalized,
        .data$subtype == asset_row$asset_subtype_normalized
      )
    
    # If no specific match, try fallback provinces in order
    # Strategy: use the first available province for this crop
    if (nrow(factors_for_crop) == 0) {
      # Get all provinces that have this crop type
      available_provinces <- drought_factors |>
        dplyr::filter(.data$subtype == asset_row$asset_subtype_normalized) |>
        dplyr::pull(.data$province) |>
        unique()
      
      if (length(available_provinces) > 0) {
        # Use the first available province as fallback
        fallback_province <- available_provinces[1]
        factors_for_crop <- drought_factors |>
          dplyr::filter(
            .data$province == fallback_province,
            .data$subtype == asset_row$asset_subtype_normalized
          )
      }
    }
    
    # If still no match, set to zero
    if (nrow(factors_for_crop) == 0) {
      result_list[[i]] <- asset_row |>
        dplyr::mutate(
          damage_factor = 0,
          cost_factor = NA_real_,
          business_disruption = NA_real_,
          growing_season = NA_character_,
          off_window = NA_real_
        )
      next
    }
    
    # Find all growing seasons for this crop (at closest intensity match)
    # First, find the closest intensity
    closest_intensity <- factors_for_crop |>
      dplyr::mutate(
        intensity_diff = abs(.data$hazard_intensity_num - asset_intensity)
      ) |>
      dplyr::arrange(.data$intensity_diff) |>
      dplyr::slice(1) |>
      dplyr::pull(.data$hazard_intensity_num)
    
    # Get all seasons at this intensity (handles multi-season crops)
    all_seasons_for_crop <- factors_for_crop |>
      dplyr::filter(.data$hazard_intensity_num == closest_intensity)
    
    # Check if user-selected season matches any growing season
    season_match <- all_seasons_for_crop |>
      dplyr::filter(.data$growing_season == user_season)
    
    if (nrow(season_match) > 0) {
      # CASE 1: User season matches a growing season - use that season's factors
      result_list[[i]] <- asset_row |>
        dplyr::bind_cols(
          season_match |>
            dplyr::slice(1) |>
            dplyr::select("damage_factor", "off_window", "growing_season") |>
            dplyr::mutate(
              damage_factor = as.numeric(.data$damage_factor),
              off_window = as.numeric(.data$off_window)
            )
        ) |>
        dplyr::mutate(
          cost_factor = NA_real_,
          business_disruption = NA_real_
        )
    } else {
      # CASE 2: User season doesn't match - average all growing seasons
      avg_factors <- all_seasons_for_crop |>
        dplyr::summarize(
          avg_damage_factor = mean(.data$damage_factor, na.rm = TRUE),
          avg_off_window = mean(.data$off_window, na.rm = TRUE),
          seasons_list = paste(sort(unique(.data$growing_season)), collapse = ", "),
          .groups = "drop"
        )
      
      # Applied damage = avg_damage_factor * avg_off_window
      applied_damage <- avg_factors$avg_damage_factor * avg_factors$avg_off_window
      
      result_list[[i]] <- asset_row |>
        dplyr::mutate(
          damage_factor = applied_damage,
          off_window = avg_factors$avg_off_window,
          growing_season = paste0("Averaged (", avg_factors$seasons_list, ")"),
          cost_factor = NA_real_,
          business_disruption = NA_real_
        )
    }
  }
  
  drought_merged <- dplyr::bind_rows(result_list)

  # Clean up temporary columns but KEEP growing_season, off_window, and season
  drought_merged <- drought_merged |>
    dplyr::select(
      -dplyr::any_of(c(
        "asset_subtype_normalized",
        "province_normalized",
        "intensity_for_match"
      ))
    )

  # Filter to agriculture assets only (drought only affects agriculture)
  drought_merged <- drought_merged |>
    dplyr::filter(.data$asset_category == "agriculture")

  return(drought_merged)
}