#' Join damage and cost factors based on hazard intensity and asset category
#'
#' @title Map hazard intensity to damage and cost factors
#' @description Joins assets with damage and cost factors from a lookup table based on
#'   rounded hazard intensity values and asset category. Hazard intensity is rounded
#'   to the nearest integer for matching.
#' @param assets_with_hazard_means Data frame with asset information including hazard mean columns
#' @param factors_csv_path Path to CSV file containing damage and cost factors
#' @return Data frame with original columns plus damage_factor and cost_factor columns
#' @examples
#' \dontrun{
#' base_dir <- system.file("tests_data", package = "climate.risk.tool")
#' assets <- read_inputs(base_dir)$assets
#' assets$hazard_dummy <- 12.4
#' factors_path <- file.path(base_dir, "damage_and_cost_factors.csv")
#' assets_with_factors <- join_damage_cost_factors(assets, factors_path)
#' }
#' @export
join_damage_cost_factors <- function(assets_with_hazard_means, factors_csv_path) {
  message("🔗 [join_damage_cost_factors] Starting damage and cost factor joining for ", nrow(assets_with_hazard_means), " assets...")

  if (!is.data.frame(assets_with_hazard_means)) {
    stop("Input must be a data.frame")
  }

  if (!file.exists(factors_csv_path)) {
    stop("Factors CSV file not found: ", factors_csv_path)
  }

  # Read the damage and cost factors CSV
  # The CSV uses comma as decimal separator and quotes around numbers
  factors_df <- utils::read.csv(factors_csv_path, stringsAsFactors = FALSE)

  # Clean up the numeric columns that have comma decimal separators and quotes
  factors_df$damage_factor <- as.numeric(gsub(",", ".", gsub('"', "", factors_df$damage_factor)))
  factors_df$cost_factor <- as.numeric(gsub(",", ".", gsub('"', "", factors_df$cost_factor)))
  
  # Start with the input dataframe
  result <- assets_with_hazard_means
  
  # Find hazard columns (numeric columns that are not standard asset columns)
  standard_columns <- c("asset_id", "company_id", "latitude", "longitude", "municipality", 
                       "province", "asset_category", "size_in_m2", "share_of_economic_activity",
                       "geometry", "centroid", "geolocation_method")
  
  all_numeric_cols <- names(assets_with_hazard_means)[sapply(assets_with_hazard_means, is.numeric)]
  hazard_columns <- setdiff(all_numeric_cols, standard_columns)
  
  if (length(hazard_columns) == 0) {
    warning("No hazard columns found to join against")
    # Still add default columns
    result$damage_factor <- NA_real_
    result$cost_factor <- NA_real_
    return(result)
  }

  # Initialize damage and cost factor columns
  result$damage_factor <- NA_real_
  result$cost_factor <- NA_real_

  # For each asset, find the best match in the factors table
  matches_found <- 0
  for (i in seq_len(nrow(assets_with_hazard_means))) {
    asset_category <- assets_with_hazard_means$asset_category[i]
    
    if (is.na(asset_category) || !nzchar(as.character(asset_category))) {
      next  # Skip if no asset category
    }
    
    # Find the first hazard column with a non-NA value for this asset
    hazard_intensity <- NA_real_
    for (hazard_col in hazard_columns) {
      if (!is.na(assets_with_hazard_means[[hazard_col]][i])) {
        hazard_intensity <- assets_with_hazard_means[[hazard_col]][i]
        break
      }
    }
    
    if (is.na(hazard_intensity)) {
      next  # Skip if no hazard intensity available
    }
    
    # Round hazard intensity to nearest integer
    hazard_intensity_rounded <- round(hazard_intensity)
    
    # Find matching row in factors table
    match_rows <- factors_df[factors_df$hazard_intensity == hazard_intensity_rounded & 
                            factors_df$asset_category == asset_category, ]
    
    if (nrow(match_rows) > 0) {
      # Use the first match
      result$damage_factor[i] <- match_rows$damage_factor[1]
      result$cost_factor[i] <- match_rows$cost_factor[1]
      matches_found <- matches_found + 1
    }
  }

  message("✅ [join_damage_cost_factors] Factor joining completed for ", nrow(assets_with_hazard_means), " assets (", matches_found, " matches found)")

  return(result)
}
