#' Summarize hazard values for each asset in long format
#'
#' @title Calculate summary statistics for hazard exposure per asset
#' @description Takes the output from cutout_hazards and transforms it to long format
#'   with hazard_name, hazard_type, and hazard_intensity columns. Each row represents
#'   one asset-hazard combination.
#' @param assets_with_hazard_values Data frame with asset information including hazard columns (from cutout_hazards)
#' @return Data frame in long format with columns: asset, company, latitude, longitude,
#'   municipality, province, asset_category, size_in_m2, share_of_economic_activity,
#'   geometry, centroid, geolocation_method, hazard_name, hazard_type, hazard_intensity
#' @examples
#' \dontrun{
#' base_dir <- system.file("tests_data", package = "climate.risk.tool")
#' assets <- read_inputs(base_dir)$assets
#' hazards <- load_hazards(file.path(base_dir, "hazards"))
#' assets_geo <- geolocate_assets(assets, hazards, base_dir)
#' assets_hazards <- cutout_hazards(assets_geo, hazards)
#' assets_summary <- summarize_hazards(assets_hazards)
#' }
#' @export
summarize_hazards <- function(assets_with_hazard_values) {
  message("📊 [summarize_hazards] Starting hazard summarization for ", nrow(assets_with_hazard_values), " assets...")

  if (!is.data.frame(assets_with_hazard_values)) {
    stop("Input must be a data.frame")
  }

  # Identify standard asset columns to preserve
  standard_columns <- c("asset", "company", "latitude", "longitude", "municipality",
                       "province", "asset_category", "size_in_m2", "size_in_hectare", "share_of_economic_activity",
                       "geometry", "centroid", "geolocation_method")

  # Find numeric columns that are not standard columns (these should be hazard columns)
  all_numeric_cols <- names(assets_with_hazard_values)[sapply(assets_with_hazard_values, is.numeric)]
  hazard_columns <- setdiff(all_numeric_cols, standard_columns)

  if (length(hazard_columns) == 0) {
    warning("No hazard columns found to summarize")
    # Return empty long format structure
    result <- assets_with_hazard_values[, standard_columns, drop = FALSE]
    result$hazard_name <- character(0)
    result$hazard_type <- character(0) 
    result$hazard_intensity <- numeric(0)
    return(result[0, ])
  }

  # Extract hazard information from column names
  # Expected format: hazard_type__scenario (e.g., "flood__rcp85_h100")
  hazard_info <- data.frame(
    hazard_column = hazard_columns,
    stringsAsFactors = FALSE
  )
  
  # Parse hazard names to extract hazard_type
  # Split on "__" to separate hazard type from scenario
  hazard_parts <- strsplit(hazard_columns, "__", fixed = TRUE)
  
  hazard_info$hazard_type <- sapply(hazard_parts, function(x) {
    if (length(x) >= 1) {
      return(x[1])
    } else {
      return("unknown")
    }
  })
  
  hazard_info$hazard_name <- hazard_columns  # Full column name as hazard_name
  
  # Convert to long format
  # Get base asset data
  base_data <- assets_with_hazard_values[, standard_columns, drop = FALSE]
  
  # Create long format data
  long_data_list <- list()
  
  for (i in seq_len(nrow(hazard_info))) {
    hazard_col <- hazard_info$hazard_column[i]
    hazard_type <- hazard_info$hazard_type[i]
    hazard_name <- hazard_info$hazard_name[i]
    
    # Create one row per asset for this hazard
    asset_hazard_data <- base_data
    asset_hazard_data$hazard_name <- hazard_name
    asset_hazard_data$hazard_type <- hazard_type
    asset_hazard_data$hazard_intensity <- assets_with_hazard_values[[hazard_col]]
    
    # Only include rows where hazard_intensity is not NA
    asset_hazard_data <- asset_hazard_data[!is.na(asset_hazard_data$hazard_intensity), ]
    
    if (nrow(asset_hazard_data) > 0) {
      long_data_list[[i]] <- asset_hazard_data
    }
  }
  
  # Combine all hazard data
  if (length(long_data_list) > 0) {
    result <- do.call(rbind, long_data_list)
    rownames(result) <- NULL
  } else {
    # No valid hazard data found
    result <- base_data[0, ]
    result$hazard_name <- character(0)
    result$hazard_type <- character(0)
    result$hazard_intensity <- numeric(0)
  }

  message("✅ [summarize_hazards] Transformed to long format: ", nrow(result), " asset-hazard combinations from ", length(hazard_columns), " hazard types")
  return(result)
}

