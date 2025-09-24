#' Summarize hazard values for each asset
#'
#' @title Calculate summary statistics for hazard exposure per asset
#' @description Takes the output from cutout_hazards and creates summary statistics
#'   for each hazard column. Adds mean columns with "_mean" suffix for each hazard.
#' @param assets_with_hazard_values Data frame with asset information including hazard columns (from cutout_hazards)
#' @return Data frame with original columns plus mean summary columns for each hazard
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
  
  # Start with the input dataframe
  result <- assets_with_hazard_values
  
  # Identify hazard columns (numeric columns that are not standard asset columns)
  standard_columns <- c("asset_id", "company_id", "latitude", "longitude", "municipality", 
                       "province", "asset_category", "size_in_m2", "share_of_economic_activity",
                       "geometry", "centroid", "geolocation_method")
  
  # Find numeric columns that are not standard columns (these should be hazard columns)
  all_numeric_cols <- names(assets_with_hazard_values)[sapply(assets_with_hazard_values, is.numeric)]
  hazard_columns <- setdiff(all_numeric_cols, standard_columns)
  
  if (length(hazard_columns) == 0) {
    warning("No hazard columns found to summarize")
    return(result)
  }
  
  message("🔍 [summarize_hazards] Found ", length(hazard_columns), " hazard columns to summarize:")
  for (hazard_col in hazard_columns) {
    message("  - ", hazard_col)
  }
  
  # For each hazard column, create a mean summary column
  message("⏳ [summarize_hazards] Creating summary columns...")
  for (hazard_col in hazard_columns) {
    if (hazard_col %in% names(assets_with_hazard_values)) {
      mean_col_name <- paste0(hazard_col, "_mean")
      
      # Calculate mean (which should be the same as the original value since cutout_hazards already gives means)
      # But we add this for consistency with the pipeline expectation
      result[[mean_col_name]] <- assets_with_hazard_values[[hazard_col]]
      message("  ✅ Created summary column: ", mean_col_name)
    }
  }
  
  message("✅ [summarize_hazards] Successfully created ", length(hazard_columns), " summary columns")
  return(result)
}
