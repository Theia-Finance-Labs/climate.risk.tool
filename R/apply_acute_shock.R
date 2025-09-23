#' Apply Acute Shock
#'
#' Calculates acute climate shock impacts based on hazard intensity and shock year.
#' Acute shocks represent sudden, year-specific climate events like extreme floods or droughts.
#'
#' @title Apply Acute Shock
#' @description Computes acute shock values based on available hazard data and the specified shock year.
#'   Acute shocks are typically higher magnitude but shorter duration impacts compared to chronic shocks.
#'   Uses hazard intensity data when available, otherwise applies default shock values.
#' @param assets_with_factors data.frame. Asset data with hazard and damage factor information
#' @param shock_year numeric. Year for which to calculate the acute shock (must be future year >= 2025)
#' @return data.frame with all original columns plus 'acute_shock' column
#' @examples
#' \dontrun{
#' assets <- data.frame(asset_id = 1:3, hazard_mean_flood = c(5, 10, 15))
#' result <- apply_acute_shock(assets, shock_year = 2030)
#' }
#' @export
apply_acute_shock <- function(assets_with_factors, shock_year) {
  # Validate inputs
  if (!is.data.frame(assets_with_factors)) {
    stop("assets_with_factors must be a data.frame")
  }
  
  if (!is.numeric(shock_year) || length(shock_year) != 1) {
    stop("shock_year must be a single numeric value")
  }
  
  current_year <- as.numeric(format(Sys.Date(), "%Y"))
  if (shock_year < 2025) {
    stop("shock_year must be a valid future year (>= 2025)")
  }
  
  # Make a copy to avoid modifying the input
  result <- assets_with_factors
  
  # Find hazard columns (columns starting with "hazard_mean_" or "hazard_max_")
  hazard_cols <- grep("^hazard_(mean|max)_", names(result), value = TRUE)
  
  if (length(hazard_cols) > 0) {
    # Calculate acute shock based on hazard data
    
    # Extract hazard values and compute composite hazard intensity
    hazard_matrix <- as.matrix(result[, hazard_cols, drop = FALSE])
    
    # Replace NA values with 0 for calculation
    hazard_matrix[is.na(hazard_matrix)] <- 0
    
    # Compute composite hazard intensity (weighted average of all hazards)
    # Different hazards may have different scales, so normalize first
    normalized_hazards <- apply(hazard_matrix, 2, function(x) {
      if (max(x, na.rm = TRUE) > 0) {
        x / max(x, na.rm = TRUE)  # Normalize to [0,1] scale
      } else {
        x
      }
    })
    
    # Compute average normalized hazard intensity
    composite_hazard <- rowMeans(normalized_hazards, na.rm = TRUE)
    
    # Apply acute shock formula
    # Acute shock increases with hazard intensity and is influenced by shock year
    # Formula: base_shock * hazard_multiplier * time_factor
    
    base_shock <- 0.02  # Base 2% acute shock
    hazard_multiplier <- 1 + 2 * composite_hazard  # 1x to 3x multiplier based on hazard
    time_factor <- 1 + (shock_year - 2025) * 0.01  # Slight increase with future years
    
    result$acute_shock <- base_shock * hazard_multiplier * time_factor
    
  } else {
    # No hazard data available - use default acute shock
    # Default increases slightly with future years
    base_default_shock <- 0.03  # Base 3% default acute shock
    time_factor <- 1 + (shock_year - 2025) * 0.005  # Smaller increase for default
    
    result$acute_shock <- rep(base_default_shock * time_factor, nrow(result))
  }
  
  # Ensure acute shock is non-negative and bounded
  result$acute_shock <- pmax(0, result$acute_shock)
  result$acute_shock <- pmin(1, result$acute_shock)  # Cap at 100%
  
  # Validate the result
  if (!is.numeric(result$acute_shock)) {
    stop("Calculated acute_shock is not numeric")
  }
  
  if (any(is.na(result$acute_shock))) {
    # Replace any remaining NAs with default value
    result$acute_shock[is.na(result$acute_shock)] <- 0.03
  }
  
  return(result)
}
