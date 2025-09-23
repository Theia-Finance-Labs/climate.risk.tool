#' Apply Chronic Shock
#'
#' Calculates chronic climate shock impacts based on long-term climate trends.
#' Chronic shocks represent gradual, persistent climate changes like rising temperatures or changing precipitation patterns.
#'
#' @title Apply Chronic Shock
#' @description Computes chronic shock values based on available hazard data representing long-term climate trends.
#'   Chronic shocks are typically lower magnitude but persistent impacts that compound over time.
#'   Uses hazard intensity data when available, focusing on temperature and precipitation trends.
#' @param assets_with_acute_shock data.frame. Asset data with hazard information and acute shock already applied
#' @return data.frame with all original columns plus 'chronic_shock' column
#' @examples
#' \dontrun{
#' assets <- data.frame(asset_id = 1:3, hazard_mean_temperature = c(30, 35, 40))
#' result <- apply_chronic_shock(assets)
#' }
#' @export
apply_chronic_shock <- function(assets_with_acute_shock) {
  # Validate inputs
  if (!is.data.frame(assets_with_acute_shock)) {
    stop("assets_with_acute_shock must be a data.frame")
  }
  
  # Make a copy to avoid modifying the input
  result <- assets_with_acute_shock
  
  # Find chronic hazard columns (focus on long-term climate indicators)
  # Temperature and precipitation changes are key chronic climate indicators
  temp_cols <- grep("(temperature|temp)", names(result), value = TRUE, ignore.case = TRUE)
  precip_cols <- grep("(precipitation|precip|rainfall)", names(result), value = TRUE, ignore.case = TRUE)
  drought_cols <- grep("drought", names(result), value = TRUE, ignore.case = TRUE)
  
  # Also include any other hazard columns for comprehensive chronic impact
  all_hazard_cols <- grep("^hazard_", names(result), value = TRUE)
  chronic_relevant_cols <- unique(c(temp_cols, precip_cols, drought_cols, all_hazard_cols))
  
  if (length(chronic_relevant_cols) > 0) {
    # Calculate chronic shock based on available hazard data
    
    hazard_matrix <- as.matrix(result[, chronic_relevant_cols, drop = FALSE])
    
    # Replace NA values with 0 for calculation
    hazard_matrix[is.na(hazard_matrix)] <- 0
    
    # Weight different types of hazards for chronic impact
    # Temperature and drought typically have higher chronic impact
    hazard_weights <- rep(1, ncol(hazard_matrix))
    names(hazard_weights) <- chronic_relevant_cols
    
    # Assign higher weights to chronic-relevant hazards
    temp_indices <- which(chronic_relevant_cols %in% temp_cols)
    drought_indices <- which(chronic_relevant_cols %in% drought_cols)
    precip_indices <- which(chronic_relevant_cols %in% precip_cols)
    
    if (length(temp_indices) > 0) hazard_weights[temp_indices] <- 1.5
    if (length(drought_indices) > 0) hazard_weights[drought_indices] <- 1.3
    if (length(precip_indices) > 0) hazard_weights[precip_indices] <- 1.2
    
    # Normalize hazards to [0,1] scale, but preserve relative differences
    normalized_hazards <- apply(hazard_matrix, 2, function(x) {
      max_val <- max(x, na.rm = TRUE)
      min_val <- min(x, na.rm = TRUE)
      if (max_val > min_val && max_val > 0) {
        (x - min_val) / (max_val - min_val)  # Normalize to [0,1] preserving relative order
      } else if (max_val > 0) {
        x / max_val  # Simple normalization when all values are similar
      } else {
        x  # Keep as is if all zeros
      }
    })
    
    # Apply weights and compute weighted average
    weighted_hazards <- sweep(normalized_hazards, 2, hazard_weights, "*")
    total_weight <- sum(hazard_weights[!is.na(colMeans(normalized_hazards))])
    if (total_weight > 0) {
      composite_chronic_hazard <- rowSums(weighted_hazards, na.rm = TRUE) / total_weight
    } else {
      composite_chronic_hazard <- rep(0, nrow(result))
    }
    
    # Apply chronic shock formula
    # Chronic shocks are generally smaller but more persistent than acute shocks
    base_chronic_shock <- 0.01  # Base 1% chronic shock
    chronic_multiplier <- 1 + composite_chronic_hazard  # 1x to 2x multiplier
    
    result$chronic_shock <- base_chronic_shock * chronic_multiplier
    
  } else {
    # No relevant hazard data available - use default chronic shock
    default_chronic_shock <- 0.015  # Default 1.5% chronic shock
    result$chronic_shock <- rep(default_chronic_shock, nrow(result))
  }
  
  # Ensure chronic shock is non-negative and reasonable
  result$chronic_shock <- pmax(0, result$chronic_shock)
  result$chronic_shock <- pmin(0.1, result$chronic_shock)  # Cap at 10% (chronic shocks should be moderate)
  
  # Validate the result
  if (!is.numeric(result$chronic_shock)) {
    stop("Calculated chronic_shock is not numeric")
  }
  
  if (any(is.na(result$chronic_shock))) {
    # Replace any remaining NAs with default value
    result$chronic_shock[is.na(result$chronic_shock)] <- 0.015
  }
  
  return(result)
}
