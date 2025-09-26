#' Compute Company-Level Yearly Trajectories
#'
#' @title Compute Company-Level Yearly Trajectories
#' @description Aggregates asset-level yearly trajectories to company-level yearly trajectories.
#'   This provides yearly revenue and profit trajectories at the company level for detailed analysis.
#' @param yearly_discounted_df data.frame with columns: asset, company, year, scenario, revenue, profit, discounted_profit, discounted_net_profit
#' @return data.frame with columns: company, year, scenario, total_revenue, total_profit, total_discounted_profit, total_discounted_net_profit
#' @examples
#' \dontrun{
#' yearly_data <- data.frame(
#'   asset = c("A1", "A1", "A2", "A2"),
#'   company = c("C1", "C1", "C1", "C1"),
#'   year = c(2025, 2025, 2025, 2025),
#'   scenario = c("baseline", "baseline", "baseline", "baseline"),
#'   revenue = c(600, 600, 400, 400),
#'   profit = c(60, 60, 40, 40),
#'   discounted_profit = c(60, 60, 40, 40),
#'   discounted_net_profit = c(60, 60, 40, 40)
#' )
#' result <- compute_company_yearly_trajectories(yearly_data)
#' }
#' @export
compute_company_yearly_trajectories <- function(yearly_discounted_df) {
  # Validate inputs
  if (!is.data.frame(yearly_discounted_df) || nrow(yearly_discounted_df) == 0) {
    stop("yearly_discounted_df must be a non-empty data.frame")
  }
  
  # Check required columns
  required_cols <- c("asset", "company", "year", "scenario", "revenue", "profit", 
                    "discounted_profit", "discounted_net_profit")
  missing_cols <- setdiff(required_cols, names(yearly_discounted_df))
  if (length(missing_cols) > 0) {
    stop(paste("Missing required columns in yearly_discounted_df:", 
               paste(missing_cols, collapse = ", ")))
  }
  
  # Aggregate by company, year, and scenario
  result <- stats::aggregate(
    cbind(revenue, profit, discounted_profit, discounted_net_profit) ~ company + year + scenario,
    data = yearly_discounted_df,
    FUN = sum,
    na.rm = TRUE
  )
  
  # Rename columns for clarity
  names(result)[names(result) == "revenue"] <- "total_revenue"
  names(result)[names(result) == "profit"] <- "total_profit"  
  names(result)[names(result) == "discounted_profit"] <- "total_discounted_profit"
  names(result)[names(result) == "discounted_net_profit"] <- "total_discounted_net_profit"
  
  # Validate the result
  numeric_cols <- c("total_revenue", "total_profit", "total_discounted_profit", "total_discounted_net_profit")
  for (col in numeric_cols) {
    if (!is.numeric(result[[col]])) {
      stop(paste("Calculated", col, "is not numeric"))
    }
    
    if (any(is.na(result[[col]]))) {
      stop(paste("Calculated", col, "contains NA values"))
    }
    
    # Ensure values are finite (allow negative profits for losses)
    if (col == "total_revenue") {
      # Revenue should be non-negative
      result[[col]] <- pmax(0, result[[col]])
    }
    # Profits can be negative (losses), so don't force non-negative
  }
  
  # Sort by company, scenario, and year for consistency
  result <- result[order(result$company, result$scenario, result$year), ]
  
  # Reset row names
  rownames(result) <- NULL
  
  return(result)
}
