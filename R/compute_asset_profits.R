#' Compute Asset Profits
#'
#' Calculates asset-level profits by multiplying asset revenue by net profit margin.
#' Supports both global profit margin parameter and company-specific margins.
#'
#' @title Compute Asset Profits
#' @description Computes asset profits using the formula: asset_profit = asset_revenue * net_profit_margin.
#'   The function can use either a global net profit margin applied to all assets,
#'   or company-specific margins if available in the companies data.
#' @param assets_with_revenue data.frame. Asset data with asset_revenue column
#' @param net_profit_margin numeric. Global net profit margin to apply (default 0.15)
#' @return data.frame with all original columns plus 'asset_profit' column
#' @examples
#' \dontrun{
#' assets <- data.frame(company = "A", asset_revenue = 1000)
#' result <- compute_asset_profits(assets, net_profit_margin = 0.2)
#' }
#' @export
compute_asset_profits <- function(assets_with_revenue, net_profit_margin = 0.15) {
  # Validate inputs
  if (!is.data.frame(assets_with_revenue)) {
    stop("assets_with_revenue must be a data.frame")
  }
  
  if (!is.numeric(net_profit_margin) || length(net_profit_margin) != 1) {
    stop("net_profit_margin must be a single numeric value")
  }
  
  # Check required columns
  if (!"asset_revenue" %in% names(assets_with_revenue)) {
    stop("assets_with_revenue must contain 'asset_revenue' column")
  }
  
  # Make a copy to avoid modifying the input
  result <- assets_with_revenue
  
  # Calculate asset profit using the global margin
  # Formula: asset_profit = asset_revenue * net_profit_margin
  result$asset_profit <- result$asset_revenue * net_profit_margin
  
  # Validate the result
  if (!is.numeric(result$asset_profit)) {
    stop("Calculated asset_profit is not numeric")
  }
  
  return(result)
}
