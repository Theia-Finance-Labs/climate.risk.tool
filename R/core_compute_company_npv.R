#' Compute Company NPV
#'
#' Aggregates asset-level discounted net profits to company-level NPV by scenario.
#' Computes Net Present Value (NPV) for each company in each scenario.
#'
#' @title Compute Company NPV
#' @description Aggregates discounted net profits from assets to compute company-level NPV.
#'   Groups by company and scenario, summing discounted_net_profit values.
#' @param assets_with_discounted_profits data.frame. Asset data with discounted_net_profit column
#' @return data.frame with columns: company, scenario, npv
#' @examples
#' \dontrun{
#' assets <- data.frame(company = "A", scenario = "baseline", discounted_net_profit = 1000)
#' result <- compute_company_npv(assets)
#' }
#' @export
compute_company_npv <- function(assets_with_discounted_profits) {
  # Validate inputs
  if (!is.data.frame(assets_with_discounted_profits)) {
    stop("assets_with_discounted_profits must be a data.frame")
  }
  
  # Check required columns
  required_cols <- c("company", "scenario", "discounted_net_profit")
  missing_cols <- setdiff(required_cols, names(assets_with_discounted_profits))
  if (length(missing_cols) > 0) {
    stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
  }
  
  # Aggregate discounted net profits by company and scenario
  # Using base R to avoid dependencies
  result <- aggregate(
    discounted_net_profit ~ company + scenario,
    data = assets_with_discounted_profits,
    FUN = sum,
    na.rm = TRUE
  )
  
  # Rename the aggregated column to npv (snake_case preferred)
  names(result)[names(result) == "discounted_net_profit"] <- "npv"
  
  # Ensure proper column types
  result$company <- as.character(result$company)
  
  # Preserve scenario as ordered factor if it was one
  if (is.factor(assets_with_discounted_profits$scenario)) {
    if (is.ordered(assets_with_discounted_profits$scenario)) {
      result$scenario <- factor(result$scenario, 
                               levels = levels(assets_with_discounted_profits$scenario),
                               ordered = TRUE)
    } else {
      result$scenario <- factor(result$scenario, 
                               levels = levels(assets_with_discounted_profits$scenario))
    }
  }
  
  # Ensure npv is numeric
  if (!is.numeric(result$npv)) {
    stop("Calculated NPV is not numeric")
  }
  
  # Sort by company and scenario for consistent output
  result <- result[order(result$company, result$scenario), ]
  
  # Reset row names
  rownames(result) <- NULL
  
  return(result)
}
