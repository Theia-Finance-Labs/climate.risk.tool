#' Gather and Pivot Results
#'
#' Transforms scenario-based results into wide format for reporting.
#' Creates separate pivoted tables for assets and companies.
#'
#' @title Gather and Pivot Results
#' @description Pivots long-format scenario data into wide format suitable for reporting.
#'   Creates separate tables for assets (with NPV columns) and companies 
#'   (with NPV, PD, and Expected Loss columns) by scenario.
#' @param assets_discounted data.frame. Asset data with discounted_net_profit by scenario
#' @param companies_expected_loss data.frame. Company data with NPV, PD, and expected_loss by scenario
#' @return list with two elements: assets (pivoted asset data) and companies (pivoted company data)
#' @examples
#' \dontrun{
#' assets <- data.frame(company = "A", scenario = "baseline", discounted_net_profit = 1000)
#' companies <- data.frame(company = "A", scenario = "baseline", npv = 1000, 
#'                        merton_pd = 0.1, expected_loss = 100)
#' result <- gather_and_pivot_results(assets, companies)
#' }
#' @export
gather_and_pivot_results <- function(assets_discounted, companies_expected_loss) {
  # Validate inputs
  if (!is.data.frame(assets_discounted) || !is.data.frame(companies_expected_loss)) {
    stop("Both assets_discounted and companies_expected_loss must be data.frames")
  }
  
  # Check required columns for assets
  required_asset_cols <- c("scenario", "discounted_net_profit")
  missing_asset_cols <- setdiff(required_asset_cols, names(assets_discounted))
  if (length(missing_asset_cols) > 0) {
    stop(paste("Missing required columns in assets_discounted:", 
               paste(missing_asset_cols, collapse = ", ")))
  }
  
  # Check required columns for companies
  required_company_cols <- c("company", "scenario", "npv", "merton_pd", "expected_loss")
  missing_company_cols <- setdiff(required_company_cols, names(companies_expected_loss))
  if (length(missing_company_cols) > 0) {
    stop(paste("Missing required columns in companies_expected_loss:", 
               paste(missing_company_cols, collapse = ", ")))
  }
  
  # Pivot assets data
  # Group by asset identifier columns (excluding scenario and value columns)
  asset_id_cols <- setdiff(names(assets_discounted), 
                          c("scenario", "discounted_net_profit"))
  
  # Create asset NPV by scenario
  if (length(asset_id_cols) > 0) {
    # Use base R approach to pivot
    assets_baseline <- assets_discounted[assets_discounted$scenario == "baseline", ]
    assets_shock <- assets_discounted[assets_discounted$scenario == "shock", ]
    
    # Merge baseline and shock data
    if (nrow(assets_baseline) > 0 && nrow(assets_shock) > 0) {
      assets_baseline_npv <- assets_baseline[, c(asset_id_cols, "discounted_net_profit")]
      names(assets_baseline_npv)[names(assets_baseline_npv) == "discounted_net_profit"] <- "NPV_baseline"
      
      assets_shock_npv <- assets_shock[, c(asset_id_cols, "discounted_net_profit")]
      names(assets_shock_npv)[names(assets_shock_npv) == "discounted_net_profit"] <- "NPV_shock"
      
      # Join by asset identifier columns
      assets_pivoted <- merge(assets_baseline_npv, assets_shock_npv, 
                             by = asset_id_cols, all = TRUE)
    } else {
      # Handle case where one scenario is missing
      assets_pivoted <- data.frame()
    }
  } else {
    assets_pivoted <- data.frame()
  }
  
  # Pivot companies data
  companies_baseline <- companies_expected_loss[companies_expected_loss$scenario == "baseline", ]
  companies_shock <- companies_expected_loss[companies_expected_loss$scenario == "shock", ]
  
  if (nrow(companies_baseline) > 0 && nrow(companies_shock) > 0) {
    # Extract baseline values
    companies_baseline_vals <- companies_baseline[, c("company", "npv", "merton_pd", "expected_loss")]
    names(companies_baseline_vals) <- c("company", "NPV_baseline", "PD_baseline", "Expected_loss_baseline")
    
    # Extract shock values
    companies_shock_vals <- companies_shock[, c("company", "npv", "merton_pd", "expected_loss")]
    names(companies_shock_vals) <- c("company", "NPV_shock", "PD_shock", "Expected_loss_shock")
    
    # Join baseline and shock data
    companies_pivoted <- merge(companies_baseline_vals, companies_shock_vals, 
                              by = "company", all = TRUE)
  } else {
    # Handle case where one scenario is missing
    companies_pivoted <- data.frame(company = character(0),
                                   NPV_baseline = numeric(0),
                                   NPV_shock = numeric(0),
                                   PD_baseline = numeric(0),
                                   PD_shock = numeric(0),
                                   Expected_loss_baseline = numeric(0),
                                   Expected_loss_shock = numeric(0))
  }
  
  # Return list with both pivoted tables
  result <- list(
    assets = assets_pivoted,
    companies = companies_pivoted
  )
  
  return(result)
}
