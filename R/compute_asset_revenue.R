#' Compute Asset Revenue
#'
#' Calculates asset-level revenue by distributing company revenues to assets 
#' based on their share of economic activity and applying growth rates.
#'
#' @title Compute Asset Revenue
#' @description Joins asset data with company data and computes asset-level revenue.
#'   Revenue is allocated to each asset based on: 
#'   company_revenue * (1 + growth_rate) * asset_share_of_economic_activity
#' @param assets_scenarios data.frame. Asset data with scenario column (from build_scenarios)
#' @param companies data.frame. Company data with company_name and revenues columns
#' @param growth_rate numeric. Growth rate to apply to company revenues (default 0.03)
#' @return data.frame with all original columns plus 'asset_revenue' column
#' @examples
#' \dontrun{
#' assets <- data.frame(company = "A", scenario = "baseline", 
#'                     share_of_economic_activity = 0.5)
#' companies <- data.frame(company_name = "A", revenues = 1000)
#' result <- compute_asset_revenue(assets, companies, growth_rate = 0.05)
#' }
#' @export
compute_asset_revenue <- function(assets_scenarios, companies, growth_rate = 0.03) {
  # Validate inputs
  if (!is.data.frame(assets_scenarios) || !is.data.frame(companies)) {
    stop("Both assets_scenarios and companies must be data.frames")
  }
  
  if (!is.numeric(growth_rate) || length(growth_rate) != 1) {
    stop("growth_rate must be a single numeric value")
  }
  
  # Check required columns
  required_asset_cols <- c("company", "share_of_economic_activity")
  missing_asset_cols <- setdiff(required_asset_cols, names(assets_scenarios))
  if (length(missing_asset_cols) > 0) {
    stop(paste("Missing required columns in assets_scenarios:", 
               paste(missing_asset_cols, collapse = ", ")))
  }
  
  required_company_cols <- c("company_name", "revenues")
  missing_company_cols <- setdiff(required_company_cols, names(companies))
  if (length(missing_company_cols) > 0) {
    stop(paste("Missing required columns in companies:", 
               paste(missing_company_cols, collapse = ", ")))
  }
  
  # Prepare companies data for joining
  companies_for_join <- companies[, c("company_name", "revenues"), drop = FALSE]
  names(companies_for_join) <- c("company", "revenues")
  
  # Check for unmatched companies
  asset_companies <- unique(assets_scenarios$company)
  company_companies <- unique(companies_for_join$company)
  unmatched <- setdiff(asset_companies, company_companies)
  
  if (length(unmatched) > 0) {
    stop(paste("Assets contain companies not found in companies data:", 
               paste(unmatched, collapse = ", ")))
  }
  
  # Join assets with company data
  assets_with_companies <- merge(assets_scenarios, companies_for_join, 
                                by = "company", all.x = TRUE)
  
  # Check for any failed joins (should not happen due to validation above)
  if (any(is.na(assets_with_companies$revenues))) {
    stop("Join failed: some assets could not be matched to companies")
  }
  
  # Calculate asset revenue
  # Formula: company_revenue * (1 + growth_rate) * asset_share
  adjusted_company_revenue <- assets_with_companies$revenues * (1 + growth_rate)
  assets_with_companies$asset_revenue <- adjusted_company_revenue * 
    assets_with_companies$share_of_economic_activity
  
  # Remove the temporary revenues column
  assets_with_companies$revenues <- NULL
  
  # Ensure asset_revenue is numeric and non-negative
  if (!is.numeric(assets_with_companies$asset_revenue)) {
    stop("Calculated asset_revenue is not numeric")
  }
  
  # Check for any NAs in asset_revenue (shouldn't happen with valid inputs)
  if (any(is.na(assets_with_companies$asset_revenue))) {
    stop("Calculated asset_revenue contains NA values")
  }
  
  return(assets_with_companies)
}
