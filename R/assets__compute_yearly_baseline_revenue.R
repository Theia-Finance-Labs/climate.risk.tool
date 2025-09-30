#' Compute Yearly Baseline Revenue Trajectories for Assets
#'
#' @title Compute Yearly Baseline Revenue Trajectories
#' @description Computes yearly baseline revenue trajectories for assets from 2025 onwards.
#'   Starting with 2025 revenues = company_revenue * share_of_economic_activity,
#'   then applying growth rate for subsequent years: 2026 = 2025 * (1 + growth_rate), etc.
#' @param baseline_assets data.frame with columns: asset, company, share_of_economic_activity
#' @param companies data.frame with columns: company_name, revenues
#' @param growth_rate numeric. Annual growth rate (default: 0.02)
#' @param start_year numeric. Starting year for projections (default: 2025)
#' @param end_year numeric. Ending year for projections (default: 2050)
#' @return data.frame with columns: asset, company, year, baseline_revenue
#' @examples
#' \dontrun{
#' baseline_assets <- data.frame(
#'   asset = c("A1", "A2"),
#'   company = c("C1", "C1"),
#'   share_of_economic_activity = c(0.6, 0.4)
#' )
#' companies <- data.frame(
#'   company_name = "C1",
#'   revenues = 1000
#' )
#' result <- compute_yearly_baseline_revenue(baseline_assets, companies, 0.02)
#' }
#' @export
compute_yearly_baseline_revenue <- function(
    baseline_assets,
    companies,
    growth_rate = 0.02,
    start_year = 2025,
    end_year = 2050) {
  # Validate inputs
  if (!is.data.frame(baseline_assets) || nrow(baseline_assets) == 0) {
    stop("baseline_assets must be a non-empty data.frame")
  }
  if (!is.data.frame(companies) || nrow(companies) == 0) {
    stop("companies must be a non-empty data.frame")
  }
  if (!is.numeric(growth_rate) || length(growth_rate) != 1) {
    stop("growth_rate must be a single numeric value")
  }
  if (!is.numeric(start_year) || !is.numeric(end_year) || start_year >= end_year) {
    stop("start_year and end_year must be numeric with start_year < end_year")
  }

  # Check required columns
  required_asset_cols <- c("asset", "company", "share_of_economic_activity")
  missing_asset_cols <- setdiff(required_asset_cols, names(baseline_assets))
  if (length(missing_asset_cols) > 0) {
    stop(paste(
      "Missing required columns in baseline_assets:",
      paste(missing_asset_cols, collapse = ", ")
    ))
  }

  required_company_cols <- c("company_name", "revenues")
  missing_company_cols <- setdiff(required_company_cols, names(companies))
  if (length(missing_company_cols) > 0) {
    stop(paste(
      "Missing required columns in companies:",
      paste(missing_company_cols, collapse = ", ")
    ))
  }

  # Prepare companies data for joining
  companies_for_join <- companies[, c("company_name", "revenues"), drop = FALSE]
  names(companies_for_join) <- c("company", "revenues")

  # Check for unmatched companies
  asset_companies <- unique(baseline_assets$company)
  company_companies <- unique(companies_for_join$company)
  unmatched <- setdiff(asset_companies, company_companies)


  # Join assets with company data
  assets_with_companies <- merge(baseline_assets, companies_for_join,
    by = "company", all.x = TRUE
  )

  # Check for any failed joins
  if (any(is.na(assets_with_companies$revenues))) {
    stop("Join failed: some assets could not be matched to companies")
  }

  # Calculate 2025 baseline revenue for each asset
  # Formula: company_revenue * share_of_economic_activity
  assets_with_companies$revenue_2025 <- assets_with_companies$revenues *
    assets_with_companies$share_of_economic_activity

  # Generate yearly trajectories
  years <- start_year:end_year

  # Create expanded data frame with one row per asset-year combination
  result_list <- lapply(seq_len(nrow(assets_with_companies)), function(i) {
    asset_row <- assets_with_companies[i, ]

    # Calculate revenue for each year
    yearly_data <- data.frame(
      asset = rep(asset_row$asset, length(years)),
      company = rep(asset_row$company, length(years)),
      year = years,
      stringsAsFactors = FALSE
    )

    # Apply growth rate: revenue_year = revenue_2025 * (1 + growth_rate)^(year - 2025)
    yearly_data$baseline_revenue <- asset_row$revenue_2025 *
      (1 + growth_rate)^(years - start_year)

    yearly_data
  })

  # Combine all asset trajectories
  result <- do.call(rbind, result_list)

  # Validate the result
  if (!is.numeric(result$baseline_revenue)) {
    stop("Calculated baseline_revenue is not numeric")
  }

  if (any(is.na(result$baseline_revenue))) {
    stop("Calculated baseline_revenue contains NA values")
  }

  # Ensure revenue is non-negative
  result$baseline_revenue <- pmax(0, result$baseline_revenue)

  return(result)
}
