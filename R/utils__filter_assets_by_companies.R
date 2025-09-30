#' Filter Assets Based on Available Companies
#'
#' @title Filter Assets by Companies
#' @description Filters assets to only include those belonging to companies that exist in the companies data.
#'   This ensures that all assets can be properly matched to company financial data during analysis.
#' @param assets data.frame with columns: company, asset, and other asset attributes
#' @param companies data.frame with columns: company_name and other company attributes
#' @return data.frame containing only assets that belong to companies present in the companies data
#' @examples
#' \dontrun{
#' # Filter assets to only include those with matching companies
#' filtered_assets <- filter_assets_by_companies(assets, companies)
#' }
#' @export
filter_assets_by_companies <- function(assets, companies) {
  # Validate inputs
  if (!is.data.frame(assets) || nrow(assets) == 0) {
    stop("assets must be a non-empty data.frame")
  }
  if (!is.data.frame(companies) || nrow(companies) == 0) {
    stop("companies must be a non-empty data.frame")
  }

  # Check required columns
  if (!"company" %in% names(assets)) {
    stop("assets must contain a 'company' column")
  }
  if (!"company_name" %in% names(companies)) {
    stop("companies must contain a 'company_name' column")
  }

  # Get unique company names from both datasets
  asset_companies <- unique(assets$company)
  company_names <- unique(companies$company_name)

  # Find companies that exist in both datasets
  valid_companies <- intersect(asset_companies, company_names)

  # Filter assets to only include those with valid companies
  filtered_assets <- assets[assets$company %in% valid_companies, , drop = FALSE]

  # Provide informative message about filtering
  removed_companies <- setdiff(asset_companies, company_names)
  if (length(removed_companies) > 0) {
    message(paste(
      "Filtered out assets from companies not found in companies data:",
      paste(removed_companies, collapse = ", ")
    ))
    message(paste(
      "Kept", nrow(filtered_assets), "assets from", length(valid_companies), "companies"
    ))
  } else {
    message(paste(
      "All assets retained:", nrow(filtered_assets), "assets from", length(valid_companies), "companies"
    ))
  }

  # Validate result
  if (nrow(filtered_assets) == 0) {
    stop("No assets remain after filtering - no companies match between assets and companies data")
  }

  return(filtered_assets)
}
