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

  # Get unique company names from both datasets
  asset_companies <- unique(assets$company)
  company_names <- unique(companies$company)

  # Find companies that exist in both datasets
  valid_companies <- intersect(asset_companies, company_names)

  # Filter assets to only include those with valid companies
  filtered_assets <- assets[assets$company %in% valid_companies, , drop = FALSE]


  return(filtered_assets)
}
