#' Filter Assets Based on Available Companies
#'
#' @title Filter Assets by Companies
#' @description Filters assets to only include those belonging to companies that exist in the companies data.
#'   This ensures that all assets can be properly matched to company financial data during analysis.
#' @param assets tibble with columns: company, asset, and other asset attributes
#' @param companies tibble with columns: company_name and other company attributes
#' @return tibble containing only assets that belong to companies present in the companies data
#' @examples
#' \dontrun{
#' # Filter assets to only include those with matching companies
#' filtered_assets <- filter_assets_by_companies(assets, companies)
#' }
#' @export
filter_assets_by_companies <- function(assets, companies) {
  # Get unique company names from both datasets
  asset_companies <- assets |>
    dplyr::distinct(.data$company) |>
    dplyr::pull(.data$company)
  
  company_names <- companies |>
    dplyr::distinct(.data$company) |>
    dplyr::pull(.data$company)

  # Find companies that exist in both datasets
  valid_companies <- intersect(asset_companies, company_names)

  # Filter assets to only include those with valid companies
  filtered_assets <- assets |>
    dplyr::filter(.data$company %in% valid_companies)

  return(filtered_assets)
}
