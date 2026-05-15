#' Input column catalog for assets and companies
#'
#' @return List of required and optional input columns
#' @noRd
get_input_columns_catalog <- function() {
  list(
    assets_required = c(
      "asset", "company", "share_of_economic_activity"
    ),
    assets_optional = c(
      "asset_category", "asset_subtype", "latitude", "longitude",
      "municipality", "municipality_code", "state", "state_code",
      "size_in_m2", "size_in_hectare", "cnae", "cost_factor"
    ),
    companies_required = c(
      "company", "revenues", "debt", "volatility",
      "net_profit_margin", "loan_size", "lgd", "term"
    ),
    companies_optional = character(0),
    computed = c(
      "revenue", "profit", "year", "event_year",
      "scenario_name", "return_period", "season",
      "hazard_type", "hazard_indicator", "hazard_name"
    )
  )
}

#' All known input-like columns for formulas and UI
#'
#' @return Character vector of column names
#' @noRd
get_input_columns <- function() {
  catalog <- get_input_columns_catalog()
  unique(c(
    catalog$assets_required,
    catalog$assets_optional,
    catalog$companies_required,
    catalog$companies_optional,
    catalog$computed
  ))
}
