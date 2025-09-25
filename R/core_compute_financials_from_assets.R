#' Compute financial results from asset scenarios
#'
#' @description Takes asset-level scenarios and computes revenue, profits, discounting,
#' and aggregates to company level, returning the same final shape as compute_risk.
#' This is extracted to decouple hazard application from financials.
#'
#' @param assets_scenarios data.frame with scenario column and share_of_economic_activity
#' @param companies data.frame of company inputs
#' @param growth_rate numeric
#' @param net_profit_margin numeric
#' @param discount_rate numeric
#' @return list(assets = data.frame, companies = data.frame)
#' @export
compute_financials_from_assets <- function(
  assets_scenarios,
  companies,
  growth_rate = 0.02,
  net_profit_margin = 0.1,
  discount_rate = 0.05
) {
  # Reuse existing pipeline functions 12-18 assuming inputs match expectations
  assets_with_revenue <- compute_asset_revenue(assets_scenarios, companies, growth_rate)
  assets_with_profits <- compute_asset_profits(assets_with_revenue, net_profit_margin)
  assets_discounted <- discount_net_profits(assets_with_profits, discount_rate)
  companies_npv <- compute_company_npv(assets_discounted)
  companies_pd <- compute_company_pd_merton(companies_npv)
  companies_el <- compute_expected_loss(companies_pd)
  final_results <- gather_and_pivot_results(assets_discounted, companies_el)
  list(assets = final_results$assets, companies = final_results$companies)
}


