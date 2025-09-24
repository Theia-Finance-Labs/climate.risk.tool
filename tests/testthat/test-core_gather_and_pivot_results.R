# Tests for function: gather_and_pivot_results


testthat::test_that("gather_and_pivot_results returns pivoted assets and companies", {
  data <- create_baseline_and_shock()
  scen <- build_scenarios(data$baseline, data$shocked)
  rev <- compute_asset_revenue(scen, data$companies, growth_rate = 0.03)
  prof <- compute_asset_profits(rev, net_profit_margin = 0.2)
  dnp <- discount_net_profits(prof, discount_rate = 0.1)
  npv <- compute_company_npv(dnp)
  pd <- compute_company_pd_merton(npv)
  el <- compute_expected_loss(pd)

  piv <- gather_and_pivot_results(dnp, el)
  testthat::expect_type(piv, "list")
  testthat::expect_true(all(c("companies", "assets") %in% names(piv)))

  comps <- piv$companies
  assets <- piv$assets

  # Companies: NPV, PD, Expected loss for baseline and shock
  expected_company_cols <- c(
    "NPV_baseline", "NPV_shock",
    "PD_baseline", "PD_shock",
    "Expected_loss_baseline", "Expected_loss_shock"
  )
  testthat::expect_true(all(expected_company_cols %in% names(comps)))

  # Assets: NPV for baseline and shock
  expected_asset_cols <- c("NPV_baseline", "NPV_shock")
  testthat::expect_true(all(expected_asset_cols %in% names(assets)))
})