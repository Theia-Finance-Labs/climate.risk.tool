testthat::test_that("compute_companies_financials works with yearly trajectories", {
  # Company yearly trajectories
  company_yearly <- data.frame(
    company = c("C1", "C1", "C1", "C1"),
    year = c(2025, 2026, 2025, 2026),
    scenario = c("baseline", "baseline", "shock", "shock"),
    total_revenue = c(1000, 1020, 970, 989),
    total_profit = c(100, 102, 97, 98.9),
    total_discounted_profit = c(100, 97.14, 97, 94.19),
    total_discounted_net_profit = c(100, 97.14, 97, 94.19)
  )

  # Asset yearly trajectories
  assets_yearly <- data.frame(
    asset = c("A1", "A1", "A1", "A1"),
    company = c("C1", "C1", "C1", "C1"),
    year = c(2025, 2026, 2025, 2026),
    scenario = c("baseline", "baseline", "shock", "shock"),
    revenue = c(1000, 1020, 970, 989),
    profit = c(100, 102, 97, 98.9),
    discounted_profit = c(100, 97.14, 97, 94.19),
    discounted_net_profit = c(100, 97.14, 97, 94.19)
  )

  res <- compute_companies_financials(
    company_yearly_trajectories = company_yearly,
    assets_discounted_yearly = assets_yearly,
    discount_rate = 0.05
  )

  testthat::expect_true(all(c("assets", "companies") %in% names(res)))
  testthat::expect_true(is.data.frame(res$assets))
  testthat::expect_true(is.data.frame(res$companies))

  # Should have NPV, PD, and Expected Loss columns
  expected_company_cols <- c(
    "NPV_baseline", "NPV_shock", "PD_baseline", "PD_shock",
    "Expected_loss_baseline", "Expected_loss_shock"
  )
  testthat::expect_true(all(expected_company_cols %in% names(res$companies)))
})
