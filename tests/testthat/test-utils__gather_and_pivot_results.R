# Tests for function: gather_and_pivot_results

testthat::test_that("gather_and_pivot_results returns pivoted assets and companies", {
  # Create asset-scenario data directly
  assets_discounted <- data.frame(
    asset = c("A1", "A1", "A2", "A2"),
    company = c("C1", "C1", "C1", "C1"),
    scenario = c("baseline", "shock", "baseline", "shock"),
    discounted_net_profit = c(100, 95, 80, 76)
  )

  # Create company expected loss data
  companies_el <- data.frame(
    company = c("C1", "C1"),
    scenario = c("baseline", "shock"),
    npv = c(180, 171),
    merton_pd = c(0.05, 0.08),
    expected_loss = c(15, 20)
  )

  comps <- gather_and_pivot_results(companies_el)
  testthat::expect_s3_class(comps, "data.frame")

  # Companies: NPV, PD, Expected loss for baseline and shock
  expected_company_cols <- c(
    "NPV_baseline", "NPV_shock",
    "PD_baseline", "PD_shock",
    "Expected_loss_baseline", "Expected_loss_shock"
  )
  testthat::expect_true(all(expected_company_cols %in% names(comps)))

  # Check specific values
  testthat::expect_equal(comps$NPV_baseline, 180)
  testthat::expect_equal(comps$NPV_shock, 171)
  testthat::expect_equal(comps$PD_baseline, 0.05)
  testthat::expect_equal(comps$PD_shock, 0.08)
})
