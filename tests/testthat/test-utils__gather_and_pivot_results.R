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

  piv <- gather_and_pivot_results(assets_discounted, companies_el)
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
  
  # Check specific values
  testthat::expect_equal(comps$NPV_baseline, 180)
  testthat::expect_equal(comps$NPV_shock, 171)
  testthat::expect_equal(comps$PD_baseline, 0.05)
  testthat::expect_equal(comps$PD_shock, 0.08)
})

testthat::test_that("gather_and_pivot_results handles edge cases", {
  # Test with single scenario
  assets_single <- data.frame(
    asset = "A1",
    company = "C1",
    scenario = "baseline",
    discounted_net_profit = 100
  )
  
  companies_single <- data.frame(
    company = "C1",
    scenario = "baseline",
    npv = 100,
    merton_pd = 0.05,
    expected_loss = 5
  )
  
  # Should handle single scenario gracefully
  testthat::expect_no_error(
    gather_and_pivot_results(assets_single, companies_single)
  )
})

testthat::test_that("gather_and_pivot_results validates inputs", {
  assets_discounted <- data.frame(
    asset = "A1",
    company = "C1",
    scenario = "baseline",
    discounted_net_profit = 100
  )
  
  companies_el <- data.frame(
    company = "C1",
    scenario = "baseline",
    npv = 100,
    merton_pd = 0.05,
    expected_loss = 5
  )
  
  # Should work with valid inputs
  testthat::expect_no_error(gather_and_pivot_results(assets_discounted, companies_el))
  
  # Should error with invalid inputs
  testthat::expect_error(
    gather_and_pivot_results("not_a_dataframe", companies_el),
    info = "Should error with non-data.frame assets"
  )
  
  testthat::expect_error(
    gather_and_pivot_results(assets_discounted, "not_a_dataframe"),
    info = "Should error with non-data.frame companies"
  )
})