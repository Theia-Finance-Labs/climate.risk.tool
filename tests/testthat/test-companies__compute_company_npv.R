# Tests for function: compute_company_npv

testthat::test_that("compute_company_npv aggregates asset discounted profits by company and scenario", {
  # Use new helper to create discounted asset data
  discounted_assets <- create_discounted_assets()

  out <- compute_company_npv(discounted_assets)
  
  # Should have expected columns
  expected_cols <- c("company", "scenario", "npv")
  testthat::expect_true(all(expected_cols %in% names(out)))
  testthat::expect_true(is.numeric(out$npv))
  
  # Should have one row per company-scenario combination
  testthat::expect_equal(nrow(out), 2)  # C1-baseline, C1-shock
  testthat::expect_equal(sort(out$scenario), c("baseline", "shock"))
})

testthat::test_that("compute_company_npv performs exact aggregation from assets", {
  # Create test data with known values
  discounted_assets <- data.frame(
    asset = c("A1", "A1", "A2", "A2"),
    company = c("C1", "C1", "C1", "C1"),
    scenario = c("baseline", "shock", "baseline", "shock"),
    discounted_net_profit = c(100, 95, 80, 76)
  )
  
  out <- compute_company_npv(discounted_assets)
  
  # Should aggregate correctly: baseline = 100 + 80 = 180, shock = 95 + 76 = 171
  baseline_npv <- out$npv[out$scenario == "baseline"]
  shock_npv <- out$npv[out$scenario == "shock"]
  
  testthat::expect_equal(baseline_npv, 180)
  testthat::expect_equal(shock_npv, 171)
})

testthat::test_that("compute_company_npv handles multiple companies", {
  # Test with multiple companies
  discounted_assets <- data.frame(
    asset = c("A1", "A1", "B1", "B1"),
    company = c("C1", "C1", "C2", "C2"),
    scenario = c("baseline", "shock", "baseline", "shock"),
    discounted_net_profit = c(100, 95, 50, 48)
  )
  
  out <- compute_company_npv(discounted_assets)
  
  # Should have 4 rows (2 companies × 2 scenarios)
  testthat::expect_equal(nrow(out), 4)
  
  # Check individual company NPVs
  c1_baseline <- out$npv[out$company == "C1" & out$scenario == "baseline"]
  c2_baseline <- out$npv[out$company == "C2" & out$scenario == "baseline"]
  
  testthat::expect_equal(c1_baseline, 100)
  testthat::expect_equal(c2_baseline, 50)
})

testthat::test_that("compute_company_npv validates schema and types", {
  # Test with valid input
  discounted_assets <- create_discounted_assets()
  out <- compute_company_npv(discounted_assets)
  
  # Schema validation
  testthat::expect_true(is.data.frame(out))
  testthat::expect_true(is.character(out$company))
  testthat::expect_true(is.numeric(out$npv))
  
  # Test with invalid input
  testthat::expect_error(
    compute_company_npv("not_a_dataframe"),
    regexp = "data.frame"
  )
  
  # Test with missing columns
  incomplete_data <- data.frame(company = "C1", scenario = "baseline")
  testthat::expect_error(
    compute_company_npv(incomplete_data),
    regexp = "must contain either.*discounted_net_profit"
  )
})

testthat::test_that("compute_company_npv maintains scenario consistency", {
  discounted_assets <- data.frame(
    asset = c("A1", "A1"),
    company = c("C1", "C1"),
    scenario = c("baseline", "shock"),
    discounted_net_profit = c(100, 95)
  )
  
  out <- compute_company_npv(discounted_assets)
  
  # Should preserve scenario ordering and types
  testthat::expect_equal(nrow(out), 2)
  testthat::expect_true(all(c("baseline", "shock") %in% out$scenario))
  
  # Check that data is properly sorted
  out_sorted <- out[order(out$company, out$scenario), ]
  testthat::expect_equal(out, out_sorted)
})

testthat::test_that("compute_company_npv handles edge cases", {
  # Test with zero profits
  zero_data <- data.frame(
    asset = "A1",
    company = "C1",
    scenario = "baseline",
    discounted_net_profit = 0
  )
  
  out_zero <- compute_company_npv(zero_data)
  testthat::expect_equal(out_zero$npv, 0)
  
  # Test with negative profits
  negative_data <- data.frame(
    asset = "A1",
    company = "C1",
    scenario = "baseline",
    discounted_net_profit = -50
  )
  
  out_negative <- compute_company_npv(negative_data)
  testthat::expect_equal(out_negative$npv, -50)
  
  # Test with single asset
  single_data <- data.frame(
    asset = "A1",
    company = "C1",
    scenario = "baseline",
    discounted_net_profit = 100
  )
  
  out_single <- compute_company_npv(single_data)
  testthat::expect_equal(nrow(out_single), 1)
  testthat::expect_equal(out_single$npv, 100)
})