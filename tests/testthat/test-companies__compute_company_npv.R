# Tests for function: compute_company_npv

testthat::test_that("compute_company_npv aggregates company yearly discounted profits by scenario", {
  # Create company yearly data with known values
  company_yearly_data <- data.frame(
    company = c("C1", "C1", "C1", "C1"),
    year = c(2025, 2026, 2025, 2026),
    scenario = c("baseline", "baseline", "shock", "shock"),
    total_discounted_net_profit = c(100, 97, 95, 92)
  )

  out <- compute_company_npv(company_yearly_data)

  # Should have expected columns
  expected_cols <- c("company", "scenario", "npv")
  testthat::expect_true(all(expected_cols %in% names(out)))
  testthat::expect_true(is.numeric(out$npv))

  # Should have one row per company-scenario combination
  testthat::expect_equal(nrow(out), 2) # C1-baseline, C1-shock
  testthat::expect_equal(sort(out$scenario), c("baseline", "shock"))
})

testthat::test_that("compute_company_npv performs exact aggregation from company yearly data", {
  # Create test data with known values across multiple years
  company_yearly_data <- data.frame(
    company = c("C1", "C1", "C1", "C1"),
    year = c(2025, 2026, 2027, 2028),
    scenario = c("baseline", "baseline", "baseline", "baseline"),
    total_discounted_net_profit = c(100, 97, 94, 91)
  )

  out <- compute_company_npv(company_yearly_data)

  # Should aggregate correctly: 100 + 97 + 94 + 91 = 382
  baseline_npv <- out$npv[out$scenario == "baseline"]
  testthat::expect_equal(baseline_npv, 382)
})

testthat::test_that("compute_company_npv handles multiple companies", {
  # Test with multiple companies
  company_yearly_data <- data.frame(
    company = c("C1", "C1", "C2", "C2"),
    year = c(2025, 2026, 2025, 2026),
    scenario = c("baseline", "baseline", "baseline", "baseline"),
    total_discounted_net_profit = c(100, 97, 50, 48)
  )

  out <- compute_company_npv(company_yearly_data)

  # Should have 2 rows (2 companies × 1 scenario)
  testthat::expect_equal(nrow(out), 2)

  # Check individual company NPVs
  c1_npv <- out$npv[out$company == "C1"]
  c2_npv <- out$npv[out$company == "C2"]

  testthat::expect_equal(c1_npv, 197) # 100 + 97
  testthat::expect_equal(c2_npv, 98) # 50 + 48
})

testthat::test_that("compute_company_npv maintains scenario consistency", {
  company_yearly_data <- data.frame(
    company = c("C1", "C1"),
    year = c(2025, 2026),
    scenario = c("baseline", "shock"),
    total_discounted_net_profit = c(100, 95)
  )

  out <- compute_company_npv(company_yearly_data)

  # Should preserve scenario ordering and types
  testthat::expect_equal(nrow(out), 2)
  testthat::expect_true(all(c("baseline", "shock") %in% out$scenario))

  # Check that data is properly sorted
  out_sorted <- out[order(out$company, out$scenario), ]
  testthat::expect_equal(out, out_sorted)
})


