# Tests for compute_baseline_trajectories

testthat::test_that("compute_baseline_trajectories integrates baseline revenue and profit computation", {
  baseline_assets <- data.frame(
    asset = c("A1", "A2"),
    company = c("C1", "C1"),
    share_of_economic_activity = c(0.6, 0.4)
  )

  companies <- data.frame(
    company_name = "C1",
    revenues = 1000
  )

  result <- compute_baseline_trajectories(
    baseline_assets, companies,
    growth_rate = 0.02, net_profit_margin = 0.1,
    start_year = 2025, end_year = 2027
  )

  # Should have both revenue and profit columns
  expected_cols <- c("asset", "company", "year", "revenue", "profit")
  testthat::expect_true(all(expected_cols %in% names(result)))

  # Should have correct number of rows (2 assets * 3 years = 6 rows)
  testthat::expect_equal(nrow(result), 6)

  # Should have correct profit calculation (revenue * margin)
  testthat::expect_equal(result$profit, result$revenue * 0.1)

  # Should have growth applied correctly
  a1_2025 <- result$revenue[result$asset == "A1" & result$year == 2025]
  a1_2026 <- result$revenue[result$asset == "A1" & result$year == 2026]

  testthat::expect_equal(a1_2026, a1_2025 * 1.02, tolerance = 1e-8)
})

testthat::test_that("compute_baseline_trajectories validates parameters", {
  baseline_assets <- data.frame(
    asset = "A1",
    company = "C1",
    share_of_economic_activity = 0.5
  )

  companies <- data.frame(
    company_name = "C1",
    revenues = 1000
  )

  # Should work with valid inputs
  testthat::expect_no_error(
    compute_baseline_trajectories(baseline_assets, companies)
  )

  # Should error with invalid inputs
  testthat::expect_error(
    compute_baseline_trajectories(NULL, companies),
    info = "Should error with NULL baseline_assets"
  )

  testthat::expect_error(
    compute_baseline_trajectories(baseline_assets, NULL),
    info = "Should error with NULL companies"
  )
})
