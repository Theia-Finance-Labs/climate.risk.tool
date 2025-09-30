# Tests for compute_yearly_baseline_profits

testthat::test_that("compute_yearly_baseline_profits applies profit margin correctly", {
  yearly_revenue <- data.frame(
    asset = c("A1", "A1"),
    company = c("C1", "C1"),
    year = c(2025, 2026),
    baseline_revenue = c(1000, 1020)
  )

  result <- compute_yearly_baseline_profits(yearly_revenue, net_profit_margin = 0.1)

  # Should have all original columns plus baseline_profit
  expected_cols <- c("asset", "company", "year", "baseline_revenue", "baseline_profit")
  testthat::expect_true(all(expected_cols %in% names(result)))

  # Should preserve row count
  testthat::expect_equal(nrow(result), nrow(yearly_revenue))

  # Should apply profit margin correctly
  expected_profits <- yearly_revenue$baseline_revenue * 0.1
  testthat::expect_equal(result$baseline_profit, expected_profits)
})

testthat::test_that("compute_yearly_baseline_profits validates inputs", {
  yearly_revenue <- data.frame(
    asset = "A1",
    company = "C1",
    year = 2025,
    baseline_revenue = 1000
  )

  # Should work with valid inputs
  testthat::expect_no_error(
    compute_yearly_baseline_profits(yearly_revenue, 0.1)
  )

  # Should error with invalid inputs
  testthat::expect_error(
    compute_yearly_baseline_profits(NULL, 0.1),
    regexp = "non-empty data.frame"
  )

  testthat::expect_error(
    compute_yearly_baseline_profits(yearly_revenue, "invalid"),
    regexp = "single numeric value"
  )

  # Should error with missing columns
  incomplete_data <- data.frame(asset = "A1", year = 2025)
  testthat::expect_error(
    compute_yearly_baseline_profits(incomplete_data, 0.1),
    regexp = "Missing required columns"
  )
})

testthat::test_that("compute_yearly_baseline_profits handles edge cases", {
  # Test with zero revenue
  zero_revenue <- data.frame(
    asset = "A1",
    company = "C1",
    year = 2025,
    baseline_revenue = 0
  )

  result <- compute_yearly_baseline_profits(zero_revenue, 0.1)
  testthat::expect_equal(result$baseline_profit, 0)

  # Test with zero margin
  normal_revenue <- data.frame(
    asset = "A1",
    company = "C1",
    year = 2025,
    baseline_revenue = 1000
  )

  result_zero_margin <- compute_yearly_baseline_profits(normal_revenue, 0.0)
  testthat::expect_equal(result_zero_margin$baseline_profit, 0)

  # Test with high margin
  result_high_margin <- compute_yearly_baseline_profits(normal_revenue, 0.5)
  testthat::expect_equal(result_high_margin$baseline_profit, 500)
})
