# Tests for compute_profits_from_revenue

testthat::test_that("compute_profits_from_revenue works with baseline_revenue column", {
  yearly_revenue <- data.frame(
    asset = c("A1", "A1", "A2", "A2"),
    company = c("C1", "C1", "C1", "C1"),
    year = c(2025, 2026, 2025, 2026),
    baseline_revenue = c(1000, 1020, 500, 510)
  )

  result <- compute_profits_from_revenue(
    yearly_revenue,
    revenue_col = "baseline_revenue",
    profit_col = "baseline_profit",
    net_profit_margin = 0.1
  )

  # Should add baseline_profit column
  testthat::expect_true("baseline_profit" %in% names(result))
  testthat::expect_equal(nrow(result), nrow(yearly_revenue))

  # Should calculate profit correctly: profit = revenue * margin
  testthat::expect_equal(result$baseline_profit, result$baseline_revenue * 0.1)

  # Should preserve all original columns
  testthat::expect_true(all(names(yearly_revenue) %in% names(result)))
})

testthat::test_that("compute_profits_from_revenue works with shocked_revenue column", {
  yearly_revenue <- data.frame(
    asset = c("A1", "A1"),
    company = c("C1", "C1"),
    year = c(2025, 2026),
    shocked_revenue = c(950, 970)
  )

  result <- compute_profits_from_revenue(
    yearly_revenue,
    revenue_col = "shocked_revenue",
    profit_col = "shocked_profit",
    net_profit_margin = 0.15
  )

  # Should add shocked_profit column
  testthat::expect_true("shocked_profit" %in% names(result))
  
  # Should calculate profit correctly
  testthat::expect_equal(result$shocked_profit, result$shocked_revenue * 0.15)
})

testthat::test_that("compute_profits_from_revenue works with custom column names", {
  yearly_revenue <- data.frame(
    asset = c("A1"),
    company = c("C1"),
    year = c(2025),
    my_revenue = c(1000)
  )

  result <- compute_profits_from_revenue(
    yearly_revenue,
    revenue_col = "my_revenue",
    profit_col = "my_profit",
    net_profit_margin = 0.2
  )

  # Should create custom profit column
  testthat::expect_true("my_profit" %in% names(result))
  testthat::expect_equal(result$my_profit, 200)
})

testthat::test_that("compute_profits_from_revenue validates inputs", {
  valid_data <- data.frame(
    asset = "A1",
    company = "C1",
    year = 2025,
    baseline_revenue = 1000
  )

  # Should error with missing data frame
  testthat::expect_error(
    compute_profits_from_revenue(NULL, "baseline_revenue", "baseline_profit", 0.1),
    "yearly_revenue_df must be a non-empty data.frame"
  )

  # Should error with empty data frame
  testthat::expect_error(
    compute_profits_from_revenue(data.frame(), "baseline_revenue", "baseline_profit", 0.1),
    "yearly_revenue_df must be a non-empty data.frame"
  )

  # Should error with missing revenue column
  testthat::expect_error(
    compute_profits_from_revenue(valid_data, "nonexistent_revenue", "profit", 0.1),
    "revenue column 'nonexistent_revenue' not found"
  )

  # Should error with invalid margin
  testthat::expect_error(
    compute_profits_from_revenue(valid_data, "baseline_revenue", "baseline_profit", "not_numeric"),
    "net_profit_margin must be a single numeric value"
  )
})

testthat::test_that("compute_profits_from_revenue handles edge cases", {
  # Zero revenue
  zero_revenue <- data.frame(
    asset = "A1",
    company = "C1",
    year = 2025,
    baseline_revenue = 0
  )

  result <- compute_profits_from_revenue(zero_revenue, "baseline_revenue", "baseline_profit", 0.1)
  testthat::expect_equal(result$baseline_profit, 0)

  # Negative revenue (should be made non-negative)
  negative_revenue <- data.frame(
    asset = "A1",
    company = "C1",
    year = 2025,
    baseline_revenue = -100
  )

  result <- compute_profits_from_revenue(negative_revenue, "baseline_revenue", "baseline_profit", 0.1)
  testthat::expect_true(result$baseline_profit >= 0)
})

