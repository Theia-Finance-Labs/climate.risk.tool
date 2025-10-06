# Tests for compute_profits_from_revenue

testthat::test_that("compute_profits_from_revenue works without errors", {
  yearly_revenue <- data.frame(
    asset = c("A1", "A1", "A2", "A2"),
    company = c("C1", "C1", "C1", "C1"),
    year = c(2025, 2026, 2025, 2026),
    revenue = c(1000, 1020, 500, 510)
  )

  result <- compute_profits_from_revenue(
    yearly_revenue,
    net_profit_margin = 0.1
  )

  # Should add baseline_profit column
  testthat::expect_true("profit" %in% names(result))
  testthat::expect_equal(nrow(result), nrow(yearly_revenue))

  # Should calculate profit correctly: profit = revenue * margin
  testthat::expect_equal(result$profit, result$revenue * 0.1)

  # Should preserve all original columns
  testthat::expect_true(all(names(yearly_revenue) %in% names(result)))
})
