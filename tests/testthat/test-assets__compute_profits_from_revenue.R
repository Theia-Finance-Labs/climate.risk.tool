# Tests for compute_profits_from_revenue

testthat::test_that("compute_profits_from_revenue works without errors", {
  yearly_revenue <- data.frame(
    asset = c("A1", "A1", "A2", "A2"),
    company = c("C1", "C1", "C1", "C1"),
    year = c(2025, 2026, 2025, 2026),
    revenue = c(1000, 1020, 500, 510)
  )

  companies <- data.frame(
    company = "C1",
    net_profit_margin = 0.1
  )

  result <- compute_profits_from_revenue(
    yearly_revenue,
    companies
  )

  # Should add profit column
  testthat::expect_true("profit" %in% names(result))
  testthat::expect_equal(nrow(result), nrow(yearly_revenue))

  # Should calculate profit correctly: profit = revenue * margin
  testthat::expect_equal(result$profit, result$revenue * 0.1)

  # Should preserve all original columns
  testthat::expect_true(all(names(yearly_revenue) %in% names(result)))
})

testthat::test_that("compute_profits_from_revenue uses company-specific net profit margins", {
  yearly_revenue <- data.frame(
    asset = c("A1", "A1", "A2", "A2"),
    company = c("C1", "C1", "C2", "C2"),
    year = c(2025, 2026, 2025, 2026),
    revenue = c(1000, 1020, 500, 510)
  )

  companies <- data.frame(
    company = c("C1", "C2"),
    net_profit_margin = c(0.1, 0.15)
  )

  result <- compute_profits_from_revenue(
    yearly_revenue,
    companies
  )

  # C1 should use 0.1 margin
  c1_rows <- result$company == "C1"
  testthat::expect_equal(result$profit[c1_rows], result$revenue[c1_rows] * 0.1)

  # C2 should use 0.15 margin
  c2_rows <- result$company == "C2"
  testthat::expect_equal(result$profit[c2_rows], result$revenue[c2_rows] * 0.15)
})
