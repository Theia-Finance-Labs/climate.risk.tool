# Tests for compute_yearly_baseline_revenue

testthat::test_that("compute_yearly_baseline_revenue creates yearly trajectories", {
  baseline_assets <- data.frame(
    asset = c("A1", "A2"),
    company = c("C1", "C1"),
    share_of_economic_activity = c(0.6, 0.4)
  )

  companies <- data.frame(
    company = "C1",
    revenues = 1000
  )

  result <- compute_yearly_baseline_revenue(
    baseline_assets, companies,
    growth_rate = 0.02, start_year = 2025, end_year = 2027
  )

  # Should have required columns
  expected_cols <- c("asset", "company", "year", "revenue")
  testthat::expect_true(all(expected_cols %in% names(result)))

  # Should have correct number of rows (2 assets * 3 years = 6 rows)
  testthat::expect_equal(nrow(result), 6)

  # Should have correct years
  testthat::expect_equal(sort(unique(result$year)), c(2025, 2026, 2027))

  # Should have correct assets and companies
  testthat::expect_equal(sort(unique(result$asset)), c("A1", "A2"))
  testthat::expect_equal(unique(result$company), "C1")
})

testthat::test_that("compute_yearly_baseline_revenue applies correct growth formula", {
  baseline_assets <- data.frame(
    asset = "A1",
    company = "C1",
    share_of_economic_activity = 0.5
  )

  companies <- data.frame(
    company = "C1",
    revenues = 1000
  )

  result <- compute_yearly_baseline_revenue(
    baseline_assets, companies,
    growth_rate = 0.02, start_year = 2025, end_year = 2027
  )

  # Check growth formula: 2025 = 500, 2026 = 500 * 1.02 = 510, 2027 = 510 * 1.02 = 520.2
  expected_2025 <- 1000 * 0.5 # 500
  expected_2026 <- expected_2025 * 1.02 # 510
  expected_2027 <- expected_2026 * 1.02 # 520.2

  actual_2025 <- result$revenue[result$year == 2025]
  actual_2026 <- result$revenue[result$year == 2026]
  actual_2027 <- result$revenue[result$year == 2027]

  testthat::expect_equal(actual_2025, expected_2025, tolerance = 1e-8)
  testthat::expect_equal(actual_2026, expected_2026, tolerance = 1e-8)
  testthat::expect_equal(actual_2027, expected_2027, tolerance = 1e-8)
})

testthat::test_that("compute_yearly_baseline_revenue handles multiple assets and companies", {
  baseline_assets <- data.frame(
    asset = c("A1", "A2", "B1"),
    company = c("C1", "C1", "C2"),
    share_of_economic_activity = c(0.6, 0.4, 1.0)
  )

  companies <- data.frame(
    company = c("C1", "C2"),
    revenues = c(1000, 500)
  )

  result <- compute_yearly_baseline_revenue(
    baseline_assets, companies,
    growth_rate = 0.02, start_year = 2025, end_year = 2026
  )

  # Should have 3 assets * 2 years = 6 rows
  testthat::expect_equal(nrow(result), 6)

  # Check revenue allocation
  c1_assets <- result[result$company == "C1" & result$year == 2025, ]
  c2_assets <- result[result$company == "C2" & result$year == 2025, ]

  # C1: A1 = 1000 * 0.6 = 600, A2 = 1000 * 0.4 = 400
  # C2: B1 = 500 * 1.0 = 500
  testthat::expect_equal(c1_assets$revenue[c1_assets$asset == "A1" & c1_assets$company == "C1"], 600)
  testthat::expect_equal(c1_assets$revenue[c1_assets$asset == "A2" & c1_assets$company == "C1"], 400)
  testthat::expect_equal(c2_assets$revenue, 500)
})
