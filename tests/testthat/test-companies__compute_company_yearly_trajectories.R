# Tests for aggregate_assets_to_company

testthat::test_that("aggregate_assets_to_company aggregates assets to company level", {
  yearly_discounted <- data.frame(
    asset = c("A1", "A1", "A2", "A2", "A3", "A3"),
    company = c("C1", "C1", "C1", "C1", "C2", "C2"),
    year = c(2025, 2025, 2025, 2025, 2025, 2025),
    scenario = c("baseline", "shock", "baseline", "shock", "baseline", "shock"),
    revenue = c(600, 582, 400, 388, 500, 485),
    profit = c(60, 58.2, 40, 38.8, 50, 48.5),
    discounted_profit = c(60, 58.2, 40, 38.8, 50, 48.5),
    discounted_net_profit = c(60, 58.2, 40, 38.8, 50, 48.5)
  )

  result <- aggregate_assets_to_company(yearly_discounted)

  # Should aggregate by company, year, and scenario
  expected_cols <- c(
    "company", "year", "scenario", "total_revenue", "total_profit",
    "total_discounted_profit", "total_discounted_net_profit"
  )
  testthat::expect_true(all(expected_cols %in% names(result)))

  # Should have one row per company-year-scenario combination
  testthat::expect_equal(nrow(result), 4) # C1-baseline, C1-shock, C2-baseline, C2-shock

  # Check aggregation for C1 baseline (A1 + A2)
  c1_baseline <- result[result$company == "C1" & result$scenario == "baseline", ]
  expected_c1_revenue <- 600 + 400 # A1 + A2
  expected_c1_profit <- 60 + 40
  testthat::expect_equal(c1_baseline$total_revenue, expected_c1_revenue)
  testthat::expect_equal(c1_baseline$total_profit, expected_c1_profit)

  # Check aggregation for C2 (single asset A3)
  c2_baseline <- result[result$company == "C2" & result$scenario == "baseline", ]
  testthat::expect_equal(c2_baseline$total_revenue, 500)
  testthat::expect_equal(c2_baseline$total_profit, 50)
})

testthat::test_that("aggregate_assets_to_company handles multiple years", {
  yearly_discounted <- data.frame(
    asset = c("A1", "A1", "A1", "A1"),
    company = c("C1", "C1", "C1", "C1"),
    year = c(2025, 2026, 2025, 2026),
    scenario = c("baseline", "baseline", "shock", "shock"),
    revenue = c(1000, 1020, 970, 989),
    profit = c(100, 102, 97, 98.9),
    discounted_profit = c(100, 97.14, 97, 94.19),
    discounted_net_profit = c(100, 97.14, 97, 94.19)
  )

  result <- aggregate_assets_to_company(yearly_discounted)

  # Should have one row per company-year-scenario (4 combinations)
  testthat::expect_equal(nrow(result), 4)

  # Should preserve year information
  testthat::expect_equal(sort(unique(result$year)), c(2025, 2026))

  # Check specific year-scenario combinations
  c1_2025_baseline <- result[result$company == "C1" & result$year == 2025 & result$scenario == "baseline", ]
  testthat::expect_equal(c1_2025_baseline$total_revenue, 1000)
  testthat::expect_equal(c1_2025_baseline$total_discounted_net_profit, 100)
})

testthat::test_that("aggregate_assets_to_company preserves scenario information", {
  yearly_discounted <- data.frame(
    asset = c("A1", "A1"),
    company = c("C1", "C1"),
    year = c(2025, 2025),
    scenario = c("baseline", "shock"),
    revenue = c(1000, 970),
    profit = c(100, 97),
    discounted_profit = c(100, 97),
    discounted_net_profit = c(100, 97)
  )

  result <- aggregate_assets_to_company(yearly_discounted)

  # Should have separate rows for each scenario
  testthat::expect_equal(nrow(result), 2)
  testthat::expect_equal(sort(result$scenario), c("baseline", "shock"))

  # Should preserve values correctly
  baseline_row <- result[result$scenario == "baseline", ]
  shock_row <- result[result$scenario == "shock", ]
  testthat::expect_equal(baseline_row$total_revenue, 1000)
  testthat::expect_equal(shock_row$total_revenue, 970)
})
