# Tests for function: build_yearly_scenarios

testthat::test_that("build_yearly_scenarios concatenates baseline and shocked trajectories", {
  yearly_baseline <- data.frame(
    asset = c("A1", "A1", "A2", "A2"),
    company = c("C1", "C1", "C1", "C1"),
    year = c(2025, 2026, 2025, 2026),
    baseline_revenue = c(1000, 1020, 800, 816),
    baseline_profit = c(100, 102, 80, 81.6)
  )

  yearly_shocked <- data.frame(
    asset = c("A1", "A1", "A2", "A2"),
    company = c("C1", "C1", "C1", "C1"),
    year = c(2025, 2026, 2025, 2026),
    shocked_revenue = c(1000, 979, 800, 783),
    shocked_profit = c(100, 97.9, 80, 78.3)
  )

  result <- build_yearly_scenarios(yearly_baseline, yearly_shocked)

  # Should have scenario column with baseline and shock values
  testthat::expect_true("scenario" %in% names(result))
  testthat::expect_true(all(c("baseline", "shock") %in% unique(result$scenario)))
  testthat::expect_equal(nrow(result), nrow(yearly_baseline) + nrow(yearly_shocked))

  # Should have revenue and profit columns (renamed from baseline_/shocked_)
  expected_cols <- c("asset", "company", "year", "scenario", "revenue", "profit")
  testthat::expect_true(all(expected_cols %in% names(result)))
})

testthat::test_that("build_yearly_scenarios preserves schema and creates proper scenarios", {
  yearly_baseline <- data.frame(
    asset = c("A1", "A2"),
    company = c("C1", "C1"),
    year = c(2025, 2025),
    baseline_revenue = c(1000, 800),
    baseline_profit = c(100, 80)
  )

  yearly_shocked <- data.frame(
    asset = c("A1", "A2"),
    company = c("C1", "C1"),
    year = c(2025, 2025),
    shocked_revenue = c(970, 784),
    shocked_profit = c(97, 78.4)
  )

  result <- build_yearly_scenarios(yearly_baseline, yearly_shocked)

  # All columns should be snake_case
  col_names <- names(result)
  snake_case_pattern <- "^[a-z][a-z0-9_]*$"
  testthat::expect_true(all(grepl(snake_case_pattern, col_names)))

  # Should have correct scenario values
  testthat::expect_equal(sort(unique(result$scenario)), c("baseline", "shock"))

  # Check that baseline and shock scenarios have same number of rows as input
  baseline_subset <- result[result$scenario == "baseline", ]
  shock_subset <- result[result$scenario == "shock", ]
  testthat::expect_equal(nrow(baseline_subset), nrow(yearly_baseline))
  testthat::expect_equal(nrow(shock_subset), nrow(yearly_shocked))
})

testthat::test_that("build_yearly_scenarios preserves row integrity and prevents duplication", {
  yearly_baseline <- data.frame(
    asset = c("A1", "A2"),
    company = c("C1", "C1"),
    year = c(2025, 2025),
    baseline_revenue = c(1000, 800),
    baseline_profit = c(100, 80)
  )

  yearly_shocked <- data.frame(
    asset = c("A1", "A2"),
    company = c("C1", "C1"),
    year = c(2025, 2025),
    shocked_revenue = c(970, 784),
    shocked_profit = c(97, 78.4)
  )

  result <- build_yearly_scenarios(yearly_baseline, yearly_shocked)

  # Row count should equal exactly sum of inputs (no duplication)
  testthat::expect_equal(nrow(result), nrow(yearly_baseline) + nrow(yearly_shocked))

  # Check that id keys are intact and not NA
  id_cols <- c("asset", "company", "year")
  for (col in id_cols) {
    testthat::expect_true(!any(is.na(result[[col]])),
      info = paste("ID column", col, "should not have NAs")
    )
  }

  # Verify data integrity - baseline scenario should match input baseline data
  baseline_subset <- result[result$scenario == "baseline", ]
  baseline_subset <- baseline_subset[order(baseline_subset$asset), ]
  yearly_baseline_ordered <- yearly_baseline[order(yearly_baseline$asset), ]

  testthat::expect_equal(baseline_subset$revenue, yearly_baseline_ordered$baseline_revenue)
  testthat::expect_equal(baseline_subset$profit, yearly_baseline_ordered$baseline_profit)
})

testthat::test_that("build_yearly_scenarios handles edge cases", {
  yearly_baseline <- data.frame(
    asset = "A1",
    company = "C1",
    year = 2025,
    baseline_revenue = 1000,
    baseline_profit = 100
  )

  yearly_shocked <- data.frame(
    asset = "A1",
    company = "C1",
    year = 2025,
    shocked_revenue = 970,
    shocked_profit = 97
  )

  # Test with identical baseline and shock (should still work)
  # Convert baseline to shocked format for this test
  yearly_shocked_identical <- yearly_baseline
  names(yearly_shocked_identical)[names(yearly_shocked_identical) == "baseline_revenue"] <- "shocked_revenue"
  names(yearly_shocked_identical)[names(yearly_shocked_identical) == "baseline_profit"] <- "shocked_profit"

  identical_out <- build_yearly_scenarios(yearly_baseline, yearly_shocked_identical)
  testthat::expect_equal(nrow(identical_out), 2 * nrow(yearly_baseline))

  # Test that data integrity is preserved (no unexpected mutations)
  original_baseline <- yearly_baseline
  result <- build_yearly_scenarios(yearly_baseline, yearly_shocked)
  testthat::expect_equal(yearly_baseline, original_baseline)
})

testthat::test_that("build_yearly_scenarios validates inputs", {
  yearly_baseline <- data.frame(
    asset = "A1",
    company = "C1",
    year = 2025,
    baseline_revenue = 1000,
    baseline_profit = 100
  )

  yearly_shocked <- data.frame(
    asset = "A1",
    company = "C1",
    year = 2025,
    shocked_revenue = 970,
    shocked_profit = 97
  )

  # Should work with valid inputs
  testthat::expect_no_error(build_yearly_scenarios(yearly_baseline, yearly_shocked))

  # Should error with invalid inputs
  testthat::expect_error(
    build_yearly_scenarios(NULL, yearly_shocked),
    regexp = "non-empty data.frame"
  )

  testthat::expect_error(
    build_yearly_scenarios(yearly_baseline, NULL),
    regexp = "non-empty data.frame"
  )

  # Should error with missing columns
  incomplete_baseline <- data.frame(asset = "A1", year = 2025)
  testthat::expect_error(
    build_yearly_scenarios(incomplete_baseline, yearly_shocked),
    regexp = "Missing required columns"
  )
})
