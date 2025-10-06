# Tests for function: concatenate_baseline_and_shock

testthat::test_that("concatenate_baseline_and_shock concatenates baseline and shocked trajectories", {
  yearly_baseline <- data.frame(
    asset = c("A1", "A1", "A2", "A2"),
    company = c("C1", "C1", "C1", "C1"),
    year = c(2025, 2026, 2025, 2026),
    revenue = c(1000, 1020, 800, 816),
    profit = c(100, 102, 80, 81.6)
  )

  yearly_shocked <- data.frame(
    asset = c("A1", "A1", "A2", "A2"),
    company = c("C1", "C1", "C1", "C1"),
    year = c(2025, 2026, 2025, 2026),
    revenue = c(1000, 979, 800, 783),
    profit = c(100, 97.9, 80, 78.3)
  )

  result <- concatenate_baseline_and_shock(yearly_baseline, yearly_shocked)

  # Should have scenario column with baseline and shock values
  testthat::expect_true("scenario" %in% names(result))
  testthat::expect_true(all(c("baseline", "shock") %in% unique(result$scenario)))
  testthat::expect_equal(nrow(result), nrow(yearly_baseline) + nrow(yearly_shocked))

  # Should have revenue and profit columns (renamed from baseline_/shocked_)
  expected_cols <- c("asset", "company", "year", "scenario", "revenue", "profit")
  testthat::expect_true(all(expected_cols %in% names(result)))
})

testthat::test_that("concatenate_baseline_and_shock preserves schema and creates proper scenarios", {
  yearly_baseline <- data.frame(
    asset = c("A1", "A2"),
    company = c("C1", "C1"),
    year = c(2025, 2025),
    revenue = c(1000, 800),
    profit = c(100, 80)
  )

  yearly_shocked <- data.frame(
    asset = c("A1", "A2"),
    company = c("C1", "C1"),
    year = c(2025, 2025),
    revenue = c(970, 784),
    profit = c(97, 78.4)
  )

  result <- concatenate_baseline_and_shock(yearly_baseline, yearly_shocked)

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

testthat::test_that("concatenate_baseline_and_shock preserves row integrity and prevents duplication", {
  yearly_baseline <- data.frame(
    asset = c("A1", "A2"),
    company = c("C1", "C1"),
    year = c(2025, 2025),
    revenue = c(1000, 800),
    profit = c(100, 80)
  )

  yearly_shocked <- data.frame(
    asset = c("A1", "A2"),
    company = c("C1", "C1"),
    year = c(2025, 2025),
    revenue = c(970, 784),
    profit = c(97, 78.4)
  )

  result <- concatenate_baseline_and_shock(yearly_baseline, yearly_shocked)

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

  testthat::expect_equal(baseline_subset$revenue, yearly_baseline_ordered$revenue)
  testthat::expect_equal(baseline_subset$profit, yearly_baseline_ordered$profit)
})
