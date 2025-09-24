# Tests for function: build_scenarios


testthat::test_that("build_scenarios concatenates baseline and shock", {
  data <- create_baseline_and_shock()
  out <- build_scenarios(data$baseline, data$shocked)

  testthat::expect_true("scenario" %in% names(out))
  testthat::expect_true(all(c("baseline", "shock") %in% unique(out$scenario)))
  testthat::expect_equal(nrow(out), nrow(data$baseline) + nrow(data$shocked))
})

testthat::test_that("build_scenarios preserves schema and creates ordered factor", {
  data <- create_baseline_and_shock()
  out <- build_scenarios(data$baseline, data$shocked)
  
  # Schema preservation: all original columns should be present
  baseline_cols <- names(data$baseline)
  expected_cols <- c(baseline_cols, "scenario")
  testthat::expect_true(all(expected_cols %in% names(out)))
  
  # All columns should be snake_case (no spaces or uppercase except scenario values)
  col_names <- names(out)
  snake_case_pattern <- "^[a-z][a-z0-9_]*$"
  testthat::expect_true(all(grepl(snake_case_pattern, col_names)))
  
  # scenario should be an ordered factor with baseline < shock
  testthat::expect_true(is.factor(out$scenario))
  testthat::expect_true(is.ordered(out$scenario))
  testthat::expect_equal(levels(out$scenario), c("baseline", "shock"))
  
  # Check that baseline comes before shock in the factor ordering
  baseline_rows <- which(out$scenario == "baseline")
  shock_rows <- which(out$scenario == "shock")
  testthat::expect_true(all(as.numeric(out$scenario[baseline_rows]) < as.numeric(out$scenario[shock_rows])))
})

testthat::test_that("build_scenarios preserves row integrity and prevents duplication", {
  data <- create_baseline_and_shock()
  out <- build_scenarios(data$baseline, data$shocked)
  
  # Row count should equal exactly sum of inputs (no duplication)
  testthat::expect_equal(nrow(out), nrow(data$baseline) + nrow(data$shocked))
  
  # Check that id keys are intact and not NA
  # Assuming assets have some kind of identifier - check common ID columns
  id_cols <- intersect(c("company", "asset", "asset_id", "company_name"), names(out))
  if (length(id_cols) > 0) {
    for (col in id_cols) {
      testthat::expect_true(!any(is.na(out[[col]])), 
                           info = paste("ID column", col, "should not have NAs"))
    }
  }
  
  # Verify that baseline and shock scenarios have same number of rows as input
  baseline_subset <- out[out$scenario == "baseline", ]
  shock_subset <- out[out$scenario == "shock", ]
  testthat::expect_equal(nrow(baseline_subset), nrow(data$baseline))
  testthat::expect_equal(nrow(shock_subset), nrow(data$shocked))
})

testthat::test_that("build_scenarios handles edge cases", {
  data <- create_baseline_and_shock()
  
  # Test with identical baseline and shock (should still work)
  identical_out <- build_scenarios(data$baseline, data$baseline)
  testthat::expect_equal(nrow(identical_out), 2 * nrow(data$baseline))
  
  # Test that data integrity is preserved (no unexpected mutations)
  out <- build_scenarios(data$baseline, data$shocked)
  
  # Original data should be unchanged (function should not modify inputs)
  original_baseline <- create_baseline_and_shock()$baseline
  testthat::expect_equal(data$baseline, original_baseline)
})
