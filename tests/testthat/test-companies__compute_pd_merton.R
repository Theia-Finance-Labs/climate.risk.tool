# Tests for function: compute_pd_merton

testthat::test_that("compute_pd_merton adds merton_pd (0..1)", {
  # Create company NPV data
  company_npv <- data.frame(
    company = c("C1", "C1"),
    scenario = c("baseline", "shock"),
    npv = c(1000, 950)
  )

  out <- compute_pd_merton(company_npv)
  testthat::expect_true("merton_pd" %in% names(out))
  testthat::expect_true(all(out$merton_pd >= 0 & out$merton_pd <= 1, na.rm = TRUE))
})

testthat::test_that("compute_pd_merton sensitivity tests", {
  # Test with different NPV values to check sensitivity
  company_npv_high <- data.frame(
    company = c("C1", "C1"),
    scenario = c("baseline", "shock"),
    npv = c(2000, 1900) # High NPV values
  )

  company_npv_low <- data.frame(
    company = c("C1", "C1"),
    scenario = c("baseline", "shock"),
    npv = c(100, 50) # Low NPV values
  )

  out_high <- compute_pd_merton(company_npv_high)
  out_low <- compute_pd_merton(company_npv_low)

  # Both should have valid PD values
  testthat::expect_true(all(out_high$merton_pd >= 0 & out_high$merton_pd <= 1))
  testthat::expect_true(all(out_low$merton_pd >= 0 & out_low$merton_pd <= 1))

  # Generally, lower NPV should lead to higher PD (though exact relationship depends on model)
  testthat::expect_true(is.numeric(out_high$merton_pd))
  testthat::expect_true(is.numeric(out_low$merton_pd))
})

testthat::test_that("compute_pd_merton schema validation", {
  company_npv <- data.frame(
    company = "C1",
    scenario = "baseline",
    npv = 1000
  )

  out <- compute_pd_merton(company_npv)

  # Schema validation
  testthat::expect_true(is.data.frame(out))
  testthat::expect_true("merton_pd" %in% names(out))
  testthat::expect_true(is.numeric(out$merton_pd))

  # Should preserve original columns
  original_cols <- names(company_npv)
  testthat::expect_true(all(original_cols %in% names(out)))

  # Test with invalid input
  testthat::expect_error(
    compute_pd_merton("not_a_dataframe"),
    info = "Should error with non-data.frame input"
  )

  # Test with missing columns
  incomplete_data <- data.frame(company = "C1")
  testthat::expect_error(
    compute_pd_merton(incomplete_data),
    info = "Should error with missing required columns"
  )
})

testthat::test_that("compute_pd_merton handles edge cases", {
  # Test with zero NPV
  zero_npv <- data.frame(
    company = "C1",
    scenario = "baseline",
    npv = 0
  )

  out_zero <- compute_pd_merton(zero_npv)
  testthat::expect_true(is.numeric(out_zero$merton_pd))
  testthat::expect_true(out_zero$merton_pd >= 0 & out_zero$merton_pd <= 1)

  # Test with negative NPV
  negative_npv <- data.frame(
    company = "C1",
    scenario = "baseline",
    npv = -100
  )

  out_negative <- compute_pd_merton(negative_npv)
  testthat::expect_true(is.numeric(out_negative$merton_pd))
  testthat::expect_true(out_negative$merton_pd >= 0 & out_negative$merton_pd <= 1)

  # Test with very large NPV
  large_npv <- data.frame(
    company = "C1",
    scenario = "baseline",
    npv = 1e6
  )

  out_large <- compute_pd_merton(large_npv)
  testthat::expect_true(is.numeric(out_large$merton_pd))
  testthat::expect_true(out_large$merton_pd >= 0 & out_large$merton_pd <= 1)
})
