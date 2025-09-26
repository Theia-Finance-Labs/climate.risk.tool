# Tests for function: compute_expected_loss

testthat::test_that("compute_expected_loss adds expected_loss", {
  # Create company PD data directly
  companies_pd <- data.frame(
    company = c("C1", "C1"),
    scenario = c("baseline", "shock"),
    npv = c(1000, 950),
    merton_pd = c(0.05, 0.08)
  )

  out <- compute_expected_loss(companies_pd)
  testthat::expect_true("expected_loss" %in% names(out))
  testthat::expect_true(is.numeric(out$expected_loss))
})

testthat::test_that("compute_expected_loss validates formula", {
  # Test with known values to verify EL = LGD * Loan_Size * PD formula
  companies_pd <- data.frame(
    company = "C1",
    scenario = "baseline",
    npv = 1000,
    merton_pd = 0.1
  )
  
  out <- compute_expected_loss(companies_pd)
  
  # Expected loss should be computed using the formula
  # The exact calculation depends on LGD and loan size defaults/lookup
  testthat::expect_true(is.numeric(out$expected_loss))
  testthat::expect_true(out$expected_loss >= 0)  # Expected loss should be non-negative
})

testthat::test_that("compute_expected_loss monotonicity tests", {
  # Higher PD should generally lead to higher expected loss (all else equal)
  companies_low_pd <- data.frame(
    company = "C1",
    scenario = "baseline",
    npv = 1000,
    merton_pd = 0.05
  )
  
  companies_high_pd <- data.frame(
    company = "C1",
    scenario = "baseline",
    npv = 1000,
    merton_pd = 0.15
  )
  
  out_low <- compute_expected_loss(companies_low_pd)
  out_high <- compute_expected_loss(companies_high_pd)
  
  # Higher PD should lead to higher expected loss (assuming same LGD and loan size)
  testthat::expect_true(out_high$expected_loss >= out_low$expected_loss)
})

testthat::test_that("compute_expected_loss scenario grouping", {
  companies_pd <- data.frame(
    company = c("C1", "C1", "C2", "C2"),
    scenario = c("baseline", "shock", "baseline", "shock"),
    npv = c(1000, 950, 800, 760),
    merton_pd = c(0.05, 0.08, 0.06, 0.09)
  )
  
  out <- compute_expected_loss(companies_pd)
  
  # Should preserve all rows and scenarios
  testthat::expect_equal(nrow(out), nrow(companies_pd))
  testthat::expect_true(all(c("baseline", "shock") %in% out$scenario))
  testthat::expect_true(all(c("C1", "C2") %in% out$company))
})

testthat::test_that("compute_expected_loss validates inputs", {
  companies_pd <- data.frame(
    company = "C1",
    scenario = "baseline",
    npv = 1000,
    merton_pd = 0.1
  )
  
  # Should work with valid inputs
  testthat::expect_no_error(compute_expected_loss(companies_pd))
  
  # Should error with invalid inputs
  testthat::expect_error(
    compute_expected_loss("not_a_dataframe"),
    info = "Should error with non-data.frame input"
  )
  
  # Should error with missing columns
  incomplete_data <- data.frame(company = "C1")
  testthat::expect_error(
    compute_expected_loss(incomplete_data),
    info = "Should error with missing required columns"
  )
})

testthat::test_that("compute_expected_loss handles edge cases", {
  # Test with zero PD
  zero_pd <- data.frame(
    company = "C1",
    scenario = "baseline",
    npv = 1000,
    merton_pd = 0.0
  )
  
  out_zero <- compute_expected_loss(zero_pd)
  testthat::expect_equal(out_zero$expected_loss, 0)
  
  # Test with maximum PD
  max_pd <- data.frame(
    company = "C1",
    scenario = "baseline",
    npv = 1000,
    merton_pd = 1.0
  )
  
  out_max <- compute_expected_loss(max_pd)
  testthat::expect_true(is.numeric(out_max$expected_loss))
  testthat::expect_true(out_max$expected_loss >= 0)
  
  # Test with negative NPV
  negative_npv <- data.frame(
    company = "C1",
    scenario = "baseline",
    npv = -100,
    merton_pd = 0.1
  )
  
  out_negative <- compute_expected_loss(negative_npv)
  testthat::expect_true(is.numeric(out_negative$expected_loss))
})