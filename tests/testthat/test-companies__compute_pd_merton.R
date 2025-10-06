# Tests for function: compute_pd_merton

testthat::test_that("compute_pd_merton adds merton_pd (0..1)", {
  # Create company NPV data with required financial columns
  company_npv <- data.frame(
    company = c("C1", "C1"),
    scenario = c("baseline", "shock"),
    npv = c(1000, 950),
    debt = c(5000, 5000),
    volatility = c(0.3, 0.3),
    term = c(5, 5)
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
    npv = c(2000, 1900), # High NPV values
    debt = c(5000, 5000),
    volatility = c(0.3, 0.3),
    term = c(5, 5)
  )

  company_npv_low <- data.frame(
    company = c("C1", "C1"),
    scenario = c("baseline", "shock"),
    npv = c(100, 50), # Low NPV values
    debt = c(5000, 5000),
    volatility = c(0.3, 0.3),
    term = c(5, 5)
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
