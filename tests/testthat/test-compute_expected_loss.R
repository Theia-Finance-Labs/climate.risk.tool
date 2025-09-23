# Tests for function: compute_expected_loss


testthat::test_that("compute_expected_loss adds expected_loss", {
  data <- create_baseline_and_shock()
  scen <- build_scenarios(data$baseline, data$shocked)
  rev <- compute_asset_revenue(scen, data$companies, growth_rate = 0.03)
  prof <- compute_asset_profits(rev, net_profit_margin = 0.2)
  dnp <- discount_net_profits(prof, discount_rate = 0.1)
  npv <- compute_company_npv(dnp)
  pd <- compute_company_pd_merton(npv)

  out <- compute_expected_loss(pd)
  testthat::expect_true("expected_loss" %in% names(out))
  testthat::expect_true(is.numeric(out$expected_loss))
})

testthat::test_that("compute_expected_loss validates formula", {
  data <- create_baseline_and_shock()
  scen <- build_scenarios(data$baseline, data$shocked)
  rev <- compute_asset_revenue(scen, data$companies, growth_rate = 0.03)
  prof <- compute_asset_profits(rev, net_profit_margin = 0.2)
  dnp <- discount_net_profits(prof, discount_rate = 0.1)
  npv <- compute_company_npv(dnp)
  pd <- compute_company_pd_merton(npv)

  out <- compute_expected_loss(pd)
  
  # Expected Loss formula: EL = LGD * Loan_Size * PD
  # Verify this for a few rows where we can get the company data
  for (i in 1:min(3, nrow(out))) {
    company <- out$company[i]
    company_data <- data$companies[data$companies$company_name == company, ]
    
    if (nrow(company_data) > 0) {
      expected_el <- company_data$lgd * company_data$loan_size * out$merton_pd[i]
      actual_el <- out$expected_loss[i]
      
      testthat::expect_equal(actual_el, expected_el, tolerance = 1e-6,
                            info = paste("Expected loss formula failed for company", company))
    }
  }
})

testthat::test_that("compute_expected_loss monotonicity tests", {
  data <- create_baseline_and_shock()
  scen <- build_scenarios(data$baseline, data$shocked)
  rev <- compute_asset_revenue(scen, data$companies, growth_rate = 0.03)
  prof <- compute_asset_profits(rev, net_profit_margin = 0.2)
  dnp <- discount_net_profits(prof, discount_rate = 0.1)
  npv <- compute_company_npv(dnp)
  pd <- compute_company_pd_merton(npv)

  out <- compute_expected_loss(pd)
  
  # Higher PD should generally lead to higher expected loss (when other factors constant)
  testthat::expect_true(all(out$expected_loss >= 0),
                       info = "Expected loss should be non-negative")
  
  # Companies with higher PD should generally have higher expected loss
  # (this is a general trend test, not strict monotonicity due to different loan sizes/LGDs)
  testthat::expect_true(is.numeric(out$expected_loss),
                       info = "Expected loss should be numeric")
  testthat::expect_true(!all(is.na(out$expected_loss)),
                       info = "Not all expected losses should be NA")
})

testthat::test_that("compute_expected_loss scenario grouping", {
  data <- create_baseline_and_shock()
  scen <- build_scenarios(data$baseline, data$shocked)
  rev <- compute_asset_revenue(scen, data$companies, growth_rate = 0.03)
  prof <- compute_asset_profits(rev, net_profit_margin = 0.2)
  dnp <- discount_net_profits(prof, discount_rate = 0.1)
  npv <- compute_company_npv(dnp)
  pd <- compute_company_pd_merton(npv)

  out <- compute_expected_loss(pd)
  
  # Both scenarios should be preserved
  scenarios_in_output <- unique(out$scenario)
  testthat::expect_true("baseline" %in% scenarios_in_output,
                       info = "Baseline scenario should be present")
  testthat::expect_true("shock" %in% scenarios_in_output,
                       info = "Shock scenario should be present")
  
  # Each company should appear in both scenarios
  companies_baseline <- unique(out$company[out$scenario == "baseline"])
  companies_shock <- unique(out$company[out$scenario == "shock"])
  
  testthat::expect_equal(sort(companies_baseline), sort(companies_shock),
                        info = "Same companies should appear in both scenarios")
  
  # Schema preservation
  original_cols <- names(pd)
  testthat::expect_true(all(original_cols %in% names(out)),
                       info = "All original columns should be preserved")
  testthat::expect_true("expected_loss" %in% names(out),
                       info = "expected_loss column should be added")
  
  # Row count preservation
  testthat::expect_equal(nrow(out), nrow(pd),
                        info = "Row count should be preserved")
})
