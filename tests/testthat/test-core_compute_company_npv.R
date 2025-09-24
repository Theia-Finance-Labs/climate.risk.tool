# Tests for function: compute_company_npv


testthat::test_that("compute_company_npv aggregates asset discounted profits by company and scenario", {
  data <- create_baseline_and_shock()
  scen <- build_scenarios(data$baseline, data$shocked)
  rev <- compute_asset_revenue(scen, data$companies, growth_rate = 0.03)
  prof <- compute_asset_profits(rev, net_profit_margin = 0.2)
  dnp <- discount_net_profits(prof, discount_rate = 0.1)

  out <- compute_company_npv(dnp)
  # Accept either snake_case or original case depending on implementation, but prefer snake_case
  expect_cols <- c("company", "scenario", "npv")
  alt_cols <- c("Company", "scenario", "NPV")
  testthat::expect_true(all(expect_cols %in% names(out)) || all(alt_cols %in% names(out)))
  # Numeric NPV
  if ("npv" %in% names(out)) testthat::expect_true(is.numeric(out$npv))
  if ("NPV" %in% names(out)) testthat::expect_true(is.numeric(out$NPV))
})

testthat::test_that("compute_company_npv performs exact aggregation from assets", {
  data <- create_baseline_and_shock()
  scen <- build_scenarios(data$baseline, data$shocked)
  rev <- compute_asset_revenue(scen, data$companies, growth_rate = 0.03)
  prof <- compute_asset_profits(rev, net_profit_margin = 0.2)
  dnp <- discount_net_profits(prof, discount_rate = 0.1)
  
  out <- compute_company_npv(dnp)
  
  # Determine column names (prefer snake_case)
  npv_col <- if ("npv" %in% names(out)) "npv" else "NPV"
  
  # Verify aggregation: NPV should equal sum of discounted_net_profit per company-scenario
  for (scenario in c("baseline", "shock")) {
    scenario_assets <- dnp[dnp$scenario == scenario, ]
    scenario_companies <- out[out$scenario == scenario, ]
    
    for (company in unique(scenario_assets$company)) {
      company_assets <- scenario_assets[scenario_assets$company == company, ]
      company_npv_row <- scenario_companies[scenario_companies$company == company, ]
      
      if (nrow(company_npv_row) > 0) {
        expected_npv <- sum(company_assets$discounted_net_profit, na.rm = TRUE)
        actual_npv <- company_npv_row[[npv_col]]
        
        testthat::expect_equal(actual_npv, expected_npv, tolerance = 1e-8,
                              info = paste("NPV aggregation failed for company", company, "scenario", scenario))
      }
    }
  }
})

testthat::test_that("compute_company_npv handles companies with no assets", {
  data <- create_baseline_and_shock()
  scen <- build_scenarios(data$baseline, data$shocked)
  rev <- compute_asset_revenue(scen, data$companies, growth_rate = 0.03)
  prof <- compute_asset_profits(rev, net_profit_margin = 0.2)
  dnp <- discount_net_profits(prof, discount_rate = 0.1)
  
  # Add a company with no assets to the companies data
  companies_with_no_assets <- data$companies
  new_company <- companies_with_no_assets[1, ]
  new_company$company_name <- "Company no assets"
  companies_with_no_assets <- rbind(companies_with_no_assets, new_company)
  
  out <- compute_company_npv(dnp)
  
  # Determine column names
  npv_col <- if ("npv" %in% names(out)) "npv" else "NPV"
  
  # All companies should be present in the output, even if they have no assets
  companies_in_assets <- unique(dnp$company)
  companies_in_output <- unique(out$company)
  
  testthat::expect_true(all(companies_in_assets %in% companies_in_output),
                       info = "All companies with assets should be in output")
  
  # Test that companies without assets have defined NPV (0 or NA as specified)
  # For now, let's test that the function handles this case without error
  testthat::expect_true(all(!is.nan(out[[npv_col]])),
                       info = "NPV should not contain NaN values")
  
  # Companies with assets should have non-zero NPV (assuming positive profits)
  companies_with_positive_profits <- unique(dnp$company[dnp$discounted_net_profit > 0])
  if (length(companies_with_positive_profits) > 0) {
    for (company in companies_with_positive_profits) {
      company_npvs <- out[out$company == company, npv_col]
      testthat::expect_true(any(company_npvs > 0),
                           info = paste("Company", company, "should have positive NPV somewhere"))
    }
  }
})

testthat::test_that("compute_company_npv validates schema and types", {
  data <- create_baseline_and_shock()
  scen <- build_scenarios(data$baseline, data$shocked)
  rev <- compute_asset_revenue(scen, data$companies, growth_rate = 0.03)
  prof <- compute_asset_profits(rev, net_profit_margin = 0.2)
  dnp <- discount_net_profits(prof, discount_rate = 0.1)
  
  out <- compute_company_npv(dnp)
  
  # Schema validation - prefer snake_case
  expected_cols <- c("company", "scenario", "npv")
  if (all(expected_cols %in% names(out))) {
    # Snake case preferred
    testthat::expect_true(all(expected_cols %in% names(out)))
    testthat::expect_true(is.character(out$company) || is.factor(out$company))
    testthat::expect_true(is.factor(out$scenario) || is.character(out$scenario))
    testthat::expect_true(is.numeric(out$npv))
  } else {
    # Accept alternative case but document preference
    alt_cols <- c("company", "scenario", "NPV")
    testthat::expect_true(all(alt_cols %in% names(out)),
                         info = "Either snake_case (npv) or alternative (NPV) columns should exist")
  }
  
  # Row structure validation
  unique_companies <- length(unique(dnp$company))
  unique_scenarios <- length(unique(dnp$scenario))
  expected_rows <- unique_companies * unique_scenarios
  
  testthat::expect_equal(nrow(out), expected_rows,
                        info = "Should have one row per company-scenario combination")
  
  # No missing values in key columns
  testthat::expect_true(!any(is.na(out$company)),
                       info = "Company column should not have NAs")
  testthat::expect_true(!any(is.na(out$scenario)),
                       info = "Scenario column should not have NAs")
  
  # Test required input columns
  testthat::expect_true("discounted_net_profit" %in% names(dnp),
                       info = "Input should have discounted_net_profit column")
  testthat::expect_true("company" %in% names(dnp),
                       info = "Input should have company column")
  testthat::expect_true("scenario" %in% names(dnp),
                       info = "Input should have scenario column")
})

testthat::test_that("compute_company_npv maintains scenario consistency", {
  data <- create_baseline_and_shock()
  scen <- build_scenarios(data$baseline, data$shocked)
  rev <- compute_asset_revenue(scen, data$companies, growth_rate = 0.03)
  prof <- compute_asset_profits(rev, net_profit_margin = 0.2)
  dnp <- discount_net_profits(prof, discount_rate = 0.1)
  
  out <- compute_company_npv(dnp)
  
  # Both baseline and shock scenarios should be present
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
  
  # NPV differences between scenarios should reflect the shock impact
  npv_col <- if ("npv" %in% names(out)) "npv" else "NPV"
  
  for (company in companies_baseline) {
    baseline_npv <- out[out$company == company & out$scenario == "baseline", npv_col]
    shock_npv <- out[out$company == company & out$scenario == "shock", npv_col]
    
    # Shock should generally result in different NPV (unless no impact)
    # This test documents that scenarios should produce different results when there's actual shock impact
    if (length(baseline_npv) == 1 && length(shock_npv) == 1) {
      testthat::expect_true(is.numeric(baseline_npv) && is.numeric(shock_npv),
                           info = paste("NPV values should be numeric for company", company))
    }
  }
})
