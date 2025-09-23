# Tests for function: compute_asset_revenue


testthat::test_that("compute_asset_revenue adds asset_revenue per scenario", {
  data <- create_baseline_and_shock()
  scen <- build_scenarios(data$baseline, data$shocked)

  out <- compute_asset_revenue(scen, data$companies, growth_rate = 0.03)
  testthat::expect_true("asset_revenue" %in% names(out))
  testthat::expect_true(is.numeric(out$asset_revenue))
})

testthat::test_that("compute_asset_revenue validates join keys and handles unmatched assets", {
  data <- create_baseline_and_shock()
  scen <- build_scenarios(data$baseline, data$shocked)
  
  # Test that join works with expected key column(s)
  # Assets should have 'company' column to join with companies
  testthat::expect_true("company" %in% names(scen), 
                       info = "Assets should have 'company' column for joining")
  testthat::expect_true("company_name" %in% names(data$companies), 
                       info = "Companies should have 'company_name' column for joining")
  
  # Normal case should work
  out <- compute_asset_revenue(scen, data$companies, growth_rate = 0.03)
  testthat::expect_true("asset_revenue" %in% names(out))
  
  # Test with unmatched assets (create an asset with non-existent company)
  scen_unmatched <- scen
  scen_unmatched$company[1] <- "NonExistentCompany"
  
  # Should either error or handle gracefully with NA/warning
  # Let's define that it should error for unmatched assets
  testthat::expect_error(
    compute_asset_revenue(scen_unmatched, data$companies, growth_rate = 0.03),
    regexp = "not found|companies",
    info = "Should error when assets have companies not found in companies data"
  )
})

testthat::test_that("compute_asset_revenue allocates revenue correctly", {
  data <- create_baseline_and_shock()
  scen <- build_scenarios(data$baseline, data$shocked)
  
  growth_rate <- 0.05
  out <- compute_asset_revenue(scen, data$companies, growth_rate = growth_rate)
  
  # Revenue allocation: sum(asset_revenue) per company-scenario should equal 
  # company revenue * (1 + growth_rate) * total_share_for_that_company
  for (scenario in c("baseline", "shock")) {
    scenario_data <- out[out$scenario == scenario, ]
    
    for (company in unique(scenario_data$company)) {
      company_assets <- scenario_data[scenario_data$company == company, ]
      company_info <- data$companies[data$companies$company_name == company, ]
      
      if (nrow(company_info) > 0) {
        # Total asset revenue for this company-scenario
        total_asset_revenue <- sum(company_assets$asset_revenue, na.rm = TRUE)
        
        # Expected: company revenue * growth factor * sum of shares
        expected_base_revenue <- company_info$revenues * (1 + growth_rate)
        total_share <- sum(company_assets$share_of_economic_activity, na.rm = TRUE)
        expected_total <- expected_base_revenue * total_share
        
        # Allow small tolerance for floating point arithmetic
        testthat::expect_equal(total_asset_revenue, expected_total, tolerance = 1e-6,
                              info = paste("Revenue allocation failed for company", company, "scenario", scenario))
      }
    }
  }
})

testthat::test_that("compute_asset_revenue growth_rate sensitivity", {
  data <- create_baseline_and_shock()
  scen <- build_scenarios(data$baseline, data$shocked)
  
  # Test different growth rates
  out_zero <- compute_asset_revenue(scen, data$companies, growth_rate = 0.0)
  out_positive <- compute_asset_revenue(scen, data$companies, growth_rate = 0.1)
  
  # Higher growth rate should increase total asset revenue
  total_revenue_zero <- sum(out_zero$asset_revenue, na.rm = TRUE)
  total_revenue_positive <- sum(out_positive$asset_revenue, na.rm = TRUE)
  
  testthat::expect_true(total_revenue_positive > total_revenue_zero,
                       info = "Positive growth rate should increase total asset revenue")
  
  # Zero growth rate should equal baseline company revenues scaled by shares
  baseline_zero <- out_zero[out_zero$scenario == "baseline", ]
  for (company in unique(baseline_zero$company)) {
    company_assets <- baseline_zero[baseline_zero$company == company, ]
    company_info <- data$companies[data$companies$company_name == company, ]
    
    if (nrow(company_info) > 0) {
      total_asset_revenue <- sum(company_assets$asset_revenue, na.rm = TRUE)
      expected_revenue <- company_info$revenues * sum(company_assets$share_of_economic_activity, na.rm = TRUE)
      
      testthat::expect_equal(total_asset_revenue, expected_revenue, tolerance = 1e-6,
                            info = paste("Zero growth rate should equal baseline for company", company))
    }
  }
})

testthat::test_that("compute_asset_revenue type and schema validation", {
  data <- create_baseline_and_shock()
  scen <- build_scenarios(data$baseline, data$shocked)
  
  out <- compute_asset_revenue(scen, data$companies, growth_rate = 0.03)
  
  # Type checks
  testthat::expect_true(is.numeric(out$asset_revenue))
  testthat::expect_true(all(!is.na(out$asset_revenue)), 
                       info = "asset_revenue should not contain NA values")
  testthat::expect_true(all(out$asset_revenue >= 0), 
                       info = "asset_revenue should be non-negative")
  
  # Schema preservation
  original_cols <- names(scen)
  new_cols <- names(out)
  testthat::expect_true(all(original_cols %in% new_cols),
                       info = "All original columns should be preserved")
  testthat::expect_true("asset_revenue" %in% new_cols,
                       info = "asset_revenue column should be added")
  
  # Row count preservation
  testthat::expect_equal(nrow(out), nrow(scen),
                        info = "Row count should be preserved")
  
  # Required columns check
  required_cols <- c("share_of_economic_activity", "company")
  missing_cols <- setdiff(required_cols, names(scen))
  testthat::expect_equal(length(missing_cols), 0,
                        info = paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
})
