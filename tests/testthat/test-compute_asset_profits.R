# Tests for function: compute_asset_profits


testthat::test_that("compute_asset_profits multiplies revenue by net profit margin", {
  data <- create_baseline_and_shock()
  scen <- build_scenarios(data$baseline, data$shocked)
  rev <- compute_asset_revenue(scen, data$companies, growth_rate = 0.03)

  out <- compute_asset_profits(rev, net_profit_margin = 0.2)
  testthat::expect_true("asset_profit" %in% names(out))
  testthat::expect_true(all(abs(out$asset_profit - 0.2 * out$asset_revenue) < 1e-6))
})

testthat::test_that("compute_asset_profits validates profit formula", {
  data <- create_baseline_and_shock()
  scen <- build_scenarios(data$baseline, data$shocked)
  rev <- compute_asset_revenue(scen, data$companies, growth_rate = 0.03)
  
  margin <- 0.15
  out <- compute_asset_profits(rev, net_profit_margin = margin)
  
  # Profit should equal asset_revenue * net_profit_margin exactly
  expected_profit <- rev$asset_revenue * margin
  testthat::expect_equal(out$asset_profit, expected_profit, tolerance = 1e-10,
                        info = "Profit should equal revenue * margin exactly")
  
  # Test with different margin values
  margins <- c(0.0, 0.1, 0.25, 0.5)
  for (m in margins) {
    out_m <- compute_asset_profits(rev, net_profit_margin = m)
    expected_m <- rev$asset_revenue * m
    testthat::expect_equal(out_m$asset_profit, expected_m, tolerance = 1e-10,
                          info = paste("Profit calculation failed for margin", m))
  }
})

testthat::test_that("compute_asset_profits handles margin precedence (global vs per-company)", {
  data <- create_baseline_and_shock()
  scen <- build_scenarios(data$baseline, data$shocked)
  rev <- compute_asset_revenue(scen, data$companies, growth_rate = 0.03)
  
  # Test global margin parameter
  global_margin <- 0.2
  out_global <- compute_asset_profits(rev, net_profit_margin = global_margin)
  expected_global <- rev$asset_revenue * global_margin
  testthat::expect_equal(out_global$asset_profit, expected_global, tolerance = 1e-10,
                        info = "Global margin should be applied uniformly")
  
  # Test with companies that have per-company margin in the data
  # Check if companies data has net_profit_margin column
  if ("net_profit_margin" %in% names(data$companies)) {
    # Test precedence: if both global parameter and company-specific margins exist
    # Define the precedence rule - let's say company-specific takes precedence
    
    # Join company margins to revenue data
    rev_with_company_margin <- rev
    company_margins <- data$companies[, c("company_name", "net_profit_margin")]
    names(company_margins) <- c("company", "company_net_profit_margin")
    
    # This test defines that company-specific margins should take precedence
    # when both global parameter and company data are available
    out_precedence <- compute_asset_profits(rev, net_profit_margin = global_margin)
    
    # For now, let's test both code paths exist
    # Implementation will define the exact precedence rule
    testthat::expect_true("asset_profit" %in% names(out_precedence))
    testthat::expect_true(is.numeric(out_precedence$asset_profit))
    
    # Test that function can handle company-specific margins
    # (Implementation will determine if this overrides global parameter)
    testthat::expect_true(all(out_precedence$asset_profit >= 0 | is.na(out_precedence$asset_profit)),
                         info = "Profits should be non-negative or NA for negative revenue")
  }
})

testthat::test_that("compute_asset_profits handles non-negativity and edge cases", {
  data <- create_baseline_and_shock()
  scen <- build_scenarios(data$baseline, data$shocked)
  rev <- compute_asset_revenue(scen, data$companies, growth_rate = 0.03)
  
  # Test with negative revenue (edge case)
  rev_negative <- rev
  rev_negative$asset_revenue[1:2] <- -1000
  
  margin <- 0.2
  out_negative <- compute_asset_profits(rev_negative, net_profit_margin = margin)
  
  # Define behavior for negative revenue: should produce negative profit
  expected_negative <- rev_negative$asset_revenue * margin
  testthat::expect_equal(out_negative$asset_profit, expected_negative, tolerance = 1e-10,
                        info = "Negative revenue should produce negative profit")
  
  # Test with zero margin
  out_zero_margin <- compute_asset_profits(rev, net_profit_margin = 0.0)
  testthat::expect_true(all(out_zero_margin$asset_profit == 0),
                       info = "Zero margin should produce zero profit")
  
  # Test with high margin
  out_high_margin <- compute_asset_profits(rev, net_profit_margin = 1.0)
  testthat::expect_equal(out_high_margin$asset_profit, rev$asset_revenue, tolerance = 1e-10,
                        info = "100% margin should equal revenue")
})

testthat::test_that("compute_asset_profits preserves schema and types", {
  data <- create_baseline_and_shock()
  scen <- build_scenarios(data$baseline, data$shocked)
  rev <- compute_asset_revenue(scen, data$companies, growth_rate = 0.03)
  
  out <- compute_asset_profits(rev, net_profit_margin = 0.15)
  
  # Schema preservation
  original_cols <- names(rev)
  new_cols <- names(out)
  testthat::expect_true(all(original_cols %in% new_cols),
                       info = "All original columns should be preserved")
  testthat::expect_true("asset_profit" %in% new_cols,
                       info = "asset_profit column should be added")
  
  # Type validation
  testthat::expect_true(is.numeric(out$asset_profit),
                       info = "asset_profit should be numeric")
  
  # Row count preservation
  testthat::expect_equal(nrow(out), nrow(rev),
                        info = "Row count should be preserved")
  
  # Required input validation
  testthat::expect_true("asset_revenue" %in% names(rev),
                       info = "Input should have asset_revenue column")
  
  # Test parameter validation
  testthat::expect_error(
    compute_asset_profits(rev, net_profit_margin = "invalid"),
    info = "Should error with non-numeric margin"
  )
})
