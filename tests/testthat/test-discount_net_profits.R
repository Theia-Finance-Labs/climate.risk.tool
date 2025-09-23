# Tests for function: discount_net_profits


testthat::test_that("discounted_net_profits adds discounted_net_profit", {
  data <- create_baseline_and_shock()
  scen <- build_scenarios(data$baseline, data$shocked)
  rev <- compute_asset_revenue(scen, data$companies, growth_rate = 0.03)
  prof <- compute_asset_profits(rev, net_profit_margin = 0.2)

  out <- discount_net_profits(prof, discount_rate = 0.1)
  testthat::expect_true("discounted_net_profit" %in% names(out))
  testthat::expect_true(is.numeric(out$discounted_net_profit))
})

testthat::test_that("discount_net_profits applies correct discounting math", {
  data <- create_baseline_and_shock()
  scen <- build_scenarios(data$baseline, data$shocked)
  rev <- compute_asset_revenue(scen, data$companies, growth_rate = 0.03)
  prof <- compute_asset_profits(rev, net_profit_margin = 0.2)
  
  discount_rate <- 0.08
  out <- discount_net_profits(prof, discount_rate = discount_rate)
  
  # If companies have 'term' column, discounting should use that
  if ("term" %in% names(data$companies)) {
    # Join term information to verify discounting formula
    # Assuming discounting formula: discounted = profit / (1 + discount_rate)^term
    
    # For each company, check the discounting calculation
    companies_with_term <- data$companies[, c("company_name", "term")]
    names(companies_with_term) <- c("company", "term")
    
    prof_with_term <- merge(prof, companies_with_term, by = "company", all.x = TRUE)
    out_with_term <- merge(out, companies_with_term, by = "company", all.x = TRUE)
    
    # Test discounting formula for rows with term information
    valid_rows <- !is.na(out_with_term$term)
    if (any(valid_rows)) {
      expected_discounted <- prof_with_term$asset_profit[valid_rows] / 
                            (1 + discount_rate)^out_with_term$term[valid_rows]
      actual_discounted <- out_with_term$discounted_net_profit[valid_rows]
      
      testthat::expect_equal(actual_discounted, expected_discounted, tolerance = 1e-8,
                            info = "Discounting should follow PV formula: profit / (1 + rate)^term")
    }
  } else {
    # If no term information, test simpler discounting (assuming term = 1)
    expected_simple <- prof$asset_profit / (1 + discount_rate)
    testthat::expect_equal(out$discounted_net_profit, expected_simple, tolerance = 1e-8,
                          info = "Simple discounting should use term = 1")
  }
})

testthat::test_that("discount_net_profits shows monotonicity with respect to discount rate", {
  data <- create_baseline_and_shock()
  scen <- build_scenarios(data$baseline, data$shocked)
  rev <- compute_asset_revenue(scen, data$companies, growth_rate = 0.03)
  prof <- compute_asset_profits(rev, net_profit_margin = 0.2)
  
  # Test different discount rates
  rates <- c(0.05, 0.10, 0.15, 0.20)
  results <- list()
  
  for (i in seq_along(rates)) {
    results[[i]] <- discount_net_profits(prof, discount_rate = rates[i])
  }
  
  # Higher discount rate should yield lower discounted net profit
  for (i in 2:length(rates)) {
    prev_total <- sum(results[[i-1]]$discounted_net_profit, na.rm = TRUE)
    curr_total <- sum(results[[i]]$discounted_net_profit, na.rm = TRUE)
    
    testthat::expect_true(curr_total < prev_total,
                         info = paste("Higher discount rate", rates[i], 
                                    "should yield lower total discounted profit than", rates[i-1]))
  }
  
  # Test monotonicity for individual assets (where profit > 0)
  positive_profit_rows <- prof$asset_profit > 0
  if (any(positive_profit_rows)) {
    for (i in 2:length(rates)) {
      prev_values <- results[[i-1]]$discounted_net_profit[positive_profit_rows]
      curr_values <- results[[i]]$discounted_net_profit[positive_profit_rows]
      
      testthat::expect_true(all(curr_values <= prev_values),
                           info = "Each asset should have lower discounted profit with higher discount rate")
    }
  }
})

testthat::test_that("discount_net_profits zero rate identity", {
  data <- create_baseline_and_shock()
  scen <- build_scenarios(data$baseline, data$shocked)
  rev <- compute_asset_revenue(scen, data$companies, growth_rate = 0.03)
  prof <- compute_asset_profits(rev, net_profit_margin = 0.2)
  
  # Zero discount rate should leave values unchanged (or nearly so)
  out_zero <- discount_net_profits(prof, discount_rate = 0.0)
  
  # With zero discount rate, discounted profit should equal original profit
  testthat::expect_equal(out_zero$discounted_net_profit, prof$asset_profit, tolerance = 1e-10,
                        info = "Zero discount rate should leave profits unchanged")
})

testthat::test_that("discount_net_profits determinism and reproducibility", {
  data <- create_baseline_and_shock()
  scen <- build_scenarios(data$baseline, data$shocked)
  rev <- compute_asset_revenue(scen, data$companies, growth_rate = 0.03)
  prof <- compute_asset_profits(rev, net_profit_margin = 0.2)
  
  discount_rate <- 0.12
  
  # Multiple runs should produce identical results
  out1 <- discount_net_profits(prof, discount_rate = discount_rate)
  out2 <- discount_net_profits(prof, discount_rate = discount_rate)
  
  testthat::expect_equal(out1$discounted_net_profit, out2$discounted_net_profit,
                        info = "Discounting should be deterministic across runs")
  
  # Test that function doesn't modify input data
  original_profit <- prof$asset_profit
  discount_net_profits(prof, discount_rate = discount_rate)
  testthat::expect_equal(prof$asset_profit, original_profit,
                        info = "Function should not modify input data")
})

testthat::test_that("discount_net_profits handles edge cases and validates inputs", {
  data <- create_baseline_and_shock()
  scen <- build_scenarios(data$baseline, data$shocked)
  rev <- compute_asset_revenue(scen, data$companies, growth_rate = 0.03)
  prof <- compute_asset_profits(rev, net_profit_margin = 0.2)
  
  # Test with negative profits
  prof_negative <- prof
  prof_negative$asset_profit[1:2] <- -1000
  
  out_negative <- discount_net_profits(prof_negative, discount_rate = 0.1)
  testthat::expect_true(is.numeric(out_negative$discounted_net_profit),
                       info = "Should handle negative profits")
  testthat::expect_true(all(out_negative$discounted_net_profit[1:2] < 0),
                       info = "Negative profits should remain negative when discounted")
  
  # Test schema preservation
  original_cols <- names(prof)
  out <- discount_net_profits(prof, discount_rate = 0.1)
  testthat::expect_true(all(original_cols %in% names(out)),
                       info = "All original columns should be preserved")
  testthat::expect_true("discounted_net_profit" %in% names(out),
                       info = "discounted_net_profit column should be added")
  
  # Test row count preservation
  testthat::expect_equal(nrow(out), nrow(prof),
                        info = "Row count should be preserved")
  
  # Test parameter validation
  testthat::expect_error(
    discount_net_profits(prof, discount_rate = "invalid"),
    info = "Should error with non-numeric discount rate"
  )
  
  testthat::expect_error(
    discount_net_profits(prof, discount_rate = -0.1),
    info = "Should error with negative discount rate"
  )
})
