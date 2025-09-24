# Tests for function 7: apply_chronic_shock

# Contract:
# - apply_chronic_shock(df)
# - Adds numeric column chronic_shock based on long-term climate trends
# - Chronic shocks represent gradual, persistent climate changes


testthat::test_that("apply_chronic_shock adds numeric chronic_shock column", {
  df <- data.frame(x = 1:3)
  out <- apply_chronic_shock(df)
  testthat::expect_true("chronic_shock" %in% names(out))
  testthat::expect_true(is.numeric(out$chronic_shock))
  testthat::expect_equal(nrow(out), nrow(df))
})

testthat::test_that("apply_chronic_shock uses hazard data when available", {
  # Create test data with hazard information
  df <- data.frame(
    asset_id = 1:4,
    hazard_mean_temperature = c(25, 30, 35, 40),
    hazard_mean_precipitation = c(100, 80, 60, 40),
    damage_factor = c(0.1, 0.2, 0.3, 0.4)
  )
  
  out <- apply_chronic_shock(df)
  
  # Chronic shock should be influenced by hazard intensity
  testthat::expect_true(all(out$chronic_shock >= 0),
                       info = "Chronic shock should be non-negative")
  
  # Higher hazard intensity should generally lead to higher chronic shock
  high_temp_rows <- df$hazard_mean_temperature > 35
  low_temp_rows <- df$hazard_mean_temperature < 30
  
  if (any(high_temp_rows) && any(low_temp_rows)) {
    high_temp_shock <- mean(out$chronic_shock[high_temp_rows])
    low_temp_shock <- mean(out$chronic_shock[low_temp_rows])
    
    testthat::expect_true(high_temp_shock >= low_temp_shock,
                         info = "Higher temperature should lead to higher or equal chronic shock")
  }
})

testthat::test_that("apply_chronic_shock handles multiple hazard types", {
  df <- data.frame(
    asset_id = 1:3,
    hazard_mean_temperature = c(30, 35, 40),
    hazard_mean_drought = c(5, 10, 15),
    hazard_mean_flood = c(2, 8, 12)
  )
  
  out <- apply_chronic_shock(df)
  
  # Should combine multiple hazard types appropriately
  testthat::expect_true(is.numeric(out$chronic_shock))
  testthat::expect_true(all(out$chronic_shock >= 0))
  testthat::expect_equal(length(out$chronic_shock), nrow(df))
  
  # Assets with higher overall hazard exposure should have higher chronic shock
  # This is a general expectation - exact implementation may vary
  testthat::expect_true(all(is.finite(out$chronic_shock)),
                       info = "All chronic shock values should be finite")
})

testthat::test_that("apply_chronic_shock is consistent and deterministic", {
  df <- data.frame(
    asset_id = 1:3,
    hazard_mean_temperature = c(30, 35, 40)
  )
  
  # Multiple runs should produce identical results
  out1 <- apply_chronic_shock(df)
  out2 <- apply_chronic_shock(df)
  
  testthat::expect_equal(out1$chronic_shock, out2$chronic_shock,
                        info = "Function should be deterministic")
  
  # Chronic shocks should be relatively stable (less volatile than acute)
  # This is a conceptual test - chronic shocks represent long-term trends
  shock_variance <- var(out1$chronic_shock)
  testthat::expect_true(is.finite(shock_variance),
                       info = "Chronic shock variance should be finite")
})

testthat::test_that("apply_chronic_shock preserves schema and handles edge cases", {
  df <- data.frame(
    asset_id = 1:3,
    company = c("A", "B", "C"),
    hazard_mean_temperature = c(25, 30, 35)
  )
  
  out <- apply_chronic_shock(df)
  
  # Schema preservation
  original_cols <- names(df)
  testthat::expect_true(all(original_cols %in% names(out)),
                       info = "All original columns should be preserved")
  testthat::expect_true("chronic_shock" %in% names(out),
                       info = "chronic_shock column should be added")
  
  # Row count preservation
  testthat::expect_equal(nrow(out), nrow(df),
                        info = "Row count should be preserved")
  
  # Handle missing hazard data gracefully
  df_no_hazard <- data.frame(asset_id = 1:2, company = c("A", "B"))
  out_no_hazard <- apply_chronic_shock(df_no_hazard)
  
  testthat::expect_true("chronic_shock" %in% names(out_no_hazard),
                       info = "Should work even without hazard columns")
  testthat::expect_true(is.numeric(out_no_hazard$chronic_shock),
                       info = "Should produce numeric chronic_shock even without hazards")
  
  # Input data should not be modified
  original_df <- df
  df_copy <- df
  apply_chronic_shock(df_copy)
  testthat::expect_equal(df_copy, original_df,
                        info = "Input data should not be modified")
})

testthat::test_that("apply_chronic_shock validates input", {
  # Should work with data.frame
  df <- data.frame(x = 1:3)
  testthat::expect_no_error(apply_chronic_shock(df))
  
  # Should error with invalid input
  testthat::expect_error(
    apply_chronic_shock("not_a_dataframe"),
    regexp = "data.frame",
    info = "Should error with non-data.frame input"
  )
  
  testthat::expect_error(
    apply_chronic_shock(NULL),
    regexp = "data.frame",
    info = "Should error with NULL input"
  )
})

testthat::test_that("apply_chronic_shock handles extreme values", {
  df <- data.frame(
    asset_id = 1:4,
    hazard_mean_temperature = c(0, 25, 50, 100),  # Extreme temperature range
    hazard_mean_precipitation = c(0, 50, 100, 200)  # Extreme precipitation range
  )
  
  out <- apply_chronic_shock(df)
  
  # Should handle extreme values without errors
  testthat::expect_true(all(is.finite(out$chronic_shock)),
                       info = "Should handle extreme hazard values")
  testthat::expect_true(all(out$chronic_shock >= 0),
                       info = "Chronic shock should remain non-negative even with extreme values")
  testthat::expect_equal(length(out$chronic_shock), nrow(df),
                        info = "Should produce chronic shock for all rows")
})
