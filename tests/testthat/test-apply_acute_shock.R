# Tests for function 6: apply_acute_shock

# Contract:
# - apply_acute_shock(df, shock_year)
# - Adds numeric column acute_shock based on hazard intensity and shock year
# - Acute shocks represent sudden, year-specific climate events


testthat::test_that("apply_acute_shock adds numeric acute_shock column", {
  df <- data.frame(x = 1:3)
  out <- apply_acute_shock(df, shock_year = 2030)
  testthat::expect_true("acute_shock" %in% names(out))
  testthat::expect_true(is.numeric(out$acute_shock))
  testthat::expect_equal(nrow(out), nrow(df))
})

testthat::test_that("apply_acute_shock validates shock_year parameter", {
  df <- data.frame(x = 1:3)
  
  # Valid shock years should work
  testthat::expect_no_error(apply_acute_shock(df, shock_year = 2030))
  testthat::expect_no_error(apply_acute_shock(df, shock_year = 2050))
  
  # Invalid shock years should error
  testthat::expect_error(
    apply_acute_shock(df, shock_year = "invalid"),
    regexp = "numeric",
    info = "Should error with non-numeric shock_year"
  )
  
  testthat::expect_error(
    apply_acute_shock(df, shock_year = c(2030, 2040)),
    regexp = "single",
    info = "Should error with multiple shock_year values"
  )
  
  # Historical years might be invalid depending on implementation
  testthat::expect_error(
    apply_acute_shock(df, shock_year = 1990),
    regexp = "future|valid",
    info = "Should error with historical shock_year"
  )
})

testthat::test_that("apply_acute_shock uses hazard data when available", {
  # Create test data with hazard information
  df <- data.frame(
    asset_id = 1:4,
    hazard_mean_flood = c(0, 5, 10, 20),
    hazard_mean_drought = c(2, 8, 15, 25),
    damage_factor = c(0.1, 0.2, 0.3, 0.4)
  )
  
  out <- apply_acute_shock(df, shock_year = 2030)
  
  # Acute shock should be influenced by hazard intensity
  testthat::expect_true(all(out$acute_shock >= 0),
                       info = "Acute shock should be non-negative")
  
  # Higher hazard intensity should generally lead to higher acute shock
  high_hazard_rows <- df$hazard_mean_flood > 15
  low_hazard_rows <- df$hazard_mean_flood < 5
  
  if (any(high_hazard_rows) && any(low_hazard_rows)) {
    high_hazard_shock <- mean(out$acute_shock[high_hazard_rows])
    low_hazard_shock <- mean(out$acute_shock[low_hazard_rows])
    
    testthat::expect_true(high_hazard_shock >= low_hazard_shock,
                         info = "Higher hazard intensity should lead to higher or equal acute shock")
  }
})

testthat::test_that("apply_acute_shock handles different shock years", {
  df <- data.frame(
    asset_id = 1:3,
    hazard_mean_flood = c(5, 10, 15)
  )
  
  # Test different shock years
  out_2030 <- apply_acute_shock(df, shock_year = 2030)
  out_2050 <- apply_acute_shock(df, shock_year = 2050)
  
  # Different shock years might produce different results
  # (depending on implementation - could be time-dependent)
  testthat::expect_true(is.numeric(out_2030$acute_shock))
  testthat::expect_true(is.numeric(out_2050$acute_shock))
  testthat::expect_equal(length(out_2030$acute_shock), nrow(df))
  testthat::expect_equal(length(out_2050$acute_shock), nrow(df))
})

testthat::test_that("apply_acute_shock preserves schema and handles edge cases", {
  df <- data.frame(
    asset_id = 1:3,
    company = c("A", "B", "C"),
    hazard_mean_flood = c(0, 10, 20)
  )
  
  out <- apply_acute_shock(df, shock_year = 2030)
  
  # Schema preservation
  original_cols <- names(df)
  testthat::expect_true(all(original_cols %in% names(out)),
                       info = "All original columns should be preserved")
  testthat::expect_true("acute_shock" %in% names(out),
                       info = "acute_shock column should be added")
  
  # Row count preservation
  testthat::expect_equal(nrow(out), nrow(df),
                        info = "Row count should be preserved")
  
  # Handle missing hazard data gracefully
  df_no_hazard <- data.frame(asset_id = 1:2, company = c("A", "B"))
  out_no_hazard <- apply_acute_shock(df_no_hazard, shock_year = 2030)
  
  testthat::expect_true("acute_shock" %in% names(out_no_hazard),
                       info = "Should work even without hazard columns")
  testthat::expect_true(is.numeric(out_no_hazard$acute_shock),
                       info = "Should produce numeric acute_shock even without hazards")
})

testthat::test_that("apply_acute_shock is deterministic", {
  df <- data.frame(
    asset_id = 1:3,
    hazard_mean_flood = c(5, 10, 15)
  )
  
  # Multiple runs should produce identical results
  out1 <- apply_acute_shock(df, shock_year = 2030)
  out2 <- apply_acute_shock(df, shock_year = 2030)
  
  testthat::expect_equal(out1$acute_shock, out2$acute_shock,
                        info = "Function should be deterministic")
  
  # Input data should not be modified
  original_df <- data.frame(
    asset_id = 1:3,
    hazard_mean_flood = c(5, 10, 15)
  )
  df_copy <- df
  apply_acute_shock(df_copy, shock_year = 2030)
  testthat::expect_equal(df_copy, original_df,
                        info = "Input data should not be modified")
})
