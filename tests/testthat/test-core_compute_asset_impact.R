# Tests for function: compute_asset_impact

# Contract:
# - compute_asset_impact(df)
# - Updates numeric column `share_of_economic_activity` based on damage factors and shocks
# - Removes working columns: hazard_* columns, damage_factor, cost_factor, acute_shock, chronic_shock, geometry, centroid
# - Applies impact formula that reduces economic activity based on climate impacts


testthat::test_that("compute_asset_impact updates share and removes working columns", {
  base_dir <- get_test_data_dir()
  assets <- read_assets(base_dir)
  df <- assets

  # Add minimal working columns to simulate earlier stages
  df$hazard_dummy <- 10.5
  df$damage_factor <- 0.2
  df$cost_factor <- 100
  df$acute_shock <- 0.05
  df$chronic_shock <- 0.01

  out <- compute_asset_impact(df)

  testthat::expect_true("share_of_economic_activity" %in% names(out))
  testthat::expect_true(is.numeric(out[["share_of_economic_activity"]]))

  removed_cols <- c("hazard_dummy", "damage_factor", "cost_factor", "acute_shock", "chronic_shock", "geometry", "centroid")
  testthat::expect_false(any(removed_cols %in% names(out)))
})

testthat::test_that("compute_asset_impact validates impact formula", {
  base_dir <- get_test_data_dir()
  assets <- read_assets(base_dir)
  df <- assets
  
  # Store original share values
  original_shares <- df$share_of_economic_activity
  
  # Add working columns with known values
  df$damage_factor <- c(0.1, 0.2, 0.3, 0.0, 0.15, rep(0.1, nrow(df) - 5))
  df$acute_shock <- c(0.02, 0.05, 0.08, 0.0, 0.03, rep(0.02, nrow(df) - 5))
  df$chronic_shock <- c(0.01, 0.02, 0.03, 0.0, 0.015, rep(0.01, nrow(df) - 5))
  df$cost_factor <- rep(100, nrow(df))  # Cost factor for cleanup
  
  out <- compute_asset_impact(df)
  
  # Impact should reduce share of economic activity
  # Formula expectation: new_share = original_share * (1 - total_impact)
  # where total_impact combines damage_factor, acute_shock, chronic_shock
  
  # Test that high impact reduces shares more than low impact
  high_impact_idx <- which(df$damage_factor > 0.25 | df$acute_shock > 0.06)
  low_impact_idx <- which(df$damage_factor < 0.05 & df$acute_shock < 0.02)
  
  if (length(high_impact_idx) > 0 && length(low_impact_idx) > 0) {
    high_impact_reduction <- mean(original_shares[high_impact_idx] - out$share_of_economic_activity[high_impact_idx])
    low_impact_reduction <- mean(original_shares[low_impact_idx] - out$share_of_economic_activity[low_impact_idx])
    
    testthat::expect_true(high_impact_reduction >= low_impact_reduction,
                         info = "Higher impact should lead to greater reduction in economic activity")
  }
  
  # Shares should not become negative
  testthat::expect_true(all(out$share_of_economic_activity >= 0),
                       info = "Share of economic activity should not become negative")
  
  # Shares should not exceed 1
  testthat::expect_true(all(out$share_of_economic_activity <= 1),
                       info = "Share of economic activity should not exceed 1")
})

testthat::test_that("compute_asset_impact handles zero impact correctly", {
  base_dir <- get_test_data_dir()
  assets <- read_assets(base_dir)
  df <- assets
  
  original_shares <- df$share_of_economic_activity
  
  # Zero impact scenario
  df$damage_factor <- rep(0, nrow(df))
  df$acute_shock <- rep(0, nrow(df))
  df$chronic_shock <- rep(0, nrow(df))
  df$cost_factor <- rep(100, nrow(df))
  
  out <- compute_asset_impact(df)
  
  # With zero impact, shares should remain unchanged (or very close)
  testthat::expect_equal(out$share_of_economic_activity, original_shares, tolerance = 1e-10,
                        info = "Zero impact should leave shares unchanged")
})

testthat::test_that("compute_asset_impact handles extreme impact values", {
  base_dir <- get_test_data_dir()
  assets <- read_assets(base_dir)
  df <- assets[1:3, ]  # Use subset for faster testing
  
  # Extreme impact scenario
  df$damage_factor <- c(0.9, 1.0, 1.2)  # Very high damage
  df$acute_shock <- c(0.1, 0.2, 0.3)    # High acute shock
  df$chronic_shock <- c(0.05, 0.1, 0.15) # High chronic shock
  df$cost_factor <- rep(1000, nrow(df))   # High cost
  
  out <- compute_asset_impact(df)
  
  # Even with extreme values, function should not crash
  testthat::expect_true(all(is.finite(out$share_of_economic_activity)),
                       info = "Should handle extreme impact values without producing infinite/NaN")
  
  # Shares should be bounded
  testthat::expect_true(all(out$share_of_economic_activity >= 0),
                       info = "Shares should not go negative even with extreme impact")
  testthat::expect_true(all(out$share_of_economic_activity <= 1),
                       info = "Shares should not exceed 1 even with extreme impact")
})

testthat::test_that("compute_asset_impact removes all working columns", {
  base_dir <- get_test_data_dir()
  assets <- read_assets(base_dir)
  df <- assets
  
  # Add comprehensive set of working columns
  df$hazard_mean_flood <- runif(nrow(df), 0, 20)
  df$hazard_mean_drought <- runif(nrow(df), 0, 15)
  df$hazard_max_temperature <- runif(nrow(df), 25, 45)
  df$damage_factor <- runif(nrow(df), 0, 0.5)
  df$cost_factor <- runif(nrow(df), 50, 200)
  df$acute_shock <- runif(nrow(df), 0, 0.1)
  df$chronic_shock <- runif(nrow(df), 0, 0.05)
  
  # Add geometry columns if they don't exist
  if (!"geometry" %in% names(df)) df$geometry <- paste0("POLYGON_", 1:nrow(df))
  if (!"centroid" %in% names(df)) df$centroid <- paste0("POINT_", 1:nrow(df))
  
  out <- compute_asset_impact(df)
  
  # Define all working columns that should be removed
  working_cols <- c(
    # Hazard columns (any column starting with "hazard_")
    grep("^hazard_", names(df), value = TRUE),
    # Specific working columns
    "damage_factor", "cost_factor", "acute_shock", "chronic_shock", 
    "geometry", "centroid"
  )
  
  # Check that working columns are removed
  remaining_working_cols <- intersect(working_cols, names(out))
  testthat::expect_equal(length(remaining_working_cols), 0,
                        info = paste("Working columns should be removed:", 
                                   paste(remaining_working_cols, collapse = ", ")))
  
  # Essential columns should be preserved
  essential_cols <- c("company", "asset", "share_of_economic_activity", "sector", "asset_category")
  available_essential <- intersect(essential_cols, names(df))
  testthat::expect_true(all(available_essential %in% names(out)),
                       info = "Essential columns should be preserved")
})

testthat::test_that("compute_asset_impact preserves schema and validates input", {
  base_dir <- get_test_data_dir()
  assets <- read_assets(base_dir)
  df <- assets
  
  # Add required working columns
  df$damage_factor <- runif(nrow(df), 0, 0.3)
  df$acute_shock <- runif(nrow(df), 0, 0.05)
  df$chronic_shock <- runif(nrow(df), 0, 0.02)
  
  out <- compute_asset_impact(df)
  
  # Row count preservation
  testthat::expect_equal(nrow(out), nrow(df),
                        info = "Row count should be preserved")
  
  # Required column should exist and be numeric
  testthat::expect_true("share_of_economic_activity" %in% names(out),
                       info = "share_of_economic_activity column should exist")
  testthat::expect_true(is.numeric(out$share_of_economic_activity),
                       info = "share_of_economic_activity should be numeric")
  
  # Input validation
  testthat::expect_error(
    compute_asset_impact("not_a_dataframe"),
    regexp = "data.frame",
    info = "Should error with non-data.frame input"
  )
  
  # Test with missing required columns
  df_missing <- df[, !names(df) %in% c("damage_factor")]
  testthat::expect_error(
    compute_asset_impact(df_missing),
    regexp = "damage_factor|required",
    info = "Should error when required working columns are missing"
  )
})

testthat::test_that("compute_asset_impact is deterministic and preserves data integrity", {
  base_dir <- get_test_data_dir()
  assets <- read_assets(base_dir)
  df <- assets
  
  # Add working columns
  df$damage_factor <- c(0.1, 0.2, 0.15, rep(0.1, nrow(df) - 3))
  df$acute_shock <- c(0.02, 0.04, 0.03, rep(0.02, nrow(df) - 3))
  df$chronic_shock <- c(0.01, 0.02, 0.015, rep(0.01, nrow(df) - 3))
  
  # Multiple runs should produce identical results
  out1 <- compute_asset_impact(df)
  out2 <- compute_asset_impact(df)
  
  testthat::expect_equal(out1$share_of_economic_activity, out2$share_of_economic_activity,
                        info = "Function should be deterministic")
  
  # Input data should not be modified
  original_df <- df
  df_copy <- df
  compute_asset_impact(df_copy)
  testthat::expect_equal(df_copy, original_df,
                        info = "Input data should not be modified")
  
  # All company/asset identifiers should be preserved
  id_cols <- intersect(c("company", "asset", "asset_id"), names(df))
  if (length(id_cols) > 0) {
    for (col in id_cols) {
      testthat::expect_equal(out1[[col]], df[[col]],
                           info = paste("ID column", col, "should be preserved"))
    }
  }
})
