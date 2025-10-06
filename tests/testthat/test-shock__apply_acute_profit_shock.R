# Tests for apply_acute_profit_shock

testthat::test_that("apply_acute_profit_shock passes through as placeholder", {
  # Placeholder implementation: just passes through profit unchanged
  yearly_trajectories <- data.frame(
    asset = c("A1", "A1", "A2", "A2"),
    company = c("C1", "C1", "C1", "C1"),
    year = c(2025, 2030, 2025, 2030),
    revenue = c(1000, 1200, 800, 960),
    profit = c(100, 120, 80, 96)
  )

  assets_factors <- data.frame(
    asset = c("A1", "A2"),
    hazard_type = c("flood", "flood"),
    hazard_name = c("flood__global_rcp85_h100glob_brazil", "flood__global_rcp85_h100glob_brazil"),
    cost_factor = c(100, 80),
    asset_category = c("Industrial", "Commercial")
  )

  acute_events <- data.frame(
    event_id = "e1",
    hazard_type = "flood",
    hazard_name = "flood__global_rcp85_h100glob_brazil",
    event_year = 2030L,
    chronic = FALSE,
    stringsAsFactors = FALSE
  )

  result <- apply_acute_profit_shock(yearly_trajectories, assets_factors, acute_events)

  # Should return profit column
  expected_cols <- c("asset", "company", "year", "revenue", "profit")
  testthat::expect_true(all(expected_cols %in% names(result)))
  testthat::expect_equal(nrow(result), nrow(yearly_trajectories))

  # Should apply profit shock by subtracting cost_factor for event year
  # Note: merge may reorder rows, so we need to sort both for comparison
  result_sorted <- result[order(result$asset, result$year), ]
  baseline_sorted <- yearly_trajectories[order(yearly_trajectories$asset, yearly_trajectories$year), ]
  
  testthat::expect_equal(result_sorted$revenue, baseline_sorted$revenue) # Revenue unchanged
  
  # For year 2030 (event_year), profit should be reduced by cost_factor
  # A1 in 2030: 120 - 100 = 20
  # A2 in 2030: 96 - 80 = 16
  testthat::expect_equal(result$profit[result$asset == "A1" & result$year == 2030], 20)
  testthat::expect_equal(result$profit[result$asset == "A2" & result$year == 2030], 16)
  # For year 2025 (non-event year), profit should be unchanged
  testthat::expect_equal(result$profit[result$asset == "A1" & result$year == 2025], 100)
  testthat::expect_equal(result$profit[result$asset == "A2" & result$year == 2025], 80)
})

