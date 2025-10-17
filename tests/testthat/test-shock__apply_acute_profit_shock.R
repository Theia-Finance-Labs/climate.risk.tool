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
    hazard_type = c("FloodTIF", "FloodTIF"),
    hazard_name = c("FloodTIF__Flood Height__GWL=RCP8.5__RP=100", "FloodTIF__Flood Height__GWL=RCP8.5__RP=100"),
    damage_factor = c(0.5, 0.4),
    cost_factor = c(200, 200),
    asset_category = c("Industrial", "Commercial")
  )

  # Add acute_damage column as expected by the function
  assets_factors$acute_damage <- assets_factors$damage_factor * assets_factors$cost_factor

  acute_events <- data.frame(
    event_id = "e1",
    hazard_type = "FloodTIF",
    hazard_name = "FloodTIF__Flood Height__GWL=RCP8.5__RP=100",
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

testthat::test_that("apply_acute_profit_shock processes events in order by event_id", {
  yearly_trajectories <- data.frame(
    asset = c("A1", "A1"),
    company = c("C1", "C1"),
    year = c(2025, 2030),
    revenue = c(1000, 1200),
    profit = c(100, 120)
  )

  assets_factors <- data.frame(
    asset = c("A1", "A1"),
    hazard_type = c("FloodTIF", "FloodTIF"),
    hazard_name = c("FloodTIF__Flood Height__GWL=RCP8.5__RP=100", "FloodTIF__Flood Height__GWL=RCP8.5__RP=50"),
    damage_factor = c(0.5, 0.3),
    cost_factor = c(200, 150),
    asset_category = c("Industrial", "Industrial")
  )

  # Test with events in non-alphabetical order by event_id
  acute_events <- data.frame(
    event_id = c("event_z", "event_a"),
    hazard_type = c("FloodTIF", "FloodTIF"),
    hazard_name = c("FloodTIF__Flood Height__GWL=RCP8.5__RP=100", "FloodTIF__Flood Height__GWL=RCP8.5__RP=50"),
    event_year = c(2030L, 2030L),
    chronic = c(FALSE, FALSE),
    stringsAsFactors = FALSE
  )

  result <- apply_acute_profit_shock(yearly_trajectories, assets_factors, acute_events)

  # Should process events in alphabetical order by event_id
  # event_a should be processed first, then event_z
  # Both events affect the same asset-year (A1, 2030)
  # Total damage should be: (0.3 * 150) + (0.5 * 200) = 45 + 100 = 145
  expected_profit_2030 <- 120 - 145 # -25
  testthat::expect_equal(result$profit[result$asset == "A1" & result$year == 2030], expected_profit_2030)
})
