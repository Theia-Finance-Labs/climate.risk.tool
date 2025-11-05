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
    hazard_type = c("Flood", "Flood"),
    hazard_name = c("Flood__depth(cm)__GWL=RCP8.5__RP=100", "Flood__depth(cm)__GWL=RCP8.5__RP=100"),
    event_id = c("e1", "e1"),
    damage_factor = c(0.5, 0.4),
    cost_factor = c(200, 200),
    asset_category = c("industrial building", "commercial building")
  )

  # Add acute_damage column as expected by the function
  assets_factors$acute_damage <- assets_factors$damage_factor * assets_factors$cost_factor

  acute_events <- data.frame(
    event_id = "e1",
    hazard_type = "Flood",
    hazard_name = "Flood__depth(cm)__GWL=RCP8.5__RP=100",
    event_year = 2030L,
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
    hazard_type = c("Flood", "Flood"),
    hazard_name = c("Flood__depth(cm)__GWL=RCP8.5__RP=100", "Flood__depth(cm)__GWL=RCP8.5__RP=50"),
    event_id = c("event_z", "event_a"),
    damage_factor = c(0.5, 0.3),
    cost_factor = c(200, 150),
    asset_category = c("industrial building", "industrial building")
  )

  # Test with events in non-alphabetical order by event_id
  acute_events <- data.frame(
    event_id = c("event_z", "event_a"),
    hazard_type = c("Flood", "Flood"),
    hazard_name = c("Flood__depth(cm)__GWL=RCP8.5__RP=100", "Flood__depth(cm)__GWL=RCP8.5__RP=50"),
    event_year = c(2030L, 2030L),
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

testthat::test_that("apply_acute_profit_shock excludes agriculture assets", {
  yearly_trajectories <- data.frame(
    asset = c("AG1", "AG1", "COM1", "COM1"),
    company = c("C1", "C1", "C1", "C1"),
    year = c(2025, 2030, 2025, 2030),
    revenue = c(1000, 1200, 1000, 1200),
    profit = c(100, 120, 100, 120)
  )

  assets_factors <- data.frame(
    asset = c("AG1", "COM1"),
    hazard_type = c("Flood", "Flood"),
    hazard_name = c("flood1", "flood1"),
    event_id = c("event_1", "event_1"),
    damage_factor = c(0.3, 0.3),
    cost_factor = c(100, 100),
    asset_category = c("agriculture", "commercial building")
  )

  acute_events <- data.frame(
    event_id = "event_1",
    hazard_type = "Flood",
    hazard_name = "flood1",
    event_year = 2030L
  )

  result <- apply_acute_profit_shock(yearly_trajectories, assets_factors, acute_events)

  # Agriculture asset (AG1) should NOT have profit shock applied
  testthat::expect_equal(result$profit[result$asset == "AG1" & result$year == 2030], 120)

  # Commercial asset (COM1) should have profit shock applied
  # profit = 120 - (0.3 * 100) = 90
  testthat::expect_equal(result$profit[result$asset == "COM1" & result$year == 2030], 90)
})

testthat::test_that("apply_acute_profit_shock applies to industrial buildings with correct cost_factor", {
  yearly_trajectories <- data.frame(
    asset = c("IND1", "IND1"),
    company = c("C1", "C1"),
    year = c(2025, 2030),
    revenue = c(1000, 1200),
    profit = c(100, 120)
  )

  assets_factors <- data.frame(
    asset = "IND1",
    hazard_type = "Flood",
    hazard_name = "flood1",
    event_id = "event_1",
    damage_factor = 0.4,
    cost_factor = 250, # Industrial-specific cost factor
    asset_category = "industrial building"
  )

  acute_events <- data.frame(
    event_id = "event_1",
    hazard_type = "Flood",
    hazard_name = "flood1",
    event_year = 2030L
  )

  result <- apply_acute_profit_shock(yearly_trajectories, assets_factors, acute_events)

  # Industrial building: profit = 120 - (0.4 * 250) = 20
  expected_profit_2030 <- 120 - (0.4 * 250)
  testthat::expect_equal(result$profit[result$year == 2030], expected_profit_2030)
  testthat::expect_equal(result$profit[result$year == 2025], 100) # 2025 unchanged
})

testthat::test_that("apply_acute_profit_shock handles commercial building separately from industrial", {
  yearly_trajectories <- data.frame(
    asset = c("COM1", "COM1", "IND1", "IND1"),
    company = c("C1", "C1", "C1", "C1"),
    year = c(2025, 2030, 2025, 2030),
    revenue = c(1000, 1200, 1000, 1200),
    profit = c(100, 120, 100, 120)
  )

  assets_factors <- data.frame(
    asset = c("COM1", "IND1"),
    hazard_type = c("Flood", "Flood"),
    hazard_name = c("flood1", "flood1"),
    event_id = c("event_1", "event_1"),
    damage_factor = c(0.3, 0.4),
    cost_factor = c(150, 250), # Different cost factors
    asset_category = c("commercial building", "industrial building")
  )

  acute_events <- data.frame(
    event_id = "event_1",
    hazard_type = "Flood",
    hazard_name = "flood1",
    event_year = 2030L
  )

  result <- apply_acute_profit_shock(yearly_trajectories, assets_factors, acute_events)

  # Commercial: 120 - (0.3 * 150) = 75
  testthat::expect_equal(result$profit[result$asset == "COM1" & result$year == 2030], 75)

  # Industrial: 120 - (0.4 * 250) = 20
  testthat::expect_equal(result$profit[result$asset == "IND1" & result$year == 2030], 20)
})

testthat::test_that("apply_acute_profit_shock applies Fire profit shock to commercial/industrial buildings correctly", {
  yearly_trajectories <- data.frame(
    asset = c("COM1", "COM1", "IND1", "IND1"),
    company = c("C1", "C1", "C1", "C1"),
    year = c(2025, 2030, 2025, 2030),
    revenue = c(1000, 1200, 1000, 1200),
    profit = c(100, 120, 100, 120)
  )

  # Fire damage formula: land_cover_risk * damage_factor * (days_danger_total / 365) * cost_factor
  # COM1: land_cover_risk=0.5, damage_factor=0.15, days=30, cost_factor=200
  # Fire damage = 0.5 * 0.15 * (30/365) * 200 = 1.23
  # IND1: land_cover_risk=0.75, damage_factor=0.20, days=45, cost_factor=300
  # Fire damage = 0.75 * 0.20 * (45/365) * 300 = 5.55
  assets_factors <- data.frame(
    asset = c("COM1", "IND1"),
    hazard_type = c("Fire", "Fire"),
    event_id = c("event_1", "event_1"),
    land_cover_risk = c(0.5, 0.75),
    damage_factor = c(0.15, 0.20),
    days_danger_total = c(30, 45),
    cost_factor = c(200, 300),
    asset_category = c("commercial building", "industrial building")
  )

  acute_events <- data.frame(
    event_id = "event_1",
    hazard_type = "Fire",
    event_year = 2030L
  )

  result <- apply_acute_profit_shock(yearly_trajectories, assets_factors, acute_events)

  # COM1: 120 - (0.5 * 0.15 * (30/365) * 200) = 118.77
  expected_profit_COM1 <- 120 - (0.5 * 0.15 * (30 / 365) * 200)
  testthat::expect_equal(result$profit[result$asset == "COM1" & result$year == 2030], expected_profit_COM1, tolerance = 0.1)

  # IND1: 120 - (0.75 * 0.20 * (45/365) * 300) = 114.45
  expected_profit_IND1 <- 120 - (0.75 * 0.20 * (45 / 365) * 300)
  testthat::expect_equal(result$profit[result$asset == "IND1" & result$year == 2030], expected_profit_IND1, tolerance = 0.1)

  # 2025 profits should be unchanged
  testthat::expect_equal(result$profit[result$year == 2025], yearly_trajectories$profit[yearly_trajectories$year == 2025])
})

testthat::test_that("apply_acute_profit_shock excludes agriculture assets from Fire profit shock", {
  yearly_trajectories <- data.frame(
    asset = c("AG1", "AG1", "COM1", "COM1"),
    company = c("C1", "C1", "C1", "C1"),
    year = c(2025, 2030, 2025, 2030),
    revenue = c(1000, 1200, 1000, 1200),
    profit = c(100, 120, 100, 120)
  )

  assets_factors <- data.frame(
    asset = c("AG1", "COM1"),
    hazard_type = c("Fire", "Fire"),
    event_id = c("event_1", "event_1"),
    land_cover_risk = c(0.5, 0.5),
    damage_factor = c(0.15, 0.15),
    days_danger_total = c(30, 30),
    cost_factor = c(200, 200),
    asset_category = c("agriculture", "commercial building")
  )

  acute_events <- data.frame(
    event_id = "event_1",
    hazard_type = "Fire",
    event_year = 2030L
  )

  result <- apply_acute_profit_shock(yearly_trajectories, assets_factors, acute_events)

  # Agriculture asset (AG1) should NOT have profit shock applied
  testthat::expect_equal(result$profit[result$asset == "AG1" & result$year == 2030], 120)

  # Commercial asset (COM1) should have profit shock applied
  expected_profit_COM1 <- 120 - (0.5 * 0.15 * (30 / 365) * 200)
  testthat::expect_equal(result$profit[result$asset == "COM1" & result$year == 2030], expected_profit_COM1, tolerance = 0.1)
})

testthat::test_that("apply_acute_profit_shock handles multiple Fire events in same year (sums damage)", {
  yearly_trajectories <- data.frame(
    asset = c("COM1", "COM1"),
    company = c("C1", "C1"),
    year = c(2025, 2030),
    revenue = c(1000, 1200),
    profit = c(100, 120)
  )

  # Two Fire events affecting same asset
  assets_factors <- data.frame(
    asset = c("COM1", "COM1"),
    hazard_type = c("Fire", "Fire"),
    event_id = c("event_1", "event_2"),
    land_cover_risk = c(0.5, 0.5),
    damage_factor = c(0.15, 0.10),
    days_danger_total = c(30, 20),
    cost_factor = c(200, 150),
    asset_category = c("commercial building", "commercial building")
  )

  acute_events <- data.frame(
    event_id = c("event_1", "event_2"),
    hazard_type = c("Fire", "Fire"),
    event_year = c(2030L, 2030L)
  )

  result <- apply_acute_profit_shock(yearly_trajectories, assets_factors, acute_events)

  # Total damage should be sum of both events
  # event_1: 0.5 * 0.15 * (30/365) * 200 = 1.23
  # event_2: 0.5 * 0.10 * (20/365) * 150 = 0.41
  # Total: 1.23 + 0.41 = 1.64
  total_damage <- (0.5 * 0.15 * (30 / 365) * 200) + (0.5 * 0.10 * (20 / 365) * 150)
  expected_profit <- 120 - total_damage
  testthat::expect_equal(result$profit[result$year == 2030], expected_profit, tolerance = 0.1)
})

testthat::test_that("apply_acute_profit_shock allows Fire profit to go negative", {
  yearly_trajectories <- data.frame(
    asset = c("COM1", "COM1"),
    company = c("C1", "C1"),
    year = c(2025, 2030),
    revenue = c(1000, 1200),
    profit = c(100, 120)
  )

  # Extreme Fire damage that makes profit negative
  # land_cover_risk=1.0, damage_factor=0.5, days=365, cost_factor=500
  # Fire damage = 1.0 * 0.5 * (365/365) * 500 = 250
  assets_factors <- data.frame(
    asset = "COM1",
    hazard_type = "Fire",
    event_id = "event_1",
    land_cover_risk = 1.0,
    damage_factor = 0.5,
    days_danger_total = 365, # Full year
    cost_factor = 500,
    asset_category = "commercial building"
  )

  acute_events <- data.frame(
    event_id = "event_1",
    hazard_type = "Fire",
    event_year = 2030L
  )

  result <- apply_acute_profit_shock(yearly_trajectories, assets_factors, acute_events)

  # Profit should be negative: 120 - 250 = -130
  expected_profit <- 120 - (1.0 * 0.5 * 1.0 * 500)
  testthat::expect_equal(result$profit[result$year == 2030], expected_profit, tolerance = 0.1)
  testthat::expect_true(result$profit[result$year == 2030] < 0)
})

testthat::test_that("apply_acute_profit_shock handles Fire with default land_cover_risk 0.50 for assets without coordinates", {
  yearly_trajectories <- data.frame(
    asset = c("COM1", "COM1"),
    company = c("C1", "C1"),
    year = c(2025, 2030),
    revenue = c(1000, 1200),
    profit = c(100, 120)
  )

  # Asset without coordinates uses default land_cover_risk = 0.50
  assets_factors <- data.frame(
    asset = "COM1",
    hazard_type = "Fire",
    event_id = "event_1",
    land_cover_risk = 0.5, # Default for assets without coordinates
    damage_factor = 0.15,
    days_danger_total = 30,
    cost_factor = 200,
    asset_category = "commercial building"
  )

  acute_events <- data.frame(
    event_id = "event_1",
    hazard_type = "Fire",
    event_year = 2030L
  )

  result <- apply_acute_profit_shock(yearly_trajectories, assets_factors, acute_events)

  expected_profit <- 120 - (0.5 * 0.15 * (30 / 365) * 200)
  testthat::expect_equal(result$profit[result$year == 2030], expected_profit, tolerance = 0.1)
})
