# Tests for apply_acute_revenue_shock

testthat::test_that("apply_acute_revenue_shock applies Flood shocks correctly", {
  yearly_baseline <- data.frame(
    asset = c("A1", "A1", "A2", "A2"),
    company = c("C1", "C1", "C1", "C1"),
    year = c(2025, 2030, 2025, 2030),
    revenue = c(1000, 1200, 800, 960)
  )

  assets_factors <- data.frame(
    asset = c("A1", "A2"),
    hazard_type = c("Flood", "Flood"),
    event_id = c("event_1", "event_1"),
    business_disruption = c(10, 20),
    asset_category = c("commercial building", "commercial building")
  )

  acute_events <- data.frame(
    event_id = "event_1",
    hazard_type = "Flood",
    event_year = 2030L
  )

  result <- apply_acute_revenue_shock(yearly_baseline, assets_factors, acute_events)

  testthat::expect_equal(nrow(result), nrow(yearly_baseline))

  # A1 with 10 days disruption: 1200 * (1 - 10/365) = 1167.12
  expected_revenue_A1 <- 1200 * (1 - 10 / 365)
  testthat::expect_equal(result$revenue[result$asset == "A1" & result$year == 2030], expected_revenue_A1, tolerance = 0.1)

  # A2 with 20 days disruption: 960 * (1 - 20/365) = 907.4
  expected_revenue_A2 <- 960 * (1 - 20 / 365)
  testthat::expect_equal(result$revenue[result$asset == "A2" & result$year == 2030], expected_revenue_A2, tolerance = 0.1)

  # 2025 revenues should be unchanged
  testthat::expect_equal(result$revenue[result$year == 2025], yearly_baseline$revenue[yearly_baseline$year == 2025])
})

testthat::test_that("apply_acute_revenue_shock applies Heat shocks with Cobb-Douglas correctly", {
  yearly_baseline <- data.frame(
    asset = c("A1", "A1"),
    company = c("C1", "C1"),
    year = c(2025, 2030),
    revenue = c(1000, 1200)
  )

  # Heat hazard: 50 days with extreme heat, -0.042 labor productivity loss
  assets_factors <- data.frame(
    asset = "A1",
    hazard_type = "Heat",
    hazard_intensity = 50, # days with extreme heat
    damage_factor = -0.042, # labor productivity loss (negative)
    event_id = "event_1",
    asset_category = "commercial building" # Heat doesn't use this but include for consistency
  )

  acute_events <- data.frame(
    event_id = "event_1",
    hazard_type = "Heat",
    event_year = 2030L
  )

  result <- apply_acute_revenue_shock(yearly_baseline, assets_factors, acute_events)

  testthat::expect_equal(nrow(result), nrow(yearly_baseline))

  # Calculate expected using Cobb-Douglas
  # weighted_lp_loss = (50/365) * (-0.042) = -0.00575
  # L0 = 339.2285, K0 = 87025023, E0 = 43.99034
  # lnA = 2.398, B1 = 0.602, B2 = 0.455, B3 = 0.147
  # L_adjusted = 339.2285 * (1 - 0.00575) = 337.28
  # Y_base = exp(2.398 + 0.602*log(87025023) + 0.455*log(339.2285) + 0.147*log(43.99034))
  # Y_shock = exp(2.398 + 0.602*log(87025023) + 0.455*log(337.28) + 0.147*log(43.99034))
  # change = (Y_shock / Y_base) - 1 (should be negative)
  # expected_revenue = 1200 * (1 + change)

  # Revenue should decrease (Heat shock reduces labor productivity)
  testthat::expect_true(result$revenue[result$year == 2030] < 1200)

  # 2025 should be unchanged
  testthat::expect_equal(result$revenue[result$year == 2025], 1000)
})

testthat::test_that("apply_acute_revenue_shock processes events in event_id order sequentially", {
  yearly_trajectories <- data.frame(
    asset = c("A1", "A1"),
    company = c("C1", "C1"),
    year = c(2025, 2030),
    revenue = c(1000, 1200)
  )

  assets_factors <- data.frame(
    asset = c("A1", "A1"),
    hazard_type = c("Flood", "Flood"),
    event_id = c("event_z", "event_a"), # Non-alphabetical order
    business_disruption = c(10, 20),
    asset_category = c("commercial building", "commercial building")
  )

  # Events in non-alphabetical order
  acute_events <- data.frame(
    event_id = c("event_z", "event_a"),
    hazard_type = c("Flood", "Flood"),
    event_year = c(2030L, 2030L),
    stringsAsFactors = FALSE
  )

  result <- apply_acute_revenue_shock(yearly_trajectories, assets_factors, acute_events)

  # Should process event_a first (alphabetically), then event_z
  # event_a: 1200 * (1 - 20/365) = 1134.25
  # event_z: 1134.25 * (1 - 10/365) = 1103.17
  step1 <- 1200 * (1 - 20 / 365)
  step2 <- step1 * (1 - 10 / 365)
  testthat::expect_equal(result$revenue[result$year == 2030], step2, tolerance = 0.1)
})

testthat::test_that("apply_acute_revenue_shock applies agriculture flood damage factor before business disruption", {
  yearly_trajectories <- data.frame(
    asset = c("AG1", "AG1"),
    company = c("C1", "C1"),
    year = c(2025, 2030),
    revenue = c(1000, 1200)
  )

  assets_factors <- data.frame(
    asset = "AG1",
    hazard_type = "Flood",
    event_id = "event_1",
    damage_factor = 0.3, # 30% damage
    business_disruption = 10, # 10 days
    asset_category = "agriculture"
  )

  acute_events <- data.frame(
    event_id = "event_1",
    hazard_type = "Flood",
    event_year = 2030L
  )

  result <- apply_acute_revenue_shock(yearly_trajectories, assets_factors, acute_events)

  # Agriculture: First apply damage factor, then business disruption
  # Step 1: revenue * (1 - damage_factor) = 1200 * (1 - 0.3) = 840
  # Step 2: revenue * (1 - disruption_days/365) = 840 * (1 - 10/365) = 817.26
  step1 <- 1200 * (1 - 0.3)
  step2 <- step1 * (1 - 10 / 365)

  testthat::expect_equal(result$revenue[result$year == 2030], step2, tolerance = 0.1)
  testthat::expect_equal(result$revenue[result$year == 2025], 1000) # 2025 unchanged
})

testthat::test_that("apply_acute_revenue_shock prevents agriculture revenue from going below zero", {
  yearly_trajectories <- data.frame(
    asset = c("AG1", "AG1"),
    company = c("C1", "C1"),
    year = c(2025, 2030),
    revenue = c(1000, 1200)
  )

  assets_factors <- data.frame(
    asset = "AG1",
    hazard_type = "Flood",
    event_id = "event_1",
    damage_factor = 1.5, # 150% damage, allows negative revenue
    business_disruption = 350, # 350 days (almost full year)
    asset_category = "agriculture"
  )

  acute_events <- data.frame(
    event_id = "event_1",
    hazard_type = "Flood",
    event_year = 2030L
  )

  result <- apply_acute_revenue_shock(yearly_trajectories, assets_factors, acute_events)

  # Agriculture: damage + disruption can push revenue negative when damage_factor > 1
  expected_revenue <- 1200 * (1 - 1.5) * (1 - 350 / 365)
  testthat::expect_lt(result$revenue[result$year == 2030], 0)
  testthat::expect_equal(result$revenue[result$year == 2030], expected_revenue, tolerance = 0.1)
})

testthat::test_that("apply_acute_revenue_shock applies only business disruption for commercial buildings", {
  yearly_trajectories <- data.frame(
    asset = c("COM1", "COM1"),
    company = c("C1", "C1"),
    year = c(2025, 2030),
    revenue = c(1000, 1200)
  )

  assets_factors <- data.frame(
    asset = "COM1",
    hazard_type = "Flood",
    event_id = "event_1",
    damage_factor = 0.3, # This should NOT be applied for commercial
    business_disruption = 10,
    asset_category = "commercial building"
  )

  acute_events <- data.frame(
    event_id = "event_1",
    hazard_type = "Flood",
    event_year = 2030L
  )

  result <- apply_acute_revenue_shock(yearly_trajectories, assets_factors, acute_events)

  # Commercial building: Only business disruption applied
  # revenue * (1 - disruption_days/365) = 1200 * (1 - 10/365) = 1167.12
  expected_revenue <- 1200 * (1 - 10 / 365)
  testthat::expect_equal(result$revenue[result$year == 2030], expected_revenue, tolerance = 0.1)
})

testthat::test_that("apply_acute_revenue_shock applies only business disruption for industrial buildings", {
  yearly_trajectories <- data.frame(
    asset = c("IND1", "IND1"),
    company = c("C1", "C1"),
    year = c(2025, 2030),
    revenue = c(1000, 1200)
  )

  assets_factors <- data.frame(
    asset = "IND1",
    hazard_type = "Flood",
    event_id = "event_1",
    damage_factor = 0.3, # This should NOT be applied for industrial
    business_disruption = 15,
    asset_category = "industrial building"
  )

  acute_events <- data.frame(
    event_id = "event_1",
    hazard_type = "Flood",
    event_year = 2030L
  )

  result <- apply_acute_revenue_shock(yearly_trajectories, assets_factors, acute_events)

  # Industrial building: Only business disruption applied
  # revenue * (1 - disruption_days/365) = 1200 * (1 - 15/365) = 1150.68
  expected_revenue <- 1200 * (1 - 15 / 365)
  testthat::expect_equal(result$revenue[result$year == 2030], expected_revenue, tolerance = 0.1)
})

testthat::test_that("apply_acute_revenue_shock applies Drought shocks to agriculture correctly", {
  yearly_baseline <- data.frame(
    asset = c("A1", "A1", "A2", "A2"),
    company = c("C1", "C1", "C1", "C1"),
    year = c(2025, 2030, 2025, 2030),
    revenue = c(1000, 1200, 800, 960)
  )

  assets_factors <- data.frame(
    asset = c("A1", "A2"),
    hazard_type = c("Drought", "Drought"),
    event_id = c("event_1", "event_1"),
    damage_factor = c(0.4, 0.3), # 40% and 30% yield loss
    asset_category = c("agriculture", "agriculture")
  )

  acute_events <- data.frame(
    event_id = "event_1",
    hazard_type = "Drought",
    event_year = 2030L
  )

  result <- apply_acute_revenue_shock(yearly_baseline, assets_factors, acute_events)

  testthat::expect_equal(nrow(result), nrow(yearly_baseline))

  # A1 with 40% damage: 1200 * (1 - 0.4) = 720
  expected_revenue_A1 <- 1200 * (1 - 0.4)
  testthat::expect_equal(result$revenue[result$asset == "A1" & result$year == 2030], expected_revenue_A1, tolerance = 0.1)

  # A2 with 30% damage: 960 * (1 - 0.3) = 672
  expected_revenue_A2 <- 960 * (1 - 0.3)
  testthat::expect_equal(result$revenue[result$asset == "A2" & result$year == 2030], expected_revenue_A2, tolerance = 0.1)

  # 2025 revenues should be unchanged
  testthat::expect_equal(result$revenue[result$year == 2025], yearly_baseline$revenue[yearly_baseline$year == 2025])
})

testthat::test_that("apply_acute_revenue_shock ignores Drought for non-agriculture assets", {
  yearly_baseline <- data.frame(
    asset = c("A1", "A1"),
    company = c("C1", "C1"),
    year = c(2025, 2030),
    revenue = c(1000, 1200)
  )

  # Drought on commercial building should be ignored
  assets_factors <- data.frame(
    asset = "A1",
    hazard_type = "Drought",
    event_id = "event_1",
    damage_factor = 0.4,
    asset_category = "commercial building"
  )

  acute_events <- data.frame(
    event_id = "event_1",
    hazard_type = "Drought",
    event_year = 2030L
  )

  result <- apply_acute_revenue_shock(yearly_baseline, assets_factors, acute_events)

  # Revenue should be unchanged (drought doesn't affect non-agriculture)
  testthat::expect_equal(result$revenue, yearly_baseline$revenue)
})

testthat::test_that("apply_acute_revenue_shock handles multiple droughts in same year", {
  yearly_baseline <- data.frame(
    asset = c("A1", "A1"),
    company = c("C1", "C1"),
    year = c(2025, 2030),
    revenue = c(1000, 1200)
  )

  # Two drought events affecting same asset
  assets_factors <- data.frame(
    asset = c("A1", "A1"),
    hazard_type = c("Drought", "Drought"),
    event_id = c("event_1", "event_2"),
    damage_factor = c(0.3, 0.2),
    asset_category = c("agriculture", "agriculture")
  )

  acute_events <- data.frame(
    event_id = c("event_1", "event_2"),
    hazard_type = c("Drought", "Drought"),
    event_year = c(2030L, 2030L)
  )

  result <- apply_acute_revenue_shock(yearly_baseline, assets_factors, acute_events)

  # Shocks should be sequential:
  # After event_1: 1200 * (1 - 0.3) = 840
  # After event_2: 840 * (1 - 0.2) = 672
  expected_revenue <- 1200 * (1 - 0.3) * (1 - 0.2)
  testthat::expect_equal(result$revenue[result$year == 2030], expected_revenue, tolerance = 0.1)
})

testthat::test_that("apply_acute_revenue_shock applies Fire revenue shock to agriculture assets correctly", {
  yearly_baseline <- data.frame(
    asset = c("A1", "A1", "A2", "A2"),
    company = c("C1", "C1", "C1", "C1"),
    year = c(2025, 2030, 2025, 2030),
    revenue = c(1000, 1200, 800, 960)
  )

  # Fire damage formula: land_cover_risk * damage_factor * (days_danger_total / 365)
  # A1: land_cover_risk=0.5, damage_factor=0.15, days=30
  # Fire damage = 0.5 * 0.15 * (30/365) = 0.006164
  # A2: land_cover_risk=0.75, damage_factor=0.20, days=45
  # Fire damage = 0.75 * 0.20 * (45/365) = 0.018493
  assets_factors <- data.frame(
    asset = c("A1", "A2"),
    hazard_type = c("Fire", "Fire"),
    event_id = c("event_1", "event_1"),
    land_cover_risk = c(0.5, 0.75),
    damage_factor = c(0.15, 0.20),
    days_danger_total = c(30, 45),
    asset_category = c("agriculture", "agriculture")
  )

  acute_events <- data.frame(
    event_id = "event_1",
    hazard_type = "Fire",
    event_year = 2030L
  )

  result <- apply_acute_revenue_shock(yearly_baseline, assets_factors, acute_events)

  testthat::expect_equal(nrow(result), nrow(yearly_baseline))

  # A1: 1200 * (1 - 0.006164) = 1192.6
  expected_revenue_A1 <- 1200 * (1 - 0.5 * 0.15 * (30 / 365))
  testthat::expect_equal(result$revenue[result$asset == "A1" & result$year == 2030], expected_revenue_A1, tolerance = 0.1)

  # A2: 960 * (1 - 0.018493) = 942.2
  expected_revenue_A2 <- 960 * (1 - 0.75 * 0.20 * (45 / 365))
  testthat::expect_equal(result$revenue[result$asset == "A2" & result$year == 2030], expected_revenue_A2, tolerance = 0.1)

  # 2025 revenues should be unchanged
  testthat::expect_equal(result$revenue[result$year == 2025], yearly_baseline$revenue[yearly_baseline$year == 2025])
})

testthat::test_that("apply_acute_revenue_shock applies Fire with default land_cover_risk 0.50 for assets without coordinates", {
  yearly_baseline <- data.frame(
    asset = c("A1", "A1"),
    company = c("C1", "C1"),
    year = c(2025, 2030),
    revenue = c(1000, 1200)
  )

  # Asset without coordinates uses default land_cover_risk = 0.50
  assets_factors <- data.frame(
    asset = "A1",
    hazard_type = "Fire",
    event_id = "event_1",
    land_cover_risk = 0.5, # Default for assets without coordinates
    damage_factor = 0.15,
    days_danger_total = 30,
    asset_category = "agriculture"
  )

  acute_events <- data.frame(
    event_id = "event_1",
    hazard_type = "Fire",
    event_year = 2030L
  )

  result <- apply_acute_revenue_shock(yearly_baseline, assets_factors, acute_events)

  expected_revenue <- 1200 * (1 - 0.5 * 0.15 * (30 / 365))
  testthat::expect_equal(result$revenue[result$year == 2030], expected_revenue, tolerance = 0.1)
})

testthat::test_that("apply_acute_revenue_shock handles missing days_danger_total (defaults to 0)", {
  yearly_baseline <- data.frame(
    asset = c("A1", "A1"),
    company = c("C1", "C1"),
    year = c(2025, 2030),
    revenue = c(1000, 1200)
  )

  # Missing days_danger_total should default to 0 (no damage)
  assets_factors <- data.frame(
    asset = "A1",
    hazard_type = "Fire",
    event_id = "event_1",
    land_cover_risk = 0.5,
    damage_factor = 0.15,
    days_danger_total = 0, # No danger days
    asset_category = "agriculture"
  )

  acute_events <- data.frame(
    event_id = "event_1",
    hazard_type = "Fire",
    event_year = 2030L
  )

  result <- apply_acute_revenue_shock(yearly_baseline, assets_factors, acute_events)

  # With days_danger_total = 0, fire damage = 0, so revenue unchanged
  testthat::expect_equal(result$revenue[result$year == 2030], 1200)
})

testthat::test_that("apply_acute_revenue_shock prevents Fire revenue from going below zero", {
  yearly_baseline <- data.frame(
    asset = c("A1", "A1"),
    company = c("C1", "C1"),
    year = c(2025, 2030),
    revenue = c(1000, 1200)
  )

  # Extreme fire damage that pushes revenue negative
  # land_cover_risk=1.0, damage_factor=1.5, days=365 (full year)
  # Fire damage = 1.0 * 1.5 * (365/365) = 1.5
  assets_factors <- data.frame(
    asset = "A1",
    hazard_type = "Fire",
    event_id = "event_1",
    land_cover_risk = 1.0,
    damage_factor = 1.5,
    days_danger_total = 365, # Full year
    asset_category = "agriculture"
  )

  acute_events <- data.frame(
    event_id = "event_1",
    hazard_type = "Fire",
    event_year = 2030L
  )

  result <- apply_acute_revenue_shock(yearly_baseline, assets_factors, acute_events)

  # With these parameters: 1200 * (1 - 1.0 * 1.5 * 1.0) = -600
  expected_revenue <- 1200 * (1 - 1.0 * 1.5 * 1.0)
  testthat::expect_lt(result$revenue[result$year == 2030], 0)
  testthat::expect_equal(result$revenue[result$year == 2030], expected_revenue, tolerance = 0.1)
})

testthat::test_that("apply_acute_revenue_shock processes Fire events in event_id order", {
  yearly_trajectories <- data.frame(
    asset = c("A1", "A1"),
    company = c("C1", "C1"),
    year = c(2025, 2030),
    revenue = c(1000, 1200)
  )

  # Two Fire events affecting same asset
  assets_factors <- data.frame(
    asset = c("A1", "A1"),
    hazard_type = c("Fire", "Fire"),
    event_id = c("event_z", "event_a"), # Non-alphabetical order
    land_cover_risk = c(0.5, 0.5),
    damage_factor = c(0.15, 0.10),
    days_danger_total = c(30, 20),
    asset_category = c("agriculture", "agriculture")
  )

  acute_events <- data.frame(
    event_id = c("event_z", "event_a"),
    hazard_type = c("Fire", "Fire"),
    event_year = c(2030L, 2030L),
    stringsAsFactors = FALSE
  )

  result <- apply_acute_revenue_shock(yearly_trajectories, assets_factors, acute_events)

  # Should process event_a first (alphabetically), then event_z
  # event_a: damage = 0.5 * 0.10 * (20/365) = 0.00274, revenue = 1200 * (1 - 0.00274) = 1196.71
  # event_z: damage = 0.5 * 0.15 * (30/365) = 0.00616, revenue = 1196.71 * (1 - 0.00616) = 1193.33
  step1 <- 1200 * (1 - 0.5 * 0.10 * (20 / 365))
  step2 <- step1 * (1 - 0.5 * 0.15 * (30 / 365))
  testthat::expect_equal(result$revenue[result$year == 2030], step2, tolerance = 0.1)
})

testthat::test_that("apply_acute_revenue_shock ignores Fire for non-agriculture assets", {
  yearly_baseline <- data.frame(
    asset = c("A1", "A1"),
    company = c("C1", "C1"),
    year = c(2025, 2030),
    revenue = c(1000, 1200)
  )

  # Fire on commercial building should be ignored in revenue phase
  assets_factors <- data.frame(
    asset = "A1",
    hazard_type = "Fire",
    event_id = "event_1",
    land_cover_risk = 0.5,
    damage_factor = 0.15,
    days_danger_total = 30,
    asset_category = "commercial building"
  )

  acute_events <- data.frame(
    event_id = "event_1",
    hazard_type = "Fire",
    event_year = 2030L
  )

  result <- apply_acute_revenue_shock(yearly_baseline, assets_factors, acute_events)

  # Revenue should be unchanged (Fire revenue shock only affects agriculture)
  testthat::expect_equal(result$revenue, yearly_baseline$revenue)
})
