# Tests for apply_acute_revenue_shock

testthat::test_that("apply_acute_revenue_shock applies FloodTIF shocks correctly", {
  yearly_baseline <- data.frame(
    asset = c("A1", "A1", "A2", "A2"),
    company = c("C1", "C1", "C1", "C1"),
    year = c(2025, 2030, 2025, 2030),
    revenue = c(1000, 1200, 800, 960)
  )

  assets_factors <- data.frame(
    asset = c("A1", "A2"),
    hazard_type = c("FloodTIF", "FloodTIF"),
    event_id = c("event_1", "event_1"),
    business_disruption = c(10, 20),
    asset_category = c("commercial building", "commercial building")
  )

  acute_events <- data.frame(
    event_id = "event_1",
    hazard_type = "FloodTIF",
    event_year = 2030L,
    chronic = FALSE
  )

  result <- apply_acute_revenue_shock(yearly_baseline, assets_factors, acute_events)

  testthat::expect_equal(nrow(result), nrow(yearly_baseline))
  
  # A1 with 10 days disruption: 1200 * (1 - 10/365) = 1167.12
  expected_revenue_A1 <- 1200 * (1 - 10/365)
  testthat::expect_equal(result$revenue[result$asset == "A1" & result$year == 2030], expected_revenue_A1, tolerance = 0.1)
  
  # A2 with 20 days disruption: 960 * (1 - 20/365) = 907.4
  expected_revenue_A2 <- 960 * (1 - 20/365)
  testthat::expect_equal(result$revenue[result$asset == "A2" & result$year == 2030], expected_revenue_A2, tolerance = 0.1)
  
  # 2025 revenues should be unchanged
  testthat::expect_equal(result$revenue[result$year == 2025], yearly_baseline$revenue[yearly_baseline$year == 2025])
})

testthat::test_that("apply_acute_revenue_shock applies Compound shocks with Cobb-Douglas correctly", {
  yearly_baseline <- data.frame(
    asset = c("A1", "A1"),
    company = c("C1", "C1"),
    year = c(2025, 2030),
    revenue = c(1000, 1200)
  )

  # Compound hazard: 50 days with extreme heat, -0.042 labor productivity loss
  assets_factors <- data.frame(
    asset = "A1",
    hazard_type = "Compound",
    hazard_intensity = 50,  # days with extreme heat
    damage_factor = -0.042,  # labor productivity loss (negative)
    event_id = "event_1",
    asset_category = "commercial building"  # Compound doesn't use this but include for consistency
  )

  acute_events <- data.frame(
    event_id = "event_1",
    hazard_type = "Compound",
    event_year = 2030L,
    chronic = FALSE
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
  
  # Revenue should decrease (Compound shock reduces labor productivity)
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
    hazard_type = c("FloodTIF", "FloodTIF"),
    event_id = c("event_z", "event_a"),  # Non-alphabetical order
    business_disruption = c(10, 20),
    asset_category = c("commercial building", "commercial building")
  )

  # Events in non-alphabetical order
  acute_events <- data.frame(
    event_id = c("event_z", "event_a"),
    hazard_type = c("FloodTIF", "FloodTIF"),
    event_year = c(2030L, 2030L),
    chronic = c(FALSE, FALSE),
    stringsAsFactors = FALSE
  )

  result <- apply_acute_revenue_shock(yearly_trajectories, assets_factors, acute_events)

  # Should process event_a first (alphabetically), then event_z
  # event_a: 1200 * (1 - 20/365) = 1134.25
  # event_z: 1134.25 * (1 - 10/365) = 1103.17
  step1 <- 1200 * (1 - 20/365)
  step2 <- step1 * (1 - 10/365)
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
    hazard_type = "FloodTIF",
    event_id = "event_1",
    damage_factor = 0.3,  # 30% damage
    business_disruption = 10,  # 10 days
    asset_category = "agriculture"
  )

  acute_events <- data.frame(
    event_id = "event_1",
    hazard_type = "FloodTIF",
    event_year = 2030L,
    chronic = FALSE
  )

  result <- apply_acute_revenue_shock(yearly_trajectories, assets_factors, acute_events)

  # Agriculture: First apply damage factor, then business disruption
  # Step 1: revenue * (1 - damage_factor) = 1200 * (1 - 0.3) = 840
  # Step 2: revenue * (1 - disruption_days/365) = 840 * (1 - 10/365) = 817.26
  step1 <- 1200 * (1 - 0.3)
  step2 <- step1 * (1 - 10/365)
  
  testthat::expect_equal(result$revenue[result$year == 2030], step2, tolerance = 0.1)
  testthat::expect_equal(result$revenue[result$year == 2025], 1000)  # 2025 unchanged
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
    hazard_type = "FloodTIF",
    event_id = "event_1",
    damage_factor = 0.95,  # 95% damage
    business_disruption = 350,  # 350 days (almost full year)
    asset_category = "agriculture"
  )

  acute_events <- data.frame(
    event_id = "event_1",
    hazard_type = "FloodTIF",
    event_year = 2030L,
    chronic = FALSE
  )

  result <- apply_acute_revenue_shock(yearly_trajectories, assets_factors, acute_events)

  # Agriculture: damage + disruption should not go below 0
  # Step 1: 1200 * (1 - 0.95) = 60
  # Step 2: 60 * (1 - 350/365) = 2.47
  # But if it were negative, should be capped at 0
  testthat::expect_true(result$revenue[result$year == 2030] >= 0)
  testthat::expect_true(result$revenue[result$year == 2030] < 100)  # Should be very small
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
    hazard_type = "FloodTIF",
    event_id = "event_1",
    damage_factor = 0.3,  # This should NOT be applied for commercial
    business_disruption = 10,
    asset_category = "commercial building"
  )

  acute_events <- data.frame(
    event_id = "event_1",
    hazard_type = "FloodTIF",
    event_year = 2030L,
    chronic = FALSE
  )

  result <- apply_acute_revenue_shock(yearly_trajectories, assets_factors, acute_events)

  # Commercial building: Only business disruption applied
  # revenue * (1 - disruption_days/365) = 1200 * (1 - 10/365) = 1167.12
  expected_revenue <- 1200 * (1 - 10/365)
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
    hazard_type = "FloodTIF",
    event_id = "event_1",
    damage_factor = 0.3,  # This should NOT be applied for industrial
    business_disruption = 15,
    asset_category = "industrial building"
  )

  acute_events <- data.frame(
    event_id = "event_1",
    hazard_type = "FloodTIF",
    event_year = 2030L,
    chronic = FALSE
  )

  result <- apply_acute_revenue_shock(yearly_trajectories, assets_factors, acute_events)

  # Industrial building: Only business disruption applied
  # revenue * (1 - disruption_days/365) = 1200 * (1 - 15/365) = 1150.68
  expected_revenue <- 1200 * (1 - 15/365)
  testthat::expect_equal(result$revenue[result$year == 2030], expected_revenue, tolerance = 0.1)
})
