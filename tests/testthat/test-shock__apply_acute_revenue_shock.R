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
    business_disruption = c(10, 20)
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
    event_id = "event_1"
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

testthat::test_that("apply_acute_revenue_shock handles mixed FloodTIF and Compound events", {
  yearly_baseline <- data.frame(
    asset = c("A1", "A1"),
    company = c("C1", "C1"),
    year = c(2025, 2030),
    revenue = c(1000, 1200)
  )

  assets_factors <- data.frame(
    asset = c("A1", "A1"),
    hazard_type = c("FloodTIF", "Compound"),
    event_id = c("event_1", "event_2"),
    business_disruption = c(10, NA),
    hazard_intensity = c(NA, 50),
    damage_factor = c(NA, -0.042)
  )

  # FloodTIF event first, then Compound
  acute_events <- data.frame(
    event_id = c("event_1", "event_2"),
    hazard_type = c("FloodTIF", "Compound"),
    event_year = c(2030L, 2030L),
    chronic = c(FALSE, FALSE),
    stringsAsFactors = FALSE
  )

  result <- apply_acute_revenue_shock(yearly_baseline, assets_factors, acute_events)

  # Both shocks should compound
  # First: FloodTIF reduces by 10 days: 1200 * (1 - 10/365) = 1167.12
  # Then: Compound applies to already-reduced revenue
  testthat::expect_true(result$revenue[result$year == 2030] < 1200)
  testthat::expect_true(result$revenue[result$year == 2030] < 1167.12)  # Further reduced by Compound
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
    business_disruption = c(10, 20)
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

testthat::test_that("apply_acute_revenue_shock returns unchanged revenue when no matching events", {
  yearly_trajectories <- data.frame(
    asset = c("A1", "A1"),
    company = c("C1", "C1"),
    year = c(2025, 2030),
    revenue = c(1000, 1200)
  )

  assets_factors <- data.frame(
    asset = "A1",
    hazard_type = "FloodTIF",
    event_id = "event_1",
    business_disruption = 10
  )

  # Event in different year
  acute_events <- data.frame(
    event_id = "event_1",
    hazard_type = "FloodTIF",
    event_year = 2035L,
    chronic = FALSE
  )

  result <- apply_acute_revenue_shock(yearly_trajectories, assets_factors, acute_events)

  # No matching year, so revenue should be unchanged
  testthat::expect_equal(result$revenue, yearly_trajectories$revenue)
})
