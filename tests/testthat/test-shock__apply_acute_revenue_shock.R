# Tests for apply_acute_revenue_shock

testthat::test_that("apply_acute_revenue_shock applies shocks to yearly trajectories", {
  yearly_baseline <- data.frame(
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
    business_disruption = c(10, 20)
  )

  acute_events <- data.frame(
    event_id = "e1",
    hazard_type = "flood",
    hazard_name = "flood__global_rcp85_h100glob_brazil",
    event_year = 2030L,
    chronic = FALSE
  )

  result <- apply_acute_revenue_shock(yearly_baseline, assets_factors, acute_events)

  # Should keep all columns including profit (baseline profit carried through)
  expected_cols <- c("asset", "company", "year", "revenue", "profit")
  testthat::expect_true(all(expected_cols %in% names(result)))
  testthat::expect_equal(nrow(result), nrow(yearly_baseline))

  # Should keep profit column (baseline profit carried through to apply profit shocks later)
  testthat::expect_true("profit" %in% names(result))
})

testthat::test_that("apply_acute_revenue_shock processes events in order by event_id", {
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
    business_disruption = c(10, 20)
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

  result <- apply_acute_revenue_shock(yearly_trajectories, assets_factors, acute_events)

  # Should process events in alphabetical order by event_id
  # Both events affect the same asset-year (A1, 2030)
  # Total disruption should be: 20 + 10 = 30 days
  # Revenue reduction: 1200 * (1 - 30/365) = 1200 * (1 - 0.082) = 1200 * 0.918 = 1101.6
  expected_revenue_2030 <- 1200 * (1 - 30/365)
  testthat::expect_equal(result$revenue[result$asset == "A1" & result$year == 2030], expected_revenue_2030, tolerance = 0.1)
})
