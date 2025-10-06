# Tests for apply_chronic_revenue_shock

testthat::test_that("apply_chronic_revenue_shock applies shocks to yearly trajectories", {
  yearly_baseline <- data.frame(
    asset = c("A1", "A1", "A2", "A2"),
    company = c("C1", "C1", "C1", "C1"),
    year = c(2025, 2030, 2025, 2030),
    revenue = c(1000, 1200, 800, 960),
    profit = c(100, 120, 80, 96)
  )

  assets_factors <- data.frame(
    asset = c("A1", "A2"),
    hazard_name = c("flood__global_rcp85_h100glob_brazil", "flood__global_rcp85_h100glob_brazil"),
    damage_factor = c(35, 40),
    asset_category = c("Industrial", "Industrial")
  )

  chronic_events <- data.frame(
    event_id = "e1",
    hazard_type = "flood",
    hazard_name = "flood__global_rcp85_h100glob_brazil",
    event_year = NA_integer_,
    chronic = TRUE
  )

  result <- apply_chronic_revenue_shock(yearly_baseline, assets_factors, chronic_events)

  # Should return shocked_revenue column only (NOT profit - that's computed separately)
  expected_cols <- c("asset", "company", "year", "revenue")
  testthat::expect_true(all(expected_cols %in% names(result)))
  testthat::expect_equal(nrow(result), nrow(yearly_baseline))

  # Since this is a placeholder, values should be copied from baseline
  testthat::expect_equal(result$revenue, yearly_baseline$revenue)
  
})

testthat::test_that("apply_chronic_revenue_shock works with already shocked trajectories", {
  # Test with trajectories that already have shocked values
  yearly_shocked <- data.frame(
    asset = c("A1", "A1"),
    company = c("C1", "C1"),
    year = c(2025, 2030),
    revenue = c(900, 1080)
  )

  assets_factors <- data.frame(
    asset = "A1",
    hazard_name = "flood__global_rcp85_h100glob_brazil",
    damage_factor = 35
  )

  chronic_events <- data.frame(
    event_id = "e1",
    hazard_type = "flood",
    hazard_name = "flood__global_rcp85_h100glob_brazil",
    event_year = NA_integer_,
    chronic = TRUE
  )

  result <- apply_chronic_revenue_shock(yearly_shocked, assets_factors, chronic_events)

  # Should preserve shocked_revenue values (placeholder behavior)
  testthat::expect_equal(result$revenue, yearly_shocked$revenue)
  
})

