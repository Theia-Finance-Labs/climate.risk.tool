# Tests for apply_acute_profit_shock

testthat::test_that("apply_acute_profit_shock passes through as placeholder", {
  # Placeholder implementation: just passes through profit unchanged
  yearly_trajectories <- data.frame(
    asset = c("A1", "A1", "A2", "A2"),
    company = c("C1", "C1", "C1", "C1"),
    year = c(2025, 2030, 2025, 2030),
    shocked_revenue = c(1000, 1200, 800, 960),
    shocked_profit = c(100, 120, 80, 96)
  )

  assets_factors <- data.frame(
    asset = c("A1", "A2"),
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

  # Should return shocked_profit column
  expected_cols <- c("asset", "company", "year", "shocked_revenue", "shocked_profit")
  testthat::expect_true(all(expected_cols %in% names(result)))
  testthat::expect_equal(nrow(result), nrow(yearly_trajectories))

  # Placeholder: should pass through unchanged for now
  testthat::expect_equal(result$shocked_profit, yearly_trajectories$shocked_profit)
  testthat::expect_equal(result$shocked_revenue, yearly_trajectories$shocked_revenue)
})

testthat::test_that("apply_acute_profit_shock handles missing columns gracefully", {
  # Test with only revenue (no profit yet)
  yearly_trajectories <- data.frame(
    asset = c("A1"),
    company = c("C1"),
    year = c(2025),
    shocked_revenue = c(1000)
  )

  assets_factors <- data.frame(
    asset = "A1",
    hazard_name = "flood__global_rcp85_h100glob_brazil",
    cost_factor = 100,
    asset_category = "Industrial"
  )

  acute_events <- data.frame(
    event_id = "e1",
    hazard_type = "flood",
    hazard_name = "flood__global_rcp85_h100glob_brazil",
    event_year = 2030L,
    chronic = FALSE
  )

  # Should error if shocked_profit column is missing
  testthat::expect_error(
    apply_acute_profit_shock(yearly_trajectories, assets_factors, acute_events),
    "shocked_profit"
  )
})

testthat::test_that("apply_acute_profit_shock validates inputs", {
  valid_trajectories <- data.frame(
    asset = "A1",
    company = "C1",
    year = 2025,
    shocked_revenue = 1000,
    shocked_profit = 100
  )

  valid_assets <- data.frame(
    asset = "A1",
    hazard_name = "flood__global_rcp85_h100glob_brazil",
    cost_factor = 100
  )

  valid_events <- data.frame(
    event_id = "e1",
    hazard_type = "flood",
    hazard_name = "flood__global_rcp85_h100glob_brazil",
    event_year = 2030L,
    chronic = FALSE
  )

  # Should error with NULL inputs
  testthat::expect_error(
    apply_acute_profit_shock(NULL, valid_assets, valid_events),
    "yearly_trajectories must be a non-empty data.frame"
  )

  testthat::expect_error(
    apply_acute_profit_shock(valid_trajectories, NULL, valid_events),
    "assets_factors must be a non-empty data.frame"
  )

  testthat::expect_error(
    apply_acute_profit_shock(valid_trajectories, valid_assets, NULL),
    "acute_events must be a non-empty data.frame"
  )
})

testthat::test_that("apply_acute_profit_shock ensures non-negative profits", {
  yearly_trajectories <- data.frame(
    asset = c("A1"),
    company = c("C1"),
    year = c(2025),
    shocked_revenue = c(1000),
    shocked_profit = c(-50)  # Negative profit
  )

  assets_factors <- data.frame(
    asset = "A1",
    hazard_name = "flood__global_rcp85_h100glob_brazil",
    cost_factor = 100
  )

  acute_events <- data.frame(
    event_id = "e1",
    hazard_type = "flood",
    hazard_name = "flood__global_rcp85_h100glob_brazil",
    event_year = 2030L,
    chronic = FALSE
  )

  result <- apply_acute_profit_shock(yearly_trajectories, assets_factors, acute_events)

  # Should ensure non-negative
  testthat::expect_true(all(result$shocked_profit >= 0))
})

