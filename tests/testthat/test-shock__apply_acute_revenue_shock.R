# Tests for apply_acute_revenue_shock

testthat::test_that("apply_acute_revenue_shock applies shocks to yearly trajectories", {
  yearly_baseline <- data.frame(
    asset = c("A1", "A1", "A2", "A2"),
    company = c("C1", "C1", "C1", "C1"),
    year = c(2025, 2030, 2025, 2030),
    baseline_revenue = c(1000, 1200, 800, 960),
    baseline_profit = c(100, 120, 80, 96)
  )

  assets_factors <- data.frame(
    asset = c("A1", "A2"),
    hazard_mean_flood = c(5, 10)
  )

  acute_events <- data.frame(
    event_id = "e1",
    hazard_type = "flood",
    scenario = "rcp85",
    event_year = 2030L,
    chronic = FALSE
  )

  result <- apply_acute_revenue_shock(yearly_baseline, assets_factors, acute_events)

  # Should return shocked_revenue column only (NOT profit - that's computed separately)
  expected_cols <- c("asset", "company", "year", "shocked_revenue")
  testthat::expect_true(all(expected_cols %in% names(result)))
  testthat::expect_equal(nrow(result), nrow(yearly_baseline))

  # Since this is a placeholder, values should be copied from baseline
  testthat::expect_equal(result$shocked_revenue, yearly_baseline$baseline_revenue)
  
  # Should NOT have shocked_profit column (that's computed separately)
  testthat::expect_false("shocked_profit" %in% names(result))
})

testthat::test_that("apply_acute_revenue_shock works with already shocked trajectories", {
  # Test with trajectories that already have shocked values
  yearly_shocked <- data.frame(
    asset = c("A1", "A1"),
    company = c("C1", "C1"),
    year = c(2025, 2030),
    shocked_revenue = c(900, 1080)
  )

  assets_factors <- data.frame(
    asset = "A1",
    hazard_mean_flood = 5
  )

  acute_events <- data.frame(
    event_id = "e1",
    hazard_type = "flood",
    scenario = "rcp85",
    event_year = 2030L,
    chronic = FALSE
  )

  result <- apply_acute_revenue_shock(yearly_shocked, assets_factors, acute_events)

  # Should preserve shocked_revenue values (placeholder behavior)
  testthat::expect_equal(result$shocked_revenue, yearly_shocked$shocked_revenue)
  
  # Should NOT have shocked_profit column
  testthat::expect_false("shocked_profit" %in% names(result))
})

testthat::test_that("apply_acute_revenue_shock validates input parameters", {
  yearly_baseline <- data.frame(
    asset = "A1",
    company = "C1",
    year = 2025,
    baseline_revenue = 1000,
    baseline_profit = 100
  )

  assets_factors <- data.frame(
    asset = "A1",
    hazard_mean_flood = 5
  )

  acute_events <- data.frame(
    event_id = "e1",
    hazard_type = "flood",
    scenario = "rcp85",
    event_year = 2030L,
    chronic = FALSE
  )

  # Valid inputs should work
  testthat::expect_no_error(apply_acute_revenue_shock(yearly_baseline, assets_factors, acute_events))

  # Invalid trajectories should error
  testthat::expect_error(
    apply_acute_revenue_shock(NULL, assets_factors, acute_events),
    regexp = "data.frame"
  )

  testthat::expect_error(
    apply_acute_revenue_shock(data.frame(), assets_factors, acute_events),
    regexp = "non-empty"
  )

  # Invalid assets_factors should error
  testthat::expect_error(
    apply_acute_revenue_shock(yearly_baseline, NULL, acute_events),
    regexp = "data.frame"
  )

  # Invalid events should error
  testthat::expect_error(
    apply_acute_revenue_shock(yearly_baseline, assets_factors, NULL),
    regexp = "data.frame"
  )

  testthat::expect_error(
    apply_acute_revenue_shock(yearly_baseline, assets_factors, data.frame()),
    regexp = "non-empty"
  )
})

testthat::test_that("apply_acute_revenue_shock handles missing columns", {
  yearly_baseline <- data.frame(
    asset = "A1",
    company = "C1",
    year = 2025
    # Missing baseline_revenue or shocked_revenue
  )

  assets_factors <- data.frame(
    asset = "A1",
    hazard_mean_flood = 5
  )

  acute_events <- data.frame(
    event_id = "e1",
    hazard_type = "flood",
    scenario = "rcp85",
    event_year = 2030L,
    chronic = FALSE
  )

  testthat::expect_error(
    apply_acute_revenue_shock(yearly_baseline, assets_factors, acute_events),
    regexp = "baseline_revenue|shocked_revenue"
  )
})

testthat::test_that("apply_acute_revenue_shock ensures non-negative values", {
  yearly_baseline <- data.frame(
    asset = "A1",
    company = "C1",
    year = 2025,
    baseline_revenue = -100, # Negative value
    baseline_profit = -10
  )

  assets_factors <- data.frame(
    asset = "A1",
    hazard_mean_flood = 5
  )

  acute_events <- data.frame(
    event_id = "e1",
    hazard_type = "flood",
    scenario = "rcp85",
    event_year = 2030L,
    chronic = FALSE
  )

  result <- apply_acute_revenue_shock(yearly_baseline, assets_factors, acute_events)

  # Should ensure non-negative revenue values
  testthat::expect_true(all(result$shocked_revenue >= 0))
})
