# Tests for compute_shock_trajectories

testthat::test_that("compute_shock_trajectories returns concatenated baseline and shock scenarios", {
  yearly_baseline <- data.frame(
    asset = c("A1", "A1"),
    company = c("C1", "C1"),
    year = c(2025, 2030),
    revenue = c(1000, 1200),
    profit = c(100, 120)
  )

  assets_factors <- data.frame(
    asset = "A1",
    hazard_name = "flood__global_rcp85_h100glob_brazil",
    damage_factor = 0.5,
    cost_factor = 100,
    asset_category = "Industrial"
  )

  events <- data.frame(
    event_id = "e1",
    hazard_type = "flood",
    hazard_name = "flood__global_rcp85_h100glob_brazil",
    event_year = 2030L,
    chronic = FALSE,
    stringsAsFactors = FALSE
  )

  result <- compute_shock_trajectories(yearly_baseline, assets_factors, events)

  # Should return concatenated scenarios with baseline and shock
  expected_cols <- c("asset", "company", "year", "scenario", "revenue", "profit")
  testthat::expect_true(all(expected_cols %in% names(result)))
  
  # Should have both baseline and shock scenarios
  testthat::expect_true("baseline" %in% result$scenario)
  testthat::expect_true("shock" %in% result$scenario)
  
  # Should have double the rows (baseline + shock)
  testthat::expect_equal(nrow(result), nrow(yearly_baseline) * 2)
  
  # Baseline scenario should match input
  baseline_rows <- result[result$scenario == "baseline", ]
  testthat::expect_equal(nrow(baseline_rows), nrow(yearly_baseline))
  testthat::expect_equal(baseline_rows$revenue, yearly_baseline$revenue)
  testthat::expect_equal(baseline_rows$profit, yearly_baseline$profit)
})

testthat::test_that("compute_shock_trajectories applies full shock sequence", {
  # Test that the function applies:
  # 1. Acute revenue shock
  # 2. Chronic revenue shock  
  # 3. Compute profit from shocked revenue
  # 4. Acute profit shock
  # 5. Concatenate with baseline
  
  yearly_baseline <- data.frame(
    asset = c("A1", "A1"),
    company = c("C1", "C1"),
    year = c(2025, 2030),
    revenue = c(1000, 1200),
    profit = c(100, 120)
  )

  assets_factors <- data.frame(
    asset = "A1",
    hazard_name = "flood__global_rcp85_h100glob_brazil",
    damage_factor = 0.5,
    cost_factor = 100
  )

  events <- data.frame(
    event_id = "e1",
    hazard_type = "flood",
    hazard_name = "flood__global_rcp85_h100glob_brazil",
    event_year = 2030L,
    chronic = FALSE
  )

  result <- compute_shock_trajectories(yearly_baseline, assets_factors, events, net_profit_margin = 0.1)

  # Check shock scenario exists
  shock_rows <- result[result$scenario == "shock", ]
  testthat::expect_equal(nrow(shock_rows), nrow(yearly_baseline))
  
  # With placeholder shocks, revenue should match baseline (for now)
  # Once real shock logic is implemented, this test will need updating
  testthat::expect_equal(shock_rows$revenue, yearly_baseline$revenue)
  
  # Profit should be computed from revenue using margin
  expected_profit <- shock_rows$revenue * 0.1
  testthat::expect_equal(shock_rows$profit, expected_profit)
})

testthat::test_that("compute_shock_trajectories handles mixed acute and chronic events", {
  yearly_baseline <- data.frame(
    asset = c("A1", "A1", "A1"),
    company = c("C1", "C1", "C1"),
    year = c(2025, 2030, 2035),
    revenue = c(1000, 1200, 1440),
    profit = c(100, 120, 144)
  )

  assets_factors <- data.frame(
    asset = "A1",
    hazard_name = "flood__global_rcp85_h100glob_brazil",
    damage_factor = 0.3,
    cost_factor = 50
  )

  events <- data.frame(
    event_id = c("e1", "e2"),
    hazard_type = c("flood", "flood"),
    hazard_name = c("flood__global_rcp85_h100glob_brazil", "flood__global_rcp85_h100glob_brazil"),
    event_year = c(2030L, NA_integer_),
    chronic = c(FALSE, TRUE)
  )

  result <- compute_shock_trajectories(yearly_baseline, assets_factors, events)

  # Should have both scenarios
  testthat::expect_true("baseline" %in% result$scenario)
  testthat::expect_true("shock" %in% result$scenario)
  
  # Should have 6 rows total (3 years * 2 scenarios)
  testthat::expect_equal(nrow(result), 6)
})

testthat::test_that("compute_shock_trajectories validates inputs", {
  valid_baseline <- data.frame(
    asset = "A1",
    company = "C1",
    year = 2025,
    revenue = 1000,
    profit = 100
  )

  valid_assets <- data.frame(
    asset = "A1",
    hazard_name = "flood__global_rcp85_h100glob_brazil",
    damage_factor = 0.5
  )

  valid_events <- data.frame(
    event_id = "e1",
    hazard_type = "flood",
    hazard_name = "flood__global_rcp85_h100glob_brazil",
    event_year = 2030L,
    chronic = FALSE
  )

  # Should work with valid inputs
  testthat::expect_no_error(
    compute_shock_trajectories(valid_baseline, valid_assets, valid_events)
  )

  # Should error with invalid inputs
  testthat::expect_error(
    compute_shock_trajectories(NULL, valid_assets, valid_events),
    "data.frame"
  )

  testthat::expect_error(
    compute_shock_trajectories(valid_baseline, NULL, valid_events),
    "data.frame"
  )

  testthat::expect_error(
    compute_shock_trajectories(valid_baseline, valid_assets, NULL),
    "data.frame"
  )
})

testthat::test_that("compute_shock_trajectories ensures non-negative values", {
  yearly_baseline <- data.frame(
    asset = "A1",
    company = "C1",
    year = 2025,
    revenue = 1000,
    profit = 100
  )

  assets_factors <- data.frame(
    asset = "A1",
    hazard_name = "flood__global_rcp85_h100glob_brazil",
    damage_factor = 0.5
  )

  events <- data.frame(
    event_id = "e1",
    hazard_type = "flood",
    hazard_name = "flood__global_rcp85_h100glob_brazil",
    event_year = 2030L,
    chronic = FALSE
  )

  result <- compute_shock_trajectories(yearly_baseline, assets_factors, events)

  # All revenue and profit values should be non-negative
  testthat::expect_true(all(result$revenue >= 0))
  testthat::expect_true(all(result$profit >= 0))
})
