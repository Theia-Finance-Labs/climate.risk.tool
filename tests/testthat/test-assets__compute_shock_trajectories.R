# Tests for compute_shock_trajectories

testthat::test_that("compute_shock_trajectories processes acute events", {
  yearly_baseline <- data.frame(
    asset = c("A1", "A1", "A2", "A2"),
    company = c("C1", "C1", "C1", "C1"),
    year = c(2025, 2030, 2025, 2030),
    baseline_revenue = c(1000, 1200, 800, 960),
    baseline_profit = c(100, 120, 80, 96)
  )

  assets_factors <- data.frame(
    asset = c("A1", "A2"),
    hazard_name = c("flood__global_rcp85_h100glob_brazil", "flood__global_rcp85_h100glob_brazil"),
    damage_factor = c(0.5, 0.3),
    cost_factor = c(100, 80),
    asset_category = c("Industrial", "Commercial")
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

  # Should return shocked revenue and profit columns
  expected_cols <- c("asset", "company", "year", "shocked_revenue", "shocked_profit")
  testthat::expect_true(all(expected_cols %in% names(result)))
  testthat::expect_equal(nrow(result), nrow(yearly_baseline))

  # Should preserve pre-shock years
  pre_shock <- result[result$year < 2030, ]
  baseline_pre_shock <- yearly_baseline[yearly_baseline$year < 2030, ]
  testthat::expect_equal(pre_shock$shocked_revenue, baseline_pre_shock$baseline_revenue)

  # Should apply shock to shock year
  shock_year_rows <- result[result$year >= 2030, ]
  baseline_shock_year <- yearly_baseline[yearly_baseline$year >= 2030, ]
  testthat::expect_true(all(shock_year_rows$shocked_revenue <= baseline_shock_year$baseline_revenue))
})

testthat::test_that("compute_shock_trajectories processes chronic events", {
  yearly_baseline <- data.frame(
    asset = c("A1", "A1", "A1"),
    company = c("C1", "C1", "C1"),
    year = c(2025, 2030, 2035),
    baseline_revenue = c(1000, 1200, 1440),
    baseline_profit = c(100, 120, 144)
  )

  assets_factors <- data.frame(
    asset = "A1",
    hazard_name = "flood__global_rcp85_h100glob_brazil",
    damage_factor = 35,
    cost_factor = 50,
    asset_category = "Industrial"
  )

  events <- data.frame(
    event_id = "e1",
    hazard_type = "flood",
    hazard_name = "flood__global_rcp85_h100glob_brazil",
    event_year = NA_integer_,
    chronic = TRUE,
    stringsAsFactors = FALSE
  )

  result <- compute_shock_trajectories(yearly_baseline, assets_factors, events, start_year = 2025)

  # Should return shocked trajectories
  expected_cols <- c("asset", "company", "year", "shocked_revenue", "shocked_profit")
  testthat::expect_true(all(expected_cols %in% names(result)))

  # Chronic shock should compound over time
  shock_ratios <- result$shocked_revenue / yearly_baseline$baseline_revenue
  testthat::expect_true(shock_ratios[3] <= shock_ratios[2]) # 2035 <= 2030
  testthat::expect_true(shock_ratios[2] <= shock_ratios[1]) # 2030 <= 2025
})

testthat::test_that("compute_shock_trajectories handles multiple events", {
  yearly_baseline <- data.frame(
    asset = c("A1", "A1"),
    company = c("C1", "C1"),
    year = c(2025, 2030),
    baseline_revenue = c(1000, 1200),
    baseline_profit = c(100, 120)
  )

  assets_factors <- data.frame(
    asset = c("A1", "A1"),
    hazard_name = c("flood__global_rcp85_h100glob_brazil", "flood__global_rcp85_h100glob_brazil"),
    damage_factor = c(10, 35),
    cost_factor = c(200, 150),
    asset_category = c("Industrial", "Industrial")
  )

  events <- data.frame(
    event_id = c("e1", "e2"),
    hazard_type = c("flood", "flood"),
    hazard_name = c("flood__global_rcp85_h100glob_brazil", "flood__global_rcp85_h100glob_brazil"),
    event_year = c(2030L, NA_integer_),
    chronic = c(FALSE, TRUE),
    stringsAsFactors = FALSE
  )

  result <- compute_shock_trajectories(yearly_baseline, assets_factors, events)

  # Should take minimum impact across events
  testthat::expect_true(all(result$shocked_revenue <= yearly_baseline$baseline_revenue))
  testthat::expect_true(all(result$shocked_profit <= yearly_baseline$baseline_profit))
  testthat::expect_equal(nrow(result), nrow(yearly_baseline))
})

testthat::test_that("compute_shock_trajectories applies hazard-specific filtering", {
  yearly_baseline <- data.frame(
    asset = c("A1", "A1", "A2", "A2"),
    company = c("C1", "C1", "C1", "C1"),
    year = c(2025, 2030, 2025, 2030),
    baseline_revenue = c(1000, 1200, 800, 960),
    baseline_profit = c(100, 120, 80, 96)
  )

  assets_factors <- data.frame(
    asset = c("A1", "A1", "A2", "A2"),
    hazard_name = c("flood__global_rcp85_h100glob_brazil", "flood__global_rcp85_h100glob_brazil", "flood__global_rcp85_h100glob_brazil", "flood__global_rcp85_h100glob_brazil"),
    damage_factor = c(0.2, 0.1, 0.3, 0.15),
    cost_factor = c(500, 300, 700, 400),
    asset_category = c("Industrial", "Industrial", "Commercial", "Commercial")
  )

  events <- data.frame(
    event_id = c("e1", "e2"),
    hazard_type = c("flood", "flood"),
    hazard_name = c("flood__global_rcp85_h100glob_brazil", "flood__global_rcp85_h100glob_brazil"),
    event_year = c(2030L, NA_integer_),
    chronic = c(FALSE, TRUE),
    stringsAsFactors = FALSE
  )

  result <- compute_shock_trajectories(yearly_baseline, assets_factors, events)

  testthat::expect_true(all(c("shocked_revenue", "shocked_profit") %in% names(result)))
})

testthat::test_that("compute_shock_trajectories validates inputs", {
  yearly_baseline <- data.frame(
    asset = "A1",
    company = "C1",
    year = 2025,
    baseline_revenue = 1000,
    baseline_profit = 100
  )

  assets_factors <- data.frame(
    asset = "A1",
    hazard_name = "flood__global_rcp85_h100glob_brazil",
    damage_factor = 5,
    asset_category = "Industrial"
  )

  events <- data.frame(
    event_id = "e1",
    hazard_type = "flood",
    hazard_name = "flood__global_rcp85_h100glob_brazil",
    event_year = 2030L,
    chronic = FALSE
  )

  # Should work with valid inputs
  testthat::expect_no_error(
    compute_shock_trajectories(yearly_baseline, assets_factors, events)
  )

  # Should error with invalid inputs
  testthat::expect_error(
    compute_shock_trajectories(NULL, assets_factors, events),
    regexp = "non-empty data.frame"
  )

  testthat::expect_error(
    compute_shock_trajectories(yearly_baseline, NULL, events),
    regexp = "non-empty data.frame"
  )

  testthat::expect_error(
    compute_shock_trajectories(yearly_baseline, assets_factors, NULL),
    regexp = "non-empty data.frame"
  )

  # Should error with missing event columns
  incomplete_events <- data.frame(event_id = "e1")
  testthat::expect_error(
    compute_shock_trajectories(yearly_baseline, assets_factors, incomplete_events),
    regexp = "missing required columns"
  )
})
