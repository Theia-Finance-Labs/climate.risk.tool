# Tests for compute_shock_trajectories

testthat::test_that("compute_shock_trajectories returns only shocked trajectories", {
  yearly_baseline <- data.frame(
    asset = c("A1", "A1"),
    company = c("C1", "C1"),
    year = c(2025, 2030),
    revenue = c(1000, 1200),
    profit = c(100, 120)
  )

  assets_factors <- data.frame(
    asset = "A1",
    hazard_type = "flood",
    hazard_name = "flood__global_rcp85_h100glob_brazil",
    business_disruption = 10,
    cost_factor = 50,
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

  # Should return shocked trajectories (no baseline, no scenario column)
  expected_cols <- c("asset", "company", "year", "revenue", "profit")
  testthat::expect_true(all(expected_cols %in% names(result)))
  
  # Should NOT have scenario column (that's added by concatenate_baseline_and_shock)
  testthat::expect_false("scenario" %in% names(result))
  
  # Should have same number of rows as input (only shocked scenario)
  testthat::expect_equal(nrow(result), nrow(yearly_baseline))
})

testthat::test_that("compute_shock_trajectories applies full shock sequence", {
  # Test that the function applies:
  # 1. Acute revenue shock
  # 2. Chronic revenue shock  
  # 3. Compute profit from shocked revenue
  # 4. Acute profit shock
  
  yearly_baseline <- data.frame(
    asset = c("A1", "A1"),
    company = c("C1", "C1"),
    year = c(2025, 2030),
    revenue = c(1000, 1200),
    profit = c(100, 120)
  )

  assets_factors <- data.frame(
    asset = "A1",
    hazard_type = "flood",
    hazard_name = "flood__global_rcp85_h100glob_brazil",
    business_disruption = 10,
    cost_factor = 50
  )

  events <- data.frame(
    event_id = "e1",
    hazard_type = "flood",
    hazard_name = "flood__global_rcp85_h100glob_brazil",
    event_year = 2030L,
    chronic = FALSE
  )

  result <- compute_shock_trajectories(yearly_baseline, assets_factors, events, net_profit_margin = 0.1)

  # Should return shocked trajectories only (no baseline)
  testthat::expect_equal(nrow(result), nrow(yearly_baseline))
  
  # Should have revenue and profit columns
  testthat::expect_true("revenue" %in% names(result))
  testthat::expect_true("profit" %in% names(result))
  
  # Profit should be computed from shocked revenue using margin, then modified by acute profit shock
  # (actual values will depend on shock implementation)
  testthat::expect_true(all(!is.na(result$profit)))
  testthat::expect_true(all(result$profit >= 0))
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
    hazard_type = "flood",
    hazard_name = "flood__global_rcp85_h100glob_brazil",
    business_disruption = 10,
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

  # Should return shocked trajectories only (no scenario column)
  testthat::expect_false("scenario" %in% names(result))
  
  # Should have 3 rows (one for each year in baseline)
  testthat::expect_equal(nrow(result), 3)
})
