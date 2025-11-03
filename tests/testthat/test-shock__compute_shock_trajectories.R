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
    damage_factor = 0.5,
    cost_factor = 100,
    asset_category = "Industrial"
  )

  events <- data.frame(
    event_id = "e1",
    hazard_type = "flood",
    hazard_name = "flood__global_rcp85_h100glob_brazil",
    event_year = 2030L,
    stringsAsFactors = FALSE
  )

  companies <- data.frame(
    company = "C1",
    net_profit_margin = 0.1
  )

  result <- compute_shock_trajectories(yearly_baseline, assets_factors, events, companies)

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
  # 2. Compute profit from shocked revenue
  # 3. Acute profit shock

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
    damage_factor = 0.5,
    cost_factor = 100
  )

  events <- data.frame(
    event_id = "e1",
    hazard_type = "flood",
    hazard_name = "flood__global_rcp85_h100glob_brazil",
    event_year = 2030L
  )

  companies <- data.frame(
    company = "C1",
    net_profit_margin = 0.1
  )

  result <- compute_shock_trajectories(yearly_baseline, assets_factors, events, companies)

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
