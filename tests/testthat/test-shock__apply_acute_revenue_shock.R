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
