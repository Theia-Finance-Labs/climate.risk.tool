# Tests for function 5: join_damage_cost_factors

# Contract:
# - join_damage_cost_factors(assets_with_hazard_means, factors_csv)
# - Map hazard mean to nearest integer hazard_intensity; also join on asset_category
# - Adds numeric columns damage_factor and cost_factor


testthat::test_that("join_damage_cost_factors adds numeric damage_factor and cost_factor", {
  base_dir <- get_test_data_dir()
  assets <- read_assets(base_dir)

  # Build minimal hazard mean column to drive the join
  df <- assets
  df$hazard_dummy <- 12.4

  out <- join_damage_cost_factors(df, hazard_factor_path())
  testthat::expect_true(all(c("damage_factor", "cost_factor") %in% names(out)))
  testthat::expect_true(is.numeric(out$damage_factor))
  testthat::expect_true(is.numeric(out$cost_factor))
})
