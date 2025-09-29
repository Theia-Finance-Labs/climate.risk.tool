# Tests for function 5: join_damage_cost_factors

# Contract:
# - join_damage_cost_factors(assets_with_hazards, damage_factors_df)
# - Joins on hazard_type, rounded hazard_intensity, and asset_category
# - Expects long format input with hazard_type, hazard_intensity columns
# - Adds numeric columns damage_factor and cost_factor


testthat::test_that("join_damage_cost_factors adds numeric damage_factor and cost_factor", {
  base_dir <- get_test_data_dir()
  
  # Create test data in long format with required columns
  assets_long <- data.frame(
    asset = c("A1", "A1", "A2", "A2"),
    company = c("C1", "C1", "C2", "C2"),
    latitude = c(-10, -10, -15, -15),
    longitude = c(-50, -50, -55, -55),
    municipality = c("Mun1", "Mun1", "Mun2", "Mun2"),
    province = c("Prov1", "Prov1", "Prov2", "Prov2"),
    asset_category = c("commercial building", "commercial building", "residential building", "residential building"),
    size_in_m2 = c(1000, 1000, 800, 800),
    share_of_economic_activity = c(0.5, 0.5, 0.3, 0.3),
    hazard_name = c("flood__global_rcp85_h100glob_brazil", "flood__global_rcp85_h100glob_brazil", "flood__global_rcp85_h100glob_brazil", "flood__global_rcp85_h100glob_brazil"),
    hazard_type = c("flood", "flood", "flood", "flood"),
    hazard_intensity = c(12.4, 2.1, 8.7, 1.8),
    stringsAsFactors = FALSE
  )
  
  # Load damage factors
  damage_factors <- read_damage_cost_factors(base_dir)

  out <- join_damage_cost_factors(assets_long, damage_factors)
  testthat::expect_true(all(c("damage_factor", "cost_factor") %in% names(out)))
  testthat::expect_true(is.numeric(out$damage_factor))
  testthat::expect_true(is.numeric(out$cost_factor))
  testthat::expect_equal(nrow(out), nrow(assets_long))
})
