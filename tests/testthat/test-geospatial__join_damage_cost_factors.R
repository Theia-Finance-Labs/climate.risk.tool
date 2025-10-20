# Tests for join_damage_cost_factors

# Contract:
# - join_damage_cost_factors(assets_with_hazards, damage_factors_df)
# - FloodTIF: Joins on hazard_type, hazard_indicator, rounded hazard_intensity, and asset_category
# - Compound: Joins on hazard_type, province, and scenario_name (GWL)
# - Expects long format input with hazard_type, hazard_indicator, hazard_intensity, scenario_name columns
# - Adds numeric columns damage_factor, cost_factor, business_disruption


testthat::test_that("join_damage_cost_factors handles FloodTIF with intensity-based matching", {
  base_dir <- get_test_data_dir()

  # Create FloodTIF test data
  assets_long <- data.frame(
    asset = c("A1", "A2"),
    company = c("C1", "C2"),
    latitude = c(-10, -15),
    longitude = c(-50, -55),
    municipality = c("Mun1", "Mun2"),
    province = c("Prov1", "Prov2"),
    asset_category = c("commercial building", "commercial building"),
    size_in_m2 = c(1000, 800),
    share_of_economic_activity = c(0.5, 0.3),
    hazard_name = c("flood__extraction_method=mean", "flood__extraction_method=mean"),
    hazard_type = c("FloodTIF", "FloodTIF"),
    hazard_indicator = c("Flood Height", "Flood Height"),
    hazard_intensity = c(12.4, 2.1),
    scenario_name = c("rcp85", "rcp85"),
    event_id = c("event_1", "event_1"),
    event_year = c(2030, 2030),
    chronic = c(FALSE, FALSE),
    stringsAsFactors = FALSE
  )

  # Load damage factors
  damage_factors <- read_damage_cost_factors(base_dir)

  out <- join_damage_cost_factors(assets_long, damage_factors)
  
  testthat::expect_true(all(c("damage_factor", "cost_factor", "business_disruption") %in% names(out)))
  testthat::expect_true(is.numeric(out$damage_factor))
  testthat::expect_true(is.numeric(out$cost_factor))
  testthat::expect_true(is.numeric(out$business_disruption))
  testthat::expect_equal(nrow(out), nrow(assets_long))
  
  # FloodTIF should have non-NA values for all three factors
  testthat::expect_true(all(!is.na(out$damage_factor)))
  testthat::expect_true(all(!is.na(out$cost_factor)))
  testthat::expect_true(all(!is.na(out$business_disruption)))
})

testthat::test_that("join_damage_cost_factors handles Compound with province and GWL matching", {
  base_dir <- get_test_data_dir()

  # Create Compound test data
  assets_long <- data.frame(
    asset = c("A1", "A2"),
    company = c("C1", "C2"),
    latitude = c(-10, -15),
    longitude = c(-50, -55),
    municipality = c("Mun1", "Mun2"),
    province = c("Acre", "Bahia"),  # Provinces from damage_and_cost_factors.csv
    asset_category = c("commercial building", "commercial building"),
    size_in_m2 = c(1000, 800),
    share_of_economic_activity = c(0.5, 0.3),
    hazard_name = c("HI__extraction_method=mean", "HI__extraction_method=mean"),
    hazard_type = c("Compound", "Compound"),
    hazard_indicator = c("HI", "HI"),
    hazard_intensity = c(50, 30),  # Days with extreme heat
    scenario_name = c("present", "1.5"),  # GWL scenarios
    event_id = c("event_1", "event_1"),
    event_year = c(2030, 2030),
    chronic = c(FALSE, FALSE),
    stringsAsFactors = FALSE
  )

  # Load damage factors
  damage_factors <- read_damage_cost_factors(base_dir)

  out <- join_damage_cost_factors(assets_long, damage_factors)
  
  testthat::expect_true(all(c("damage_factor", "cost_factor", "business_disruption") %in% names(out)))
  testthat::expect_equal(nrow(out), nrow(assets_long))
  
  # Compound should have damage_factor but NA for cost_factor and business_disruption
  testthat::expect_true(all(!is.na(out$damage_factor)))
  testthat::expect_true(all(is.na(out$cost_factor)))
  testthat::expect_true(all(is.na(out$business_disruption)))
  
  # Damage factor should be negative (labor productivity loss)
  testthat::expect_true(all(out$damage_factor < 0))
})

testthat::test_that("join_damage_cost_factors handles mixed FloodTIF and Compound hazards", {
  base_dir <- get_test_data_dir()

  # Create mixed test data
  assets_long <- data.frame(
    asset = c("A1", "A2"),
    company = c("C1", "C2"),
    latitude = c(-10, -15),
    longitude = c(-50, -55),
    municipality = c("Mun1", "Mun2"),
    province = c("Acre", "Bahia"),
    asset_category = c("commercial building", "commercial building"),
    size_in_m2 = c(1000, 800),
    share_of_economic_activity = c(0.5, 0.3),
    hazard_name = c("flood__extraction_method=mean", "HI__extraction_method=mean"),
    hazard_type = c("FloodTIF", "Compound"),
    hazard_indicator = c("Flood Height", "HI"),
    hazard_intensity = c(12.4, 30),
    scenario_name = c("rcp85", "present"),
    event_id = c("event_1", "event_2"),
    event_year = c(2030, 2030),
    chronic = c(FALSE, FALSE),
    stringsAsFactors = FALSE
  )

  # Load damage factors
  damage_factors <- read_damage_cost_factors(base_dir)

  out <- join_damage_cost_factors(assets_long, damage_factors)
  
  testthat::expect_equal(nrow(out), nrow(assets_long))
  
  # FloodTIF asset should have all three factors
  flood_row <- out[out$hazard_type == "FloodTIF", ]
  testthat::expect_true(!is.na(flood_row$damage_factor))
  testthat::expect_true(!is.na(flood_row$cost_factor))
  testthat::expect_true(!is.na(flood_row$business_disruption))
  
  # Compound asset should have damage_factor only
  compound_row <- out[out$hazard_type == "Compound", ]
  testthat::expect_true(!is.na(compound_row$damage_factor))
  testthat::expect_true(is.na(compound_row$cost_factor))
  testthat::expect_true(is.na(compound_row$business_disruption))
  testthat::expect_true(compound_row$damage_factor < 0)
})
