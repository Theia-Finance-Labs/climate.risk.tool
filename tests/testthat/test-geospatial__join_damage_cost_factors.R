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

testthat::test_that("join_drought_damage_factors handles crop/province/season matching - on season", {
  base_dir <- get_test_data_dir()
  
  # Create Drought test data for agriculture assets with on-season matching
  # Using actual provinces and crops from damage_and_cost_factors.csv
  assets_long <- data.frame(
    asset = c("A1", "A2"),
    company = c("C1", "C2"),
    latitude = c(-10, -15),
    longitude = c(-50, -55),
    municipality = c("Mun1", "Mun2"),
    province = c("Bahia", "Mato Grosso"),
    asset_category = c("agriculture", "agriculture"),
    asset_subtype = c("Soybean", "Corn"),
    size_in_m2 = c(10000, 8000),
    share_of_economic_activity = c(0.5, 0.3),
    hazard_name = c("SPI3__extraction_method=mean", "SPI3__extraction_method=mean"),
    hazard_type = c("Drought", "Drought"),
    hazard_indicator = c("SPI3", "SPI3"),
    hazard_intensity = c(-2.5, -3.0),
    scenario_name = c("present", "present"),
    event_id = c("event_1", "event_1"),
    event_year = c(2030, 2030),
    chronic = c(FALSE, FALSE),
    season = c("Summer", "Autumn"),  # On-season for these crops in these provinces
    stringsAsFactors = FALSE
  )
  
  # Load damage factors
  damage_factors <- read_damage_cost_factors(base_dir)
  
  out <- join_damage_cost_factors(assets_long, damage_factors)
  
  testthat::expect_true(all(c("damage_factor", "cost_factor", "business_disruption") %in% names(out)))
  testthat::expect_equal(nrow(out), nrow(assets_long))
  
  # Drought should have damage_factor but NA for cost_factor and business_disruption
  testthat::expect_true(all(!is.na(out$damage_factor)))
  testthat::expect_true(all(is.na(out$cost_factor)))
  testthat::expect_true(all(is.na(out$business_disruption)))
  
  # Damage factor should be positive (yield loss)
  testthat::expect_true(all(out$damage_factor > 0))
  testthat::expect_true(all(out$damage_factor <= 1))
})

testthat::test_that("join_drought_damage_factors handles crop/province/season matching - off season", {
  base_dir <- get_test_data_dir()
  
  # Create Drought test data for agriculture assets with off-season matching
  # Soybean in Bahia grows in Summer (from data), but we select Winter drought
  assets_long <- data.frame(
    asset = c("A1"),
    company = c("C1"),
    latitude = c(-10),
    longitude = c(-50),
    municipality = c("Mun1"),
    province = c("Bahia"),
    asset_category = c("agriculture"),
    asset_subtype = c("Soybean"),
    size_in_m2 = c(10000),
    share_of_economic_activity = c(0.5),
    hazard_name = c("SPI3__extraction_method=mean"),
    hazard_type = c("Drought"),
    hazard_indicator = c("SPI3"),
    hazard_intensity = c(-2.5),
    scenario_name = c("present"),
    event_id = c("event_1"),
    event_year = c(2030),
    chronic = c(FALSE),
    season = c("Winter"),  # Off-season (Soybean in Bahia grows in Summer)
    stringsAsFactors = FALSE
  )
  
  # Load damage factors
  damage_factors <- read_damage_cost_factors(base_dir)
  
  out <- join_damage_cost_factors(assets_long, damage_factors)
  
  testthat::expect_equal(nrow(out), 1)
  testthat::expect_true(!is.na(out$damage_factor))
  
  # Off-season damage should be reduced (multiplied by off_window < 1)
  # So damage factor should be positive but less than full damage
  testthat::expect_true(out$damage_factor >= 0)
})

testthat::test_that("join_drought_damage_factors handles missing subtype (defaults to Other)", {
  base_dir <- get_test_data_dir()
  
  # Create Drought test data with missing subtype
  # Should default to "Other" province/crop which has Soybean in Summer
  assets_long <- data.frame(
    asset = c("A1", "A2"),
    company = c("C1", "C2"),
    latitude = c(-10, -15),
    longitude = c(-50, -55),
    municipality = c("Mun1", "Mun2"),
    province = c("UnknownProvince", "UnknownProvince"),  # Will fall back to "Other"
    asset_category = c("agriculture", "agriculture"),
    asset_subtype = c(NA, ""),  # Missing subtype should default to "Other" → treated as Soybean
    size_in_m2 = c(10000, 8000),
    share_of_economic_activity = c(0.5, 0.3),
    hazard_name = c("SPI3__extraction_method=mean", "SPI3__extraction_method=mean"),
    hazard_type = c("Drought", "Drought"),
    hazard_indicator = c("SPI3", "SPI3"),
    hazard_intensity = c(-2.5, -3.0),
    scenario_name = c("present", "present"),
    event_id = c("event_1", "event_1"),
    event_year = c(2030, 2030),
    chronic = c(FALSE, FALSE),
    season = c("Summer", "Summer"),  # Matches Soybean growing season in "Other"
    stringsAsFactors = FALSE
  )
  
  # Load damage factors
  damage_factors <- read_damage_cost_factors(base_dir)
  
  out <- join_damage_cost_factors(assets_long, damage_factors)
  
  testthat::expect_equal(nrow(out), nrow(assets_long))
  testthat::expect_true(all(!is.na(out$damage_factor)))
  testthat::expect_true(all(out$damage_factor >= 0))
})

testthat::test_that("join_drought_damage_factors handles intensity capping", {
  base_dir <- get_test_data_dir()
  
  # Create Drought test data with extreme intensities
  # Using "Other" province and Soybean crop which exists in the data
  assets_long <- data.frame(
    asset = c("A1", "A2", "A3"),
    company = c("C1", "C2", "C3"),
    latitude = c(-10, -15, -20),
    longitude = c(-50, -55, -60),
    municipality = c("Mun1", "Mun2", "Mun3"),
    province = c("Bahia", "Bahia", "Bahia"),
    asset_category = c("agriculture", "agriculture", "agriculture"),
    asset_subtype = c("Soybean", "Soybean", "Soybean"),
    size_in_m2 = c(10000, 10000, 10000),
    share_of_economic_activity = c(0.5, 0.5, 0.5),
    hazard_name = c("SPI3__extraction_method=mean", "SPI3__extraction_method=mean", "SPI3__extraction_method=mean"),
    hazard_type = c("Drought", "Drought", "Drought"),
    hazard_indicator = c("SPI3", "SPI3", "SPI3"),
    hazard_intensity = c(-5.0, -0.5, -2.0),  # < -3, > -1, and normal
    scenario_name = c("present", "present", "present"),
    event_id = c("event_1", "event_1", "event_1"),
    event_year = c(2030, 2030, 2030),
    chronic = c(FALSE, FALSE, FALSE),
    season = c("Summer", "Summer", "Summer"),  # Matches Soybean in Bahia
    stringsAsFactors = FALSE
  )
  
  # Load damage factors
  damage_factors <- read_damage_cost_factors(base_dir)
  
  out <- join_damage_cost_factors(assets_long, damage_factors)
  
  testthat::expect_equal(nrow(out), 3)
  
  # A1: intensity -5.0 should be capped to -3.0 (max damage)
  # A2: intensity -0.5 should result in damage_factor = 0 (no damage)
  # A3: intensity -2.0 should use normal damage factor
  
  testthat::expect_true(out$damage_factor[out$asset == "A2"] == 0)  # No damage for intensity > -1
  testthat::expect_true(out$damage_factor[out$asset == "A3"] > 0)   # Normal damage
  testthat::expect_true(out$damage_factor[out$asset == "A1"] > 0)   # Capped to -3, should have max damage
})

testthat::test_that("join_drought_damage_factors filters to agriculture only", {
  base_dir <- get_test_data_dir()
  
  # Create mixed test data: drought on agriculture (should be kept) and commercial building (should be filtered)
  assets_long <- data.frame(
    asset = c("A1", "A2", "A3"),
    company = c("C1", "C2", "C3"),
    latitude = c(-10, -15, -20),
    longitude = c(-50, -55, -60),
    municipality = c("Mun1", "Mun2", "Mun3"),
    province = c("Bahia", "Bahia", "Prov3"),
    asset_category = c("agriculture", "commercial building", "commercial building"),
    asset_subtype = c("Soybean", NA, NA),
    size_in_m2 = c(10000, 8000, 5000),
    share_of_economic_activity = c(0.5, 0.3, 0.2),
    hazard_name = c("SPI3__extraction_method=mean", "SPI3__extraction_method=mean", "flood__extraction_method=mean"),
    hazard_type = c("Drought", "Drought", "FloodTIF"),
    hazard_indicator = c("SPI3", "SPI3", "Flood Height"),
    hazard_intensity = c(-2.5, -2.5, 10),
    scenario_name = c("present", "present", "rcp85"),
    event_id = c("event_1", "event_1", "event_2"),
    event_year = c(2030, 2030, 2030),
    chronic = c(FALSE, FALSE, FALSE),
    season = c("Summer", "Summer", NA),
    stringsAsFactors = FALSE
  )
  
  # Load damage factors
  damage_factors <- read_damage_cost_factors(base_dir)
  
  out <- join_damage_cost_factors(assets_long, damage_factors)
  
  # Should have 2 assets: A1 (agriculture with drought) and A3 (commercial with flood)
  # A2 (commercial with drought) should be filtered out
  testthat::expect_equal(nrow(out), 2)
  testthat::expect_true("A1" %in% out$asset)  # Agriculture with drought
  testthat::expect_true("A3" %in% out$asset)  # Commercial with flood
  testthat::expect_false("A2" %in% out$asset)  # Commercial with drought - filtered
  
  # Check that A1 is agriculture
  testthat::expect_equal(out$asset_category[out$asset == "A1"], "agriculture")
})
