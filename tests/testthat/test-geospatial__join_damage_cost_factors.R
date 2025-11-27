# Tests for join_damage_cost_factors

# Contract:
# - join_damage_cost_factors(assets_with_hazards, damage_factors_df)
# - Flood: Joins on hazard_type, hazard_indicator, rounded hazard_intensity, and asset_category
# - Heat: Joins on hazard_type, province, and scenario_name (GWL)
# - Expects long format input with hazard_type, hazard_indicator, hazard_intensity, scenario_name columns
# - Adds numeric columns damage_factor, cost_factor, business_disruption


testthat::test_that("join_damage_cost_factors handles Flood with intensity-based matching", {
  base_dir <- get_test_data_dir()

  # Create Flood test data
  assets_long <- data.frame(
    asset = c("A1", "A2"),
    company = c("C1", "C2"),
    latitude = c(-10, -15),
    longitude = c(-50, -55),
    municipality = c("Mun1", "Mun2"),
    state = c("Prov1", "Prov2"),
    asset_category = c("commercial building", "commercial building"),
    size_in_m2 = c(1000, 800),
    share_of_economic_activity = c(0.5, 0.3),
    hazard_name = c("flood__extraction_method=mean", "flood__extraction_method=mean"),
    hazard_type = c("Flood", "Flood"),
    hazard_indicator = c("depth(cm)", "depth(cm)"),
    hazard_intensity = c(12.4, 2.1),
    scenario_name = c("rcp85", "rcp85"),
    event_id = c("event_1", "event_1"),
    event_year = c(2030, 2030),
    cnae = NA_real_,
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

  # Flood should have non-NA values for all three factors
  testthat::expect_true(all(!is.na(out$damage_factor)))
  testthat::expect_true(all(!is.na(out$cost_factor)))
  testthat::expect_true(all(!is.na(out$business_disruption)))
})

testthat::test_that("join_damage_cost_factors handles Heat with province and GWL matching", {
  base_dir <- get_test_data_dir()

  # Create Heat test data
  assets_long <- data.frame(
    asset = c("A1", "A2"),
    company = c("C1", "C2"),
    latitude = c(-10, -15),
    longitude = c(-50, -55),
    municipality = c("Mun1", "Mun2"),
    state = c("Acre", "Bahia"), # Provinces from damage_and_cost_factors.csv
    asset_category = c("commercial building", "commercial building"),
    size_in_m2 = c(1000, 800),
    share_of_economic_activity = c(0.5, 0.3),
    hazard_name = c("HI__extraction_method=mean", "HI__extraction_method=mean"),
    hazard_type = c("Heat", "Heat"),
    hazard_indicator = c("HI", "HI"),
    hazard_intensity = c(50, 30), # Days with extreme heat
    scenario_name = c("present", "1.5"), # GWL scenarios
    event_id = c("event_1", "event_1"),
    event_year = c(2030, 2030),
    cnae = NA_real_,
    stringsAsFactors = FALSE
  )

  # Load damage factors
  damage_factors <- read_damage_cost_factors(base_dir)

  out <- join_damage_cost_factors(assets_long, damage_factors)

  testthat::expect_true(all(c("damage_factor", "cost_factor", "business_disruption") %in% names(out)))
  testthat::expect_equal(nrow(out), nrow(assets_long))

  # Heat should have damage_factor but NA for cost_factor and business_disruption
  testthat::expect_true(all(!is.na(out$damage_factor)))
  testthat::expect_true(all(is.na(out$cost_factor)))
  testthat::expect_true(all(is.na(out$business_disruption)))

  # Damage factor should be negative (labor productivity loss)
  testthat::expect_true(all(out$damage_factor < 0))
})

testthat::test_that("read_cnae_labor_productivity_exposure loads file correctly", {
  base_dir <- get_test_data_dir()

  cnae_exposure <- read_cnae_labor_productivity_exposure(base_dir)

  testthat::expect_true(all(c("cnae", "description", "lp_exposure") %in% names(cnae_exposure)))
  testthat::expect_gt(nrow(cnae_exposure), 0)
  testthat::expect_true(is.numeric(cnae_exposure$cnae))
  testthat::expect_true(all(cnae_exposure$lp_exposure %in% c("high", "median", "low")))
})

testthat::test_that("join_compound_damage_factors uses cnae-based metric selection", {
  base_dir <- get_test_data_dir()

  # Load CNAE exposure data
  cnae_exposure <- read_cnae_labor_productivity_exposure(base_dir)
  damage_factors <- read_damage_cost_factors(base_dir)

  # Find examples of CNAE codes with different exposures
  high_exposure <- cnae_exposure$cnae[cnae_exposure$lp_exposure == "high"][1]
  median_exposure <- cnae_exposure$cnae[cnae_exposure$lp_exposure == "median"][1]
  low_exposure <- cnae_exposure$cnae[cnae_exposure$lp_exposure == "low"][1]

  # Create Heat test data with different cnae and missing cases
  assets_long <- data.frame(
    asset = c("A1", "A2", "A3", "A4", "A5"),
    company = c("C1", "C2", "C3", "C4", "C5"),
    latitude = c(-10, -15, -20, -25, -30),
    longitude = c(-50, -55, -60, -65, -70),
    municipality = c("Mun1", "Mun2", "Mun3", "Mun4", "Mun5"),
    state = c("Acre", "Acre", "Acre", "Acre", "Acre"), # Same province, different metrics
    asset_category = c("commercial building", "commercial building", "commercial building", "commercial building", "agriculture"),
    cnae = c(high_exposure, median_exposure, low_exposure, NA, NA), # Different CNAEs + missing
    size_in_m2 = c(1000, 800, 600, 400, 200),
    share_of_economic_activity = c(0.5, 0.3, 0.2, 0.1, 0.05),
    hazard_name = rep("HI__extraction_method=mean", 5),
    hazard_type = rep("Heat", 5),
    hazard_indicator = rep("HI", 5),
    hazard_intensity = rep(50, 5),
    scenario_name = rep("present", 5),
    event_id = rep("event_1", 5),
    event_year = rep(2030, 5),
    stringsAsFactors = FALSE
  )

  out <- join_damage_cost_factors(assets_long, damage_factors, cnae_exposure)

  testthat::expect_equal(nrow(out), 5)
  testthat::expect_true(all(!is.na(out$damage_factor)))

  # Get damage factors for each asset
  a1_df <- out$damage_factor[out$asset == "A1"] # High exposure
  a2_df <- out$damage_factor[out$asset == "A2"] # Median exposure
  a3_df <- out$damage_factor[out$asset == "A3"] # Low exposure
  a4_df <- out$damage_factor[out$asset == "A4"] # Missing cnae (median)
  a5_df <- out$damage_factor[out$asset == "A5"] # Missing cnae + agriculture (high)

  # All should be negative (labor productivity loss)
  testthat::expect_true(all(c(a1_df, a2_df, a3_df, a4_df, a5_df) < 0))

  # High exposure should have more negative (worse) damage than low exposure
  testthat::expect_true(a1_df < a3_df) # High < Low (more negative)

  # Agriculture without cnae should use high metric (should match A1 in value if factors are correct)
  # For strict equality test, damage_factors must give same value for "high" metric and scenario/province match
  testthat::expect_equal(a5_df, a1_df)
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
    state = c("Bahia", "Mato Grosso"),
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
    season = c("Summer", "Autumn"), # On-season for these crops in these provinces
    cnae = NA,
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
    state = c("Bahia"),
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
    season = c("Winter"), # Off-season (Soybean in Bahia grows in Summer)
    cnae = NA,
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

testthat::test_that("join_drought_damage_factors handles multi-season crops - exact season match", {
  base_dir <- get_test_data_dir()

  # Test Sugarcane in Alagoas which has 2 growing seasons: Winter (37%, off=30%) and Autumn (35%, off=30%)
  # User selects Winter via season in hazard name - should match Winter season exactly
  assets_long <- data.frame(
    asset = c("A1"),
    company = c("C1"),
    latitude = c(-10),
    longitude = c(-50),
    municipality = c("Mun1"),
    state = c("Alagoas"),
    asset_category = c("agriculture"),
    asset_subtype = c("Sugarcane"),
    size_in_m2 = c(10000),
    share_of_economic_activity = c(0.5),
    hazard_name = c("Drought__SPI3__GWL=present__RP=10__season=Winter__ensemble=median__extraction_method=mean"),
    hazard_type = c("Drought"),
    hazard_indicator = c("SPI3"),
    hazard_intensity = c(-3.0),
    scenario_name = c("present"),
    event_id = c("event_1"),
    event_year = c(2030),
    cnae = NA,
    stringsAsFactors = FALSE
  )

  # Load damage factors
  damage_factors <- read_damage_cost_factors(base_dir)

  out <- join_damage_cost_factors(assets_long, damage_factors)

  testthat::expect_equal(nrow(out), 1)
  testthat::expect_true(!is.na(out$damage_factor))

  # Should have metadata columns in output
  testthat::expect_true("growing_season" %in% names(out))
  testthat::expect_true("off_window" %in% names(out))
  testthat::expect_true("season" %in% names(out))

  # Should match Winter season (37% damage factor)
  testthat::expect_equal(out$growing_season, "Winter")
  testthat::expect_equal(out$damage_factor, 0.37, tolerance = 0.001)
  testthat::expect_equal(out$off_window, 0.3, tolerance = 0.001)
})

testthat::test_that("join_drought_damage_factors handles multi-season crops - off-season averages", {
  base_dir <- get_test_data_dir()

  # Test Sugarcane in Alagoas with Summer (not a growing season) in hazard name
  # Has Winter (37%, off=30%) and Autumn (35%, off=30%)
  # Should average: damage = (37+35)/2 = 36%, off_window = (30+30)/2 = 30%
  # Final damage_factor = 36% * 30% = 10.8%
  assets_long <- data.frame(
    asset = c("A1"),
    company = c("C1"),
    latitude = c(-10),
    longitude = c(-50),
    municipality = c("Mun1"),
    state = c("Alagoas"),
    asset_category = c("agriculture"),
    asset_subtype = c("Sugarcane"),
    size_in_m2 = c(10000),
    share_of_economic_activity = c(0.5),
    hazard_name = c("Drought__SPI3__GWL=present__RP=10__season=Summer__ensemble=median__extraction_method=mean"),
    hazard_type = c("Drought"),
    hazard_indicator = c("SPI3"),
    hazard_intensity = c(-3.0),
    scenario_name = c("present"),
    event_id = c("event_1"),
    event_year = c(2030),
    cnae = NA,
    stringsAsFactors = FALSE
  )

  # Load damage factors
  damage_factors <- read_damage_cost_factors(base_dir)

  out <- join_damage_cost_factors(assets_long, damage_factors)

  testthat::expect_equal(nrow(out), 1)
  testthat::expect_true(!is.na(out$damage_factor))

  # Should have metadata columns in output
  testthat::expect_true("growing_season" %in% names(out))
  testthat::expect_true("off_window" %in% names(out))
  testthat::expect_true("season" %in% names(out))

  # Should show "Averaged" with sorted season names
  testthat::expect_equal(out$growing_season, "Averaged (Autumn, Winter)")

  # Average damage: (0.37 + 0.35) / 2 = 0.36
  # Average off_window: (0.3 + 0.3) / 2 = 0.3
  # Applied damage: 0.36 * 0.3 = 0.108
  testthat::expect_equal(out$damage_factor, 0.108, tolerance = 0.001)
  testthat::expect_equal(out$off_window, 0.3, tolerance = 0.001)
})

testthat::test_that("join_drought_damage_factors handles multi-season crops - autumn match", {
  base_dir <- get_test_data_dir()

  # Test Sugarcane in Alagoas with Autumn season in hazard name
  # Should match Autumn season (35%, off=30%)
  assets_long <- data.frame(
    asset = c("A1"),
    company = c("C1"),
    latitude = c(-10),
    longitude = c(-50),
    municipality = c("Mun1"),
    state = c("Alagoas"),
    asset_category = c("agriculture"),
    asset_subtype = c("Sugarcane"),
    size_in_m2 = c(10000),
    share_of_economic_activity = c(0.5),
    hazard_name = c("Drought__SPI3__GWL=present__RP=10__season=Autumn__ensemble=median__extraction_method=mean"),
    hazard_type = c("Drought"),
    hazard_indicator = c("SPI3"),
    hazard_intensity = c(-3.0),
    scenario_name = c("present"),
    event_id = c("event_1"),
    event_year = c(2030),
    cnae = NA,
    stringsAsFactors = FALSE
  )

  # Load damage factors
  damage_factors <- read_damage_cost_factors(base_dir)

  out <- join_damage_cost_factors(assets_long, damage_factors)

  testthat::expect_equal(nrow(out), 1)
  testthat::expect_true(!is.na(out$damage_factor))

  # Should match Autumn season (35% damage factor)
  testthat::expect_equal(out$growing_season, "Autumn")
  testthat::expect_equal(out$damage_factor, 0.35, tolerance = 0.001)
  testthat::expect_equal(out$off_window, 0.3, tolerance = 0.001)
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
    state = c("UnknownProvince", "UnknownProvince"), # Will fall back to "Other"
    asset_category = c("agriculture", "agriculture"),
    asset_subtype = c(NA, ""), # Missing subtype should default to "Other" → treated as Soybean
    size_in_m2 = c(10000, 8000),
    share_of_economic_activity = c(0.5, 0.3),
    hazard_name = c("SPI3__extraction_method=mean", "SPI3__extraction_method=mean"),
    hazard_type = c("Drought", "Drought"),
    hazard_indicator = c("SPI3", "SPI3"),
    hazard_intensity = c(-2.5, -3.0),
    scenario_name = c("present", "present"),
    event_id = c("event_1", "event_1"),
    event_year = c(2030, 2030),
    season = c("Summer", "Summer"), # Matches Soybean growing season in "Other"
    cnae = NA,
    stringsAsFactors = FALSE
  )

  # Load damage factors
  damage_factors <- read_damage_cost_factors(base_dir)

  out <- join_damage_cost_factors(assets_long, damage_factors)

  testthat::expect_equal(nrow(out), nrow(assets_long))
  testthat::expect_true(all(!is.na(out$damage_factor)))
  testthat::expect_true(all(out$damage_factor >= 0))
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
    state = c("Bahia", "Bahia", "Prov3"),
    asset_category = c("agriculture", "commercial building", "commercial building"),
    asset_subtype = c("Soybean", NA, NA),
    size_in_m2 = c(10000, 8000, 5000),
    share_of_economic_activity = c(0.5, 0.3, 0.2),
    hazard_name = c("SPI3__extraction_method=mean", "SPI3__extraction_method=mean", "flood__extraction_method=mean"),
    hazard_type = c("Drought", "Drought", "Flood"),
    hazard_indicator = c("SPI3", "SPI3", "depth(cm)"),
    hazard_intensity = c(-2.5, -2.5, 10),
    scenario_name = c("present", "present", "rcp85"),
    event_id = c("event_1", "event_1", "event_2"),
    event_year = c(2030, 2030, 2030),
    season = c("Summer", "Summer", NA),
    cnae = NA,
    stringsAsFactors = FALSE
  )

  # Load damage factors
  damage_factors <- read_damage_cost_factors(base_dir)

  out <- join_damage_cost_factors(assets_long, damage_factors)

  # Should have 2 assets: A1 (agriculture with drought) and A3 (commercial with flood)
  # A2 (commercial with drought) should be filtered out
  testthat::expect_equal(nrow(out), 2)
  testthat::expect_true("A1" %in% out$asset) # Agriculture with drought
  testthat::expect_true("A3" %in% out$asset) # Commercial with flood
  testthat::expect_false("A2" %in% out$asset) # Commercial with drought - filtered

  # Check that A1 is agriculture
  testthat::expect_equal(out$asset_category[out$asset == "A1"], "agriculture")
})
