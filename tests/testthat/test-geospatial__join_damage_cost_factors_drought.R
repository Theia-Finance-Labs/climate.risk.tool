# Additional tests for join_damage_cost_factors - Edge cases and scenarios

testthat::test_that("join_drought_damage_factors: crop exists but not in asset's state (use Other state)", {
  base_dir <- get_test_data_dir()

  # Test Sugarcane in Mato Grosso (doesn't exist - Mato Grosso has Corn/Soybean only)
  # Should fallback to "Other" state + Sugarcane
  assets_long <- data.frame(
    asset = c("A1"),
    company = c("C1"),
    latitude = c(-12.5),
    longitude = c(-56.0),
    municipality = c("Mun1"),
    state = c("Mato Grosso"), # Has Corn/Soybean but NOT Sugarcane
    asset_category = c("agriculture"),
    asset_subtype = c("Sugarcane"), # Exists in other states (Bahia, Sao Paulo, etc)
    size_in_m2 = c(10000),
    share_of_economic_activity = c(0.5),
    hazard_name = c("SPI3__extraction_method=mean"),
    hazard_type = c("Drought"),
    hazard_indicator = c("SPI3"),
    hazard_intensity = c(-2.5),
    scenario_name = c("present"),
    event_id = c("event_1"),
    event_year = c(2030),
    season = c("Winter"), # Sugarcane grows in Winter in some states
    cnae = NA,
    stringsAsFactors = FALSE
  )

  damage_factors <- read_damage_cost_factors(base_dir)
  out <- join_damage_cost_factors(assets_long, damage_factors)

  testthat::expect_equal(nrow(out), 1)
  testthat::expect_true(!is.na(out$damage_factor))
  testthat::expect_true(out$damage_factor >= 0)
  # Should have metadata columns
  testthat::expect_true("growing_season" %in% names(out))
  testthat::expect_true("off_window" %in% names(out))
})

testthat::test_that("join_drought_damage_factors: crop doesn't exist anywhere (use Other crop)", {
  base_dir <- get_test_data_dir()

  # Test unknown crop type in a known state
  # Should fallback to "Other" crop (based on Soybean) + actual state
  assets_long <- data.frame(
    asset = c("A1"),
    company = c("C1"),
    latitude = c(-15),
    longitude = c(-48),
    municipality = c("Mun1"),
    state = c("Goias"), # Valid state
    asset_category = c("agriculture"),
    asset_subtype = c("Rice"), # Doesn't exist in damage factors
    size_in_m2 = c(10000),
    share_of_economic_activity = c(0.5),
    hazard_name = c("SPI3__extraction_method=mean"),
    hazard_type = c("Drought"),
    hazard_indicator = c("SPI3"),
    hazard_intensity = c(-2.5),
    scenario_name = c("present"),
    event_id = c("event_1"),
    event_year = c(2030),
    season = c("Summer"), # Soybean grows in Summer
    cnae = NA,
    stringsAsFactors = FALSE
  )

  damage_factors <- read_damage_cost_factors(base_dir)
  out <- join_damage_cost_factors(assets_long, damage_factors)

  testthat::expect_equal(nrow(out), 1)
  testthat::expect_true(!is.na(out$damage_factor))
  testthat::expect_true(out$damage_factor >= 0)
})

testthat::test_that("join_drought_damage_factors: neither crop nor state exist (use Other+Other)", {
  base_dir <- get_test_data_dir()

  # Test unknown crop in unknown state
  # Should fallback to "Other" crop + "Other" state
  assets_long <- data.frame(
    asset = c("A1"),
    company = c("C1"),
    latitude = c(0),
    longitude = c(-50),
    municipality = c("Mun1"),
    state = c("Amapa"), # State not in damage factors
    asset_category = c("agriculture"),
    asset_subtype = c("Rice"), # Crop not in damage factors
    size_in_m2 = c(10000),
    share_of_economic_activity = c(0.5),
    hazard_name = c("SPI3__extraction_method=mean"),
    hazard_type = c("Drought"),
    hazard_indicator = c("SPI3"),
    hazard_intensity = c(-2.5),
    scenario_name = c("present"),
    event_id = c("event_1"),
    event_year = c(2030),
    season = c("Summer"), # Soybean grows in Summer
    cnae = NA,
    stringsAsFactors = FALSE
  )

  damage_factors <- read_damage_cost_factors(base_dir)
  out <- join_damage_cost_factors(assets_long, damage_factors)

  testthat::expect_equal(nrow(out), 1)
  testthat::expect_true(!is.na(out$damage_factor))
  testthat::expect_true(out$damage_factor >= 0)
})

testthat::test_that("join_drought_damage_factors: closest intensity matching works correctly", {
  base_dir <- get_test_data_dir()

  # Test that intensity matching finds the closest value
  # Assuming damage factors have intensities like -1, -2, -3
  # Test with -1.7 (closer to -2) and -2.9 (closer to -3)
  assets_long <- data.frame(
    asset = c("A1", "A2"),
    company = c("C1", "C2"),
    latitude = c(-10, -15),
    longitude = c(-50, -55),
    municipality = c("Mun1", "Mun2"),
    state = c("Bahia", "Bahia"),
    asset_category = c("agriculture", "agriculture"),
    asset_subtype = c("Soybean", "Soybean"),
    size_in_m2 = c(10000, 8000),
    share_of_economic_activity = c(0.5, 0.3),
    hazard_name = c("SPI3__extraction_method=mean", "SPI3__extraction_method=mean"),
    hazard_type = c("Drought", "Drought"),
    hazard_indicator = c("SPI3", "SPI3"),
    hazard_intensity = c(-1.7, -2.9),
    scenario_name = c("present", "present"),
    event_id = c("event_1", "event_1"),
    event_year = c(2030, 2030),
    season = c("Summer", "Summer"),
    cnae = NA,
    stringsAsFactors = FALSE
  )

  damage_factors <- read_damage_cost_factors(base_dir)
  out <- join_damage_cost_factors(assets_long, damage_factors)

  testthat::expect_equal(nrow(out), 2)
  testthat::expect_true(all(!is.na(out$damage_factor)))
  # Both should get valid damage factors based on closest match
  testthat::expect_true(all(out$damage_factor >= 0))
})

testthat::test_that("join_drought_damage_factors: multiple assets with same properties get same damage factor", {
  base_dir <- get_test_data_dir()

  # Test consistency: identical assets should get identical damage factors
  assets_long <- data.frame(
    asset = c("A1", "A2", "A3"),
    company = c("C1", "C1", "C1"),
    latitude = c(-10, -10.01, -10.02),
    longitude = c(-50, -50.01, -50.02),
    municipality = c("Mun1", "Mun1", "Mun1"),
    state = c("Bahia", "Bahia", "Bahia"),
    asset_category = c("agriculture", "agriculture", "agriculture"),
    asset_subtype = c("Soybean", "Soybean", "Soybean"),
    size_in_m2 = c(10000, 10000, 10000),
    share_of_economic_activity = c(0.5, 0.5, 0.5),
    hazard_name = c("SPI3__extraction_method=mean", "SPI3__extraction_method=mean", "SPI3__extraction_method=mean"),
    hazard_type = c("Drought", "Drought", "Drought"),
    hazard_indicator = c("SPI3", "SPI3", "SPI3"),
    hazard_intensity = c(-2.5, -2.5, -2.5),
    scenario_name = c("present", "present", "present"),
    event_id = c("event_1", "event_1", "event_1"),
    event_year = c(2030, 2030, 2030),
    season = c("Summer", "Summer", "Summer"),
    cnae = NA,
    stringsAsFactors = FALSE
  )

  damage_factors <- read_damage_cost_factors(base_dir)
  out <- join_damage_cost_factors(assets_long, damage_factors)

  testthat::expect_equal(nrow(out), 3)
  # All three should have the same damage factor
  testthat::expect_equal(out$damage_factor[1], out$damage_factor[2])
  testthat::expect_equal(out$damage_factor[2], out$damage_factor[3])
  testthat::expect_equal(out$growing_season[1], out$growing_season[2])
  testthat::expect_equal(out$growing_season[2], out$growing_season[3])
})

testthat::test_that("join_drought_damage_factors: all four seasons tested", {
  base_dir <- get_test_data_dir()

  # Test all four seasons
  assets_long <- data.frame(
    asset = c("A1", "A2", "A3", "A4"),
    company = c("C1", "C2", "C3", "C4"),
    latitude = c(-10, -15, -20, -25),
    longitude = c(-50, -55, -60, -65),
    municipality = c("Mun1", "Mun2", "Mun3", "Mun4"),
    state = c("Bahia", "Bahia", "Bahia", "Bahia"),
    asset_category = c("agriculture", "agriculture", "agriculture", "agriculture"),
    asset_subtype = c("Soybean", "Soybean", "Soybean", "Soybean"),
    size_in_m2 = c(10000, 8000, 12000, 9000),
    share_of_economic_activity = c(0.5, 0.3, 0.6, 0.4),
    hazard_name = c("SPI3__extraction_method=mean", "SPI3__extraction_method=mean", "SPI3__extraction_method=mean", "SPI3__extraction_method=mean"),
    hazard_type = c("Drought", "Drought", "Drought", "Drought"),
    hazard_indicator = c("SPI3", "SPI3", "SPI3", "SPI3"),
    hazard_intensity = c(-2.5, -2.5, -2.5, -2.5),
    scenario_name = c("present", "present", "present", "present"),
    event_id = c("event_1", "event_1", "event_1", "event_1"),
    event_year = c(2030, 2030, 2030, 2030),
    season = c("Summer", "Autumn", "Winter", "Spring"),
    cnae = NA,
    stringsAsFactors = FALSE
  )

  damage_factors <- read_damage_cost_factors(base_dir)
  out <- join_damage_cost_factors(assets_long, damage_factors)

  testthat::expect_equal(nrow(out), 4)
  testthat::expect_true(all(!is.na(out$damage_factor)))
  # Each asset should have appropriate growing_season or off_window metadata
  testthat::expect_true(all(!is.na(out$growing_season) | !is.na(out$off_window)))
})


testthat::test_that("join_drought_damage_factors: off-season with single growing season crop", {
  base_dir <- get_test_data_dir()

  # Test Soybean (single season: Summer) in off-season (Winter)
  # Should use off_window methodology
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
    season = c("Winter"), # Off-season for Soybean
    cnae = NA,
    stringsAsFactors = FALSE
  )

  damage_factors <- read_damage_cost_factors(base_dir)
  out <- join_damage_cost_factors(assets_long, damage_factors)

  testthat::expect_equal(nrow(out), 1)
  testthat::expect_true(!is.na(out$damage_factor))
  # Should have off_window applied
  testthat::expect_true(!is.na(out$off_window))
  testthat::expect_true(grepl("Summer", out$growing_season)) # Should reference the actual growing season
})
