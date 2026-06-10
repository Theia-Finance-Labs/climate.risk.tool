testthat::test_that("join_damage_cost_factors joins flood mapping tables", {
  hazard_configs <- load_hazard_configs(get_hazards_dir())

  assets_with_events <- tibble::tibble(
    asset = "A1",
    company = "C1",
    hazard_type = "Flood",
    hazard_indicator = "flood_depth",
    flood_depth_cm = 12.0,
    scenario_name = "rcp85",
    return_period = 10,
    event_id = "ev1",
    event_year = 2030,
    asset_category = "commercial building"
  )

  joined <- join_damage_cost_factors(
    assets_with_hazards = assets_with_events,
    hazard_configs = hazard_configs,
    hazards_dir = get_hazards_dir()
  )

  testthat::expect_true("damage_factor" %in% names(joined))
  testthat::expect_equal(joined$damage_factor[1], 0.1464, tolerance = 0.0001)
})

testthat::test_that("join_damage_cost_factors prefers asset cost_factor override", {
  hazard_configs <- load_hazard_configs(get_hazards_dir())

  assets_with_events <- tibble::tibble(
    asset = c("A1", "A2"),
    company = c("C1", "C1"),
    hazard_type = c("Flood", "Flood"),
    hazard_indicator = c("flood_depth", "flood_depth"),
    flood_depth_cm = c(12.0, 12.0),
    scenario_name = c("rcp85", "rcp85"),
    return_period = c(10, 10),
    event_id = c("ev1", "ev1"),
    event_year = c(2030, 2030),
    asset_category = c("commercial building", "commercial building"),
    cost_factor = c(999, NA_real_)
  )

  joined <- join_damage_cost_factors(
    assets_with_hazards = assets_with_events,
    hazard_configs = hazard_configs,
    hazards_dir = get_hazards_dir()
  )

  joined <- joined |>
    dplyr::arrange(.data$asset)

  testthat::expect_equal(joined$cost_factor[1], 999)
  testthat::expect_equal(joined$cost_factor[2], 838625.370183151, tolerance = 0.0001)
})

testthat::test_that("join_damage_cost_factors combines fire indicators", {
  hazard_configs <- load_hazard_configs(get_hazards_dir())

  assets_with_events <- tibble::tibble(
    asset = c("A1", "A1"),
    company = c("C1", "C1"),
    hazard_type = c("Fire", "Fire"),
    hazard_indicator = c("fire_weather_index", "land_cover"),
    fwi = c(5, NA_real_),
    land_cover = c(NA_real_, 1),
    scenario_name = c("present", "present"),
    return_period = c(10, 10),
    event_id = c("ev1", "ev1"),
    event_year = c(2030, 2030),
    asset_category = c("commercial building", "commercial building")
  )

  joined <- join_damage_cost_factors(
    assets_with_hazards = assets_with_events,
    hazard_configs = hazard_configs,
    hazards_dir = get_hazards_dir()
  )

  testthat::expect_true("damage_factor" %in% names(joined))
  testthat::expect_true("land_cover_risk" %in% names(joined))
  testthat::expect_equal(joined$land_cover_risk[1], 0.5)
})

testthat::test_that("join_damage_cost_factors applies Drought assets_fallbacks for unknown crop and state", {
  hazard_configs <- load_hazard_configs(get_hazards_dir())

  assets_with_events <- tibble::tibble(
    asset = c("A1", "A2"),
    company = c("C1", "C1"),
    hazard_type = c("Drought", "Drought"),
    hazard_indicator = c("standardized_precipitation_index_3", "standardized_precipitation_index_3"),
    spi3 = c(-1.68, -1.68),
    scenario_name = c("present", "present"),
    return_period = c(10, 10),
    event_id = c("ev1", "ev2"),
    event_year = c(2030, 2031),
    asset_category = c("agriculture", "agriculture"),
    asset_subtype = c("banana", NA_character_),
    state = c("UnknownState", "Rio Grande do Sul"),
    season = c("Summer", "Summer")
  )

  joined <- join_damage_cost_factors(
    assets_with_hazards = assets_with_events,
    hazard_configs = hazard_configs,
    hazards_dir = get_hazards_dir()
  )

  testthat::expect_true("damage_factor" %in% names(joined))
  testthat::expect_equal(joined$damage_factor[1], 0.3016, tolerance = 0.0001)
  testthat::expect_equal(joined$damage_factor[2], 0.52, tolerance = 0.0001)
})

testthat::test_that("join_damage_cost_factors handles Drought off_window when event season doesn't match crop season", {
  hazard_configs <- load_hazard_configs(get_hazards_dir())

  assets_with_events <- tibble::tibble(
    asset = c("A1", "A2"),
    company = c("C1", "C1"),
    hazard_type = c("Drought", "Drought"),
    hazard_indicator = c("standardized_precipitation_index_3", "standardized_precipitation_index_3"),
    spi3 = c(-1.1, -1.1),
    scenario_name = c("present", "present"),
    return_period = c(10, 10),
    event_id = c("ev1", "ev2"),
    event_year = c(2030, 2031),
    asset_category = c("agriculture", "agriculture"),
    asset_subtype = c("Soybean", "Soybean"),
    state = c("Rio Grande do Sul", "Rio Grande do Sul"),
    season = c("Summer", "Winter")
  )

  joined <- join_damage_cost_factors(
    assets_with_hazards = assets_with_events,
    hazard_configs = hazard_configs,
    hazards_dir = get_hazards_dir()
  )

  testthat::expect_true("damage_factor" %in% names(joined))
  
  # Asset 1: Summer matches the crop's growing season (Summer) -> use damage_factor directly
  # From drought_factors.csv: Rio Grande do Sul, Soybean, Summer, spi3=-1.1 -> damage_factor = 0.375
  testthat::expect_equal(joined$damage_factor[1], 0.375, tolerance = 0.0001)
  
  # Asset 2: Winter doesn't match -> apply off_window logic
  # Should average all growing seasons' damage_factors * off_window
  # For Rio Grande do Sul Soybean at spi3=-1.1 in Summer: damage_factor=0.375, off_window=0.15
  # Expected: 0.375 * 0.15 = 0.05625
  testthat::expect_equal(joined$damage_factor[2], 0.05625, tolerance = 0.0001)
})
