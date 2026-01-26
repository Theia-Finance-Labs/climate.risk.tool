testthat::test_that("join_damage_cost_factors joins flood mapping tables", {
  hazard_configs <- load_hazard_configs(get_hazards_dir())

  assets_with_events <- tibble::tibble(
    asset = "A1",
    company = "C1",
    hazard_type = "Flood",
    hazard_indicator = "depth",
    depth = 12.0,
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

testthat::test_that("join_damage_cost_factors combines fire indicators", {
  hazard_configs <- load_hazard_configs(get_hazards_dir())

  assets_with_events <- tibble::tibble(
    asset = c("A1", "A1"),
    company = c("C1", "C1"),
    hazard_type = c("Fire", "Fire"),
    hazard_indicator = c("fwi", "land_cover"),
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


