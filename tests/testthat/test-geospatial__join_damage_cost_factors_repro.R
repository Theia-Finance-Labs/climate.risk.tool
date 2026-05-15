testthat::test_that("join_damage_cost_factors handles Drought agriculture fallback for unknown crop and state", {
  hazard_configs <- load_hazard_configs(get_hazards_dir())

  # Case 1: Unknown crop (banana) and unknown state
  assets_with_events <- tibble::tibble(
    asset = "A9",
    company = "Company 3",
    hazard_type = "Drought",
    hazard_indicator = "standardized_precipitation_index_3",
    spi3 = -1.68,
    scenario_name = "rcp85",
    return_period = 10,
    event_id = "ev1",
    event_year = 2030,
    asset_category = "agriculture",
    asset_subtype = "banana",
    state = "UnknownState",
    season = "Summer"
  )

  joined <- join_damage_cost_factors(
    assets_with_hazards = assets_with_events,
    hazard_configs = hazard_configs,
    hazards_dir = get_hazards_dir()
  )

  # Check if we got a damage factor (currently it should be NA because banana/UnknownState won't match)
  # The goal is to make this test pass with a real damage factor after the fix
  testthat::expect_true("damage_factor" %in% names(joined))
  
  # If the bug exists, this will be NA
  # If the fix works, it should match Soybean/Other/Summer/-1.68
  # Looking at workspace/demo_inputs_refacto/hazards/mappings/drought_factors.csv:
  # 24:-1.68,agriculture,0.3016,Other,Soybean,Summer,0.15
  
  # Current behavior applies fallback crop/state and returns a matched factor
  testthat::expect_false(is.na(joined$damage_factor[1]))
})

testthat::test_that("join_damage_cost_factors handles Drought agriculture fallback for empty crop", {
  hazard_configs <- load_hazard_configs(get_hazards_dir())

  # Case 2: Empty crop (NA)
  assets_with_events <- tibble::tibble(
    asset = "A9",
    company = "Company 3",
    hazard_type = "Drought",
    hazard_indicator = "standardized_precipitation_index_3",
    spi3 = -1.68,
    scenario_name = "rcp85",
    return_period = 10,
    event_id = "ev1",
    event_year = 2030,
    asset_category = "agriculture",
    asset_subtype = NA_character_,
    state = "Rio Grande do Sul",
    season = "Summer"
  )

  joined <- join_damage_cost_factors(
    assets_with_hazards = assets_with_events,
    hazard_configs = hazard_configs,
    hazards_dir = get_hazards_dir()
  )

  # Current behavior applies fallback crop and returns a matched factor
  testthat::expect_false(is.na(joined$damage_factor[1]))
})
