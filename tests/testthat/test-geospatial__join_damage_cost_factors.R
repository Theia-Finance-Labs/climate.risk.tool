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

testthat::test_that("join_damage_cost_factors deduplicates drought mapping joins", {
  tmp_dir <- file.path(tempdir(), "hazards")
  hazards_dir <- file.path(tmp_dir, "config")
  mappings_dir <- file.path(tmp_dir, "mappings")
  dir.create(hazards_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(mappings_dir, recursive = TRUE, showWarnings = FALSE)

  mapping_path <- file.path(mappings_dir, "drought_factors.csv")
  mapping_df <- tibble::tibble(
    spi3 = c(-1.5, -1.5),
    asset_subtype = c("Corn", "Corn"),
    state = c("Other", "Other"),
    season = c("Summer", "Summer"),
    damage_factor = c(0.2, 0.4)
  )
  readr::write_csv(mapping_df, mapping_path)

  hazard_configs <- list(
    Drought = list(
      primary_indicator = "standardized_precipitation_index_3",
      index_indicator = "standardized_precipitation_index_3",
      indicators = list(
        standardized_precipitation_index_3 = list(variable = "spi3")
      ),
      mappings = list(
        drought_sensitivity = list(
          file = "drought_factors.csv",
          variables = c("damage_factor"),
          join = list(
            on_indicator_index = c("season"),
            on_assets = c("asset_subtype", "state"),
            on_indicator_intensity = c("spi3")
          )
        )
      )
    )
  )

  assets_with_events <- tibble::tibble(
    asset = "A1",
    company = "C1",
    hazard_type = "Drought",
    hazard_indicator = "standardized_precipitation_index_3",
    spi3 = -1.5,
    season = "Summer",
    scenario_name = "1.5",
    return_period = 5,
    event_id = "ev1",
    event_year = 2030,
    asset_category = "agriculture",
    asset_subtype = "Corn",
    state = "Other"
  )

  joined <- join_damage_cost_factors(
    assets_with_hazards = assets_with_events,
    hazard_configs = hazard_configs,
    hazards_dir = hazards_dir
  )

  testthat::expect_equal(nrow(joined), 1)
  testthat::expect_true("damage_factor" %in% names(joined))
  testthat::expect_equal(joined$damage_factor[1], 0.3)
})


