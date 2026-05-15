testthat::test_that("compute_risk keeps excluded assets with spatial status and no synthetic zero exposure", {
  hazard_data <- load_hazards_and_inventory(
    hazards_dir = get_hazards_dir(),
    hazard_indicators_dir = get_hazard_indicators_dir(),
    aggregate_factor = 16L
  )

  assets <- read_assets(get_test_data_dir("user_input"))

  companies <- read_companies(get_test_data_dir("user_input"))
  precomputed <- read_precomputed_hazards(get_test_data_dir())
  cnae_exposure <- load_mapping_from_config(get_test_data_dir(), hazard_data$configs, "Heat", "cnae_exposure")

  flood_row <- hazard_data$inventory |>
    dplyr::filter(.data$hazard_type == "Flood") |>
    dplyr::slice(1)

  events <- tibble::tibble(
    event_id = "ev1",
    hazard_type = "Flood",
    hazard_indicator = flood_row$hazard_indicator,
    hazard_name = flood_row$hazard_name,
    scenario_name = flood_row$scenario_name,
    return_period = flood_row$return_period,
    event_year = 2030,
    season = NA_character_,
    spatial_scheme = "adm_regions",
    spatial_level = "state",
    spatial_region_codes = "00",
    spatial_region_labels = "NoState"
  )

  results <- compute_risk(
    assets = assets,
    companies = companies,
    events = events,
    hazards = hazard_data$hazards,
    hazards_inventory = hazard_data$inventory,
    precomputed_hazards = precomputed,
    hazard_configs = hazard_data$configs,
    hazards_dir = get_hazards_dir(),
    cnae_exposure = cnae_exposure,
    validate_inputs = FALSE,
    growth_rate = 0.02,
    discount_rate = 0.05,
    risk_free_rate = 0.02,
    aggregation_method = "mean"
  )

  testthat::expect_true("spatial_exposure_status" %in% names(results$assets_factors))
  testthat::expect_gt(nrow(results$assets_factors), 0)
  testthat::expect_true(all(results$assets_factors$spatial_exposure_status == spatial_status_not_exposed()))

  if ("flood_depth_cm" %in% names(results$assets_factors)) {
    testthat::expect_true(all(is.na(results$assets_factors$flood_depth_cm)))
  }
})

testthat::test_that("compute_risk uses geometry fallback for hydro municipality separation without overlaps tables", {
  testthat::skip_if_not_installed("sf")

  hazard_data <- load_hazards_and_inventory(
    hazards_dir = get_hazards_dir(),
    hazard_indicators_dir = get_hazard_indicators_dir(),
    aggregate_factor = 16L
  )

  companies <- read_companies(get_test_data_dir("user_input")) |>
    dplyr::slice(1)

  assets <- read_assets(get_test_data_dir("user_input")) |>
    dplyr::slice(1) |>
    dplyr::mutate(
      company = companies$company[[1]],
      asset = "A_FALLBACK",
      latitude = NA_real_,
      longitude = NA_real_,
      state = "Amazonas",
      state_code = "13",
      state_name = "Amazonas",
      municipality = "Eirunepe",
      municipality_code = "1301407",
      municipality_name = "Eirunepe",
      share_of_economic_activity = 1
    )

  precomputed <- read_precomputed_hazards(get_test_data_dir())
  cnae_exposure <- load_mapping_from_config(get_test_data_dir(), hazard_data$configs, "Heat", "cnae_exposure")

  flood_row <- hazard_data$inventory |>
    dplyr::filter(.data$hazard_type == "Flood") |>
    dplyr::slice(1)

  municipality_poly <- sf::st_polygon(list(rbind(
    c(0, 0), c(10, 0), c(10, 10), c(0, 10), c(0, 0)
  )))
  meso_1 <- sf::st_polygon(list(rbind(
    c(0, 0), c(3, 0), c(3, 10), c(0, 10), c(0, 0)
  )))
  meso_2 <- sf::st_polygon(list(rbind(
    c(3, 0), c(9.99, 0), c(9.99, 10), c(3, 10), c(3, 0)
  )))

  spatial_data <- list(
    adm = list(
      state = sf::st_sf(
        region_code = "13",
        region_label = "Amazonas",
        geometry = sf::st_sfc(municipality_poly, crs = 4326)
      ),
      municipality = sf::st_sf(
        region_code = "1301407",
        region_label = "Eirunepe",
        geometry = sf::st_sfc(municipality_poly, crs = 4326)
      )
    ),
    hydro = list(
      macro = NULL,
      meso = sf::st_sf(
        region_code = c("M1", "M2"),
        region_label = c("Meso 1", "Meso 2"),
        geometry = sf::st_sfc(meso_1, meso_2, crs = 4326)
      ),
      micro = NULL
    ),
    overlaps = list(),
    lookup = list(
      state_name_to_code = c(amazonas = "13"),
      municipality_name_to_code = c(eirunepe = "1301407")
    )
  )

  events <- tibble::tibble(
    event_id = "ev1",
    hazard_type = "Flood",
    hazard_indicator = flood_row$hazard_indicator,
    hazard_name = flood_row$hazard_name,
    scenario_name = flood_row$scenario_name,
    return_period = flood_row$return_period,
    event_year = 2030,
    season = NA_character_,
    spatial_scheme = "hydro_regions",
    spatial_level = "meso",
    spatial_region_codes = "M1|M2",
    spatial_region_labels = "Meso 1|Meso 2"
  )

  warnings <- character()
  results <- withCallingHandlers(
    compute_risk(
      assets = assets,
      companies = companies,
      events = events,
      hazards = hazard_data$hazards,
      hazards_inventory = hazard_data$inventory,
      precomputed_hazards = precomputed,
      hazard_configs = hazard_data$configs,
      hazards_dir = get_hazards_dir(),
      cnae_exposure = cnae_exposure,
      spatial_separation_data = spatial_data,
      validate_inputs = FALSE,
      growth_rate = 0.02,
      discount_rate = 0.05,
      risk_free_rate = 0.02,
      aggregation_method = "mean"
    ),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  testthat::expect_true(any(grepl("Hydro overlap table for municipality -> meso is missing or incomplete", warnings)))
  fallback_row <- results$assets_factors |>
    dplyr::filter(.data$asset == "A_FALLBACK", .data$event_id == "ev1") |>
    dplyr::slice(1)

  testthat::expect_equal(nrow(fallback_row), 1)
  testthat::expect_gt(fallback_row$spatial_multiplier[[1]], 0.99)
  testthat::expect_lt(fallback_row$spatial_multiplier[[1]], 1)
  testthat::expect_true(is.na(fallback_row$spatial_exposure_status[[1]]))
})
