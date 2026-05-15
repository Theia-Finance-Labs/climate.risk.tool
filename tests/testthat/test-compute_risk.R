testthat::test_that("compute_risk runs with config-driven hazards", {
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
    season = NA_character_
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

  testthat::expect_true(is.list(results))
  testthat::expect_true(all(c("assets_factors", "companies", "assets_yearly", "companies_yearly") %in% names(results)))
})

testthat::test_that("compute_risk produces stable snapshot output", {
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
    season = NA_character_
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

  testthat::expect_snapshot_value(
    results$companies,
    style = "deparse",
    cran = TRUE
  )
})

testthat::test_that("compute_risk keeps geometry-derived ADM values for geolocated assets", {
  testthat::skip_if_not_installed("sf")

  hazard_data <- load_hazards_and_inventory(
    hazards_dir = get_hazards_dir(),
    hazard_indicators_dir = get_hazard_indicators_dir(),
    aggregate_factor = 16L
  )

  base_dir <- get_test_data_dir()
  assets <- read_assets(get_test_data_dir("user_input")) |>
    dplyr::filter(!is.na(.data$latitude), !is.na(.data$longitude)) |>
    dplyr::slice(1) |>
    dplyr::mutate(
      state = "Fake State",
      state_code = "99",
      state_name = "Fake State",
      municipality = "Fake Municipality",
      municipality_code = "9999999",
      municipality_name = "Fake Municipality"
    )
  testthat::skip_if(nrow(assets) == 0, "No geolocated test assets available")
  companies <- read_companies(get_test_data_dir("user_input"))
  precomputed <- read_precomputed_hazards(base_dir)
  cnae_exposure <- load_mapping_from_config(base_dir, hazard_data$configs, "Heat", "cnae_exposure")

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
    season = NA_character_
  )

  adm1 <- sf::st_read(file.path(base_dir, "areas", "state", "geoBoundaries-BRA-ADM1_simplified.geojson"), quiet = TRUE)
  adm2 <- sf::st_read(file.path(base_dir, "areas", "municipality", "geoBoundaries-BRA-ADM2_simplified.geojson"), quiet = TRUE)

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
    adm1_boundaries = adm1,
    adm2_boundaries = adm2,
    base_dir = base_dir,
    validate_inputs = FALSE,
    growth_rate = 0.02,
    discount_rate = 0.05,
    risk_free_rate = 0.02,
    aggregation_method = "mean"
  )

  out <- results$assets_factors |>
    dplyr::filter(.data$asset == assets$asset[[1]]) |>
    dplyr::slice(1)

  testthat::expect_equal(nrow(out), 1)
  testthat::expect_true(!is.na(out$state_code[[1]]))
  testthat::expect_true(!is.na(out$municipality_code[[1]]))
  testthat::expect_true(out$state_code[[1]] != "99")
  testthat::expect_true(out$municipality_code[[1]] != "9999999")
})
