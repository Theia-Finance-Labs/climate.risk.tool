testthat::test_that("compute_risk runs with config-driven hazards", {
  hazard_data <- load_hazards_and_inventory(
    hazards_dir = get_hazards_dir(),
    hazard_indicators_dir = get_hazard_indicators_dir(),
    aggregate_factor = 16L
  )

  assets <- read_assets(get_test_data_dir("user_input"))
  companies <- read_companies(get_test_data_dir("user_input"))
  precomputed <- read_precomputed_hazards(get_test_data_dir())

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
