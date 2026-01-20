testthat::test_that("validate_input_coherence errors on missing hazards_dir", {
  assets <- data.frame(asset = "A1", company = "C1")
  companies <- data.frame(company = "C1")

  testthat::expect_error(
    validate_input_coherence(
    assets_df = assets,
    companies_df = companies,
      hazards_dir = "missing_dir",
      hazard_configs = list(Flood = list(primary_indicator = "depth", indicators = list(depth = list()))),
      precomputed_hazards_df = NULL,
      adm1_names = character(0),
      adm2_names = character(0),
      events_df = NULL
    ),
    "hazards_dir does not exist"
  )
})

testthat::test_that("validate_input_coherence errors on empty hazard_configs", {
  assets <- data.frame(asset = "A1", company = "C1")
  companies <- data.frame(company = "C1")

  testthat::expect_error(
    validate_input_coherence(
      assets_df = assets,
      companies_df = companies,
      hazards_dir = get_hazards_dir(),
      hazard_configs = list(),
      precomputed_hazards_df = NULL,
      adm1_names = character(0),
      adm2_names = character(0),
      events_df = NULL
    ),
    "hazard_configs is empty"
  )
})

testthat::test_that("validate_input_coherence does not error when assets with coords are in states missing precomputed data", {
  # Asset with coordinates in a state (e.g., Amapa)
  assets <- data.frame(
    asset = "A1",
    company = "C1",
    latitude = 0.856,
    longitude = -51.2,
    state = "Amapa",
    municipality = NA_character_,
    share_of_economic_activity = 1.0,
    cnae = 123
  )
  companies <- data.frame(
    company = "C1",
    revenues = 1000,
    debt = 500,
    volatility = 0.2,
    net_profit_margin = 0.1,
    loan_size = 100,
    lgd = 0.4,
    term = 5
  )
  events <- data.frame(
    hazard_type = "Flood",
    hazard_indicator = "depth",
    hazard_name = "Flood__depth__GWL=present__RP=10__ensemble=mean",
    gwl = "present",
    return_period = 10,
    event_year = 2030
  )

  # Precomputed data ONLY for Amazonas
  precomputed <- data.frame(
    region = "Amazonas",
    adm_level = "ADM1",
    hazard_type = "Flood",
    hazard_indicator = "depth",
    hazard_name = "Flood__depth__GWL=present__RP=10__ensemble=mean",
    aggregation_method = "mean",
    hazard_value = 10
  )

  # Should pass because A1 has coordinates
  testthat::expect_message(
    validate_input_coherence(
      assets_df = assets,
      companies_df = companies,
      hazards_dir = get_hazards_dir(),
      hazard_configs = list(Flood = list(primary_indicator = "depth", indicators = list(depth = list(file = "f.tif")))),
      precomputed_hazards_df = precomputed,
      adm1_names = c("Amazonas", "Amapa"),
      adm2_names = c("Manaus"),
      events_df = events
    ),
    "\\[validate_input_coherence\\] All validation checks passed"
  )
})

testthat::test_that("validate_input_coherence ERRORS when asset WITHOUT coords is in state missing precomputed data", {
  # Asset WITHOUT coordinates in a state (e.g., Amapa)
  assets <- data.frame(
    asset = "A1",
    company = "C1",
    latitude = NA,
    longitude = NA,
    state = "Amapa",
    municipality = NA_character_,
    share_of_economic_activity = 1.0,
    cnae = 123
  )
  companies <- data.frame(
    company = "C1",
    revenues = 1000,
    debt = 500,
    volatility = 0.2,
    net_profit_margin = 0.1,
    loan_size = 100,
    lgd = 0.4,
    term = 5
  )
  events <- data.frame(
    hazard_type = "Flood",
    hazard_indicator = "depth",
    hazard_name = "Flood__depth__GWL=present__RP=10__ensemble=mean",
    gwl = "present",
    return_period = 10,
    event_year = 2030
  )

  # Precomputed data ONLY for Amazonas
  precomputed <- data.frame(
    region = "Amazonas",
    adm_level = "ADM1",
    hazard_type = "Flood",
    hazard_indicator = "depth",
    hazard_name = "Flood__depth__GWL=present__RP=10__ensemble=mean",
    aggregation_method = "mean",
    hazard_value = 10
  )

  # Should error because A1 lacks coordinates and Amapa is missing from precomputed
  testthat::expect_error(
    validate_input_coherence(
      assets_df = assets,
      companies_df = companies,
      hazards_dir = get_hazards_dir(),
      hazard_configs = list(Flood = list(primary_indicator = "depth", indicators = list(depth = list(file = "f.tif")))),
      precomputed_hazards_df = precomputed,
      adm1_names = c("Amazonas", "Amapa"),
      adm2_names = character(0),
      events_df = events
    ),
    "State 'Amapa' is missing precomputed hazard data for Flood__depth"
  )
})


