make_test_cnae_exposure <- function() {
  data.frame(
    cnae = c("1", "6", "10", "35", "123"),
    description = c("Agriculture", "Oil and Gas Extraction", "Manufacturing", "Hydropower Generation", "Test Sector"),
    stringsAsFactors = FALSE
  )
}

testthat::test_that("validate_input_coherence errors on missing hazards_dir", {
  assets <- data.frame(asset = "A1", company = "C1")
  companies <- data.frame(company = "C1")

  testthat::expect_error(
    validate_input_coherence(
    assets_df = assets,
      companies_df = companies,
      hazards_dir = "missing_dir",
      hazard_configs = list(Flood = list(primary_indicator = "depth", indicators = list(depth = list()))),
      cnae_exposure_df = make_test_cnae_exposure(),
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
      cnae_exposure_df = make_test_cnae_exposure(),
      precomputed_hazards_df = NULL,
      adm1_names = character(0),
      adm2_names = character(0),
      events_df = NULL
    ),
    "hazard_configs is empty"
  )
})

testthat::test_that("validate_input_coherence errors on missing required input columns", {
  assets <- data.frame(asset = "A1", company = "C1")
  companies <- data.frame(company = "C1")

  testthat::expect_error(
    validate_input_coherence(
      assets_df = assets,
      companies_df = companies,
      hazards_dir = get_hazards_dir(),
      hazard_configs = list(Flood = list(primary_indicator = "depth", indicators = list(depth = list()))),
      cnae_exposure_df = make_test_cnae_exposure(),
      precomputed_hazards_df = NULL,
      adm1_names = character(0),
      adm2_names = character(0),
      events_df = NULL
    ),
    "missing required column"
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
    scenario_name = "present",
    return_period = 10,
    event_year = 2030
  )

  # Precomputed data ONLY for Amazonas
  precomputed <- data.frame(
    adm_name = "Amazonas",
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
      cnae_exposure_df = make_test_cnae_exposure(),
      precomputed_hazards_df = precomputed,
      adm1_names = c("Amazonas", "Amapa"),
      adm2_names = c("Manaus"),
      events_df = events
    ),
    "\\[validate_input_coherence\\] All validation checks passed"
  )
})

testthat::test_that("validate_precomputed_hazards_geography prefers ADM identifiers over typoed names", {
  precomputed <- data.frame(
    adm_name = c("Rio Granda do Norte", "Rio de Jeneiro", "Gracho Cardoso"),
    adm_code = c("24", "33", "2802602"),
    shape_id = c(
      "14911670B91095571793858",
      "14911670B85657526756793",
      "56859067B69544192053845"
    ),
    adm_level = c("ADM1", "ADM1", "ADM2"),
    hazard_type = "Flood",
    hazard_indicator = "depth",
    stringsAsFactors = FALSE
  )

  validation <- validate_precomputed_hazards_geography(
    precomputed_hazards_df = precomputed,
    adm1_names = c("Rio Grande do Norte", "Rio de Janeiro"),
    adm2_names = "Gracho Cardoso",
    validation_results = list(errors = character(), warnings = character()),
    adm1_codes = c("24", "33"),
    adm2_codes = "2802602",
    adm1_shape_ids = c("14911670B91095571793858", "14911670B85657526756793"),
    adm2_shape_ids = "56859067B69544192053845"
  )

  testthat::expect_equal(validation$errors, character(0))
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
    scenario_name = "present",
    return_period = 10,
    event_year = 2030
  )

  # Precomputed data ONLY for Amazonas
  precomputed <- data.frame(
    adm_name = "Amazonas",
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
      cnae_exposure_df = make_test_cnae_exposure(),
      precomputed_hazards_df = precomputed,
      adm1_names = c("Amazonas", "Amapa"),
      adm2_names = character(0),
      events_df = events
    ),
    "State 'Amapa' is missing precomputed hazard data for Flood__depth"
  )
})

testthat::test_that("validate_input_coherence accepts IBGE codes for state and municipality", {
  assets <- data.frame(
    asset = "A1",
    company = "C1",
    share_of_economic_activity = 1.0,
    latitude = NA_real_,
    longitude = NA_real_,
    state = "11",
    municipality = "1100023",
    stringsAsFactors = FALSE
  )
  companies <- data.frame(
    company = "C1",
    revenues = 1000,
    debt = 500,
    volatility = 0.2,
    net_profit_margin = 0.1,
    loan_size = 100,
    lgd = 0.4,
    term = 5,
    stringsAsFactors = FALSE
  )

  testthat::expect_message(
    validate_input_coherence(
      assets_df = assets,
      companies_df = companies,
      hazards_dir = get_hazards_dir(),
      hazard_configs = list(Flood = list(primary_indicator = "depth", indicators = list(depth = list(file = "f.tif")))),
      cnae_exposure_df = make_test_cnae_exposure(),
      precomputed_hazards_df = NULL,
      adm1_names = c("Rondonia"),
      adm2_names = c("Ariquemes"),
      events_df = NULL
    ),
    "\\[validate_input_coherence\\] All validation checks passed"
  )
})

testthat::test_that("validate_input_coherence errors when CNAE exposure mapping is missing", {
  assets <- data.frame(
    asset = "A1",
    company = "C1",
    share_of_economic_activity = 1.0,
    stringsAsFactors = FALSE
  )
  companies <- data.frame(
    company = "C1",
    revenues = 1000,
    debt = 500,
    volatility = 0.2,
    net_profit_margin = 0.1,
    loan_size = 100,
    lgd = 0.4,
    term = 5,
    stringsAsFactors = FALSE
  )

  testthat::expect_error(
    validate_input_coherence(
      assets_df = assets,
      companies_df = companies,
      hazards_dir = get_hazards_dir(),
      hazard_configs = list(Flood = list(primary_indicator = "depth", indicators = list(depth = list(file = "f.tif")))),
      cnae_exposure_df = NULL,
      precomputed_hazards_df = NULL,
      adm1_names = character(0),
      adm2_names = character(0),
      events_df = NULL
    ),
    "CNAE exposure mapping is required"
  )
})

testthat::test_that("validate_input_coherence errors on unresolved sector code", {
  assets <- data.frame(
    asset = c("A1", "A2"),
    company = c("C1", "C1"),
    share_of_economic_activity = c(0.5, 0.5),
    sector = c("9999", "35"),
    state = c("11", "11"),
    municipality = c("1100023", "1100023"),
    stringsAsFactors = FALSE
  )
  companies <- data.frame(
    company = "C1",
    revenues = 1000,
    debt = 500,
    volatility = 0.2,
    net_profit_margin = 0.1,
    loan_size = 100,
    lgd = 0.4,
    term = 5,
    stringsAsFactors = FALSE
  )

  cnae_exposure <- data.frame(
    cnae = c("35"),
    description = c("Hydropower Generation"),
    stringsAsFactors = FALSE
  )

  testthat::expect_error(
    validate_input_coherence(
      assets_df = assets,
      companies_df = companies,
      hazards_dir = get_hazards_dir(),
      hazard_configs = list(Flood = list(primary_indicator = "depth", indicators = list(depth = list(file = "f.tif")))),
      cnae_exposure_df = cnae_exposure,
      precomputed_hazards_df = NULL,
      adm1_names = c("Rondonia"),
      adm2_names = c("Ariquemes"),
      events_df = NULL
    ),
    "unresolved sector codes"
  )
})

testthat::test_that("validate_input_coherence passes when sector codes resolve to names", {
  assets <- data.frame(
    asset = c("A1", "A2"),
    company = c("C1", "C1"),
    share_of_economic_activity = c(0.5, 0.5),
    sector = c("06", "35"),
    state = c("11", "11"),
    municipality = c("1100023", "1100023"),
    stringsAsFactors = FALSE
  )
  companies <- data.frame(
    company = "C1",
    revenues = 1000,
    debt = 500,
    volatility = 0.2,
    net_profit_margin = 0.1,
    loan_size = 100,
    lgd = 0.4,
    term = 5,
    stringsAsFactors = FALSE
  )

  cnae_exposure <- data.frame(
    cnae = c("6", "35"),
    description = c("Oil and Gas Extraction", "Hydropower Generation"),
    stringsAsFactors = FALSE
  )

  testthat::expect_message(
    validate_input_coherence(
      assets_df = assets,
      companies_df = companies,
      hazards_dir = get_hazards_dir(),
      hazard_configs = list(Flood = list(primary_indicator = "depth", indicators = list(depth = list(file = "f.tif")))),
      cnae_exposure_df = cnae_exposure,
      precomputed_hazards_df = NULL,
      adm1_names = c("Rondonia"),
      adm2_names = c("Ariquemes"),
      events_df = NULL
    ),
    "\\[validate_input_coherence\\] All validation checks passed"
  )
})
