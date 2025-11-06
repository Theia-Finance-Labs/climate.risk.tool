# Tests for validate_input_coherence and related validation functions

testthat::test_that("validate_input_coherence runs successfully with valid data", {
  base_dir <- get_test_data_dir()

  # Load all data
  assets <- read_assets(base_dir)
  companies <- read_companies(file.path(base_dir, "user_input", "company.xlsx"))
  damage_factors <- read_damage_cost_factors(base_dir)
  cnae_exposure <- read_cnae_labor_productivity_exposure(base_dir)
  precomputed_hazards <- read_precomputed_hazards(base_dir)

  # Load boundary names
  adm1_names <- load_adm1_province_names(base_dir)
  adm2_names <- load_adm2_municipality_names(base_dir)

  # This should complete successfully
  result <- validate_input_coherence(
    assets_df = assets,
    companies_df = companies,
    damage_factors_df = damage_factors,
    precomputed_hazards_df = precomputed_hazards,
    cnae_exposure_df = cnae_exposure,
    adm1_names = adm1_names,
    adm2_names = adm2_names
  )

  testthat::expect_true(is.list(result))
  testthat::expect_true("errors" %in% names(result))
  testthat::expect_true("warnings" %in% names(result))
})

testthat::test_that("load_adm1_province_names loads and normalizes province names", {
  base_dir <- get_test_data_dir()

  province_names <- load_adm1_province_names(base_dir)

  testthat::expect_true(is.character(province_names))
  testthat::expect_gt(length(province_names), 0)

  # Check that names are ASCII-normalized (no accents)
  # All characters should be ASCII (no UTF-8 special chars)
  all_ascii <- vapply(province_names, function(name) all(charToRaw(name) < 128), logical(1))
  testthat::expect_true(all(all_ascii),
    info = paste("Non-ASCII province names:", paste(province_names[!all_ascii], collapse = ", "))
  )

  # Should include known Brazilian states (normalized)
  testthat::expect_true("Sao Paulo" %in% province_names || "So Paulo" %in% province_names)
})

testthat::test_that("load_adm2_municipality_names loads and normalizes municipality names", {
  base_dir <- get_test_data_dir()

  municipality_names <- load_adm2_municipality_names(base_dir)

  testthat::expect_true(is.character(municipality_names))
  testthat::expect_gt(length(municipality_names), 0)

  # Check that names are ASCII-normalized
  all_ascii <- vapply(municipality_names, function(name) all(charToRaw(name) < 128), logical(1))
  testthat::expect_true(all(all_ascii),
    info = paste("Non-ASCII municipality names:", paste(municipality_names[!all_ascii], collapse = ", "))
  )
})

testthat::test_that("validate_damage_factors_provinces detects mismatched provinces", {
  base_dir <- get_test_data_dir()

  # Create test damage factors with a non-existent province
  damage_factors <- data.frame(
    hazard_type = c("Compound", "Compound"),
    province = c("Acre", "FakeProvince"), # "FakeProvince" doesn't exist
    gwl = c("1.5", "1.5"),
    metric = c("median", "median"),
    damage_factor = c(-0.05, -0.06),
    stringsAsFactors = FALSE
  )

  adm1_names <- load_adm1_province_names(base_dir)
  validation_results <- list(errors = character(), warnings = character())

  result <- validate_damage_factors_provinces(damage_factors, adm1_names, validation_results)

  # Should have an error about FakeProvince
  testthat::expect_gt(length(result$errors), 0)
  testthat::expect_true(any(grepl("FakeProvince", result$errors)))
})

testthat::test_that("validate_damage_factors_required_fields passes on reference data", {
  base_dir <- get_test_data_dir()
  damage_factors <- read_damage_cost_factors(base_dir)
  validation_results <- list(errors = character(), warnings = character())

  result <- validate_damage_factors_required_fields(damage_factors, validation_results)

  testthat::expect_equal(length(result$errors), 0)
})

testthat::test_that("validate_damage_factors_required_fields flags missing required columns per hazard", {
  base_dir <- get_test_data_dir()
  damage_factors <- read_damage_cost_factors(base_dir)

  # Pick a hazard_type present in reference data, e.g., Flood, and blank a required col
  idx <- which(damage_factors$hazard_type == "Flood")[1]
  testthat::skip_if(length(idx) == 0)

  damage_factors_bad <- damage_factors
  damage_factors_bad$damage_factor[idx] <- NA_real_

  validation_results <- list(errors = character(), warnings = character())
  result <- validate_damage_factors_required_fields(damage_factors_bad, validation_results)

  testthat::expect_gt(length(result$errors), 0)
  testthat::expect_true(any(grepl("missing required column 'damage_factor'", result$errors)))
})

testthat::test_that("validate_assets_geography detects mismatched provinces", {
  base_dir <- get_test_data_dir()

  # Create test assets with a non-existent province
  assets <- data.frame(
    asset = c("A1", "A2"),
    company = c("C1", "C2"),
    province = c("Acre", "FakeProvince"),
    municipality = c(NA, NA),
    latitude = c(-10, -15),
    longitude = c(-50, -55),
    share_of_economic_activity = c(0.5, 0.5),
    cnae = c(1, 2),
    stringsAsFactors = FALSE
  )

  adm1_names <- load_adm1_province_names(base_dir)
  adm2_names <- load_adm2_municipality_names(base_dir)
  validation_results <- list(errors = character(), warnings = character())

  result <- validate_assets_geography(assets, adm1_names, adm2_names, validation_results)

  # Should have an error about FakeProvince
  testthat::expect_gt(length(result$errors), 0)
  testthat::expect_true(any(grepl("FakeProvince", result$errors)))
})

testthat::test_that("validate_cnae_codes detects invalid CNAE codes", {
  base_dir <- get_test_data_dir()

  # Load real CNAE exposure data
  cnae_exposure <- read_cnae_labor_productivity_exposure(base_dir)

  # Create test assets with an invalid CNAE code
  assets <- data.frame(
    asset = c("A1", "A2"),
    company = c("C1", "C2"),
    cnae = c(1, 99999), # 99999 doesn't exist in reference
    share_of_economic_activity = c(0.5, 0.5),
    stringsAsFactors = FALSE
  )

  validation_results <- list(errors = character(), warnings = character())

  result <- validate_cnae_codes(assets, cnae_exposure, validation_results)

  # Should have an error about CNAE code 99999
  testthat::expect_gt(length(result$errors), 0)
  testthat::expect_true(any(grepl("99999", result$errors)))
})

testthat::test_that("validate_economic_activity_shares detects companies with shares not summing to 1", {
  # Create test assets where Company A sums to 1.0 but Company B sums to 0.5
  assets <- data.frame(
    asset = c("A1", "A2", "B1"),
    company = c("Company A", "Company A", "Company B"),
    share_of_economic_activity = c(0.6, 0.4, 0.5), # A=1.0, B=0.5
    stringsAsFactors = FALSE
  )

  validation_results <- list(errors = character(), warnings = character())

  result <- validate_economic_activity_shares(assets, validation_results)

  # Should have an error about Company B
  testthat::expect_gt(length(result$errors), 0)
  testthat::expect_true(any(grepl("Company B", result$errors)))
  # Should NOT have an error about Company A
  testthat::expect_false(any(grepl("Company A", result$errors)))
})

testthat::test_that("validate_economic_activity_shares allows small tolerance for floating point", {
  # Create test assets where shares sum to 0.999 (within tolerance)
  assets <- data.frame(
    asset = c("A1", "A2", "A3"),
    company = c("Company A", "Company A", "Company A"),
    share_of_economic_activity = c(0.333, 0.333, 0.333), # Sums to 0.999
    stringsAsFactors = FALSE
  )

  validation_results <- list(errors = character(), warnings = character())

  result <- validate_economic_activity_shares(assets, validation_results)

  # Should have NO errors (within 0.01 tolerance)
  testthat::expect_equal(length(result$errors), 0)
})

testthat::test_that("validate_assets_geography flags rows with no geographic information", {
  # Assets without lat/lon, municipality, or province should be flagged
  assets <- data.frame(
    asset = c("A1", "A2"),
    company = c("C1", "C2"),
    latitude = c(NA_real_, NA_real_),
    longitude = c(NA_real_, NA_real_),
    municipality = c(NA_character_, NA_character_),
    province = c(NA_character_, NA_character_),
    share_of_economic_activity = c(0.5, 0.5),
    cnae = c(1, 2),
    stringsAsFactors = FALSE
  )

  base_dir <- get_test_data_dir()
  adm1_names <- load_adm1_province_names(base_dir)
  adm2_names <- load_adm2_municipality_names(base_dir)

  validation_results <- list(errors = character(), warnings = character())
  result <- validate_assets_geography(assets, adm1_names, adm2_names, validation_results)

  testthat::expect_gt(length(result$errors), 0)
})

testthat::test_that("validate_assets_geography detects misspelled/encoded municipality names", {
  # Create assets with a municipality name that is likely to be mis-encoded/misspelled
  # Reference names come from geojson via load_adm2_municipality_names()
  assets <- data.frame(
    asset = c("A1"),
    company = c("C1"),
    municipality = c("S\u00E3o Pa\u00F4lo"), # "São Paôlo" (intentionally odd accents)
    province = c(NA_character_),
    latitude = c(NA_real_),
    longitude = c(NA_real_),
    share_of_economic_activity = c(1.0),
    cnae = c(1),
    stringsAsFactors = FALSE
  )

  base_dir <- get_test_data_dir()
  adm1_names <- load_adm1_province_names(base_dir)
  adm2_names <- load_adm2_municipality_names(base_dir)

  validation_results <- list(errors = character(), warnings = character())
  result <- validate_assets_geography(assets, adm1_names, adm2_names, validation_results)

  # Should raise at least one error due to non-matching municipality spelling/encoding
  testthat::expect_gt(length(result$errors), 0)
})

testthat::test_that("validate_economic_activity_shares flags missing share_of_economic_activity", {
  assets <- data.frame(
    asset = c("A1", "A2"),
    company = c("C1", "C1"),
    share_of_economic_activity = c(NA_real_, 1.0),
    stringsAsFactors = FALSE
  )

  validation_results <- list(errors = character(), warnings = character())
  result <- validate_economic_activity_shares(assets, validation_results)

  testthat::expect_gt(length(result$errors), 0)
})

testthat::test_that("validate_input_coherence stops on error with invalid data", {
  base_dir <- get_test_data_dir()

  # Create assets with invalid data
  assets <- data.frame(
    asset = c("A1"),
    company = c("Company A"),
    province = c("FakeProvince"),
    municipality = c(NA),
    share_of_economic_activity = c(0.5), # Doesn't sum to 1
    cnae = c(99999), # Invalid CNAE
    stringsAsFactors = FALSE
  )

  companies <- read_companies(file.path(base_dir, "user_input", "company.xlsx"))
  damage_factors <- read_damage_cost_factors(base_dir)
  cnae_exposure <- read_cnae_labor_productivity_exposure(base_dir)
  precomputed_hazards <- read_precomputed_hazards(base_dir)
  adm1_names <- load_adm1_province_names(base_dir)
  adm2_names <- load_adm2_municipality_names(base_dir)

  # This SHOULD throw an error
  testthat::expect_error(
    validate_input_coherence(
      assets_df = assets,
      companies_df = companies,
      damage_factors_df = damage_factors,
      precomputed_hazards_df = precomputed_hazards,
      cnae_exposure_df = cnae_exposure,
      adm1_names = adm1_names,
      adm2_names = adm2_names
    ),
    regexp = "validation error"
  )
})

testthat::test_that("validate_companies_against_assets detects companies with no assets", {
  base_dir <- get_test_data_dir()
  assets <- read_assets(base_dir)
  companies <- read_companies(file.path(base_dir, "user_input", "company.xlsx"))

  # Inject a fake company with no assets
  companies <- dplyr::bind_rows(
    companies,
    tibble::tibble(company = "Company Without Assets")
  )

  validation_results <- list(errors = character(), warnings = character())
  result <- validate_companies_against_assets(companies, assets, validation_results)

  testthat::expect_gt(length(result$errors), 0)
  testthat::expect_true(any(grepl("Company Without Assets", result$errors)))
})

testthat::test_that("validate_companies_against_assets flags missing values in companies columns", {
  base_dir <- get_test_data_dir()
  assets <- read_assets(base_dir)

  # Create minimal companies with a missing value
  companies <- tibble::tibble(
    company = c("C1", "C2"),
    revenues = c(1000, NA_real_)
  )

  validation_results <- list(errors = character(), warnings = character())
  result <- validate_companies_against_assets(companies, assets, validation_results)

  testthat::expect_gt(length(result$errors), 0)
  testthat::expect_true(any(grepl("missing values", result$errors)))
})
