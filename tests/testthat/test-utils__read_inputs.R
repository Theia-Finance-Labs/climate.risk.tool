# Tests for functions: read_assets, read_companies

# Contracts:
# - read_assets(base_dir) reads asset Excel under base_dir/user_input/asset_information.xlsx
# - read_companies(file_path) reads company Excel from specified file path
# - All functions parse numeric columns correctly and convert column names to snake_case
# - Return non-empty data frames


testthat::test_that("read_assets returns assets as data.frame", {
  base_dir <- get_test_data_dir()
  assets <- read_assets(base_dir)

  testthat::expect_s3_class(assets, "data.frame")
  testthat::expect_gt(nrow(assets), 0)
})


testthat::test_that("read_assets parses key columns with correct types and snake_case names", {
  base_dir <- get_test_data_dir()
  assets <- read_assets(base_dir)

  # Assets required columns (snake_case)
  req_asset_cols <- c(
    "company", "asset", "share_of_economic_activity",
    "latitude", "longitude", "province", "municipality", "asset_category"
  )
  testthat::expect_true(all(req_asset_cols %in% names(assets)))

  # Types
  testthat::expect_type(assets$company, "character")
  testthat::expect_type(assets$asset, "character")
  testthat::expect_true(is.numeric(assets$share_of_economic_activity))
  testthat::expect_true(is.numeric(assets$latitude))
  testthat::expect_true(is.numeric(assets$longitude))
  testthat::expect_type(assets$province, "character")
  testthat::expect_type(assets$municipality, "character")
  testthat::expect_type(assets$asset_category, "character")
})


testthat::test_that("read_companies returns companies as data.frame", {
  base_dir <- get_test_data_dir()
  companies_path <- file.path(base_dir, "user_input", "company.xlsx")
  companies <- read_companies(companies_path)

  testthat::expect_s3_class(companies, "data.frame")
  testthat::expect_gt(nrow(companies), 0)
})


testthat::test_that("read_companies parses key columns with correct types and snake_case names", {
  base_dir <- get_test_data_dir()
  companies_path <- file.path(base_dir, "user_input", "company.xlsx")
  companies <- read_companies(companies_path)

  # Companies required columns (snake_case)
  req_company_cols <- c(
    "company", "revenues", "debt", "volatility", "net_profit_margin",
    "loan_size", "lgd", "term"
  )
  testthat::expect_true(all(req_company_cols %in% names(companies)))

  # Types
  testthat::expect_type(companies$company, "character")
  testthat::expect_true(is.numeric(companies$revenues))
  testthat::expect_true(is.numeric(companies$debt))
  testthat::expect_true(is.numeric(companies$volatility))
  testthat::expect_true(is.numeric(companies$net_profit_margin))
  testthat::expect_true(is.numeric(companies$loan_size))
  testthat::expect_true(is.numeric(companies$lgd))
  testthat::expect_true(is.numeric(companies$term))
})


testthat::test_that("read_companies handles missing file gracefully", {
  fake_path <- "/nonexistent/path/company.xlsx"
  testthat::expect_error(
    read_companies(fake_path),
    "Company file not found at"
  )
})


testthat::test_that("read_damage_cost_factors returns factors as data.frame", {
  base_dir <- get_test_data_dir()
  factors <- read_damage_cost_factors(base_dir)

  testthat::expect_s3_class(factors, "data.frame")
  testthat::expect_gt(nrow(factors), 0)
})


testthat::test_that("read_damage_cost_factors parses key columns with correct types", {
  base_dir <- get_test_data_dir()
  factors <- read_damage_cost_factors(base_dir)

  # Required columns (should be snake_case after processing)
  req_factor_cols <- c(
    "hazard_intensity", "asset_category",
    "damage_factor", "cost_factor", "hazard_type"
  )
  testthat::expect_true(all(req_factor_cols %in% names(factors)))

  # Types
  testthat::expect_true(is.numeric(factors$hazard_intensity))
  testthat::expect_type(factors$asset_category, "character")
  testthat::expect_true(is.numeric(factors$damage_factor))
  testthat::expect_true(is.numeric(factors$cost_factor))
  testthat::expect_type(factors$hazard_type, "character")
})


testthat::test_that("read_damage_cost_factors handles missing file gracefully", {
  fake_dir <- "/nonexistent/path"
  testthat::expect_error(
    read_damage_cost_factors(fake_dir),
    "Damage and cost factors file not found at"
  )
})


# Tests for function: read_precomputed_hazards

# Contract:
# - read_precomputed_hazards(base_dir) -> data.frame
# - Reads precomputed_adm_hazards.csv from base_dir/
# - Returns data frame with columns: region, adm_level, scenario_code, scenario_name,
#   hazard_return_period, hazard_type, min, max, mean, median, p2_5, p5, p95, p97_5
# - adm_level values: "ADM1" (province), "ADM2" (municipality)
# - Used to look up hazard statistics for assets matched by municipality or province name


testthat::test_that("read_precomputed_hazards loads CSV and returns expected structure", {
  base_dir <- get_test_data_dir()
  precomputed <- read_precomputed_hazards(base_dir)

  # Should return a data frame
  testthat::expect_true(is.data.frame(precomputed))
  testthat::expect_gt(nrow(precomputed), 0)

  # Should have required columns
  required_cols <- c(
    "region", "adm_level", "scenario_code", "scenario_name",
    "hazard_return_period", "hazard_type", "min", "max", "mean",
    "median", "p2_5", "p5", "p95", "p97_5"
  )
  testthat::expect_true(all(required_cols %in% names(precomputed)))

  # adm_level should be ADM1 or ADM2
  testthat::expect_true(all(precomputed$adm_level %in% c("ADM1", "ADM2")))

  # Numeric columns should be numeric
  numeric_cols <- c("min", "max", "mean", "median", "p2_5", "p5", "p95", "p97_5", "hazard_return_period")
  for (col in numeric_cols) {
    testthat::expect_true(is.numeric(precomputed[[col]]))
  }
})


testthat::test_that("read_precomputed_hazards contains both ADM1 and ADM2 data", {
  base_dir <- get_test_data_dir()
  precomputed <- read_precomputed_hazards(base_dir)

  # Should have both province (ADM1) and municipality (ADM2) data
  adm_levels <- unique(precomputed$adm_level)
  testthat::expect_true("ADM1" %in% adm_levels)
  testthat::expect_true("ADM2" %in% adm_levels)
})

