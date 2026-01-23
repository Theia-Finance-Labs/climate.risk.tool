# Tests for functions: read_assets, read_companies

# Contracts:
# - read_assets(folder_path) reads asset Excel from folder_path/asset_information.xlsx
# - read_companies(file_path) reads company Excel from specified file path
# - All functions parse numeric columns correctly and convert column names to snake_case
# - Return non-empty data frames


testthat::test_that("read_assets returns assets as data.frame", {
  base_dir <- get_test_data_dir()
  input_folder <- file.path(base_dir, "user_input")
  assets <- read_assets(input_folder)

  testthat::expect_s3_class(assets, "data.frame")
  testthat::expect_gt(nrow(assets), 0)
})


testthat::test_that("read_assets parses key columns with correct types and snake_case names", {
  base_dir <- get_test_data_dir()
  input_folder <- file.path(base_dir, "user_input")
  assets <- read_assets(input_folder)

  # Assets required columns (snake_case)
  req_asset_cols <- c(
    "company", "asset", "share_of_economic_activity",
    "latitude", "longitude", "state", "municipality", "asset_category"
  )
  testthat::expect_true(all(req_asset_cols %in% names(assets)))

  # Types
  testthat::expect_type(assets$company, "character")
  testthat::expect_type(assets$asset, "character")
  testthat::expect_true(is.numeric(assets$share_of_economic_activity))
  testthat::expect_true(is.numeric(assets$latitude))
  testthat::expect_true(is.numeric(assets$longitude))
  testthat::expect_type(assets$state, "character")
  testthat::expect_type(assets$municipality, "character")
  testthat::expect_type(assets$asset_category, "character")
})


testthat::test_that("read_companies returns companies as data.frame", {
  base_dir <- get_test_data_dir()
  companies_path <- file.path(base_dir, "user_input", "company_information.xlsx")
  companies <- read_companies(companies_path)

  testthat::expect_s3_class(companies, "data.frame")
  testthat::expect_gt(nrow(companies), 0)
})


testthat::test_that("read_companies parses key columns with correct types and snake_case names", {
  base_dir <- get_test_data_dir()
  companies_path <- file.path(base_dir, "user_input", "company_information.xlsx")
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
  fake_path <- "/nonexistent/path/company_information.xlsx"
  testthat::expect_error(
    read_companies(fake_path),
    "Company file not found at"
  )
})



# Tests for function: read_precomputed_hazards

# Contract:
# - read_precomputed_hazards(base_dir) -> data.frame
# - Reads precomputed_adm_hazards.csv from base_dir/
# - Returns data frame with columns: region, adm_level, scenario_code, gwl,
#   return_period, hazard_type, min, max, mean, median, p2_5, p5, p95, p97_5
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
    "region", "adm_level", "gwl",
    "return_period", "hazard_type", "hazard_name",
    "aggregation_method", "hazard_value"
  )
  testthat::expect_true(all(required_cols %in% names(precomputed)))

  # adm_level should be ADM1 or ADM2
  testthat::expect_true(all(precomputed$adm_level %in% c("ADM1", "ADM2")))

  # Numeric columns should be numeric
  # Note: mean, median, p2_5, p5, p95, p97_5 are pivoted into aggregation_method and hazard_value
  numeric_cols <- c("min", "max", "return_period", "hazard_value")
  for (col in numeric_cols) {
    if (col %in% names(precomputed)) {
      testthat::expect_true(is.numeric(precomputed[[col]]))
    }
  }
  
  # aggregation_method should contain the summary statistics
  testthat::expect_true(all(c("mean", "median") %in% unique(precomputed$aggregation_method)))
})


testthat::test_that("read_precomputed_hazards contains both ADM1 and ADM2 data", {
  base_dir <- get_test_data_dir()
  precomputed <- read_precomputed_hazards(base_dir)

  # Should have both province (ADM1) and municipality (ADM2) data
  adm_levels <- unique(precomputed$adm_level)
  testthat::expect_true("ADM1" %in% adm_levels)
  testthat::expect_true("ADM2" %in% adm_levels)
})


testthat::test_that("read_cnae_labor_productivity_exposure returns data.frame", {
  base_dir <- get_test_data_dir()
  cnae_exposure <- read_cnae_labor_productivity_exposure(base_dir)

  testthat::expect_s3_class(cnae_exposure, "data.frame")
  testthat::expect_gt(nrow(cnae_exposure), 0)

  # Check columns
  testthat::expect_true(all(c("cnae", "description", "lp_exposure") %in% names(cnae_exposure)))
  testthat::expect_true(is.numeric(cnae_exposure$cnae))
  testthat::expect_type(cnae_exposure$lp_exposure, "character")
})


testthat::test_that("read_cnae_labor_productivity_exposure handles missing file gracefully", {
  fake_dir <- "/nonexistent/path"
  testthat::expect_error(
    read_cnae_labor_productivity_exposure(fake_dir),
    "CNAE labor productivity exposure file not found at"
  )
})
