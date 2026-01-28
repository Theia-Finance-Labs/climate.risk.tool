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
# - Reads precomputed_adm_indicators.csv from base_dir/hazards/
# - Maps indicator_file/indicator_variable to hazard_type + hazard_indicator from config
# - Returns data frame with columns: region, adm_level, scenario_name, return_period,
#   hazard_type, hazard_indicator, hazard_name, aggregation_method, hazard_value
# - adm_level values: "ADM1" (province), "ADM2" (municipality)
# - Used to look up hazard statistics for assets matched by municipality or province name




testthat::test_that("read_precomputed_hazards contains both ADM1 and ADM2 data", {
  base_dir <- get_test_data_dir()
  precomputed <- read_precomputed_hazards(base_dir)

  # Should have both province (ADM1) and municipality (ADM2) data
  adm_levels <- unique(precomputed$adm_level)
  testthat::expect_true("ADM1" %in% adm_levels)
  testthat::expect_true("ADM2" %in% adm_levels)
})

testthat::test_that("read_precomputed_hazards builds indicator_key with config index dims", {
  base_dir <- get_test_data_dir()
  hazards_dir <- file.path(base_dir, "hazards", "config")
  hazard_configs <- load_hazard_configs(hazards_dir)
  precomputed <- read_precomputed_hazards(base_dir)

  drought_index <- hazard_configs$Drought$indicators$standardized_precipitation_index_3$index
  testthat::expect_true("gwl" %in% drought_index)
  testthat::expect_false("scenario_name" %in% drought_index)

  drought_rows <- precomputed |>
    dplyr::filter(.data$hazard_type == "Drought", .data$hazard_indicator == "standardized_precipitation_index_3")

  testthat::expect_gt(nrow(drought_rows), 0)
  testthat::expect_true(all(grepl("__gwl=", drought_rows$indicator_key)))
  testthat::expect_false(any(grepl("__scenario_name=", drought_rows$indicator_key)))
})


# Tests for function: load_mapping_from_config

# Contract:
# - load_mapping_from_config(base_dir, hazard_configs, hazard_type, mapping_key) -> data.frame
# - Loads mapping tables from hazard config files (generalized replacement for hardcoded readers)
# - Returns tibble with mapping data, columns converted to snake_case


testthat::test_that("load_mapping_from_config loads cnae_exposure from Heat config", {
  base_dir <- get_test_data_dir()
  hazards_dir <- file.path(base_dir, "hazards", "config")
  hazard_configs <- load_hazard_configs(hazards_dir)
  
  cnae_exposure <- load_mapping_from_config(base_dir, hazard_configs, "Heat", "cnae_exposure")

  testthat::expect_s3_class(cnae_exposure, "data.frame")
  testthat::expect_gt(nrow(cnae_exposure), 0)

  # Check columns (snake_case conversion applied)
  testthat::expect_true(all(c("cnae", "description", "lp_exposure") %in% names(cnae_exposure)))
  testthat::expect_true(is.numeric(cnae_exposure$cnae))
  testthat::expect_type(cnae_exposure$lp_exposure, "character")
})


testthat::test_that("load_mapping_from_config loads land_cover_legend from Fire config", {
  base_dir <- get_test_data_dir()
  hazards_dir <- file.path(base_dir, "hazards", "config")
  hazard_configs <- load_hazard_configs(hazards_dir)
  
  legend <- load_mapping_from_config(base_dir, hazard_configs, "Fire", "land_cover_legend")

  testthat::expect_s3_class(legend, "data.frame")
  testthat::expect_gt(nrow(legend), 0)
  
  # Should have land cover related columns (exact names depend on CSV structure)
  testthat::expect_true(any(grepl("land_cover|code|class|category|risk", names(legend), ignore.case = TRUE)))
})


testthat::test_that("load_mapping_from_config errors on missing hazard type", {
  base_dir <- get_test_data_dir()
  hazards_dir <- file.path(base_dir, "hazards", "config")
  hazard_configs <- load_hazard_configs(hazards_dir)
  
  testthat::expect_error(
    load_mapping_from_config(base_dir, hazard_configs, "Nonexistent", "cnae_exposure"),
    "Hazard type 'Nonexistent' not found"
  )
})


testthat::test_that("load_mapping_from_config errors on missing mapping key", {
  base_dir <- get_test_data_dir()
  hazards_dir <- file.path(base_dir, "hazards", "config")
  hazard_configs <- load_hazard_configs(hazards_dir)
  
  testthat::expect_error(
    load_mapping_from_config(base_dir, hazard_configs, "Heat", "nonexistent_mapping"),
    "Mapping 'nonexistent_mapping' not found"
  )
})
