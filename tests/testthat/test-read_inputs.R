# Tests for function: read_inputs

# Contract:
# - read_inputs(base_dir) reads CSVs under base_dir/user_input/
# - Returns list(assets=data.frame, companies=data.frame)
# - Parses numeric columns correctly; converts column names to snake_case
# - Non-empty data frames


testthat::test_that("read_inputs returns assets and companies as data.frames", {
  base_dir <- get_test_data_dir()
  res <- read_inputs(base_dir)

  testthat::expect_type(res, "list")
  testthat::expect_true(all(c("assets", "companies") %in% names(res)))
  testthat::expect_s3_class(res$assets, "data.frame")
  testthat::expect_s3_class(res$companies, "data.frame")
  testthat::expect_gt(nrow(res$assets), 0)
  testthat::expect_gt(nrow(res$companies), 0)
})


testthat::test_that("read_inputs parses key columns with correct types and snake_case names", {
  base_dir <- get_test_data_dir()
  res <- read_inputs(base_dir)
  assets <- res$assets
  companies <- res$companies

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

  # Companies required columns (snake_case)
  req_company_cols <- c(
    "company_name", "revenues", "debt", "volatility", "net_profit_margin",
    "loan_size", "lgd", "term"
  )
  testthat::expect_true(all(req_company_cols %in% names(companies)))

  # Types
  testthat::expect_type(companies$company_name, "character")
  testthat::expect_true(is.numeric(companies$revenues))
  testthat::expect_true(is.numeric(companies$debt))
  testthat::expect_true(is.numeric(companies$volatility))
  testthat::expect_true(is.numeric(companies$net_profit_margin))
  testthat::expect_true(is.numeric(companies$loan_size))
  testthat::expect_true(is.numeric(companies$lgd))
  testthat::expect_true(is.numeric(companies$term))
})
