# Test filtering assets by companies

testthat::test_that("filter_assets_by_companies filters assets correctly", {
  # Create test data
  assets <- data.frame(
    company = c("Company A", "Company B", "Company C", "Company D"),
    asset = c("Asset A1", "Asset B1", "Asset C1", "Asset D1"),
    share_of_economic_activity = c(0.5, 0.3, 0.7, 0.2),
    stringsAsFactors = FALSE
  )
  
  companies <- data.frame(
    company_name = c("Company A", "Company C"),
    revenues = c(1000, 2000),
    stringsAsFactors = FALSE
  )
  
  # Test filtering
  result <- filter_assets_by_companies(assets, companies)
  
  # Should only keep assets from Company A and Company C
  expect_equal(nrow(result), 2)
  expect_true(all(result$company %in% c("Company A", "Company C")))
  expect_equal(sort(result$company), c("Company A", "Company C"))
})

testthat::test_that("filter_assets_by_companies handles empty companies", {
  assets <- data.frame(
    company = c("Company A", "Company B"),
    asset = c("Asset A1", "Asset B1"),
    share_of_economic_activity = c(0.5, 0.3),
    stringsAsFactors = FALSE
  )
  
  companies <- data.frame(
    company_name = character(0),
    revenues = numeric(0),
    stringsAsFactors = FALSE
  )
  
  # Should error when companies data.frame is empty
  expect_error(
    filter_assets_by_companies(assets, companies),
    "companies must be a non-empty data.frame"
  )
})

testthat::test_that("filter_assets_by_companies handles all companies matching", {
  assets <- data.frame(
    company = c("Company A", "Company B"),
    asset = c("Asset A1", "Asset B1"),
    share_of_economic_activity = c(0.5, 0.3),
    stringsAsFactors = FALSE
  )
  
  companies <- data.frame(
    company_name = c("Company A", "Company B"),
    revenues = c(1000, 2000),
    stringsAsFactors = FALSE
  )
  
  # Should keep all assets
  result <- filter_assets_by_companies(assets, companies)
  expect_equal(nrow(result), 2)
  expect_equal(sort(result$company), c("Company A", "Company B"))
})

testthat::test_that("filter_assets_by_companies validates inputs", {
  # Create test data for validation tests
  assets <- data.frame(
    company = c("Company A", "Company B"),
    asset = c("Asset A1", "Asset B1"),
    share_of_economic_activity = c(0.5, 0.3),
    stringsAsFactors = FALSE
  )
  
  companies <- data.frame(
    company_name = c("Company A", "Company B"),
    revenues = c(1000, 2000),
    stringsAsFactors = FALSE
  )
  
  # Test empty assets
  expect_error(
    filter_assets_by_companies(data.frame(), companies),
    "assets must be a non-empty data.frame"
  )
  
  # Test empty companies
  expect_error(
    filter_assets_by_companies(assets, data.frame()),
    "companies must be a non-empty data.frame"
  )
  
  # Test missing company column
  assets_no_company <- data.frame(
    asset = c("Asset A1", "Asset B1"),
    share_of_economic_activity = c(0.5, 0.3),
    stringsAsFactors = FALSE
  )
  
  expect_error(
    filter_assets_by_companies(assets_no_company, companies),
    "assets must contain a 'company' column"
  )
  
  # Test missing company_name column
  companies_no_name <- data.frame(
    revenues = c(1000, 2000),
    stringsAsFactors = FALSE
  )
  
  expect_error(
    filter_assets_by_companies(assets, companies_no_name),
    "companies must contain a 'company_name' column"
  )
})

testthat::test_that("filter_assets_by_companies preserves all asset columns", {
  assets <- data.frame(
    company = c("Company A", "Company B", "Company C"),
    asset = c("Asset A1", "Asset B1", "Asset C1"),
    share_of_economic_activity = c(0.5, 0.3, 0.7),
    sector = c("Energy", "Manufacturing", "Retail"),
    latitude = c(-23.5, -24.0, -25.0),
    stringsAsFactors = FALSE
  )
  
  companies <- data.frame(
    company_name = c("Company A", "Company C"),
    revenues = c(1000, 2000),
    stringsAsFactors = FALSE
  )
  
  result <- filter_assets_by_companies(assets, companies)
  
  # Should preserve all columns
  expect_equal(names(result), names(assets))
  expect_equal(nrow(result), 2)
  
  # Check that all columns are preserved
  expect_true("sector" %in% names(result))
  expect_true("latitude" %in% names(result))
})
