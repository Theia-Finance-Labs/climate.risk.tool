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
    company = c("Company A", "Company C"),
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


testthat::test_that("filter_assets_by_companies handles all companies matching", {
  assets <- data.frame(
    company = c("Company A", "Company B"),
    asset = c("Asset A1", "Asset B1"),
    share_of_economic_activity = c(0.5, 0.3),
    stringsAsFactors = FALSE
  )

  companies <- data.frame(
    company = c("Company A", "Company B"),
    revenues = c(1000, 2000),
    stringsAsFactors = FALSE
  )

  # Should keep all assets
  result <- filter_assets_by_companies(assets, companies)
  expect_equal(nrow(result), 2)
  expect_equal(sort(result$company), c("Company A", "Company B"))
})


testthat::test_that("filter_assets_by_companies preserves all asset columns", {
  assets <- data.frame(
    company = c("Company A", "Company B", "Company C"),
    asset = c("Asset A1", "Asset B1", "Asset C1"),
    share_of_economic_activity = c(0.5, 0.3, 0.7),
    sector = c("47 (Retail)", "47 (Retail)", "47 (Retail)"),
    latitude = c(-23.5, -24.0, -25.0),
    stringsAsFactors = FALSE
  )

  companies <- data.frame(
    company = c("Company A", "Company C"),
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
