# Tests for agriculture portfolio edge cases in read_assets

create_temp_user_input_dir <- function() {
  base_dir <- tempfile("read_assets_base_")
  dir.create(base_dir)
  dir.create(file.path(base_dir, "user_input"))
  dir.create(file.path(base_dir, "areas"))
  file.copy(
    file.path(get_test_data_dir(), "areas", "brazil_adm_codes.csv"),
    file.path(base_dir, "areas", "brazil_adm_codes.csv")
  )
  list(
    base_dir = base_dir,
    user_input_dir = file.path(base_dir, "user_input")
  )
}

testthat::test_that("read_assets handles missing municipality column gracefully", {
  tmp <- create_temp_user_input_dir()
  on.exit(unlink(tmp$base_dir, recursive = TRUE), add = TRUE)
  temp_file <- file.path(tmp$user_input_dir, "asset_information.xlsx")
  
  # Create test data without municipality column
  test_data <- tibble::tibble(
    Company = "Test Company",
    Asset = "Test Asset",
    `Asset Category` = "agriculture",
    `Asset Subtype` = "Soybean",
    State = "Amazonas",
    Latitude = -3.0,
    Longitude = -60.0,
    `Share of Economic Activity` = 0.5,
    `Size in m2` = 100000
  )
  
  writexl::write_xlsx(test_data, temp_file)
  
  # This should not error - should handle missing municipality column
  assets <- read_assets(tmp$user_input_dir)
  
  # Verify the result
  testthat::expect_s3_class(assets, "data.frame")
  testthat::expect_equal(nrow(assets), 1)
  # municipality column should not exist or be NA
  if ("municipality" %in% names(assets)) {
    testthat::expect_true(all(is.na(assets$municipality)))
  }
  
})


testthat::test_that("read_assets handles all-NA municipality column gracefully", {
  tmp <- create_temp_user_input_dir()
  on.exit(unlink(tmp$base_dir, recursive = TRUE), add = TRUE)
  temp_file <- file.path(tmp$user_input_dir, "asset_information.xlsx")
  
  # Create test data with empty municipality column
  test_data <- tibble::tibble(
    Company = "Test Company",
    Asset = "Test Asset",
    `Asset Category` = "agriculture",
    `Asset Subtype` = "Soybean",
    Municipality = NA_character_,  # All NA
    State = "Amazonas",
    Latitude = -3.0,
    Longitude = -60.0,
    `Share of Economic Activity` = 0.5,
    `Size in m2` = 100000
  )
  
  writexl::write_xlsx(test_data, temp_file)
  
  # This should not error - should handle all-NA municipality column
  assets <- read_assets(tmp$user_input_dir)
  
  # Verify the result
  testthat::expect_s3_class(assets, "data.frame")
  testthat::expect_equal(nrow(assets), 1)
  testthat::expect_type(assets$municipality, "character")
  testthat::expect_true(all(is.na(assets$municipality)))
  
})


testthat::test_that("read_assets handles empty string municipality column gracefully", {
  tmp <- create_temp_user_input_dir()
  on.exit(unlink(tmp$base_dir, recursive = TRUE), add = TRUE)
  temp_file <- file.path(tmp$user_input_dir, "asset_information.xlsx")
  
  # Create test data with empty municipality column
  test_data <- tibble::tibble(
    Company = "Test Company",
    Asset = "Test Asset",
    `Asset Category` = "agriculture",
    `Asset Subtype` = "Soybean",
    Municipality = "",  # Empty string
    State = "Amazonas",
    Latitude = -3.0,
    Longitude = -60.0,
    `Share of Economic Activity` = 0.5,
    `Size in m2` = 100000
  )
  
  writexl::write_xlsx(test_data, temp_file)
  
  # This should not error - should convert empty strings to NA
  assets <- read_assets(tmp$user_input_dir)
  
  # Verify the result
  testthat::expect_s3_class(assets, "data.frame")
  testthat::expect_equal(nrow(assets), 1)
  testthat::expect_type(assets$municipality, "character")
  testthat::expect_true(all(is.na(assets$municipality)))
  
})

