# Tests for functions: load_location_areas, load_municipalities and load_provinces

# Contract:
# - load_location_areas(municipalities_dir, provinces_dir) loads both areas and returns list with municipalities and provinces
# - load_municipalities(municipalities_dir) reads all geojson files from municipalities directory
# - load_provinces(provinces_dir) reads all geojson files from provinces directory
# - All return named lists of sf objects with length >= 1
# - Names are derived from basenames without extension


testthat::test_that("load_location_areas loads both municipalities and provinces", {
  base_dir <- get_test_data_dir()
  municipalities <- load_municipalities(file.path(base_dir, "areas", "municipality"))
  provinces <- load_provinces(file.path(base_dir, "areas", "province"))

  base_dir <- get_test_data_dir()
  municipalities_dir <- file.path(base_dir, "areas", "municipality")
  provinces_dir <- file.path(base_dir, "areas", "province")

  areas <- load_location_areas(municipalities_dir, provinces_dir)

  testthat::expect_type(areas, "list")
  testthat::expect_true(all(c("municipalities", "provinces") %in% names(areas)))

  # Check municipalities
  testthat::expect_type(areas$municipalities, "list")
  testthat::expect_gt(length(areas$municipalities), 0)
  testthat::expect_true(!is.null(names(areas$municipalities)))

  # Check provinces
  testthat::expect_type(areas$provinces, "list")
  testthat::expect_gt(length(areas$provinces), 0)
  testthat::expect_true(!is.null(names(areas$provinces)))
})


testthat::test_that("load_municipalities loads geojson files and returns a named list", {
  base_dir <- get_test_data_dir()
  municipalities <- load_municipalities(file.path(base_dir, "areas", "municipality"))

  testthat::expect_type(municipalities, "list")
  testthat::expect_gt(length(municipalities), 0)
  testthat::expect_true(!is.null(names(municipalities)))
  testthat::expect_true(all(nzchar(names(municipalities))))
})


testthat::test_that("load_municipalities list elements are sf objects", {
  base_dir <- get_test_data_dir()
  municipalities <- load_municipalities(file.path(base_dir, "areas", "municipality"))

  # Check that first element is an sf object
  first <- municipalities[[1]]
  testthat::expect_s3_class(first, "sf")
  testthat::expect_true("geometry" %in% names(first))
})


testthat::test_that("load_provinces loads geojson files and returns a named list", {
  base_dir <- get_test_data_dir()
  provinces <- load_provinces(file.path(base_dir, "areas", "province"))

  testthat::expect_type(provinces, "list")
  testthat::expect_gt(length(provinces), 0)
  testthat::expect_true(!is.null(names(provinces)))
  testthat::expect_true(all(nzchar(names(provinces))))
})


testthat::test_that("load_provinces list elements are sf objects", {
  base_dir <- get_test_data_dir()
  provinces <- load_provinces(file.path(base_dir, "areas", "province"))

  # Check that first element is an sf object
  first <- provinces[[1]]
  testthat::expect_s3_class(first, "sf")
  testthat::expect_true("geometry" %in% names(first))
})
