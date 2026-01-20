# Test: load_tif_hazards

test_that("load_tif_hazards loads from mapping dataframe", {
  mapping_df <- tibble::tibble(
    hazard_file = c("global_rcp85_h10glob.tif", "global_rcp85_h100glob.tif"),
    hazard_type = c("Flood", "Flood"),
    hazard_indicator = c("depth", "depth"),
    gwl = c("rcp85", "rcp85"),
    return_period = c(10, 100)
  )

  hazards_dir <- file.path(get_hazard_indicators_dir(), "flood_depth")


  rasters <- load_tif_hazards(
    mapping_df = mapping_df,
    hazards_dir = hazards_dir,
    aggregate_factor = 16L # Use pre-aggregated files
  )

  # Should return a named list of rasters
  expect_type(rasters, "list")
  expect_true(!is.null(names(rasters)))
  expect_true(length(rasters) > 0)
})


test_that("load_tif_hazards loads rasters correctly", {
  mapping_df <- tibble::tibble(
    hazard_file = c("global_pc_h10glob.tif", "global_rcp85_h10glob.tif", "global_rcp85_h100glob.tif"),
    hazard_type = c("Flood", "Flood", "Flood"),
    hazard_indicator = c("depth", "depth", "depth"),
    gwl = c("present", "rcp85", "rcp85"),
    return_period = c(10, 10, 100)
  )

  hazards_dir <- file.path(get_hazard_indicators_dir(), "flood_depth")


  rasters <- load_tif_hazards(
    mapping_df = mapping_df,
    hazards_dir = hazards_dir,
    aggregate_factor = 16L # Use pre-aggregated files
  )

  # Check rasters are loaded
  expect_type(rasters, "list")
  expect_true(length(rasters) > 0)

  # Each raster should be a SpatRaster
  for (i in seq_along(rasters)) {
    expect_s4_class(rasters[[i]], "SpatRaster")
  }
})


test_that("load_tif_hazards supports aggregation parameter", {
  testthat::skip_on_ci()
  mapping_df <- tibble::tibble(
    hazard_file = c("global_pc_h10glob.tif"),
    hazard_type = c("Flood"),
    hazard_indicator = c("depth"),
    gwl = c("present"),
    return_period = c(10)
  )

  hazards_dir <- file.path(get_hazard_indicators_dir(), "flood_depth")


  # Test with aggregate_factor = 1 (no aggregation, just pass the parameter)
  rasters <- load_tif_hazards(
    mapping_df = mapping_df,
    hazards_dir = hazards_dir,
    aggregate_factor = 1L
  )

  # Should load successfully with aggregation parameter specified
  expect_type(rasters, "list")
  expect_true(length(rasters) > 0)

  # Mapping should still contain return periods (not confused with aggregation)
  expect_equal(mapping_df$return_period[1], 10)
})
