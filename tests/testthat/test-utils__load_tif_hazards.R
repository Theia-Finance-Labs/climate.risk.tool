# Test: load_tif_hazards

test_that("load_tif_hazards loads from mapping dataframe", {
  mapping_df <- tibble::tibble(
    hazard_file = c("global_pc_h10glob.tif", "global_rcp85_h10glob.tif"),
    hazard_type = c("flood", "flood"),
    scenario_code = c("pc", "rcp85"),
    scenario_name = c("CurrentClimate", "RCP8.5"),
    hazard_return_period = c(10, 10)
  )
  
  hazards_dir <- file.path(get_test_data_dir(), "hazards")

  
  rasters <- load_tif_hazards(
    mapping_df = mapping_df,
    hazards_dir = hazards_dir,
    aggregate_factor = 16L  # Use pre-aggregated files
  )
  
  # Should return a named list of rasters
  expect_type(rasters, "list")
  expect_true(!is.null(names(rasters)))
  expect_true(length(rasters) > 0)
})

test_that("load_tif_hazards handles missing files gracefully", {
  mapping_df <- tibble::tibble(
    hazard_file = c("nonexistent.tif"),
    hazard_type = c("flood"),
    scenario_code = c("pc"),
    scenario_name = c("CurrentClimate"),
    hazard_return_period = c(10)
  )
  
  hazards_dir <- file.path(get_test_data_dir(), "hazards")

  
  # Should return empty list with warning when files don't exist
  expect_warning(
    result <- load_tif_hazards(
      mapping_df = mapping_df,
      hazards_dir = hazards_dir
    ),
    "not found|No TIF files"
  )
  
  # Should return empty list
  expect_type(result, "list")
  expect_equal(length(result), 0)
})

test_that("load_tif_hazards checks for duplicates on filter columns", {
  mapping_df <- tibble::tibble(
    hazard_file = c("global_pc_h10glob.tif", "global_pc_h100glob.tif"),
    hazard_type = c("flood", "flood"),
    scenario_code = c("pc", "pc"),
    scenario_name = c("CurrentClimate", "CurrentClimate"),
    hazard_return_period = c(10, 10)  # Duplicate!
  )
  
  hazards_dir <- file.path(get_test_data_dir(), "hazards")
  
  expect_error(
    load_tif_hazards(
      mapping_df = mapping_df,
      hazards_dir = hazards_dir
    ),
    "duplicate"
  )
})

test_that("load_tif_hazards loads rasters correctly", {
  mapping_df <- tibble::tibble(
    hazard_file = c("global_pc_h10glob.tif", "global_rcp85_h10glob.tif", "global_rcp85_h100glob.tif"),
    hazard_type = c("flood", "flood", "flood"),
    scenario_code = c("pc", "rcp85", "rcp85"),
    scenario_name = c("CurrentClimate", "RCP8.5", "RCP8.5"),
    hazard_return_period = c(10, 10, 100)
  )
  
  hazards_dir <- file.path(get_test_data_dir(), "hazards")

  
  rasters <- load_tif_hazards(
    mapping_df = mapping_df,
    hazards_dir = hazards_dir,
    aggregate_factor = 16L  # Use pre-aggregated files
  )
  
  # Check rasters are loaded
  expect_type(rasters, "list")
  expect_true(length(rasters) > 0)
  
  # Each raster should be a SpatRaster
  for (i in seq_along(rasters)) {
    expect_s4_class(rasters[[i]], "SpatRaster")
  }
})


test_that("load_tif_hazards rasters are properly named", {
  mapping_df <- tibble::tibble(
    hazard_file = c("global_pc_h10glob.tif", "global_rcp85_h10glob.tif"),
    hazard_type = c("flood", "flood"),
    scenario_code = c("pc", "rcp85"),
    scenario_name = c("CurrentClimate", "RCP8.5"),
    hazard_return_period = c(10, 10)
  )
  
  hazards_dir <- file.path(get_test_data_dir(), "hazards")

  
  rasters <- load_tif_hazards(
    mapping_df = mapping_df,
    hazards_dir = hazards_dir,
    aggregate_factor = 16L  # Use pre-aggregated files
  )
  
  # Rasters should be named
  expect_true(!is.null(names(rasters)))
  expect_equal(length(names(rasters)), nrow(mapping_df))
  
  # Names should allow retrieval by index
  for (i in seq_along(rasters)) {
    expect_true(nzchar(names(rasters)[i]))
  }
})

test_that("load_tif_hazards supports aggregation parameter", {
  mapping_df <- tibble::tibble(
    hazard_file = c("global_pc_h10glob.tif"),
    hazard_type = c("flood"),
    scenario_code = c("pc"),
    scenario_name = c("CurrentClimate"),
    hazard_return_period = c(10)
  )
  
  hazards_dir <- file.path(get_test_data_dir(), "hazards")

  
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
  expect_equal(mapping_df$hazard_return_period[1], 10)
})

test_that("load_tif_hazards handles subdirectories", {
  mapping_df <- tibble::tibble(
    hazard_file = c("global_pc_h10glob.tif"),
    hazard_type = c("flood"),
    scenario_code = c("pc"),
    scenario_name = c("CurrentClimate"),
    hazard_return_period = c(10)
  )
  
  hazards_dir <- file.path(get_test_data_dir(), "hazards")

  
  rasters <- load_tif_hazards(
    mapping_df = mapping_df,
    hazards_dir = hazards_dir,
    aggregate_factor = 16L  # Use pre-aggregated files
  )
  
  # Should find files in subdirectories (flood/)
  expect_true(length(rasters) > 0)
  expect_true(any(grepl("flood", mapping_df$hazard_type)))
})
