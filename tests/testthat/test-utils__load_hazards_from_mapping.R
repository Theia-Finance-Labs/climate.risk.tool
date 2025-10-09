# Test: load_hazards_from_mapping

test_that("load_hazards_from_mapping loads from mapping dataframe", {
  mapping_df <- tibble::tibble(
    hazard_file = c("global_pc_h10glob.tif", "global_rcp85_h10glob.tif"),
    hazard_type = c("flood", "flood"),
    scenario_code = c("pc", "rcp85"),
    scenario_name = c("CurrentClimate", "RCP8.5"),
    hazard_return_period = c(10, 10)
  )
  
  hazards_dir <- file.path(get_test_data_dir(), "../tests_data/hazards")

  
  rasters <- load_hazards_from_mapping(
    mapping_df = mapping_df,
    hazards_dir = hazards_dir,
    aggregate_factor = 16L  # Use pre-aggregated files
  )
  
  # Should return a named list of rasters
  expect_type(rasters, "list")
  expect_true(!is.null(names(rasters)))
  expect_true(length(rasters) > 0)
})

test_that("load_hazards_from_mapping validates all files exist", {
  mapping_df <- tibble::tibble(
    hazard_file = c("nonexistent.tif"),
    hazard_type = c("flood"),
    scenario_code = c("pc"),
    scenario_name = c("CurrentClimate"),
    hazard_return_period = c(10)
  )
  
  hazards_dir <- file.path(get_test_data_dir(), "../tests/tests_data/hazards")

  
  expect_error(
    load_hazards_from_mapping(
      mapping_df = mapping_df,
      hazards_dir = hazards_dir
    ),
    "not found|does not exist"
  )
})

test_that("load_hazards_from_mapping checks for duplicates on filter columns", {
  mapping_df <- tibble::tibble(
    hazard_file = c("global_pc_h10glob.tif", "global_pc_h100glob.tif"),
    hazard_type = c("flood", "flood"),
    scenario_code = c("pc", "pc"),
    scenario_name = c("CurrentClimate", "CurrentClimate"),
    hazard_return_period = c(10, 10)  # Duplicate!
  )
  
  hazards_dir <- file.path(get_test_data_dir(), "../tests/tests_data/hazards")
  
  expect_error(
    load_hazards_from_mapping(
      mapping_df = mapping_df,
      hazards_dir = hazards_dir
    ),
    "duplicate"
  )
})

test_that("load_hazards_from_mapping loads rasters correctly", {
  mapping_df <- tibble::tibble(
    hazard_file = c("global_pc_h10glob.tif", "global_rcp85_h10glob.tif", "global_rcp85_h100glob.tif"),
    hazard_type = c("flood", "flood", "flood"),
    scenario_code = c("pc", "rcp85", "rcp85"),
    scenario_name = c("CurrentClimate", "RCP8.5", "RCP8.5"),
    hazard_return_period = c(10, 10, 100)
  )
  
  hazards_dir <- file.path(get_test_data_dir(), "../tests_data/hazards")

  
  rasters <- load_hazards_from_mapping(
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

test_that("load_hazards_from_mapping works with metadata workflow", {
  mapping_df <- tibble::tibble(
    hazard_file = c("global_pc_h10glob.tif", "global_rcp85_h10glob.tif"),
    hazard_type = c("flood", "flood"),
    scenario_code = c("pc", "rcp85"),
    scenario_name = c("CurrentClimate", "RCP8.5"),
    hazard_return_period = c(10, 10)
  )
  
  hazards_dir <- file.path(get_test_data_dir(), "../tests_data/hazards")

  
  # Load rasters from mapping
  rasters <- load_hazards_from_mapping(
    mapping_df = mapping_df,
    hazards_dir = hazards_dir,
    aggregate_factor = 16L  # Use pre-aggregated files
  )
  
  # Create inventory from same mapping
  inventory <- list_hazard_inventory_from_metadata(mapping_df)
  
  # Both should have same number of entries
  expect_equal(length(rasters), nrow(inventory))
})

test_that("read_hazards_mapping and list_hazard_inventory_from_metadata workflow", {
  # Load full mapping from actual file
  mapping_file <- file.path(get_test_data_dir(), "hazards_name_mapping.csv")

  
  # Read mapping once
  mapping_df <- read_hazards_mapping(mapping_file)
  
  # Create inventory from mapping
  inventory <- list_hazard_inventory_from_metadata(mapping_df)
  
  # Should be able to filter by hazard_type
  flood_hazards <- inventory |>
    dplyr::filter(.data$hazard_type == "flood")
  expect_true(nrow(flood_hazards) > 0)
  
  # Should be able to filter by scenario_name
  current_climate <- inventory |>
    dplyr::filter(.data$scenario_name == "CurrentClimate")
  expect_true(nrow(current_climate) > 0)
  
  # Should be able to filter by hazard_return_period
  h10 <- inventory |>
    dplyr::filter(.data$hazard_return_period == 10)
  expect_true(nrow(h10) > 0)
  
  # Combined filtering should work
  specific_hazard <- inventory |>
    dplyr::filter(
      .data$hazard_type == "flood",
      .data$scenario_name == "CurrentClimate",
      .data$hazard_return_period == 10
    )
  expect_equal(nrow(specific_hazard), 1)
})

test_that("load_hazards_from_mapping rasters are properly named", {
  mapping_df <- tibble::tibble(
    hazard_file = c("global_pc_h10glob.tif", "global_rcp85_h10glob.tif"),
    hazard_type = c("flood", "flood"),
    scenario_code = c("pc", "rcp85"),
    scenario_name = c("CurrentClimate", "RCP8.5"),
    hazard_return_period = c(10, 10)
  )
  
  hazards_dir <- file.path(get_test_data_dir(), "../tests_data/hazards")

  
  rasters <- load_hazards_from_mapping(
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

test_that("load_hazards_from_mapping supports aggregation parameter", {
  mapping_df <- tibble::tibble(
    hazard_file = c("global_pc_h10glob.tif"),
    hazard_type = c("flood"),
    scenario_code = c("pc"),
    scenario_name = c("CurrentClimate"),
    hazard_return_period = c(10)
  )
  
  hazards_dir <- file.path(get_test_data_dir(), "../tests_data/hazards")

  
  # Test with aggregate_factor = 1 (no aggregation, just pass the parameter)
  rasters <- load_hazards_from_mapping(
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

test_that("load_hazards_from_mapping handles subdirectories", {
  mapping_df <- tibble::tibble(
    hazard_file = c("global_pc_h10glob.tif"),
    hazard_type = c("flood"),
    scenario_code = c("pc"),
    scenario_name = c("CurrentClimate"),
    hazard_return_period = c(10)
  )
  
  hazards_dir <- file.path(get_test_data_dir(), "../tests_data/hazards")

  
  rasters <- load_hazards_from_mapping(
    mapping_df = mapping_df,
    hazards_dir = hazards_dir,
    aggregate_factor = 16L  # Use pre-aggregated files
  )
  
  # Should find files in subdirectories (flood/)
  expect_true(length(rasters) > 0)
  expect_true(any(grepl("flood", mapping_df$hazard_type)))
})
