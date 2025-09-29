testthat::test_that("precompute_assets_factors creates and saves precomputed data", {
  base_dir <- get_test_data_dir()
  assets <- read_assets(base_dir)
  hazards <- load_hazards(file.path(base_dir, "hazards"))
  areas <- load_location_areas(
    file.path(base_dir, "areas", "municipality"),
    file.path(base_dir, "areas", "province")
  )
  damage_factors <- read_damage_cost_factors(base_dir)
  
  # Create temporary hazards directory for testing
  temp_hazards_dir <- file.path(tempdir(), "test_hazards")
  dir.create(temp_hazards_dir, recursive = TRUE)
  on.exit(unlink(temp_hazards_dir, recursive = TRUE), add = TRUE)
  
  # Test precomputation
  precomputed_file <- precompute_assets_factors(
    assets = assets,
    hazards = hazards,
    areas = areas,
    damage_factors = damage_factors,
    hazards_dir = temp_hazards_dir,
    force_recompute = TRUE
  )
  
  # Check that file was created
  testthat::expect_true(file.exists(precomputed_file))
  testthat::expect_equal(precomputed_file, file.path(temp_hazards_dir, "assets_factors_precomputed.rds"))
  
  # Check that file contains valid data
  cached_data <- readRDS(precomputed_file)
  testthat::expect_s3_class(cached_data, "data.frame")
  testthat::expect_true(nrow(cached_data) > 0)
  
  # Should have expected columns from geospatial processing
  expected_cols <- c("asset", "company", "share_of_economic_activity", "hazard_name", "hazard_type", "hazard_intensity")
  testthat::expect_true(all(expected_cols %in% names(cached_data)))
})

testthat::test_that("precompute_assets_factors uses cached data when available", {
  base_dir <- get_test_data_dir()
  assets <- read_assets(base_dir)
  hazards <- load_hazards(file.path(base_dir, "hazards"))
  areas <- load_location_areas(
    file.path(base_dir, "areas", "municipality"),
    file.path(base_dir, "areas", "province")
  )
  damage_factors <- read_damage_cost_factors(base_dir)
  
  # Create temporary hazards directory for testing
  temp_hazards_dir <- file.path(tempdir(), "test_hazards_cached")
  dir.create(temp_hazards_dir, recursive = TRUE)
  on.exit(unlink(temp_hazards_dir, recursive = TRUE), add = TRUE)
  
  # First run - should compute
  precomputed_file <- precompute_assets_factors(
    assets = assets,
    hazards = hazards,
    areas = areas,
    damage_factors = damage_factors,
    hazards_dir = temp_hazards_dir,
    force_recompute = TRUE
  )
  
  # Second run - should use cached data
  precomputed_file2 <- precompute_assets_factors(
    assets = assets,
    hazards = hazards,
    areas = areas,
    damage_factors = damage_factors,
    hazards_dir = temp_hazards_dir,
    force_recompute = FALSE
  )
  
  # Should return same file path
  testthat::expect_equal(precomputed_file, precomputed_file2)
})

testthat::test_that("precompute_assets_factors handles progress callback", {
  base_dir <- get_test_data_dir()
  assets <- read_assets(base_dir)
  hazards <- load_hazards(file.path(base_dir, "hazards"))
  areas <- load_location_areas(
    file.path(base_dir, "areas", "municipality"),
    file.path(base_dir, "areas", "province")
  )
  damage_factors <- read_damage_cost_factors(base_dir)
  
  # Create temporary hazards directory for testing
  temp_hazards_dir <- file.path(tempdir(), "test_hazards_progress")
  dir.create(temp_hazards_dir, recursive = TRUE)
  on.exit(unlink(temp_hazards_dir, recursive = TRUE), add = TRUE)
  
  # Track progress calls
  progress_calls <- list()
  progress_callback <- function(processed, total, message) {
    progress_calls <<- append(progress_calls, list(list(processed = processed, total = total, message = message)))
  }
  
  # Test precomputation with progress callback
  precomputed_file <- precompute_assets_factors(
    assets = assets,
    hazards = hazards,
    areas = areas,
    damage_factors = damage_factors,
    hazards_dir = temp_hazards_dir,
    progress_callback = progress_callback,
    force_recompute = TRUE
  )
  
  # Should have made progress calls
  testthat::expect_true(length(progress_calls) > 0)
  
  # Check that progress values are reasonable
  for (call in progress_calls) {
    testthat::expect_true(call$processed >= 0)
    testthat::expect_true(call$total > 0)
    testthat::expect_true(call$processed <= call$total)
    testthat::expect_true(is.character(call$message))
  }
})

testthat::test_that("load_precomputed_assets_factors loads data correctly", {
  base_dir <- get_test_data_dir()
  assets <- read_assets(base_dir)
  hazards <- load_hazards(file.path(base_dir, "hazards"))
  areas <- load_location_areas(
    file.path(base_dir, "areas", "municipality"),
    file.path(base_dir, "areas", "province")
  )
  damage_factors <- read_damage_cost_factors(base_dir)
  
  # Create temporary hazards directory for testing
  temp_hazards_dir <- file.path(tempdir(), "test_hazards_load")
  dir.create(temp_hazards_dir, recursive = TRUE)
  on.exit(unlink(temp_hazards_dir, recursive = TRUE), add = TRUE)
  
  # Create precomputed file
  precomputed_file <- precompute_assets_factors(
    assets = assets,
    hazards = hazards,
    areas = areas,
    damage_factors = damage_factors,
    hazards_dir = temp_hazards_dir,
    force_recompute = TRUE
  )
  
  # Test loading
  loaded_data <- load_precomputed_assets_factors(precomputed_file)
  
  # Should be a data frame with expected structure
  testthat::expect_s3_class(loaded_data, "data.frame")
  testthat::expect_true(nrow(loaded_data) > 0)
  
  # Should have expected columns
  expected_cols <- c("asset", "company", "share_of_economic_activity", "hazard_name", "hazard_type", "hazard_intensity")
  testthat::expect_true(all(expected_cols %in% names(loaded_data)))
})

testthat::test_that("load_precomputed_assets_factors handles missing file", {
  # Test error handling for missing file
  testthat::expect_error(
    load_precomputed_assets_factors("nonexistent_file.rds"),
    "Precomputed assets factors file not found"
  )
})

testthat::test_that("precompute_assets_factors validates inputs", {
  base_dir <- get_test_data_dir()
  assets <- read_assets(base_dir)
  hazards <- load_hazards(file.path(base_dir, "hazards"))
  areas <- load_location_areas(
    file.path(base_dir, "areas", "municipality"),
    file.path(base_dir, "areas", "province")
  )
  damage_factors <- read_damage_cost_factors(base_dir)
  
  # Create temporary hazards directory for testing
  temp_hazards_dir <- file.path(tempdir(), "test_hazards_validation")
  dir.create(temp_hazards_dir, recursive = TRUE)
  on.exit(unlink(temp_hazards_dir, recursive = TRUE), add = TRUE)
  
  # Test invalid assets
  testthat::expect_error(
    precompute_assets_factors(
      assets = data.frame(),
      hazards = hazards,
      areas = areas,
      damage_factors = damage_factors,
      hazards_dir = temp_hazards_dir
    ),
    "assets must be a non-empty data.frame"
  )
  
  # Test invalid hazards
  testthat::expect_error(
    precompute_assets_factors(
      assets = assets,
      hazards = list(),
      areas = areas,
      damage_factors = damage_factors,
      hazards_dir = temp_hazards_dir
    ),
    "hazards must be a non-empty named list"
  )
  
  # Test invalid areas
  testthat::expect_error(
    precompute_assets_factors(
      assets = assets,
      hazards = hazards,
      areas = list(),
      damage_factors = damage_factors,
      hazards_dir = temp_hazards_dir
    ),
    "areas must be a list with 'municipalities' and 'provinces' elements"
  )
  
  # Test invalid hazards_dir
  testthat::expect_error(
    precompute_assets_factors(
      assets = assets,
      hazards = hazards,
      areas = areas,
      damage_factors = damage_factors,
      hazards_dir = "nonexistent_directory"
    ),
    "hazards_dir must be an existing directory"
  )
})
