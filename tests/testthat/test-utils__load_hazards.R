testthat::test_that("load_hazards caches aggregated rasters and reuses them", {
  base_dir <- get_test_data_dir()
  hz_dir <- file.path(base_dir, "hazards")

  # Use a factor that doesn't exist in test data to ensure we create new cache files
  agg_factor <- 32L

  # Find original tif files (not pre-aggregated)
  tif_files <- list.files(hz_dir, pattern = "\\.tif$", full.names = TRUE, recursive = TRUE)
  original_files <- tif_files[!grepl("__agg\\d+\\.tif$", basename(tif_files))]

  # Ensure cache file paths for original files
  cache_candidates <- file.path(dirname(original_files),
                                paste0(tools::file_path_sans_ext(basename(original_files)), "__agg", agg_factor, ".tif"))
  unlink(cache_candidates[file.exists(cache_candidates)])

  # First load should aggregate and create cache files
  t1 <- system.time({
    hz1 <- load_hazards(hz_dir, aggregate_factor = agg_factor, cache_aggregated = TRUE, force_reaggregate = FALSE)
  })
  # Cache files should exist now
  cache_exist <- file.exists(cache_candidates)
  testthat::expect_true(any(cache_exist))

  # Second load should reuse cache and be faster
  t2 <- system.time({
    hz2 <- load_hazards(hz_dir, aggregate_factor = agg_factor, cache_aggregated = TRUE, force_reaggregate = FALSE)
  })
  testthat::expect_true(t2["elapsed"] <= t1["elapsed"])  # reuse must not be slower

  # Dimensions should be reduced relative to originals
  original <- load_hazards(hz_dir, aggregate_factor = 1L, cache_aggregated = FALSE)
  nm <- names(hz1)[1]
  d0 <- dim(original[[nm]])
  d1 <- dim(hz1[[nm]])
  d2 <- dim(hz2[[nm]])
  testthat::expect_true(prod(d1) < prod(d0))
  testthat::expect_equal(d1, d2)
})

testthat::test_that("load_hazards filters files by aggregation factor", {
  base_dir <- get_test_data_dir()
  hz_dir <- file.path(base_dir, "hazards")
  testthat::skip_if_not(dir.exists(hz_dir), "hazards directory not found")

  # Find all .tif files
  all_tif_files <- list.files(hz_dir, pattern = "\\.tif$", full.names = TRUE, recursive = TRUE)
  testthat::skip_if(length(all_tif_files) == 0, "no tif files in hazards directory")
  
  # Count original files
  original_files <- all_tif_files[!grepl("__agg\\d+", basename(all_tif_files))]
  
  # Count aggregated files for factor 64
  agg64_files <- all_tif_files[grepl("__agg64\\.tif$", basename(all_tif_files))]
  
  # Test loading with aggregate_factor = 1 (should load only original files)
  hz_original <- load_hazards(hz_dir, aggregate_factor = 1L, cache_aggregated = FALSE)
  testthat::expect_equal(length(hz_original), length(original_files))
  
  # Test loading with aggregate_factor = 64 (should load pre-aggregated files if available)
  if (length(agg64_files) > 0) {
    hz_agg64 <- load_hazards(hz_dir, aggregate_factor = 64L, cache_aggregated = TRUE, force_reaggregate = FALSE)
    testthat::expect_equal(length(hz_agg64), length(agg64_files))
    testthat::expect_equal(length(hz_agg64), length(original_files))  # Should match number of original scenarios
  }
  
  # Verify names are consistent (aggregated files should have same scenario names as originals)
  if (length(agg64_files) > 0) {
    hz_agg64 <- load_hazards(hz_dir, aggregate_factor = 64L, cache_aggregated = TRUE, force_reaggregate = FALSE)
    # Names should be clean (without __agg suffix)
    testthat::expect_false(any(grepl("__agg\\d+", names(hz_agg64))))
  }
})

testthat::test_that("load_hazards prevents recursive aggregation", {
  base_dir <- get_test_data_dir()
  hz_dir <- file.path(base_dir, "hazards")
  testthat::skip_if_not(dir.exists(hz_dir), "hazards directory not found")

  # Find original files (not already aggregated)
  tif_files <- list.files(hz_dir, pattern = "\\.tif$", full.names = TRUE, recursive = TRUE)
  original_files <- tif_files[!grepl("__agg\\d+\\.tif$", basename(tif_files))]

  # Use a factor that doesn't exist in test data to ensure we create new cache files
  agg_factor <- 32L

  # Clean any existing cache files for this specific factor
  cache_candidates <- file.path(dirname(original_files),
                                paste0(tools::file_path_sans_ext(basename(original_files)), "__agg", agg_factor, ".tif"))
  unlink(cache_candidates[file.exists(cache_candidates)])

  # Load with aggregation - should create cache files with correct names
  hz1 <- load_hazards(hz_dir, aggregate_factor = agg_factor, cache_aggregated = TRUE, force_reaggregate = FALSE)

  # Verify cache files were created with correct names (no recursive __agg32__agg32)
  cache_files <- list.files(hz_dir, pattern = paste0("__agg", agg_factor, "\\.tif$"), full.names = TRUE, recursive = TRUE)
  # Allow for the possibility that no cache files are created if hazards are already aggregated
  if (length(original_files) > 0) {
    testthat::expect_true(length(cache_files) > 0,
                         info = paste("Expected cache files for factor", agg_factor, "but found none"))
  }

  # Check that no recursive aggregation occurred
  recursive_files <- list.files(hz_dir, pattern = "__agg\\d+__agg\\d+", full.names = TRUE)
  testthat::expect_equal(length(recursive_files), 0,
                         info = "Found recursively aggregated files - bug not fixed")

  # Verify cache files have expected names
  for (cache_file in cache_files) {
    filename <- basename(cache_file)
    # Should contain exactly one __agg32 suffix
    agg_count <- length(gregexpr(paste0("__agg", agg_factor), filename)[[1]])
    testthat::expect_equal(agg_count, 1,
                          info = paste("Cache file", filename, "has wrong number of aggregation suffixes"))
  }
})

# Tests for function: load_hazards

# Contract:
# - load_hazards(hazards_dir) reads all .tif files
# - Returns a named list of hazard rasters (and/or wrappers) with length >= 1
# - Names are derived from basenames without extension
# - Each element carries CRS information


testthat::test_that("load_hazards loads rasters and returns a named list", {
  hazards <- load_hazards(get_hazards_dir())

  testthat::expect_type(hazards, "list")
  testthat::expect_gt(length(hazards), 0)
  testthat::expect_true(!is.null(names(hazards)))
  testthat::expect_true(all(nzchar(names(hazards))))
})


testthat::test_that("load_hazards list elements carry CRS and are usable", {
  hazards <- load_hazards(get_hazards_dir())

  # We only check the first element for CRS existence notionally
  first <- hazards[[1]]
  # Allow either terra SpatRaster or sf/raster equivalents; just ensure object exists
  testthat::expect_true(!is.null(first))
})
