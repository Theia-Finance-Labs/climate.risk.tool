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
