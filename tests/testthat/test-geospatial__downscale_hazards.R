testthat::test_that("downscale_hazard_rasters reduces resolution and preserves extent/CRS", {
  skip_if_not_installed <- function(pkg) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      testthat::skip(paste0("Package not installed: ", pkg))
    }
  }
  skip_if_not_installed("terra")

  base_dir <- get_test_data_dir()
  hazards <- load_hazards(file.path(base_dir, "hazards"))

  # Use a small factor to keep tests fast
  factor <- 8
  ds <- downscale_hazard_rasters(hazards, factor = factor)

  testthat::expect_true(is.list(ds))
  testthat::expect_equal(names(ds), names(hazards))

  # Compare first raster for core properties
  r0 <- hazards[[1]]
  r1 <- ds[[1]]

  testthat::expect_equal(terra::crs(r0), terra::crs(r1))
  # Extent may differ by very small margins post-aggregation; allow tolerance
  e0 <- terra::ext(r0)
  e1 <- terra::ext(r1)
  testthat::expect_lt(abs(terra::xmin(e0) - terra::xmin(e1)), 1e-2)
  testthat::expect_lt(abs(terra::xmax(e0) - terra::xmax(e1)), 1e-2)
  testthat::expect_lt(abs(terra::ymin(e0) - terra::ymin(e1)), 1e-2)
  testthat::expect_lt(abs(terra::ymax(e0) - terra::ymax(e1)), 1e-2)

  # Resolution increases by ~factor, and ncol/nrow decrease by ~factor
  res0 <- terra::res(r0)
  res1 <- terra::res(r1)
  testthat::expect_true(all(res1 >= res0))
  testthat::expect_true(terra::ncol(r1) <= ceiling(terra::ncol(r0) / factor) + 1)
  testthat::expect_true(terra::nrow(r1) <= ceiling(terra::nrow(r0) / factor) + 1)
})


testthat::test_that("downscale_hazard_dir writes downscaled rasters preserving structure", {
  skip_if_not_installed <- function(pkg) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      testthat::skip(paste0("Package not installed: ", pkg))
    }
  }
  skip_if_not_installed("terra")

  base_dir <- get_test_data_dir()
  input_dir <- file.path(base_dir, "hazards")
  out_dir <- file.path(get_test_scratch_dir(), "downscaled_hazards_test")
  if (dir.exists(out_dir)) {
    unlink(out_dir, recursive = TRUE, force = TRUE)
  }

  factor <- 16
  downscale_hazard_dir(input_dir, out_dir, factor = factor, overwrite = TRUE)

  # Ensure files were written
  inp_files <- list.files(input_dir, pattern = "\\.tif$", recursive = TRUE, full.names = TRUE)
  out_files <- list.files(out_dir, pattern = "\\.tif$", recursive = TRUE, full.names = TRUE)
  testthat::expect_true(length(out_files) == length(inp_files))

  # Compare one pair
  # Find first basename match
  bn <- basename(inp_files[1])
  match_out <- out_files[basename(out_files) == bn][1]
  testthat::expect_true(file.exists(match_out))

  r_in <- terra::rast(inp_files[1])
  r_out <- terra::rast(match_out)
  testthat::expect_equal(terra::crs(r_in), terra::crs(r_out))
  e0 <- terra::ext(r_in)
  e1 <- terra::ext(r_out)
  testthat::expect_lt(abs(terra::xmin(e0) - terra::xmin(e1)), 1e-2)
  testthat::expect_lt(abs(terra::xmax(e0) - terra::xmax(e1)), 1e-2)
  testthat::expect_lt(abs(terra::ymin(e0) - terra::ymin(e1)), 1e-2)
  testthat::expect_lt(abs(terra::ymax(e0) - terra::ymax(e1)), 1e-2)
  testthat::expect_true(terra::ncol(r_out) < terra::ncol(r_in))
  testthat::expect_true(terra::nrow(r_out) < terra::nrow(r_in))
})


