# Test: load_hazards_and_inventory (unified TIF + NC loader)

test_that("load_hazards_and_inventory returns hazards and inventory", {
  hazards_dir <- file.path(get_test_data_dir(), "hazards")

  result <- load_hazards_and_inventory(
    hazards_dir = hazards_dir,
    aggregate_factor = 16L
  )

  # Should return list with hazards and inventory keys
  expect_type(result, "list")
  expect_true("hazards" %in% names(result))
  expect_true("inventory" %in% names(result))

  # Hazards should have tif and nc keys
  expect_type(result$hazards, "list")
  expect_true("tif" %in% names(result$hazards))
  expect_true("nc" %in% names(result$hazards))

  # Inventory should be a tibble/dataframe
  expect_s3_class(result$inventory, "data.frame")
})

test_that("load_hazards_and_inventory loads TIF files correctly", {
  hazards_dir <- file.path(get_test_data_dir(), "hazards")

  result <- load_hazards_and_inventory(
    hazards_dir = hazards_dir,
    aggregate_factor = 16L
  )

  # TIF rasters should be loaded
  expect_true(length(result$hazards$tif) >= 2)

  # Each should be a SpatRaster
  for (i in seq_along(result$hazards$tif)) {
    expect_s4_class(result$hazards$tif[[i]], "SpatRaster")
  }

  # Names should follow convention
  expect_true(!is.null(names(result$hazards$tif)))
})

test_that("load_hazards_and_inventory loads NC files correctly", {
  hazards_dir <- file.path(get_test_data_dir(), "hazards")

  result <- load_hazards_and_inventory(
    hazards_dir = hazards_dir,
    aggregate_factor = 16L
  )

  # NC rasters should be loaded if files exist
  if (length(result$hazards$nc) > 0) {
    # Each should be a SpatRaster
    for (i in seq_along(result$hazards$nc)) {
      expect_s4_class(result$hazards$nc[[i]], "SpatRaster")
    }

    # Names should follow convention: {hazard_type}__{indicator}__GWL={g}__RP={rp}__ensemble={value}
    # where {value} is currently only "mean" (other ensemble variants not loaded)
    expect_true(!is.null(names(result$hazards$nc)))
    expect_true(all(grepl("__GWL=", names(result$hazards$nc))))
    expect_true(all(grepl("__RP=", names(result$hazards$nc))))
    expect_true(all(grepl("__ensemble=", names(result$hazards$nc))))
  }
})

test_that("load_hazards_and_inventory NC rasters have proper extent (cell centers to edges)", {
  hazards_dir <- file.path(get_test_data_dir(), "hazards")

  result <- load_hazards_and_inventory(
    hazards_dir = hazards_dir,
    aggregate_factor = 16L
  )

  # Check first NC raster
  r_nc <- result$hazards$nc[[1]]

  # Should have proper georeferencing
  expect_s4_class(r_nc, "SpatRaster")

  # Should have CRS
  expect_true(nchar(terra::crs(r_nc)) > 0)

  # Extent should be reasonable (not 0,5,0,168 like raw GDAL read)
  ext <- terra::ext(r_nc)
  expect_true(ext[1] < ext[2]) # xmin < xmax
  expect_true(ext[3] < ext[4]) # ymin < ymax

  # Resolution should be calculated (not default 1.0)
  res <- terra::res(r_nc)
  expect_true(res[1] > 0)
  expect_true(res[2] > 0)
})

test_that("load_hazards_and_inventory NC names parse folder structure correctly", {
  hazards_dir <- file.path(get_test_data_dir(), "hazards")

  result <- load_hazards_and_inventory(
    hazards_dir = hazards_dir,
    aggregate_factor = 16L
  )


  # Check naming convention
  nc_names <- names(result$hazards$nc)

  # Names should contain hazard_type from folder
  # e.g., "Drought__CDD__GWL=present__RP=5__ensemble=mean"
  expect_true(any(grepl("Drought", nc_names)))

  # Should contain hazard_indicator
  expect_true(any(grepl("CDD", nc_names) | grepl("SPI6", nc_names)))

  # Should have GWL values
  expect_true(all(grepl("GWL=", nc_names)))

  # Should have return period values
  expect_true(all(grepl("RP=", nc_names)))
})

test_that("load_hazards_and_inventory NC rasters filter ensemble=mean correctly", {
  hazards_dir <- file.path(get_test_data_dir(), "hazards")

  result <- load_hazards_and_inventory(
    hazards_dir = hazards_dir,
    aggregate_factor = 16L
  )


  # NC files should load only mean ensemble (current implementation behavior)
  nc_names <- names(result$hazards$nc)

  # Check that we have mean ensemble values in the names
  expect_true(any(grepl("__ensemble=mean$", nc_names)),
    info = "Should have at least one mean ensemble"
  )
  expect_false(any(grepl("__ensemble=median$", nc_names)),
    info = "Should not have median ensemble (not loaded)"
  )
  expect_false(any(grepl("__ensemble=p10$", nc_names)),
    info = "Should not have p10 ensemble (not loaded)"
  )
  expect_false(any(grepl("__ensemble=p90$", nc_names)),
    info = "Should not have p90 ensemble (not loaded)"
  )

  # Check inventory has ensemble column for NC hazards
  nc_inventory <- result$inventory |> dplyr::filter(source == "nc")
  expect_true("ensemble" %in% names(nc_inventory))
  expect_true(all(nc_inventory$ensemble %in% c("mean")))
})

test_that("load_hazards_and_inventory creates separate raster per GWL and return_period combination", {
  hazards_dir <- file.path(get_test_data_dir(), "hazards")

  result <- load_hazards_and_inventory(
    hazards_dir = hazards_dir,
    aggregate_factor = 16L
  )


  # Check that we have multiple rasters per file
  # Each NC file should generate multiple rasters (one per GWL × RP × ensemble combination)
  nc_names <- names(result$hazards$nc)

  # Count unique GWL values (test data has 1 GWL)
  gwl_values <- unique(sub(".*__GWL=([^_]+)__.*", "\\1", nc_names))
  expect_true(length(gwl_values) >= 1) # Should have at least one GWL value

  # Count unique RP values
  rp_values <- unique(sub(".*__RP=([^_]+)__.*", "\\1", nc_names))
  expect_true(length(rp_values) > 1) # Should have multiple return periods
})

test_that("load_hazards_and_inventory passes aggregate_factor to TIF loader only", {
  hazards_dir <- file.path(get_test_data_dir(), "hazards")

  # Should work with aggregate_factor (applies to TIF only)
  result <- load_hazards_and_inventory(
    hazards_dir = hazards_dir,
    aggregate_factor = 16L
  )

  expect_type(result, "list")
  expect_true("hazards" %in% names(result))
  expect_true("inventory" %in% names(result))
  expect_true("tif" %in% names(result$hazards))
  expect_true("nc" %in% names(result$hazards))
})

test_that("load_hazards_and_inventory works with NC files when no TIF files present", {
  # Create temporary directory structure with only NC files
  temp_dir <- tempdir()
  test_hazards_dir <- file.path(temp_dir, "test_hazards_nc_only")
  dir.create(test_hazards_dir, showWarnings = FALSE, recursive = TRUE)
  on.exit(unlink(test_hazards_dir, recursive = TRUE), add = TRUE)

  # Create a minimal NetCDF file structure
  nc_dir <- file.path(test_hazards_dir, "hazards", "Drought", "CDD", "ensemble")
  dir.create(nc_dir, showWarnings = FALSE, recursive = TRUE)

  # Copy an NC file from test data if it exists
  source_hazards_dir <- file.path(get_test_data_dir(), "hazards")
  nc_files <- list.files(source_hazards_dir,
    pattern = "\\.nc$",
    full.names = TRUE, recursive = TRUE
  )


  # Copy first NC file
  file.copy(nc_files[1], file.path(nc_dir, basename(nc_files[1])))

  # Create a mapping file that references non-existent TIF files
  parent_dir <- dirname(file.path(test_hazards_dir, "hazards"))
  mapping_content <- "hazard_file,hazard_type,scenario_code,scenario_name,hazard_return_period,hazard_indicator
nonexistent1.tif,flood,ssp245,SSP2-4.5,10,inun
nonexistent2.tif,flood,ssp245,SSP2-4.5,100,inun"
  writeLines(mapping_content, file.path(parent_dir, "hazards_metadata.csv"))

  # Should not fail, should load NC files only
  result <- load_hazards_and_inventory(
    hazards_dir = file.path(test_hazards_dir, "hazards"),
    aggregate_factor = 1L
  )

  # Should return structure with empty TIF list but populated NC list
  expect_type(result, "list")
  expect_true("hazards" %in% names(result))
  expect_true("inventory" %in% names(result))

  # TIF list should be empty
  expect_equal(length(result$hazards$tif), 0)

  # NC list should have content
  expect_true(length(result$hazards$nc) > 0)

  # Inventory should contain NC entries
  expect_true(nrow(result$inventory) > 0)
  expect_true(all(result$inventory$source == "nc"))
})

test_that("load_nc_hazards_with_metadata handles multi-variable NetCDF files", {
  # This test verifies that the loader can handle NetCDF files with multiple variables
  # and selects the appropriate one (preferring 'mean' if available)

  # Note: The actual multi-variable handling is tested implicitly when loading
  # real NetCDF files from the workspace. This is a placeholder for explicit testing
  # if we create a multi-variable test file in the future.

  hazards_dir <- file.path(get_test_data_dir(), "hazards")
  result <- load_hazards_and_inventory(
    hazards_dir = hazards_dir,
    aggregate_factor = 16L
  )

  # If we successfully loaded NC files, the multi-variable logic worked
  # (either single-variable files or multi-variable with successful selection)
  expect_type(result, "list")
  expect_true("hazards" %in% names(result))

  # If NC files were loaded, they should be SpatRaster objects
  if (length(result$hazards$nc) > 0) {
    expect_s4_class(result$hazards$nc[[1]], "SpatRaster")
  }
})

test_that("load_hazards_and_inventory detects mixed types in same folder", {
  # Create temp directory with mixed file types in same folder
  temp_dir <- tempdir()
  test_hazards_dir <- file.path(temp_dir, "test_hazards_mixed")
  mixed_dir <- file.path(test_hazards_dir, "hazards", "MixedType", "indicator", "ensemble")
  dir.create(mixed_dir, showWarnings = FALSE, recursive = TRUE)
  on.exit(unlink(test_hazards_dir, recursive = TRUE), add = TRUE)

  # Create both NC and CSV files in the same folder
  # Copy NC file from test data if available
  source_hazards_dir <- file.path(get_test_data_dir(), "hazards")
  nc_files <- list.files(source_hazards_dir,
    pattern = "\\.nc$",
    full.names = TRUE, recursive = TRUE
  )

  if (length(nc_files) > 0) {
    file.copy(nc_files[1], file.path(mixed_dir, "test.nc"))
  } else {
    # Create a dummy NC file
    file.create(file.path(mixed_dir, "test.nc"))
  }

  # Create a CSV file in the same folder
  test_csv <- data.frame(
    ensemble = "mean",
    GWL = "present",
    return_period = 5,
    lat = -32.5,
    lon = -53.5,
    hazard_indicator = "indicator",
    hazard_intensity = 10.5
  )
  write.csv(test_csv, file.path(mixed_dir, "test.csv"), row.names = FALSE)

  # Should raise an error about mixed types
  expect_error(
    load_hazards_and_inventory(
      hazards_dir = file.path(test_hazards_dir, "hazards"),
      aggregate_factor = 1L
    ),
    "Mixed hazard types detected"
  )
})

test_that("load_hazards_and_inventory allows different types in different folders", {
  # Create temp directory with different types in different folders
  temp_dir <- tempdir()
  test_hazards_dir <- file.path(temp_dir, "test_hazards_separate")
  nc_dir <- file.path(test_hazards_dir, "hazards", "NCType", "indicator1", "ensemble")
  csv_dir <- file.path(test_hazards_dir, "hazards", "CSVType", "indicator2", "ensemble")
  dir.create(nc_dir, showWarnings = FALSE, recursive = TRUE)
  dir.create(csv_dir, showWarnings = FALSE, recursive = TRUE)
  on.exit(unlink(test_hazards_dir, recursive = TRUE), add = TRUE)

  # Create NC file in one folder
  source_hazards_dir <- file.path(get_test_data_dir(), "hazards")
  nc_files <- list.files(source_hazards_dir,
    pattern = "\\.nc$",
    full.names = TRUE, recursive = TRUE
  )

  if (length(nc_files) > 0) {
    file.copy(nc_files[1], file.path(nc_dir, "test.nc"))
  } else {
    file.create(file.path(nc_dir, "test.nc"))
  }

  # Create CSV file in another folder
  test_csv <- data.frame(
    ensemble = "mean",
    GWL = "present",
    return_period = 5,
    lat = -32.5,
    lon = -53.5,
    hazard_indicator = "indicator2",
    hazard_intensity = 10.5
  )
  write.csv(test_csv, file.path(csv_dir, "test.csv"), row.names = FALSE)

  # Should NOT raise an error (different folders)
  expect_no_error(
    load_hazards_and_inventory(
      hazards_dir = file.path(test_hazards_dir, "hazards"),
      aggregate_factor = 1L
    )
  )
})

test_that("load_hazards_and_inventory loads CSV hazards", {
  hazards_dir <- file.path(get_test_data_dir(), "hazards")

  result <- load_hazards_and_inventory(
    hazards_dir = hazards_dir,
    aggregate_factor = 16L
  )

  # Should have csv key in hazards
  expect_true("csv" %in% names(result$hazards))

  # CSV hazards should be a list
  expect_type(result$hazards$csv, "list")

  # If CSV files exist in test data, check they're loaded correctly
  if (length(result$hazards$csv) > 0) {
    # Each CSV hazard should be a data frame
    for (i in seq_along(result$hazards$csv)) {
      expect_s3_class(result$hazards$csv[[i]], "data.frame")
    }

    # Inventory should have CSV entries
    csv_inventory <- result$inventory |> dplyr::filter(source == "csv")
    expect_true(nrow(csv_inventory) > 0)
    expect_true(all(csv_inventory$source == "csv"))
  }
})
