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

  # Hazards should have tif and nc and csv keys
  expect_type(result$hazards, "list")
  expect_true("tif" %in% names(result$hazards))
  expect_true("nc" %in% names(result$hazards))
  expect_true("csv" %in% names(result$hazards))

  # CSV hazards should be a list
  expect_type(result$hazards$csv, "list")

  # Inventory should be a tibble/dataframe
  expect_s3_class(result$inventory, "data.frame")
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
