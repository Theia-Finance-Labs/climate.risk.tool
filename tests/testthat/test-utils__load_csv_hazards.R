# Test: load_csv_hazards_with_metadata (CSV hazard loader)

test_that("load_csv_hazards_with_metadata loads CSV files correctly", {
  hazards_dir <- get_hazards_dir()

  result <- load_csv_hazards_with_metadata(hazards_dir = hazards_dir)

  # Should return list with hazards and inventory keys
  expect_type(result, "list")
  expect_true("hazards" %in% names(result))
  expect_true("inventory" %in% names(result))

  # Hazards should be a list
  expect_type(result$hazards, "list")

  # Inventory should be a tibble/dataframe
  expect_s3_class(result$inventory, "data.frame")

  # If CSV files exist, check they're loaded as data frames
  if (length(result$hazards) > 0) {
    # Each hazard should be a data frame
    for (i in seq_along(result$hazards)) {
      expect_s3_class(result$hazards[[i]], "data.frame")
      # Should have required columns
      expect_true("lat" %in% names(result$hazards[[i]]))
      expect_true("lon" %in% names(result$hazards[[i]]))
      expect_true("hazard_intensity" %in% names(result$hazards[[i]]))
    }
  }
})

test_that("load_csv_hazards_with_metadata parses folder structure", {
  hazards_dir <-    get_hazards_dir()

  result <- load_csv_hazards_with_metadata(hazards_dir = hazards_dir)

  # If CSV files exist, check naming convention
  if (length(result$hazards) > 0) {
    csv_names <- names(result$hazards)

    # Names should follow convention: {hazard_type}__{indicator}__GWL={g}__RP={rp}__ensemble={value}
    expect_true(all(grepl("__", csv_names)))
    expect_true(all(grepl("__GWL=", csv_names)))
    expect_true(all(grepl("__RP=", csv_names)))
    expect_true(all(grepl("__ensemble=", csv_names)))
  }
})

test_that("load_csv_hazards_with_metadata filters ensemble=mean", {
  hazards_dir <- file.path(get_test_data_dir(), "hazards")

  result <- load_csv_hazards_with_metadata(hazards_dir = hazards_dir)

  # If CSV files exist, check that only mean ensemble is loaded
  if (nrow(result$inventory) > 0) {
    # All inventory entries should have ensemble = "mean"
    expect_true(all(result$inventory$ensemble == "mean"))

    # All hazard names should end with __ensemble=mean
    csv_names <- names(result$hazards)
    expect_true(all(grepl("__ensemble=mean$", csv_names)))
  }
})

test_that("load_csv_hazards_with_metadata creates proper inventory", {
  hazards_dir <- get_hazards_dir()

  result <- load_csv_hazards_with_metadata(hazards_dir = hazards_dir)

  # If inventory exists, check structure
  if (nrow(result$inventory) > 0) {
    required_cols <- c(
      "hazard_type", "hazard_indicator", "scenario_name",
      "hazard_return_period", "hazard_name",
      "ensemble", "source"
    )

    # All required columns should exist
    expect_true(all(required_cols %in% names(result$inventory)))

    # Source should all be "csv"
    expect_true(all(result$inventory$source == "csv"))

    # Each inventory row should correspond to a hazard in the hazards list
    expect_true(all(result$inventory$hazard_name %in% names(result$hazards)))
  }
})

test_that("load_csv_hazards_with_metadata returns empty for no CSV files", {
  # Create temp directory with no CSV files
  temp_dir <- tempdir()
  test_hazards_dir <- file.path(temp_dir, "test_hazards_no_csv")
  dir.create(test_hazards_dir, showWarnings = FALSE, recursive = TRUE)
  on.exit(unlink(test_hazards_dir, recursive = TRUE), add = TRUE)

  result <- load_csv_hazards_with_metadata(hazards_dir = test_hazards_dir)

  # Should return empty structures
  expect_type(result, "list")
  expect_equal(length(result$hazards), 0)
  expect_equal(nrow(result$inventory), 0)
})

test_that("load_csv_hazards_with_metadata handles CSV with required columns", {
  # Create temp directory with proper structure: hazards/{type}/{indicator}/{model}/file.csv
  temp_dir <- tempdir()
  test_hazards_dir <- file.path(temp_dir, "test_hazards_csv_valid", "hazards")
  csv_dir <- file.path(test_hazards_dir, "TestHazard", "TestIndicator", "ensemble")
  dir.create(csv_dir, showWarnings = FALSE, recursive = TRUE)
  on.exit(unlink(file.path(temp_dir, "test_hazards_csv_valid"), recursive = TRUE), add = TRUE)

  # Create a valid CSV file
  test_csv <- data.frame(
    ensemble = c("mean", "mean", "mean"),
    GWL = c("present", "present", "present"),
    return_period = c(5, 5, 5),
    lat = c(-32.5, -32.6, -32.7),
    lon = c(-53.5, -53.6, -53.7),
    hazard_indicator = c("TestIndicator", "TestIndicator", "TestIndicator"),
    hazard_intensity = c(10.5, 11.2, 9.8)
  )

  csv_file <- file.path(csv_dir, "test_hazard.csv")
  write.csv(test_csv, csv_file, row.names = FALSE)

  result <- load_csv_hazards_with_metadata(hazards_dir = test_hazards_dir)

  # Should successfully load the CSV
  expect_equal(length(result$hazards), 1)
  expect_equal(nrow(result$inventory), 1)

  # Check hazard data
  hazard_data <- result$hazards[[1]]
  expect_equal(nrow(hazard_data), 3)
  expect_true("lat" %in% names(hazard_data))
  expect_true("lon" %in% names(hazard_data))
  expect_true("hazard_intensity" %in% names(hazard_data))

  # Check inventory
  expect_equal(result$inventory$source, "csv")
  expect_equal(result$inventory$ensemble, "mean")
  expect_equal(result$inventory$hazard_type, "TestHazard")
  expect_equal(result$inventory$hazard_indicator, "TestIndicator")
})

