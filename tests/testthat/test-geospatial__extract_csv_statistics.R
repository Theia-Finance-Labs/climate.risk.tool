# Test: extract_csv_statistics (CSV hazard extraction with closest-point assignment)

test_that("extract_csv_statistics finds closest point correctly", {
  # Create test assets
  assets_df <- data.frame(
    asset = c("A1", "A2"),
    company = c("C1", "C1"),
    latitude = c(-32.5, -33.0),
    longitude = c(-53.5, -54.0),
    municipality = c(NA, NA),
    province = c(NA, NA),
    asset_category = c("building", "building"),
    asset_subtype = c(NA, NA),
    size_in_m2 = c(1000, 1000),
    share_of_economic_activity = c(0.5, 0.5),
    stringsAsFactors = FALSE
  )

  # Create test CSV hazard data (grid of points)
  hazard_csv_data <- data.frame(
    ensemble = rep("mean", 9),
    GWL = rep("present", 9),
    return_period = rep(5, 9),
    lat = c(-32.0, -32.0, -32.0, -32.5, -32.5, -32.5, -33.0, -33.0, -33.0),
    lon = c(-53.0, -53.5, -54.0, -53.0, -53.5, -54.0, -53.0, -53.5, -54.0),
    hazard_indicator = rep("HI", 9),
    hazard_intensity = c(10, 11, 12, 20, 21, 22, 30, 31, 32),
    stringsAsFactors = FALSE
  )

  # Create hazards list
  hazards_csv <- list(
    "TestHazard__HI__GWL=present__RP=5__ensemble=mean" = hazard_csv_data
  )

  # Create inventory
  csv_inventory <- tibble::tibble(
    hazard_type = "TestHazard",
    hazard_indicator = "HI",
    scenario_name = "present",
    hazard_return_period = 5,
    scenario_code = "present",
    hazard_name = "TestHazard__HI__GWL=present__RP=5__ensemble=mean",
    ensemble = "mean",
    source = "csv"
  )

  # Extract statistics
  result <- extract_csv_statistics(
    assets_df,
    hazards_csv,
    csv_inventory,
    aggregation_method = "mean"
  )

  # Check results
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2) # One row per asset

  # Check that closest points were found
  # A1 at (-32.5, -53.5) should match grid point (-32.5, -53.5) with intensity 21
  # A2 at (-33.0, -54.0) should match grid point (-33.0, -54.0) with intensity 32
  result_a1 <- result |> dplyr::filter(asset == "A1")
  result_a2 <- result |> dplyr::filter(asset == "A2")

  expect_equal(result_a1$hazard_intensity, 21)
  expect_equal(result_a2$hazard_intensity, 32)
})

test_that("extract_csv_statistics handles multiple assets", {
  # Create test assets with varying distances
  assets_df <- data.frame(
    asset = c("A1", "A2", "A3", "A4"),
    company = c("C1", "C1", "C1", "C1"),
    latitude = c(-32.1, -32.4, -32.9, -33.1),
    longitude = c(-53.1, -53.4, -53.9, -54.1),
    municipality = c(NA, NA, NA, NA),
    province = c(NA, NA, NA, NA),
    asset_category = rep("building", 4),
    asset_subtype = rep(NA, 4),
    size_in_m2 = rep(1000, 4),
    share_of_economic_activity = rep(0.25, 4),
    stringsAsFactors = FALSE
  )

  # Create test CSV hazard data
  hazard_csv_data <- data.frame(
    ensemble = rep("mean", 4),
    GWL = rep("present", 4),
    return_period = rep(10, 4),
    lat = c(-32.0, -32.5, -33.0, -33.5),
    lon = c(-53.0, -53.5, -54.0, -54.5),
    hazard_indicator = rep("HI", 4),
    hazard_intensity = c(100, 200, 300, 400),
    stringsAsFactors = FALSE
  )

  hazards_csv <- list(
    "TestHazard__HI__GWL=present__RP=10__ensemble=mean" = hazard_csv_data
  )

  csv_inventory <- tibble::tibble(
    hazard_type = "TestHazard",
    hazard_indicator = "HI",
    scenario_name = "present",
    hazard_return_period = 10,
    scenario_code = "present",
    hazard_name = "TestHazard__HI__GWL=present__RP=10__ensemble=mean",
    ensemble = "mean",
    source = "csv"
  )

  result <- extract_csv_statistics(
    assets_df,
    hazards_csv,
    csv_inventory,
    aggregation_method = "mean"
  )

  # Check that all assets got a value
  expect_equal(nrow(result), 4)
  expect_true(all(!is.na(result$hazard_intensity)))
  expect_true(all(result$hazard_intensity > 0))
})

test_that("extract_csv_statistics adds extraction_method suffix", {
  assets_df <- data.frame(
    asset = "A1",
    company = "C1",
    latitude = -32.5,
    longitude = -53.5,
    municipality = NA,
    province = NA,
    asset_category = "building",
    asset_subtype = NA,
    size_in_m2 = 1000,
    share_of_economic_activity = 1.0,
    stringsAsFactors = FALSE
  )

  hazard_csv_data <- data.frame(
    ensemble = "mean",
    GWL = "present",
    return_period = 5,
    lat = -32.5,
    lon = -53.5,
    hazard_indicator = "HI",
    hazard_intensity = 50,
    stringsAsFactors = FALSE
  )

  hazards_csv <- list(
    "TestHazard__HI__GWL=present__RP=5__ensemble=mean" = hazard_csv_data
  )

  csv_inventory <- tibble::tibble(
    hazard_type = "TestHazard",
    hazard_indicator = "HI",
    scenario_name = "present",
    hazard_return_period = 5,
    scenario_code = "present",
    hazard_name = "TestHazard__HI__GWL=present__RP=5__ensemble=mean",
    ensemble = "mean",
    source = "csv"
  )

  result <- extract_csv_statistics(
    assets_df,
    hazards_csv,
    csv_inventory,
    aggregation_method = "mean"
  )

  # Check that extraction_method suffix is added
  expect_true(grepl("__extraction_method=mean$", result$hazard_name))
  expect_equal(
    result$hazard_name,
    "TestHazard__HI__GWL=present__RP=5__ensemble=mean__extraction_method=mean"
  )
})
