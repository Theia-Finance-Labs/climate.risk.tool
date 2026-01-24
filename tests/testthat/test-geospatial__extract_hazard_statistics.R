testthat::test_that("extract_hazard_statistics returns standardized columns", {
  hazards_data <- load_hazards_and_inventory(
    hazards_dir = get_hazards_dir(),
    hazard_indicators_dir = get_hazard_indicators_dir(),
    aggregate_factor = 16L
  )

  assets <- read_assets(get_test_data_dir("user_input"))
  precomputed <- read_precomputed_hazards(get_test_data_dir())

  results <- extract_hazard_statistics(
    assets_df = assets,
    hazards = hazards_data$hazards,
    hazards_inventory = hazards_data$inventory,
    precomputed_hazards = precomputed,
    aggregation_method = "mean"
  )

  testthat::expect_true(is.data.frame(results))
  testthat::expect_gt(nrow(results), 0)
  testthat::expect_true(all(c("hazard_indicator", "gwl", "return_period") %in% names(results)))
  testthat::expect_true(any(c("depth", "hi", "spi3", "fwi", "days_danger_total", "land_cover") %in% names(results)))
})

testthat::test_that("extract_spatial_statistics handles closest and small buffer extraction", {
  testthat::skip_if_not_installed("terra")
  testthat::skip_if_not_installed("sf")

  hazard_rast <- terra::rast(
    nrows = 1,
    ncols = 1,
    xmin = 0,
    xmax = 10,
    ymin = 0,
    ymax = 10,
    crs = "EPSG:4326"
  )
  hazard_rast <- terra::setValues(hazard_rast, 7)

  hazards <- list("Flood__depth__GWL=present__RP=100__ensemble=mean" = hazard_rast)
  hazards_inventory <- tibble::tibble(
    hazard_name = "Flood__depth__GWL=present__RP=100__ensemble=mean",
    hazard_type = "Flood",
    hazard_indicator = "depth",
    return_period = 100,
    gwl = "present",
    season = NA_character_,
    ensemble = "mean",
    source = "tif",
    agg = NA_character_,
    categorical = FALSE
  )

  assets_df <- tibble::tibble(
    asset = "Asset A",
    company = "Company A",
    latitude = 0.1,
    longitude = 0.1,
    municipality = NA_character_,
    state = NA_character_,
    asset_category = "test",
    asset_subtype = "test",
    size_in_m2 = 10,
    share_of_economic_activity = 1,
    cnae = NA_character_
  )

  closest_results <- extract_spatial_statistics(
    assets_df = assets_df,
    hazards = hazards,
    hazards_inventory = hazards_inventory,
    aggregation_method = "closest"
  )
  testthat::expect_equal(closest_results$depth, 7)

  mean_results <- extract_spatial_statistics(
    assets_df = assets_df,
    hazards = hazards,
    hazards_inventory = hazards_inventory,
    aggregation_method = "mean"
  )
  testthat::expect_equal(mean_results$depth, 7)
})

