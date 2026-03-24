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
    hazard_key = "Flood__depth__GWL=present__RP=100__ensemble=mean",
    hazard_type = "Flood",
    hazard_indicator = "depth",
    variable = "depth",
    return_period = 100,
    scenario_name = "present",
    season = NA_character_,
    ensemble = "mean",
    source = "tif",
    agg = NA_character_,
    categorical = FALSE,
    indicator_key = "Flood__depth__GWL=present__RP=100__ensemble=mean"
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

testthat::test_that("extract_spatial_statistics does not mask extraction failures with zero", {
  testthat::skip_if_not_installed("terra")
  testthat::skip_if_not_installed("sf")
  
  # Create a raster with actual values
  hazard_rast <- terra::rast(
    nrows = 10,
    ncols = 10,
    xmin = -60,
    xmax = -40,
    ymin = -30,
    ymax = -10,
    crs = "EPSG:4326"
  )
  # Set all values to 45.2 (a typical FWI value)
  hazard_rast <- terra::setValues(hazard_rast, 45.2)
  
  # Key in hazards list must match indicator_key from inventory
  indicator_key_val <- "fire_weather_index__fwi__return_period=100__gwl=3__ensemble=mean"
  hazards <- list()
  hazards[[indicator_key_val]] <- hazard_rast
  
  hazards_inventory <- tibble::tibble(
    hazard_name = "Fire__fire_weather_index__return_period=100__gwl=3__ensemble=mean",
    hazard_key = "Fire__fire_weather_index__return_period=100__gwl=3__ensemble=mean",
    hazard_type = "Fire",
    hazard_indicator = "fire_weather_index",
    variable = "fwi",
    return_period = 100,
    scenario_name = NA_character_,
    gwl = 3,
    season = NA_character_,
    ensemble = "mean",
    source = "nc",
    agg = "closest",
    categorical = FALSE,
    indicator_key = indicator_key_val
  )
  
  # Asset within the raster bounds
  assets_df <- tibble::tibble(
    asset = "Asset A",
    company = "Company A",
    latitude = -20.0,
    longitude = -50.0,
    municipality = NA_character_,
    state = NA_character_,
    asset_category = "test",
    asset_subtype = "test",
    size_in_m2 = 10,
    share_of_economic_activity = 1,
    cnae = NA_character_
  )
  
  results <- extract_spatial_statistics(
    assets_df = assets_df,
    hazards = hazards,
    hazards_inventory = hazards_inventory,
    aggregation_method = "closest"
  )
  
  # Should extract the actual value (45.2), NOT 0
  testthat::expect_equal(results$fwi, 45.2, tolerance = 0.01)
  testthat::expect_false(results$fwi == 0)
  testthat::expect_true(results$fwi > 40)  # Verify it's a reasonable FWI value
})

testthat::test_that("extract_spatial_statistics with closest extracts actual NetCDF values not empty values", {
  testthat::skip_if_not_installed("terra")
  testthat::skip_if_not_installed("sf")

  # Create a NetCDF-like raster with non-zero values
  # Using a grid that covers Brazil-like coordinates
  hazard_rast <- terra::rast(
    nrows = 10,
    ncols = 10,
    xmin = -60,
    xmax = -40,
    ymin = -30,
    ymax = -10,
    crs = "EPSG:4326"
  )
  # Set values to something other than 0 or -1 (the "empty" values mentioned)
  hazard_rast <- terra::setValues(hazard_rast, rep(5.5, 100))

  hazards <- list("Heat__hi__GWL=present__RP=5__ensemble=mean" = hazard_rast)
  hazards_inventory <- tibble::tibble(
    hazard_name = "Heat__hi__GWL=present__RP=5__ensemble=mean",
    hazard_key = "Heat__hi__GWL=present__RP=5__ensemble=mean",
    indicator_key = "Heat__hi__GWL=present__RP=5__ensemble=mean",
    hazard_type = "Heat",
    hazard_indicator = "hi",
    variable = "hi",
    return_period = 5,
    scenario_name = "present",
    season = NA_character_,
    ensemble = "mean",
    source = "nc",
    agg = "closest",
    categorical = FALSE
  )

  # Asset with coordinates in the middle of the raster
  assets_df <- tibble::tibble(
    asset = "Asset B",
    company = "Company B",
    latitude = -20,
    longitude = -50,
    municipality = NA_character_,
    state = NA_character_,
    asset_category = "test",
    asset_subtype = "test",
    size_in_m2 = 100,
    share_of_economic_activity = 1,
    cnae = NA_character_
  )

  results <- extract_spatial_statistics(
    assets_df = assets_df,
    hazards = hazards,
    hazards_inventory = hazards_inventory,
    aggregation_method = "closest"
  )
  
  # Should extract the actual value (5.5), not empty value (0 or -1)
  testthat::expect_equal(results$hi, 5.5)
  testthat::expect_true(results$hi > 0)
  testthat::expect_false(results$hi == -1)
})

testthat::test_that("extract_hazard_statistics surfaces missing precomputed keys", {
  hazards_inventory <- tibble::tibble(
    hazard_name = "Heat__hi__scenario_name=present__RP=5__ensemble=mean",
    hazard_key = "Heat__hi__scenario_name=present__RP=5__ensemble=mean",
    indicator_key = "Heat__hi__scenario_name=present__RP=5__ensemble=mean",
    hazard_type = "Heat",
    hazard_indicator = "heat_index",
    return_period = 5,
    scenario_name = "present",
    season = NA_character_,
    ensemble = "mean",
    source = "csv",
    agg = NA_character_,
    categorical = FALSE,
    variable = "hi"
  )

  precomputed <- tibble::tibble(
    region = "KnownCity",
    adm_level = "ADM2",
    hazard_type = "Heat",
    hazard_indicator = "heat_index",
    hazard_name = "Heat__hi__scenario_name=present__RP=5__ensemble=mean",
    hazard_key = "Heat__hi__scenario_name=present__RP=5__ensemble=mean",
    indicator_key = "Heat__hi__scenario_name=present__RP=5__ensemble=mean",
    scenario_name = "present",
    return_period = 5,
    aggregation_method = "mean",
    hazard_value = 1,
    ensemble = "mean",
    season = NA_character_,
    variable = "hi"
  )

  assets_df <- tibble::tibble(
    asset = c("A1", "A2"),
    company = c("C1", "C2"),
    latitude = NA_real_,
    longitude = NA_real_,
    municipality = c("MissingTown", NA_character_),
    state = c("MissingState", "MissingState"),
    asset_category = "test",
    asset_subtype = "test",
    size_in_m2 = 10,
    share_of_economic_activity = 1,
    cnae = NA_character_
  )

  testthat::expect_error(
    extract_hazard_statistics(
      assets_df = assets_df,
      hazards = list(),
      hazards_inventory = hazards_inventory,
      precomputed_hazards = precomputed,
      aggregation_method = "mean"
    ),
    "Missing regions \\(ADM2\\): MissingTown"
  )

  testthat::expect_error(
    extract_hazard_statistics(
      assets_df = assets_df,
      hazards = list(),
      hazards_inventory = hazards_inventory,
      precomputed_hazards = precomputed,
      aggregation_method = "mean"
    ),
    "Missing regions \\(ADM1\\): MissingState"
  )
})

testthat::test_that("extract_hazard_statistics applies inference for precomputed indicators", {
  hazards_data <- load_hazards_and_inventory(
    hazards_dir = get_hazards_dir(),
    hazard_indicators_dir = get_hazard_indicators_dir(),
    aggregate_factor = 16L
  )

  fwi_row <- hazards_data$inventory |>
    dplyr::filter(.data$hazard_type == "Fire", .data$hazard_indicator == "fire_weather_index") |>
    dplyr::slice(1)

  days_row <- hazards_data$inventory |>
    dplyr::filter(.data$hazard_type == "Fire", .data$hazard_indicator == "days_danger_total") |>
    dplyr::slice(1)

  # Only include precomputed indicators in inventory
  hazards_inventory <- dplyr::bind_rows(fwi_row, days_row)
  hazard_configs <- hazards_data$configs

  precomputed <- tibble::tibble(
    region = "TestState",
    adm_level = "ADM1",
    hazard_name = fwi_row$hazard_name,
    hazard_key = fwi_row$hazard_key,
    scenario_name = fwi_row$scenario_name,
    return_period = fwi_row$return_period,
    gwl = fwi_row$gwl,
    indicator_key = fwi_row$indicator_key,
    hazard_type = fwi_row$hazard_type,
    hazard_indicator = fwi_row$hazard_indicator,
    indicator_file = fwi_row$indicator_file,
    indicator_variable = fwi_row$indicator_variable,
    variable = fwi_row$variable,
    ensemble = fwi_row$ensemble,
    season = fwi_row$season,
    mean = 12.3
  )

  precomputed <- dplyr::bind_rows(
    precomputed,
    tibble::tibble(
      region = "TestState",
      adm_level = "ADM1",
      hazard_name = days_row$hazard_name,
      hazard_key = days_row$hazard_key,
      scenario_name = days_row$scenario_name,
      return_period = days_row$return_period,
      gwl = days_row$gwl,
      indicator_key = days_row$indicator_key,
      hazard_type = days_row$hazard_type,
      hazard_indicator = days_row$hazard_indicator,
      indicator_file = days_row$indicator_file,
      indicator_variable = days_row$indicator_variable,
      variable = days_row$variable,
      ensemble = days_row$ensemble,
      season = days_row$season,
      mean = 2.2
    )
  )

  assets_df <- tibble::tibble(
    asset = "A1",
    company = "C1",
    latitude = NA_real_,
    longitude = NA_real_,
    municipality = NA_character_,
    state = "TestState",
    asset_category = "agriculture",
    asset_subtype = "soybean",
    size_in_m2 = 10,
    share_of_economic_activity = 1,
    cnae = NA_character_
  )

  results <- extract_hazard_statistics(
    assets_df = assets_df,
    hazards = list(),
    hazards_inventory = hazards_inventory,
    precomputed_hazards = precomputed,
    hazard_configs = hazard_configs,
    aggregation_method = "mean"
  )

  fwi_row_result <- results |>
    dplyr::filter(.data$hazard_type == "Fire", .data$hazard_indicator == "fire_weather_index")

  testthat::expect_equal(nrow(fwi_row_result), 1)
  testthat::expect_equal(fwi_row_result$fwi[1], 12.3, tolerance = 0.0001)
})

testthat::test_that("extract_hazard_statistics raises error when non-precomputed indicators required but not available", {
  hazards_data <- load_hazards_and_inventory(
    hazards_dir = get_hazards_dir(),
    hazard_indicators_dir = get_hazard_indicators_dir(),
    aggregate_factor = 16L
  )
  land_cover_row <- hazards_data$inventory |>
    dplyr::filter(.data$hazard_type == "Fire", .data$hazard_indicator == "land_cover") |>
    dplyr::slice(1)

  fwi_row <- hazards_data$inventory |>
    dplyr::filter(.data$hazard_type == "Fire", .data$hazard_indicator == "fire_weather_index") |>
    dplyr::slice(1)

  # Include non-precomputed indicator (land_cover) in inventory
  hazards_inventory <- dplyr::bind_rows(fwi_row, land_cover_row)
  hazard_configs <- hazards_data$configs

  # Only provide precomputed data for fwi (not land_cover, which is marked precomputed: false)
  precomputed <- tibble::tibble(
    region = "TestState",
    adm_level = "ADM1",
    hazard_name = fwi_row$hazard_name,
    hazard_key = fwi_row$hazard_key,
    scenario_name = fwi_row$scenario_name,
    return_period = fwi_row$return_period,
    gwl = fwi_row$gwl,
    indicator_key = fwi_row$indicator_key,
    hazard_type = fwi_row$hazard_type,
    hazard_indicator = fwi_row$hazard_indicator,
    indicator_file = fwi_row$indicator_file,
    indicator_variable = fwi_row$indicator_variable,
    variable = fwi_row$variable,
    ensemble = fwi_row$ensemble,
    season = fwi_row$season,
    mean = 12.3
  )

  assets_df <- tibble::tibble(
    asset = "A1",
    company = "C1",
    latitude = NA_real_,
    longitude = NA_real_,
    municipality = NA_character_,
    state = "TestState",
    asset_category = "agriculture",
    asset_subtype = "soybean",
    size_in_m2 = 10,
    share_of_economic_activity = 1,
    cnae = NA_character_
  )

  testthat::expect_error(
    extract_hazard_statistics(
      assets_df = assets_df,
      hazards = list(),
      hazards_inventory = hazards_inventory,
      precomputed_hazards = precomputed,
      hazard_configs = hazard_configs,
      aggregation_method = "mean"
    ),
    "No hazards available for precomputed lookup"
  )
})

testthat::test_that("extract_hazard_statistics detects and reports duplicates", {
  hazards_data <- load_hazards_and_inventory(
    hazards_dir = get_hazards_dir(),
    hazard_indicators_dir = get_hazard_indicators_dir(),
    aggregate_factor = 16L
  )
  
  fwi_row <- hazards_data$inventory |>
    dplyr::filter(.data$hazard_type == "Fire", .data$hazard_indicator == "fire_weather_index") |>
    dplyr::slice(1)
  
  hazards_inventory <- fwi_row
  hazard_configs <- hazards_data$configs
  
  precomputed <- tibble::tibble(
    region = "TestState",
    adm_level = "ADM1",
    hazard_name = fwi_row$hazard_name,
    hazard_key = fwi_row$hazard_key,
    scenario_name = fwi_row$scenario_name,
    return_period = fwi_row$return_period,
    gwl = fwi_row$gwl,
    indicator_key = fwi_row$indicator_key,
    hazard_type = fwi_row$hazard_type,
    hazard_indicator = fwi_row$hazard_indicator,
    indicator_file = fwi_row$indicator_file,
    indicator_variable = fwi_row$indicator_variable,
    variable = fwi_row$variable,
    ensemble = fwi_row$ensemble,
    season = fwi_row$season,
    mean = 12.3
  )
  
  # Create asset with coordinates (will use spatial extraction)
  assets_with_coords <- tibble::tibble(
    asset = "A1",
    company = "C1",
    latitude = -23.5,
    longitude = -46.6,
    municipality = NA_character_,
    state = "TestState",
    asset_category = "agriculture",
    asset_subtype = "soybean",
    size_in_m2 = 10,
    share_of_economic_activity = 1,
    cnae = NA_character_
  )
  
  # Create mock hazard raster for spatial extraction
  # This would normally come from load_hazards, but we'll create a simple one for testing
  mock_raster <- terra::rast(nrows = 10, ncols = 10, xmin = -50, xmax = -40, ymin = -30, ymax = -20)
  terra::values(mock_raster) <- 12.3
  terra::crs(mock_raster) <- "EPSG:4326"
  
  hazards_list <- list()
  hazards_list[[fwi_row$indicator_key]] <- mock_raster
  
  # Create duplicate scenario: same asset appears in both coordinate-based and precomputed results
  # This simulates a bug where an asset is processed twice
  assets_without_coords <- assets_with_coords |>
    dplyr::mutate(latitude = NA_real_, longitude = NA_real_)
  
  # Combine both asset sets to create duplicate scenario
  assets_duplicate <- dplyr::bind_rows(assets_with_coords, assets_without_coords)
  
  # This should detect duplicates and raise an error
  testthat::expect_error(
    extract_hazard_statistics(
      assets_df = assets_duplicate,
      hazards = hazards_list,
      hazards_inventory = hazards_inventory,
      precomputed_hazards = precomputed,
      hazard_configs = hazard_configs,
      aggregation_method = "mean"
    ),
    "duplicate asset/indicator_key combinations"
  )
})

