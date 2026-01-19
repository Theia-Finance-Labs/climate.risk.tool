testthat::test_that("geolocated assets extract from NetCDF files", {
  # Load all hazards
  hazard_data <- load_hazards_and_inventory(get_hazards_dir())

  # Define events with NetCDF hazard for focused testing (using formatted hazard name)
  events <- tibble::tibble(
    hazard_name = "Drought__SPI3__GWL=present__RP=5__season=Summer__ensemble=mean",
    event_year = 2030,
    season = "Summer"
  )

  # Filter to just the selected hazard (like the real pipeline does)
  nc_hazards <- filter_hazards_by_events(hazard_data$hazards, events)
  nc_inventory <- hazard_data$inventory |>
    dplyr::filter(.data$source == "nc", .data$hazard_name %in% names(nc_hazards))

  # Create 2 assets with coordinates (no municipality/province)
  assets <- tibble::tibble(
    asset = c("asset_nc_1", "asset_nc_2"),
    company = c("company_a", "company_b"),
    latitude = c(-3.0, -15.0),
    longitude = c(-60.0, -47.9),
    municipality = NA_character_,
    state = NA_character_,
    asset_category = "office",
    asset_subtype = NA_character_,
    size_in_m2 = 1000,
    share_of_economic_activity = 0.5,
    cnae = NA_real_
  )

  # Extract
  out <- extract_hazard_statistics(
    assets,
    nc_hazards,
    nc_inventory,
    precomputed_hazards = tibble::tibble()
  )


  # Verify: all matching_method = "coordinates"
  testthat::expect_true(all(out$matching_method == "coordinates"))

  # Verify: hazard statistics are numeric
  testthat::expect_true(is.numeric(out$hazard_intensity))
  # Drought (SPI3) can have negative values, so we don't expect all >= 0
  testthat::expect_true(any(!is.na(out$hazard_intensity)))

  # Should have results for both assets
  testthat::expect_equal(length(unique(out$asset)), 2)
})


testthat::test_that("geolocated assets extract from NC files", {
  # Load all hazards
  hazard_data <- load_hazards_and_inventory(get_hazards_dir(), aggregate_factor = 16L)

  # Define events with just 1 NC hazard for focused testing
  # NC hazards expand to all ensemble variants automatically
  # Use GWL=present which exists in test data, and include season for SPI3
  events <- tibble::tibble(
    hazard_name = "Drought__SPI3__GWL=present__RP=10__season=Summer__ensemble=mean",
    event_year = 2030,
    season = "Summer"
  )

  # Filter to just the selected hazard
  nc_hazards <- filter_hazards_by_events(hazard_data$hazards, events)
  nc_inventory <- hazard_data$inventory |>
    dplyr::filter(.data$hazard_name %in% names(nc_hazards))

  # Create 2 assets with coordinates
  assets <- tibble::tibble(
    asset = c("asset_nc_1", "asset_nc_2"),
    company = c("company_a", "company_b"),
    latitude = c(-3.0, -15.0),
    longitude = c(-60.0, -47.9),
    municipality = NA_character_,
    state = NA_character_,
    asset_category = "office",
    asset_subtype = NA_character_,
    size_in_m2 = 1000,
    share_of_economic_activity = 0.5,
    cnae = NA_real_
  )

  # Extract
  out <- extract_hazard_statistics(
    assets,
    nc_hazards,
    nc_inventory,
    precomputed_hazards = tibble::tibble()
  )

  # Verify: all matching_method = "coordinates"
  testthat::expect_true(all(out$matching_method == "coordinates"))

  # Verify: hazard_intensity column exists and is numeric
  testthat::expect_true("hazard_intensity" %in% names(out))
  testthat::expect_true(is.numeric(out$hazard_intensity))

  # Should have results for both assets
  testthat::expect_equal(length(unique(out$asset)), 2)
})

testthat::test_that("spatial extraction uses buffered geometries and returns deterministic values (fast path)", {
  # Minimal synthetic raster: 10x10 with 1-degree cells, values 1..100
  r <- terra::rast(ncols = 10, nrows = 10, xmin = 0, xmax = 10, ymin = 0, ymax = 10, crs = "EPSG:4326")
  terra::values(r) <- seq_len(terra::ncell(r))

  hazards <- list("Flood__depth(cm)__GWL=present__RP=10__ensemble=mean" = r)
  inventory <- tibble::tibble(
    hazard_name = "Flood__depth(cm)__GWL=present__RP=10__ensemble=mean",
    hazard_type = "Flood",
    hazard_indicator = "depth(cm)",
    hazard_return_period = 10,
    scenario_name = "present",
    source = "nc"
  )

  assets <- tibble::tibble(
    asset = c("a1", "a2"),
    company = c("c1", "c2"),
    latitude = c(5.5, 8.5),
    longitude = c(5.5, 1.5),
    municipality = NA_character_,
    state = NA_character_,
    asset_category = "office",
    asset_subtype = NA_character_,
    size_in_m2 = 1, # tiny buffer (should fall within a single raster cell)
    share_of_economic_activity = 0.5,
    cnae = NA_real_
  )

  out <- extract_hazard_statistics(
    assets_df = assets,
    hazards = hazards,
    hazards_inventory = inventory,
    precomputed_hazards = tibble::tibble(),
    aggregation_method = "mean"
  )

  # Deterministic expected values: for tiny buffers, mean == the cell value at the asset point
  ex <- terra::extract(r, cbind(assets$longitude, assets$latitude))
  # terra::extract() for matrix xy returns a 1-col data.frame (no ID column)
  expected_vals <- as.numeric(ex[[1]])

  testthat::expect_equal(out$hazard_name, rep(inventory$hazard_name, 2))
  testthat::expect_equal(out$matching_method, rep("coordinates", 2))
  testthat::expect_equal(out$hazard_intensity, expected_vals)
})


testthat::test_that("mixed assets use priority: coordinates > municipality > state", {
  base_dir <- get_test_data_dir()
  precomputed <- read_precomputed_hazards(base_dir)
  hazard_data <- load_hazards_and_inventory(get_hazards_dir(), aggregate_factor = 16L)

  # Define events with NetCDF hazards for focused testing
  events <- tibble::tibble(
    hazard_name = c(
      "Drought__SPI3__GWL=present__RP=10__season=Summer__ensemble=mean" # NC hazard with season
    ),
    event_year = 2030,
  )

  # Filter hazards to match events (like the real pipeline)
  all_hazards <- hazard_data$hazards
  hazards <- filter_hazards_by_events(all_hazards, events)
  inventory <- hazard_data$inventory |>
    dplyr::filter(.data$hazard_name %in% names(hazards))

  # Create 3 assets demonstrating priority cascade
  assets <- tibble::tibble(
    asset = c("asset_coords", "asset_municipality", "asset_state"),
    company = rep("company_a", 3),
    latitude = c(-3.0, NA_real_, NA_real_),
    longitude = c(-60.0, NA_real_, NA_real_),
    municipality = c("Barcelos", "Barcelos", NA_character_),
    state = c("Amazonas", "Amazonas", "Amazonas"),
    asset_category = "office",
    asset_subtype = NA_character_,
    size_in_m2 = 1000,
    share_of_economic_activity = 0.5,
    cnae = NA_real_
  )

  # Extract (pass precomputed for administrative lookup)
  out <- extract_hazard_statistics(assets, hazards, inventory, precomputed)

  # Verify all 3 assets got results
  testthat::expect_equal(length(unique(out$asset)), 3)

  # Verify each asset's matching_method
  asset1_method <- unique(out$matching_method[out$asset == "asset_coords"])
  asset2_method <- unique(out$matching_method[out$asset == "asset_municipality"])
  asset3_method <- unique(out$matching_method[out$asset == "asset_state"])

  testthat::expect_equal(asset1_method, "coordinates")
  testthat::expect_equal(asset2_method, "municipality")
  testthat::expect_equal(asset3_method, "state")

  # Verify source field for precomputed data
  asset2_source <- unique(out$source[out$asset == "asset_municipality"])
  asset3_source <- unique(out$source[out$asset == "asset_state"])
  testthat::expect_equal(asset2_source, "precomputed (municipality)")
  testthat::expect_equal(asset3_source, "precomputed (state)")

  # Verify all get valid hazard statistics
  testthat::expect_true(all(is.numeric(out$hazard_intensity)))
  testthat::expect_true(all(!is.na(out$hazard_intensity)))
})


testthat::test_that("extract_precomputed_statistics errors when a required hazard is missing", {
  assets <- tibble::tibble(
    asset = "asset_missing",
    company = "company_a",
    latitude = NA_real_,
    longitude = NA_real_,
    municipality = "TestMunicipality",
    state = "TestProvince",
    asset_category = "office",
    asset_subtype = NA_character_,
    size_in_m2 = 1000,
    share_of_economic_activity = 1,
    cnae = NA_real_
  )

  hazards_inventory <- tibble::tibble(
    hazard_name = c(
      "Flood__depth(cm)__GWL=pc__RP=10__ensemble=mean",
      "Drought__SPI3__GWL=present__RP=5__season=Summer__ensemble=mean"
    ),
    hazard_type = c("Flood", "Drought"),
    hazard_indicator = c("depth(cm)", "SPI3"),
    hazard_return_period = c(10, 5),
    scenario_name = c("CurrentClimate", "present"),
    source = c("nc")
  )

  precomputed_hazards <- tibble::tibble(
    region = "TestMunicipality",
    adm_level = "ADM2",
    hazard_name = "Flood__depth(cm)__GWL=pc__RP=10__ensemble=mean",
    hazard_type = "Flood",
    hazard_indicator = "depth(cm)",
    hazard_return_period = 10,
    scenario_name = "CurrentClimate",
    aggregation_method = "mean",
    hazard_value = 0.2
  )

  testthat::expect_error(
    extract_precomputed_statistics(
      assets_df = assets,
      precomputed_hazards = precomputed_hazards,
      hazards_inventory = hazards_inventory,
      aggregation_method = "mean"
    ),
    regexp = "Missing precomputed hazard data.*Drought__SPI3__GWL=present__RP=5__season=Summer__ensemble=mean"
  )
})

testthat::test_that("extract_precomputed_statistics matches precomputed hazards even if inventory hazard_name includes ensemble suffix", {
  assets <- tibble::tibble(
    asset = "asset_state_only",
    company = "company_a",
    latitude = NA_real_,
    longitude = NA_real_,
    municipality = NA_character_,
    state = "Amazonas",
    asset_category = "office",
    asset_subtype = NA_character_,
    size_in_m2 = 1000,
    share_of_economic_activity = 1,
    cnae = NA_real_
  )

  # Inventory hazard_name includes ensemble suffix (as loaded from NC hazards),
  # but precomputed hazard_name commonly does not (CSV ensemble is often missing).
  hazards_inventory <- tibble::tibble(
    hazard_name = "Flood__depth(cm)__GWL=rcp26__RP=100__ensemble=mean",
    hazard_type = "Flood",
    hazard_indicator = "depth(cm)",
    hazard_return_period = 100,
    scenario_name = "rcp26",
    source = "nc"
  )

  # Precomputed hazard_name has no ensemble suffix, but it provides hazard_value by aggregation_method
  precomputed_hazards <- tibble::tibble(
    region = "Amazonas",
    adm_level = "ADM1",
    hazard_name = "Flood__depth(cm)__GWL=rcp26__RP=100__ensemble=mean",
    hazard_type = "Flood",
    hazard_indicator = "depth(cm)",
    hazard_return_period = 100,
    scenario_name = "rcp26",
    aggregation_method = c("mean", "median"),
    hazard_value = c(1.23, 4.56)
  )

  out <- climate.risk.tool:::extract_precomputed_statistics(
    assets_df = assets,
    precomputed_hazards = precomputed_hazards,
    hazards_inventory = hazards_inventory,
    aggregation_method = "median"
  )

  testthat::expect_equal(unique(out$matching_method), "state")
  testthat::expect_equal(unique(out$source), "precomputed (state)")
  testthat::expect_equal(unique(out$hazard_intensity), 4.56)
  testthat::expect_equal(
    unique(out$hazard_name),
    "Flood__depth(cm)__GWL=rcp26__RP=100__ensemble=mean"
  )
  # Verify only one row is returned (not multiple aggregation methods)
  testthat::expect_equal(nrow(out), 1)
})

testthat::test_that("extract_precomputed_statistics filters to only selected aggregation method", {
  # Test that the filter correctly returns only one aggregation method
  assets <- tibble::tibble(
    asset = "test_asset",
    company = "company_a",
    latitude = NA_real_,
    longitude = NA_real_,
    municipality = NA_character_,
    state = "TestState",
    asset_category = "office",
    asset_subtype = NA_character_,
    size_in_m2 = 1000,
    share_of_economic_activity = 1,
    cnae = NA_real_
  )

  hazards_inventory <- tibble::tibble(
    hazard_name = "Flood__depth(cm)__GWL=rcp85__RP=100__ensemble=mean",
    hazard_type = "Flood",
    hazard_indicator = "depth(cm)",
    hazard_return_period = 100,
    scenario_name = "rcp85",
    source = "nc"
  )

  # Precomputed data with all 6 aggregation methods
  precomputed_hazards <- tibble::tibble(
    region = "TestState",
    adm_level = "ADM1",
    hazard_name = "Flood__depth(cm)__GWL=rcp85__RP=100__ensemble=mean",
    hazard_type = "Flood",
    hazard_indicator = "depth(cm)",
    hazard_return_period = 100,
    scenario_name = "rcp85",
    aggregation_method = c("mean", "median", "p2_5", "p5", "p95", "p97_5"),
    hazard_value = c(963, 882, 41, 82, 2103, 2279)
  )

  # Test with "mean" aggregation method
  out_mean <- climate.risk.tool:::extract_precomputed_statistics(
    assets_df = assets,
    precomputed_hazards = precomputed_hazards,
    hazards_inventory = hazards_inventory,
    aggregation_method = "mean"
  )

  testthat::expect_equal(nrow(out_mean), 1)
  testthat::expect_equal(unique(out_mean$hazard_intensity), 963)
  
  # Test with "median" aggregation method
  out_median <- climate.risk.tool:::extract_precomputed_statistics(
    assets_df = assets,
    precomputed_hazards = precomputed_hazards,
    hazards_inventory = hazards_inventory,
    aggregation_method = "median"
  )

  testthat::expect_equal(nrow(out_median), 1)
  testthat::expect_equal(unique(out_median$hazard_intensity), 882)
  
  # Test with "p95" aggregation method
  out_p95 <- climate.risk.tool:::extract_precomputed_statistics(
    assets_df = assets,
    precomputed_hazards = precomputed_hazards,
    hazards_inventory = hazards_inventory,
    aggregation_method = "p95"
  )

  testthat::expect_equal(nrow(out_p95), 1)
  testthat::expect_equal(unique(out_p95$hazard_intensity), 2103)
})


testthat::test_that("extract_hazard_statistics errors for missing precomputed hazard, scenario or region", {
  base_dir <- get_test_data_dir()
  precomputed <- read_precomputed_hazards(base_dir)
  hazard_data <- load_hazards_and_inventory(get_hazards_dir(), aggregate_factor = 16L)

  # --- CASE 1: missing hazard/scenario/return period in precomputed data ---
  # Create asset with only province (forces precomputed lookup)
  asset_missing_hazard <- tibble::tibble(
    asset = "test_asset",
    company = "company_a",
    latitude = NA_real_,
    longitude = NA_real_,
    municipality = NA_character_,
    state = "Amazonas",
    asset_category = "office",
    asset_subtype = NA_character_,
    size_in_m2 = 1000,
    share_of_economic_activity = 0.5,
    cnae = NA_real_
  )

  # Find a hazard combo in inventory NOT in precomputed for Amazonas
  precomputed_combos <- precomputed |>
    dplyr::filter(.data$region == "Amazonas", .data$adm_level == "ADM1") |>
    dplyr::distinct(.data$hazard_type, .data$scenario_name, .data$hazard_return_period)

  missing_hazard <- hazard_data$inventory |>
    dplyr::anti_join(
      precomputed_combos,
      by = c("hazard_type", "scenario_name", "hazard_return_period")
    ) |>
    # Exclude Fire/land_cover which has special handling (synthetic default value)
    dplyr::filter(!(.data$hazard_type == "Fire" & .data$hazard_indicator == "land_cover"))

  # Skip test if no missing hazards found (all hazards in inventory are in precomputed)
  testthat::skip_if(
    nrow(missing_hazard) == 0,
    "No missing hazards found - all hazards in inventory are available in precomputed data (excluding Fire/land_cover which has special handling)"
  )
  
  # Create events with this missing hazard
  events <- tibble::tibble(
    hazard_name = missing_hazard$hazard_name[1],
    event_year = 2030,
  )

  # Filter to just this hazard
  all_hazards <- hazard_data$hazards
  hazards <- filter_hazards_by_events(all_hazards, events)
  # Use full inventory so the function can check if required hazards are in precomputed
  # The missing hazard should be in inventory but not in precomputed
  inventory <- hazard_data$inventory |>
    dplyr::filter(.data$hazard_name == missing_hazard$hazard_name[1])

  # Should error with message about missing hazards
  # Note: The function will error if a hazard in inventory is not found in precomputed data
  testthat::expect_error(
    extract_hazard_statistics(asset_missing_hazard, hazards, inventory, precomputed),
    regexp = "Missing precomputed hazard data"
  )

  # --- CASE 2: missing municipality and province in precomputed data ---
  asset_noregion <- tibble::tibble(
    asset = "test_asset_noregion",
    company = "company_a",
    latitude = NA_real_,
    longitude = NA_real_,
    municipality = "NonExistentMunicipality12345",
    state = "NonExistentProvince67890",
    asset_category = "office",
    asset_subtype = NA_character_,
    size_in_m2 = 1000,
    share_of_economic_activity = 0.5,
    cnae = NA_real_
  )
  events2 <- tibble::tibble(
    hazard_name = "Flood__depth(cm)__GWL=CurrentClimate__RP=10__ensemble=mean",
    event_year = 2030,
  )
  all_hazards2 <- c(hazard_data$hazards$tif, hazard_data$hazards$nc)
  hazards2 <- filter_hazards_by_events(all_hazards2, events2)
  inventory2 <- hazard_data$inventory |>
    dplyr::filter(.data$hazard_name %in% names(hazards2))

  expect_case2 <- testthat::expect_error(
    extract_hazard_statistics(asset_noregion, hazards2, inventory2, precomputed),
    regexp = "Cannot determine|not found|NonExistent"
  )


  # final assertions are via the expect_error checks above for both cases
})


# CSV hazards test removed - CSV support has been removed in favor of NetCDF-only


testthat::test_that("agriculture portfolio with state but no municipality works", {
  # Bug reproduction: Portfolio with state data but no municipality should work
  # Error was: "Can't compute column `asset_subtype`"
  
  base_dir <- get_test_data_dir()
  precomputed <- read_precomputed_hazards(base_dir)
  damage_factors <- read_damage_cost_factors(base_dir)
  
  # Create agriculture assets with state but NO municipality
  assets <- tibble::tibble(
    asset = c("farm_1", "farm_2"),
    company = c("agri_company_a", "agri_company_b"),
    latitude = NA_real_,
    longitude = NA_real_,
    municipality = NA_character_,  # No municipality
    state = c("Amazonas", "Mato Grosso"),  # Only state provided (using states from test data)
    asset_category = "agriculture",
    asset_subtype = c("Soybean", "Corn"),  # Valid crop types
    size_in_m2 = 100000,
    share_of_economic_activity = 0.5,
    cnae = NA_real_
  )
  
  # Create drought event (agriculture-specific)
  events <- tibble::tibble(
    hazard_name = "Drought__SPI3__GWL=present__RP=5__season=Summer__ensemble=mean",
    event_year = 2030
  )
  
  # Load hazards
  hazard_data <- load_hazards_and_inventory(get_hazards_dir(), aggregate_factor = 16L)
  
  # Filter hazards (Drought is in NC format, not CSV)
  all_hazards <- hazard_data$hazards
  hazards <- filter_hazards_by_events(all_hazards, events)
  inventory <- hazard_data$inventory |>
    dplyr::filter(.data$hazard_name %in% names(hazards))
  
  # This should NOT error - should use state-level precomputed data
  out <- extract_hazard_statistics(
    assets,
    hazards,
    inventory,
    precomputed,
    damage_factors_df = damage_factors
  )
  
  # Verify results
  # After fix: only one aggregation method (default "mean") is returned per asset
  testthat::expect_equal(nrow(out), 2)  # 2 assets × 1 aggregation method
  testthat::expect_equal(length(unique(out$asset)), 2)  # 2 unique assets
  testthat::expect_true(all(out$matching_method == "state"))  # Should match at state level
  testthat::expect_true(all(out$asset_category == "agriculture"))
  testthat::expect_true(all(!is.na(out$asset_subtype)))  # asset_subtype should be preserved
  testthat::expect_true(all(out$asset_subtype %in% c("Soybean", "Corn")))
  testthat::expect_true(all(!is.na(out$state)))  # State should be preserved
})


testthat::test_that("agriculture portfolio without crop types defaults to Soybean", {
  # Bug reproduction: Portfolio without crop types should default to Soybean
  # According to methodology: when croptype cannot be matched, use Soybean by default
  
  base_dir <- get_test_data_dir()
  precomputed <- read_precomputed_hazards(base_dir)
  damage_factors <- read_damage_cost_factors(base_dir)
  
  # Create agriculture assets WITHOUT crop types (asset_subtype)
  assets <- tibble::tibble(
    asset = c("farm_unknown_1", "farm_unknown_2"),
    company = c("agri_company_c", "agri_company_d"),
    latitude = NA_real_,
    longitude = NA_real_,
    municipality = NA_character_,
    state = c("Amazonas", "Mato Grosso"),  # Using states from test data
    asset_category = "agriculture",
    asset_subtype = NA_character_,  # Missing crop type - should default to Soybean
    size_in_m2 = 100000,
    share_of_economic_activity = 0.5,
    cnae = NA_real_
  )
  
  # Create drought event (agriculture-specific)
  events <- tibble::tibble(
    hazard_name = "Drought__SPI3__GWL=present__RP=5__season=Summer__ensemble=mean",
    event_year = 2030
  )
  
  # Load hazards
  hazard_data <- load_hazards_and_inventory(get_hazards_dir(), aggregate_factor = 16L)
  
  # Filter hazards (Drought is in NC format, not CSV)
  all_hazards <- hazard_data$hazards
  hazards <- filter_hazards_by_events(all_hazards, events)
  inventory <- hazard_data$inventory |>
    dplyr::filter(.data$hazard_name %in% names(hazards))
  
  # This should NOT error - should use Soybean as default crop type
  out <- extract_hazard_statistics(
    assets,
    hazards,
    inventory,
    precomputed,
    damage_factors_df = damage_factors
  )
  
  # Verify results
  # After fix: only one aggregation method (default "mean") is returned per asset
  testthat::expect_equal(nrow(out), 2)  # 2 assets × 1 aggregation method
  testthat::expect_equal(length(unique(out$asset)), 2)  # 2 unique assets
  testthat::expect_true(all(out$matching_method == "state"))  # Should match at state level
  testthat::expect_true(all(out$asset_category == "agriculture"))
  # asset_subtype should still be NA in output (we don't change the original data)
  # but the logic should use "Other"/Soybean internally for damage factor matching
  testthat::expect_true(all(is.na(out$asset_subtype)))
  testthat::expect_true(all(!is.na(out$hazard_intensity)))  # Should have hazard data
})


testthat::test_that("agriculture portfolio with invalid crop types defaults to Soybean", {
  # Bug reproduction: Portfolio with crop types that don't match our 4 crops should default to Soybean
  
  base_dir <- get_test_data_dir()
  precomputed <- read_precomputed_hazards(base_dir)
  damage_factors <- read_damage_cost_factors(base_dir)
  
  # Create agriculture assets with INVALID crop types (not in our 4 supported crops)
  assets <- tibble::tibble(
    asset = c("farm_wheat", "farm_barley"),
    company = c("agri_company_e", "agri_company_f"),
    latitude = NA_real_,
    longitude = NA_real_,
    municipality = NA_character_,
    state = c("Amazonas", "Mato Grosso"),  # Using states from test data
    asset_category = "agriculture",
    asset_subtype = c("Wheat", "Barley"),  # Invalid crop types - not in our damage factors
    size_in_m2 = 100000,
    share_of_economic_activity = 0.5,
    cnae = NA_real_
  )
  
  # Create drought event (agriculture-specific)
  events <- tibble::tibble(
    hazard_name = "Drought__SPI3__GWL=present__RP=5__season=Summer__ensemble=mean",
    event_year = 2030
  )
  
  # Load hazards
  hazard_data <- load_hazards_and_inventory(get_hazards_dir(), aggregate_factor = 16L)
  
  # Filter hazards (Drought is in NC format, not CSV)
  all_hazards <- hazard_data$hazards
  hazards <- filter_hazards_by_events(all_hazards, events)
  inventory <- hazard_data$inventory |>
    dplyr::filter(.data$hazard_name %in% names(hazards))
  
  # This should NOT error - should use Soybean as default for unrecognized crops
  out <- extract_hazard_statistics(
    assets,
    hazards,
    inventory,
    precomputed,
    damage_factors_df = damage_factors
  )
  
  # Verify results
  # After fix: only one aggregation method (default "mean") is returned per asset
  testthat::expect_equal(nrow(out), 2)  # 2 assets × 1 aggregation method
  testthat::expect_equal(length(unique(out$asset)), 2)  # 2 unique assets
  testthat::expect_true(all(out$matching_method == "state"))  # Should match at state level
  testthat::expect_true(all(out$asset_category == "agriculture"))
  # asset_subtype should be preserved as-is in output
  testthat::expect_true(all(out$asset_subtype %in% c("Wheat", "Barley")))
  testthat::expect_true(all(!is.na(out$hazard_intensity)))  # Should have hazard data
})
