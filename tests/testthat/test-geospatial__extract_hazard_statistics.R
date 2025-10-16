# Tests for function: extract_hazard_statistics
#
# Contract:
# - extract_hazard_statistics(assets_df, hazards, hazards_inventory, precomputed_hazards, events)
# - For assets WITH coordinates: performs spatial extraction using raster data (TIF or NC)
# - For assets WITHOUT coordinates but WITH municipality: looks up precomputed ADM2 data
# - For assets WITHOUT coordinates/municipality but WITH province: looks up precomputed ADM1 data
# - Returns long format with hazard statistics for all assets
# - Priority cascade: coordinates > municipality > province
# - Raises error if an asset cannot be matched or if required hazards are missing

# ==============================================================================
# Test 1: TIF extraction with geolocated assets
# ==============================================================================

testthat::test_that("geolocated assets extract from TIF files", {
  # Load all hazards
  hazard_data <- load_hazards_and_inventory(get_hazards_dir(), aggregate_factor = 16L)
  
  # Define events with just 1 TIF hazard for focused testing (using new unified format)
  events <- tibble::tibble(
    hazard_name = "FloodTIF__Flood Height__GWL=CurrentClimate__RP=10__ensemble=mean",
    event_year = 2030,
    chronic = FALSE
  )
  
  # Filter to just the selected hazard (like the real pipeline does)
  tif_hazards <- filter_hazards_by_events(hazard_data$hazards$tif, events)
  tif_inventory <- hazard_data$inventory |> 
    dplyr::filter(.data$source == "tif", .data$hazard_name %in% names(tif_hazards))
  
  # Create 2 assets with coordinates (no municipality/province)
  assets <- tibble::tibble(
    asset = c("asset_tif_1", "asset_tif_2"),
    company = c("company_a", "company_b"),
    latitude = c(-3.0, -15.0),
    longitude = c(-60.0, -47.9),
    municipality = NA_character_,
    province = NA_character_,
    asset_category = "office",
    size_in_m2 = 1000,
    share_of_economic_activity = 0.5
  )
  
  # Extract
  out <- extract_hazard_statistics(
    assets,
    tif_hazards,
    tif_inventory,
    precomputed_hazards = tibble::tibble()
  )
  
  # Verify: all matching_method = "coordinates"
  testthat::expect_true(all(out$matching_method == "coordinates"))
  
  # Verify: hazard statistics are numeric and >= 0
  testthat::expect_true(is.numeric(out$hazard_mean))
  testthat::expect_true(is.numeric(out$hazard_max))
  testthat::expect_true(all(out$hazard_mean >= 0))
  testthat::expect_true(all(out$hazard_max >= 0))
  
  # Should have results for both assets
  testthat::expect_equal(length(unique(out$asset)), 2)
})


# ==============================================================================
# Test 2: NC extraction with geolocated assets
# ==============================================================================

testthat::test_that("geolocated assets extract from NC files", {
  # Load all hazards
  hazard_data <- load_hazards_and_inventory(get_hazards_dir(), aggregate_factor = 16L)
  
  # Define events with just 1 NC hazard for focused testing
  # NC hazards expand to all ensemble variants automatically
  events <- tibble::tibble(
    hazard_name = "Drought__SPI6__GWL=present__RP=10__ensemble=mean",
    event_year = 2030,
    chronic = FALSE
  )
  
  # Filter to just the selected hazard (expands to all ensemble variants)
  nc_hazards <- filter_hazards_by_events(hazard_data$hazards$nc, events)
  nc_inventory <- hazard_data$inventory |> 
    dplyr::filter(.data$hazard_name %in% names(nc_hazards))
  
  # Create 2 assets with coordinates
  assets <- tibble::tibble(
    asset = c("asset_nc_1", "asset_nc_2"),
    company = c("company_a", "company_b"),
    latitude = c(-3.0, -15.0),
    longitude = c(-60.0, -47.9),
    municipality = NA_character_,
    province = NA_character_,
    asset_category = "office",
    size_in_m2 = 1000,
    share_of_economic_activity = 0.5
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
  
  # Verify: ensemble columns populated (mean, median, p10, p90)
  testthat::expect_true("hazard_mean" %in% names(out))
  testthat::expect_true("hazard_median" %in% names(out))
  testthat::expect_true("hazard_p10" %in% names(out))
  testthat::expect_true("hazard_p90" %in% names(out))
  
  # Verify: ensemble values are numeric
  testthat::expect_true(is.numeric(out$hazard_mean))
  testthat::expect_true(is.numeric(out$hazard_median))
  
  # Should have results for both assets
  testthat::expect_equal(length(unique(out$asset)), 2)
})


# ==============================================================================
# Test 3: Priority cascade (coordinates > municipality > province)
# ==============================================================================

testthat::test_that("mixed assets use priority: coordinates > municipality > province", {
  base_dir <- get_test_data_dir()
  precomputed <- read_precomputed_hazards(base_dir)
  hazard_data <- load_hazards_and_inventory(get_hazards_dir(), aggregate_factor = 16L)
  
  # Define events with just 2 hazards (1 TIF + 1 NC) for focused testing
  events <- tibble::tibble(
    hazard_name = c(
      "flood__pc_h10glob",  # TIF hazard
      "Drought__SPI6__GWL=present__RP=10__ensemble=mean"  # NC hazard (expands to all ensembles)
    ),
    event_year = 2030,
    chronic = FALSE
  )
  
  # Filter hazards to match events (like the real pipeline)
  all_hazards <- c(hazard_data$hazards$tif, hazard_data$hazards$nc)
  hazards <- filter_hazards_by_events(all_hazards, events)
  inventory <- hazard_data$inventory |> 
    dplyr::filter(.data$hazard_name %in% names(hazards))
  
  # Create 3 assets demonstrating priority cascade
  assets <- tibble::tibble(
    asset = c("asset_coords", "asset_municipality", "asset_province"),
    company = rep("company_a", 3),
    latitude = c(-3.0, NA_real_, NA_real_),
    longitude = c(-60.0, NA_real_, NA_real_),
    municipality = c("Borba", "Borba", NA_character_),
    province = c("Amazonas", "Amazonas", "Amazonas"),
    asset_category = "office",
    size_in_m2 = 1000,
    share_of_economic_activity = 0.5
  )
  
  # Extract (pass events for precomputed validation)
  out <- extract_hazard_statistics(assets, hazards, inventory, precomputed, events)
  
  # Verify all 3 assets got results
  testthat::expect_equal(length(unique(out$asset)), 3)
  
  # Verify each asset's matching_method
  asset1_method <- unique(out$matching_method[out$asset == "asset_coords"])
  asset2_method <- unique(out$matching_method[out$asset == "asset_municipality"])
  asset3_method <- unique(out$matching_method[out$asset == "asset_province"])
  
  testthat::expect_equal(asset1_method, "coordinates")
  testthat::expect_equal(asset2_method, "municipality")
  testthat::expect_equal(asset3_method, "province")
  
  # Verify all get valid hazard statistics
  testthat::expect_true(all(is.numeric(out$hazard_mean)))
  testthat::expect_true(all(!is.na(out$hazard_mean)))
})


# ==============================================================================
# Test 4: Error when hazard/scenario/return period not in precomputed
# ==============================================================================

testthat::test_that("error when hazard/scenario/return period not in precomputed data", {
  base_dir <- get_test_data_dir()
  precomputed <- read_precomputed_hazards(base_dir)
  hazard_data <- load_hazards_and_inventory(get_hazards_dir(), aggregate_factor = 16L)
  
  # Create asset with only province (forces precomputed lookup)
  assets <- tibble::tibble(
    asset = "test_asset",
    company = "company_a",
    latitude = NA_real_,
    longitude = NA_real_,
    municipality = NA_character_,
    province = "Amazonas",
    asset_category = "office",
    size_in_m2 = 1000,
    share_of_economic_activity = 0.5
  )
  
  # Find a hazard combo in inventory NOT in precomputed for Amazonas
  precomputed_combos <- precomputed |>
    dplyr::filter(.data$region == "Amazonas", .data$adm_level == "ADM1") |>
    dplyr::distinct(.data$hazard_type, .data$scenario_code, .data$hazard_return_period)
  
  missing_hazard <- hazard_data$inventory |>
    dplyr::anti_join(
      precomputed_combos,
      by = c("hazard_type", "scenario_code", "hazard_return_period")
    )
  
  if (nrow(missing_hazard) > 0) {
    # Create events with this missing hazard
    events <- tibble::tibble(
      hazard_name = missing_hazard$hazard_name[1],
      event_year = 2030,
      chronic = FALSE
    )
    
    # Filter to just this hazard
    all_hazards <- c(hazard_data$hazards$tif, hazard_data$hazards$nc)
    hazards <- filter_hazards_by_events(all_hazards, events)
    inventory <- hazard_data$inventory |> 
      dplyr::filter(.data$hazard_name %in% names(hazards))
    
    # Should error with message about missing hazards
    testthat::expect_error(
      extract_hazard_statistics(assets, hazards, inventory, precomputed, events),
      regexp = "Required hazards.*not available|missing"
    )
  } else {
    # If all hazards are available, skip this test
    testthat::skip("No missing hazard combinations available for testing")
  }
})


# ==============================================================================
# Test 5: Error when region not in precomputed
# ==============================================================================

testthat::test_that("error when municipality/province not in precomputed data", {
  base_dir <- get_test_data_dir()
  precomputed <- read_precomputed_hazards(base_dir)
  hazard_data <- load_hazards_and_inventory(get_hazards_dir(), aggregate_factor = 16L)
  
  # Define events with just 1 hazard for focused testing
  events <- tibble::tibble(
    hazard_name = "flood__pc_h10glob",
    event_year = 2030,
    chronic = FALSE
  )
  
  # Filter to just the selected hazard
  all_hazards <- c(hazard_data$hazards$tif, hazard_data$hazards$nc)
  hazards <- filter_hazards_by_events(all_hazards, events)
  inventory <- hazard_data$inventory |> 
    dplyr::filter(.data$hazard_name %in% names(hazards))
  
  # Create asset with non-existent municipality AND province
  assets <- tibble::tibble(
    asset = "test_asset_noregion",
    company = "company_a",
    latitude = NA_real_,
    longitude = NA_real_,
    municipality = "NonExistentMunicipality12345",
    province = "NonExistentProvince67890",
    asset_category = "office",
    size_in_m2 = 1000,
    share_of_economic_activity = 0.5
  )
  
  # Should error mentioning the missing regions
  testthat::expect_error(
    extract_hazard_statistics(assets, hazards, inventory, precomputed),
    regexp = "Cannot determine|not found|NonExistent"
  )
})
