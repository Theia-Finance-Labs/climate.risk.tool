testthat::test_that("compute_risk orchestrates new yearly trajectory functions", {
  base_dir <- get_test_data_dir()
  assets <- read_assets(base_dir)
  companies <- read_companies(file.path(base_dir, "user_input", "company.csv"))
  hazard_data <- load_hazards_and_inventory(file.path(base_dir, "hazards"), aggregate_factor = 16L)
  hazards <- c(hazard_data$hazards$tif, hazard_data$hazards$nc)
  precomputed_hazards <- read_precomputed_hazards(base_dir)
  damage_factors <- read_damage_cost_factors(base_dir)
  inventory <- hazard_data$inventory

  # Define two events - one acute, one chronic
  # Use pc (CurrentClimate) which exists in precomputed data for all regions
  events <- data.frame(
    event_id = c("e1", "e2"),
    hazard_type = rep("FloodTIF", 2),
    hazard_name = rep("FloodTIF__Flood Height__GWL=CurrentClimate__RP=10", 2),
    scenario_name = rep("CurrentClimate", 2),
    hazard_return_period = rep(10, 2),
    event_year = c(2030L, NA_integer_),
    chronic = c(FALSE, TRUE)
  )

  # The refactored compute_risk should return both aggregated and yearly data
  res <- compute_risk(
    assets = assets,
    companies = companies,
    events = events,
    hazards = hazards,
    hazards_inventory = inventory,
    precomputed_hazards = precomputed_hazards,
    damage_factors = damage_factors,
    growth_rate = 0.02,
    net_profit_margin = 0.1,
    discount_rate = 0.05
  )

  # Should have both aggregated and yearly results
  expected_components <- c("assets_factors", "companies", "assets_yearly", "companies_yearly")
  testthat::expect_true(all(expected_components %in% names(res)))

  # Aggregated results should be data frames
  testthat::expect_s3_class(res$assets_factors, "data.frame")
  testthat::expect_s3_class(res$companies, "data.frame")
  testthat::expect_s3_class(res$assets_yearly, "data.frame")
  testthat::expect_s3_class(res$companies_yearly, "data.frame")

  # Yearly results should have more rows than aggregated (multiple years per asset/company)
  testthat::expect_true(nrow(res$assets_yearly) > nrow(res$assets_factors))
  testthat::expect_true(nrow(res$companies_yearly) > nrow(res$companies))

  # Yearly assets should have year and scenario columns
  testthat::expect_true("year" %in% names(res$assets_yearly))
  testthat::expect_true("scenario" %in% names(res$assets_yearly))
  testthat::expect_true("year" %in% names(res$companies_yearly))
  testthat::expect_true("scenario" %in% names(res$companies_yearly))

  # Should have baseline and shock scenarios
  testthat::expect_true(all(c("baseline", "shock") %in% unique(res$assets_yearly$scenario)))
  testthat::expect_true(all(c("baseline", "shock") %in% unique(res$companies_yearly$scenario)))

  # Should have matching_method column in assets_factors
  testthat::expect_true("matching_method" %in% names(res$assets_factors))
  testthat::expect_true(all(res$assets_factors$matching_method %in% c("coordinates", "municipality", "province")))

  # Should have event information columns in assets_factors
  testthat::expect_true("hazard_return_period" %in% names(res$assets_factors))
  testthat::expect_true("event_year" %in% names(res$assets_factors))
  testthat::expect_true("chronic" %in% names(res$assets_factors))

  # All assets should have matching hazards from events (no NAs)
  testthat::expect_true(all(!is.na(res$assets_factors$hazard_return_period)))
  testthat::expect_true(all(!is.na(res$assets_factors$chronic)))

  # Verify event_year and chronic values match the events
  testthat::expect_true(all(res$assets_factors$hazard_return_period %in% events$hazard_return_period))
  testthat::expect_true(all(res$assets_factors$chronic %in% events$chronic))

  # When multiple events share the same hazard_name, each event should create a separate row
  # In this test, we have 2 events both using "flood__rcp85_h10glob"
  # So we expect each asset to appear twice (once for each event)
  expected_rows_per_asset <- length(unique(events$event_id))
  assets_per_hazard <- res$assets_factors |>
    dplyr::group_by(.data$asset) |>
    dplyr::summarise(n = dplyr::n(), .groups = "drop")
  testthat::expect_true(all(assets_per_hazard$n == expected_rows_per_asset))
})

testthat::test_that("compute_risk processes single acute event", {
  base_dir <- get_test_data_dir()
  assets <- read_assets(base_dir)
  companies <- read_companies(file.path(base_dir, "user_input", "company.csv"))
  hazard_data <- load_hazards_and_inventory(file.path(base_dir, "hazards"), aggregate_factor = 16L)
  hazards <- c(hazard_data$hazards$tif, hazard_data$hazards$nc)
  precomputed_hazards <- read_precomputed_hazards(base_dir)
  damage_factors <- read_damage_cost_factors(base_dir)
  inventory <- hazard_data$inventory

  # Single acute event
  # Use pc (CurrentClimate) which exists in precomputed data
  events <- data.frame(
    event_id = "acute_2030",
    hazard_type = "FloodTIF",
    hazard_name = "FloodTIF__Flood Height__GWL=CurrentClimate__RP=10",
    scenario_name = "CurrentClimate",
    hazard_return_period = 10,
    event_year = 2030L,
    chronic = FALSE
  )

  res <- compute_risk(
    assets = assets,
    companies = companies,
    events = events,
    hazards = hazards,
    hazards_inventory = inventory,
    precomputed_hazards = precomputed_hazards,
    damage_factors = damage_factors
  )

  # Should have valid results
  testthat::expect_true(nrow(res$assets_factors) > 0)
  testthat::expect_true(nrow(res$companies) > 0)

  # Yearly data should show shock effects only from 2030 onwards
  yearly_assets <- res$assets_yearly
  pre_shock <- yearly_assets[yearly_assets$year < 2030 & yearly_assets$scenario == "shock", ]
  shock_year <- yearly_assets[yearly_assets$year >= 2030 & yearly_assets$scenario == "shock", ]
  baseline_pre <- yearly_assets[yearly_assets$year < 2030 & yearly_assets$scenario == "baseline", ]
  baseline_shock <- yearly_assets[yearly_assets$year >= 2030 & yearly_assets$scenario == "baseline", ]

  if (nrow(pre_shock) > 0 && nrow(baseline_pre) > 0) {
    # Pre-shock years should be same for baseline and shock
    testthat::expect_equal(mean(pre_shock$revenue), mean(baseline_pre$revenue))
  }

  if (nrow(shock_year) > 0 && nrow(baseline_shock) > 0) {
    # Shock year should have lower revenue than baseline
    testthat::expect_true(mean(shock_year$revenue) <= mean(baseline_shock$revenue))
  }
})

testthat::test_that("compute_risk processes chronic event", {
  base_dir <- get_test_data_dir()
  assets <- read_assets(base_dir)
  companies <- read_companies(file.path(base_dir, "user_input", "company.csv"))
  hazard_data <- load_hazards_and_inventory(file.path(base_dir, "hazards"), aggregate_factor = 16L)
  hazards <- c(hazard_data$hazards$tif, hazard_data$hazards$nc)
  precomputed_hazards <- read_precomputed_hazards(base_dir)
  damage_factors <- read_damage_cost_factors(base_dir)
  inventory <- hazard_data$inventory

  # Single chronic event
  # Use pc (CurrentClimate) which exists in precomputed data
  events <- data.frame(
    event_id = "chronic",
    hazard_type = "FloodTIF",
    hazard_name = "FloodTIF__Flood Height__GWL=CurrentClimate__RP=10",
    scenario_name = "CurrentClimate",
    hazard_return_period = 10,
    event_year = NA_integer_,
    chronic = TRUE
  )

  res <- compute_risk(
    assets = assets,
    companies = companies,
    events = events,
    hazards = hazards,
    hazards_inventory = inventory,
    precomputed_hazards = precomputed_hazards,
    damage_factors = damage_factors
  )

  # Should have valid results
  testthat::expect_true(nrow(res$assets_factors) > 0)
  testthat::expect_true(nrow(res$companies) > 0)

  # Chronic effects should compound over time
  yearly_assets <- res$assets_yearly
  shock_data <- yearly_assets[yearly_assets$scenario == "shock", ]
  baseline_data <- yearly_assets[yearly_assets$scenario == "baseline", ]

  if (nrow(shock_data) > 0 && nrow(baseline_data) > 0) {
    # Shock should have lower total revenue than baseline
    total_shock_revenue <- sum(shock_data$revenue)
    total_baseline_revenue <- sum(baseline_data$revenue)
    testthat::expect_true(total_shock_revenue <= total_baseline_revenue)
  }
})


testthat::test_that("compute_risk validates required parameters", {
  base_dir <- get_test_data_dir()
  assets <- read_assets(base_dir)
  companies <- read_companies(file.path(base_dir, "user_input", "company.csv"))
  hazard_data <- load_hazards_and_inventory(file.path(base_dir, "hazards"), aggregate_factor = 16L)
  hazards <- c(hazard_data$hazards$tif, hazard_data$hazards$nc)
  precomputed_hazards <- read_precomputed_hazards(base_dir)
  damage_factors <- read_damage_cost_factors(base_dir)
  inventory <- hazard_data$inventory

  events <- data.frame(
    event_id = "test",
    hazard_type = "flood",
    hazard_name = inventory$hazard_name[1],
    scenario_name = inventory$scenario_name[1],
    hazard_return_period = inventory$hazard_return_period[1],
    event_year = 2030L,
    chronic = FALSE
  )

  # Test missing hazards parameter
  testthat::expect_error(
    compute_risk(
      assets = assets,
      companies = companies,
      events = events,
      hazards = NULL,
      hazards_inventory = inventory,
      precomputed_hazards = precomputed_hazards,
      damage_factors = damage_factors
    ),
    "hazards must be a non-empty named list"
  )

  # Test missing precomputed_hazards parameter
  testthat::expect_error(
    compute_risk(
      assets = assets,
      companies = companies,
      events = events,
      hazards = hazards,
      hazards_inventory = inventory,
      precomputed_hazards = NULL,
      damage_factors = damage_factors
    ),
    "precomputed_hazards must be"
  )
})

testthat::test_that("compute_risk carries hazard_name through to events", {
  base_dir <- get_test_data_dir()
  assets <- read_assets(base_dir)
  hazard_data <- load_hazards_and_inventory(get_hazards_dir(), aggregate_factor = 16L)
  hazards <- c(hazard_data$hazards$tif, hazard_data$hazards$nc)
  precomputed_hazards <- read_precomputed_hazards(base_dir)
  inventory <- hazard_data$inventory

  # Filter assets to only those with coordinates to avoid precomputed data issues
  assets_with_coords <- assets |>
    dplyr::filter(!is.na(.data$latitude), !is.na(.data$longitude))

  # Use pc (CurrentClimate) which exists in precomputed data
  events <- data.frame(
    event_id = "ev1",
    hazard_type = "FloodTIF",
    hazard_name = "FloodTIF__Flood Height__GWL=CurrentClimate__RP=10",
    scenario_name = "CurrentClimate",
    hazard_return_period = 10,
    event_year = 2030,
    chronic = FALSE
  )

  companies <- read_companies(file.path(base_dir, "user_input", "company.csv"))
  damage_factors <- read_damage_cost_factors(base_dir)

  expect_error(
    compute_risk(
      assets = assets_with_coords,
      companies = companies,
      events = events,
      hazards = hazards,
      hazards_inventory = inventory,
      precomputed_hazards = precomputed_hazards,
      damage_factors = damage_factors
    ),
    NA
  )
})


# ==============================================================================
# Bug tests for precomputed hazards matching
# ==============================================================================

testthat::test_that("Bug 1: Error when selected hazard missing from precomputed results", {
  # Setup: Load test data
  base_dir <- get_test_data_dir()
  assets <- read_assets(base_dir)
  companies <- read_companies(file.path(base_dir, "user_input", "company.csv"))
  hazard_data <- load_hazards_and_inventory(file.path(base_dir, "hazards"), aggregate_factor = 16L)
  hazards <- c(hazard_data$hazards$tif, hazard_data$hazards$nc)
  precomputed_hazards <- read_precomputed_hazards(base_dir)
  damage_factors <- read_damage_cost_factors(base_dir)
  inventory <- hazard_data$inventory

  # Create an asset WITHOUT coordinates (will use precomputed lookup)
  test_assets <- assets[1:2, , drop = FALSE]
  test_assets$latitude[1] <- NA_real_
  test_assets$longitude[1] <- NA_real_
  test_assets$municipality[1] <- "Borba" # Exists in precomputed data
  test_assets$province[1] <- "Amazonas"

  # Asset 2: has coordinates (for comparison)
  test_assets$latitude[2] <- -15.0
  test_assets$longitude[2] <- -47.9
  test_assets$municipality[2] <- NA_character_
  test_assets$province[2] <- NA_character_

  # Check what hazards exist in precomputed data for this region
  # The precomputed data has: flood with scenario "pc" (CurrentClimate), return period 10
  available_precomp <- precomputed_hazards |>
    dplyr::filter(.data$region == "Borba", .data$adm_level == "ADM2")

  message("Available precomputed hazards for Borba:")
  message("  hazard_type: ", paste(unique(available_precomp$hazard_type), collapse = ", "))
  message("  scenario_code: ", paste(unique(available_precomp$scenario_code), collapse = ", "))
  message("  hazard_return_period: ", paste(unique(available_precomp$hazard_return_period), collapse = ", "))

  # Define an event that uses a hazard NOT in the precomputed data
  # Use scenario "rcp85" which should NOT exist for Borba in precomputed
  # (precomputed only has "pc" / CurrentClimate)
  events <- data.frame(
    event_id = "e1",
    hazard_type = "flood",
    hazard_name = "flood__rcp85_h10glob", # This exists in TIF hazards but not in precomputed
    scenario_name = "RCP8.5",
    hazard_return_period = 10,
    event_year = 2030L,
    chronic = FALSE
  )

  # This should raise an error because:
  # - Asset 1 requires precomputed lookup (no coordinates)
  # - The selected hazard (flood__rcp85_h10glob) is not available in precomputed data for Borba
  # - Currently, the code might either:
  #   a) Match to a different hazard from precomputed (BUG)
  #   b) Not raise an error when it should (BUG)

  # Expected behavior: Should raise an error saying the required hazard is missing
  testthat::expect_error(
    compute_risk(
      assets = test_assets,
      companies = companies,
      events = events,
      hazards = hazards,
      hazards_inventory = inventory,
      precomputed_hazards = precomputed_hazards,
      damage_factors = damage_factors
    ),
    regexp = "No data found.*No match found.*required hazards",
    info = "Should error when event hazard is missing from precomputed data for a region"
  )
})


testthat::test_that("Bug 2: Assets matched to wrong hazard via precomputed lookup", {
  # Setup: Load test data
  base_dir <- get_test_data_dir()
  assets <- read_assets(base_dir)
  companies <- read_companies(file.path(base_dir, "user_input", "company.csv"))
  hazard_data <- load_hazards_and_inventory(file.path(base_dir, "hazards"), aggregate_factor = 16L)
  hazards <- c(hazard_data$hazards$tif, hazard_data$hazards$nc)
  precomputed_hazards <- read_precomputed_hazards(base_dir)
  damage_factors <- read_damage_cost_factors(base_dir)
  inventory <- hazard_data$inventory

  # Create test assets: some with coordinates, some without
  test_assets <- assets[1:3, , drop = FALSE]

  # Asset 1: NO coordinates, will use precomputed (municipality match)
  test_assets$latitude[1] <- NA_real_
  test_assets$longitude[1] <- NA_real_
  test_assets$municipality[1] <- "Borba"
  test_assets$province[1] <- "Amazonas"

  # Asset 2: HAS coordinates (control case)
  test_assets$latitude[2] <- -15.0
  test_assets$longitude[2] <- -47.9
  test_assets$municipality[2] <- NA_character_
  test_assets$province[2] <- NA_character_

  # Asset 3: NO coordinates, will use precomputed (province match)
  test_assets$latitude[3] <- NA_real_
  test_assets$longitude[3] <- NA_real_
  test_assets$municipality[3] <- NA_character_
  test_assets$province[3] <- "Amazonas"

  # Check what's available in precomputed for Borba and Amazonas
  borba_hazards <- precomputed_hazards |>
    dplyr::filter(.data$region == "Borba", .data$adm_level == "ADM2")
  amazonas_hazards <- precomputed_hazards |>
    dplyr::filter(.data$region == "Amazonas", .data$adm_level == "ADM1")

  message("Borba (ADM2) has hazards: ", nrow(borba_hazards))
  message("Amazonas (ADM1) has hazards: ", nrow(amazonas_hazards))

  # Select ONE specific event/hazard
  # Use pc (CurrentClimate) which exists in precomputed data
  events <- data.frame(
    event_id = "e1",
    hazard_type = "FloodTIF",
    hazard_name = "FloodTIF__Flood Height__GWL=CurrentClimate__RP=10", # Only this one should be used
    scenario_name = "CurrentClimate",
    hazard_return_period = 10,
    event_year = 2030L,
    chronic = FALSE
  )

  # Run compute_risk
  # Currently, the precomputed lookup might return ALL hazards for the region,
  # not just the one specified in events
  result <- compute_risk(
    assets = test_assets,
    companies = companies,
    events = events,
    hazards = hazards,
    hazards_inventory = inventory,
    precomputed_hazards = precomputed_hazards,
    damage_factors = damage_factors
  )

  # Check results
  assets_factors <- result$assets_factors

  # All assets should ONLY be matched to the selected hazard
  unique_hazards <- unique(assets_factors$hazard_name)

  message("Hazards found in results: ", paste(unique_hazards, collapse = ", "))
  message("Expected hazard: FloodTIF__Flood Height__GWL=CurrentClimate__RP=10")

  # TEST: Only the selected hazard should appear in results
  testthat::expect_equal(
    length(unique_hazards),
    1,
    info = "Should only have ONE hazard (the one from events selection)"
  )

  testthat::expect_true(
    all(grepl("^FloodTIF__Flood Height__GWL=CurrentClimate__RP=10", assets_factors$hazard_name)),
    info = "ALL assets should be matched to the selected hazard only (FloodTIF__Flood Height__GWL=CurrentClimate__RP=10)"
  )

  # Additional check: Assets without coordinates should not be matched to
  # hazards that weren't in the events selection
  asset1_results <- assets_factors |>
    dplyr::filter(.data$asset == test_assets$asset[1])

  asset3_results <- assets_factors |>
    dplyr::filter(.data$asset == test_assets$asset[3])

  # Both should only have FloodTIF__Flood Height__GWL=CurrentClimate__RP=10, not others
  testthat::expect_true(
    all(grepl("^FloodTIF__Flood Height__GWL=CurrentClimate__RP=10", asset1_results$hazard_name)),
    info = "Asset 1 (municipality match) should only have the selected hazard"
  )

  testthat::expect_true(
    all(grepl("^FloodTIF__Flood Height__GWL=CurrentClimate__RP=10", asset3_results$hazard_name)),
    info = "Asset 3 (province match) should only have the selected hazard"
  )

  # Check that precomputed assets don't have wrong scenario_code in results
  precomp_assets <- assets_factors |>
    dplyr::filter(.data$matching_method %in% c("municipality", "province"))

  testthat::expect_true(
    nrow(precomp_assets) > 0,
    info = "Should have some assets using precomputed lookup"
  )

  # All should use the selected hazard, not others from precomputed data
  testthat::expect_true(
    all(grepl("^FloodTIF__Flood Height__GWL=CurrentClimate__RP=10", precomp_assets$hazard_name)),
    info = "Precomputed assets should not be matched to hazards outside events selection"
  )
})

testthat::test_that("compute_risk processes drought hazard correctly", {
  base_dir <- get_test_data_dir()
  assets <- read_assets(base_dir)
  hazard_data <- load_hazards_and_inventory(get_hazards_dir(), aggregate_factor = 16L)
  hazards <- c(hazard_data$hazards$tif, hazard_data$hazards$nc)
  precomputed_hazards <- read_precomputed_hazards(base_dir)
  inventory <- hazard_data$inventory

  # Filter assets to only those with coordinates to avoid precomputed data issues
  assets_with_coords <- assets |>
    dplyr::filter(!is.na(.data$latitude), !is.na(.data$longitude))

  # Use Drought hazard for testing
  events <- data.frame(
    event_id = "drought_2030",
    hazard_type = "Drought",
    hazard_name = "Drought__SPI6__GWL=present__RP=10__ensemble=mean",
    scenario_name = "present",
    hazard_return_period = 10,
    event_year = 2030,
    chronic = FALSE
  )

  companies <- read_companies(file.path(base_dir, "user_input", "company.csv"))
  damage_factors <- read_damage_cost_factors(base_dir)

  # Should work without errors
  testthat::expect_error(
    compute_risk(
      assets = assets_with_coords,
      companies = companies,
      events = events,
      hazards = hazards,
      hazards_inventory = inventory,
      precomputed_hazards = precomputed_hazards,
      damage_factors = damage_factors
    ),
    NA
  )

  # Run the actual computation
  res <- compute_risk(
    assets = assets_with_coords,
    companies = companies,
    events = events,
    hazards = hazards,
    hazards_inventory = inventory,
    precomputed_hazards = precomputed_hazards,
    damage_factors = damage_factors
  )

  # Verify we get results
  testthat::expect_true(!is.null(res))
  testthat::expect_true("assets_yearly" %in% names(res))
  testthat::expect_true("companies" %in% names(res))

  # Verify drought hazard is processed
  assets_yearly <- res$assets_yearly
  testthat::expect_true(nrow(assets_yearly) > 0)

  # Check that we have drought-related data in assets_factors
  assets_factors <- res$assets_factors
  testthat::expect_true("hazard_name" %in% names(assets_factors))
  drought_hazards <- assets_factors |>
    dplyr::filter(grepl("Drought", .data$hazard_name))
  testthat::expect_true(nrow(drought_hazards) > 0)
})

testthat::test_that("compute_risk includes event_id in assets_factors when provided in events", {
  base_dir <- get_test_data_dir()
  assets <- read_assets(base_dir)
  companies <- read_companies(file.path(base_dir, "user_input", "company.csv"))
  hazard_data <- load_hazards_and_inventory(get_hazards_dir(), aggregate_factor = 16L)
  hazards <- c(hazard_data$hazards$tif, hazard_data$hazards$nc)
  precomputed_hazards <- read_precomputed_hazards(base_dir)
  inventory <- hazard_data$inventory
  damage_factors <- read_damage_cost_factors(base_dir)

  # Filter assets to only those with coordinates to avoid precomputed data issues
  assets_with_coords <- assets |>
    dplyr::filter(!is.na(.data$latitude), !is.na(.data$longitude))

  # Use events with custom event_id
  events <- data.frame(
    event_id = c("custom_event_1", "custom_event_2"),
    hazard_type = c("FloodTIF", "FloodTIF"),
    hazard_name = c("FloodTIF__Flood Height__GWL=CurrentClimate__RP=10", "FloodTIF__Flood Height__GWL=RCP8.5__RP=100"),
    scenario_name = c("CurrentClimate", "RCP8.5"),
    hazard_return_period = c(10, 100),
    event_year = c(2030, 2035),
    chronic = c(FALSE, FALSE),
    stringsAsFactors = FALSE
  )

  res <- compute_risk(
    assets = assets_with_coords,
    companies = companies,
    events = events,
    hazards = hazards,
    hazards_inventory = inventory,
    precomputed_hazards = precomputed_hazards,
    damage_factors = damage_factors
  )

  # Verify event_id is included in assets_factors
  assets_factors <- res$assets_factors
  testthat::expect_true("event_id" %in% names(assets_factors))

  # Verify custom event_ids are preserved
  unique_event_ids <- unique(assets_factors$event_id)
  testthat::expect_true("custom_event_1" %in% unique_event_ids)
  testthat::expect_true("custom_event_2" %in% unique_event_ids)
})

testthat::test_that("compute_risk generates event_id when not provided in events", {
  base_dir <- get_test_data_dir()
  assets <- read_assets(base_dir)
  companies <- read_companies(file.path(base_dir, "user_input", "company.csv"))
  hazard_data <- load_hazards_and_inventory(get_hazards_dir(), aggregate_factor = 16L)
  hazards <- c(hazard_data$hazards$tif, hazard_data$hazards$nc)
  precomputed_hazards <- read_precomputed_hazards(base_dir)
  inventory <- hazard_data$inventory
  damage_factors <- read_damage_cost_factors(base_dir)

  # Filter assets to only those with coordinates to avoid precomputed data issues
  assets_with_coords <- assets |>
    dplyr::filter(!is.na(.data$latitude), !is.na(.data$longitude))

  # Use events without event_id column
  events <- data.frame(
    hazard_type = c("FloodTIF", "FloodTIF"),
    hazard_name = c("FloodTIF__Flood Height__GWL=CurrentClimate__RP=10", "FloodTIF__Flood Height__GWL=RCP8.5__RP=100"),
    scenario_name = c("CurrentClimate", "RCP8.5"),
    hazard_return_period = c(10, 100),
    event_year = c(2030, 2035),
    chronic = c(FALSE, FALSE),
    stringsAsFactors = FALSE
  )

  res <- compute_risk(
    assets = assets_with_coords,
    companies = companies,
    events = events,
    hazards = hazards,
    hazards_inventory = inventory,
    precomputed_hazards = precomputed_hazards,
    damage_factors = damage_factors
  )

  # Verify event_id is generated and included in assets_factors
  assets_factors <- res$assets_factors
  testthat::expect_true("event_id" %in% names(assets_factors))

  # Verify generated event_ids follow expected pattern
  unique_event_ids <- unique(assets_factors$event_id)
  testthat::expect_true("event_1" %in% unique_event_ids)
  testthat::expect_true("event_2" %in% unique_event_ids)
})
