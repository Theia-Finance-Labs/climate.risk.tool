# Tests for join_fire_damage_factors

# Contract:
# - join_fire_damage_factors(fire_assets, damage_factors_df, land_cover_legend)
# - Input: fire_assets in long format (3 rows per asset: land_cover, FWI, days_danger_total)
# - Pivots to wide format (1 row per asset with 3 columns)
# - Joins land_cover_code with legend to get land_cover_risk
# - Caps FWI at maximum 50
# - Joins FWI-based damage_factor and cost_factor
# - Returns columns: damage_factor, cost_factor, business_disruption, land_cover_risk, hazard_intensity (FWI), days_danger_total


testthat::test_that("join_fire_damage_factors handles Fire hazard with all three indicators", {
  base_dir <- get_test_data_dir()
  damage_factors <- read_damage_cost_factors(base_dir)
  land_cover_legend <- read_land_cover_legend(base_dir)

  # Create Fire assets in long format (3 rows per asset for 3 indicators)
  fire_assets_long <- data.frame(
    asset = c("A1", "A1", "A1", "A2", "A2", "A2"),
    company = c("C1", "C1", "C1", "C1", "C1", "C1"),
    latitude = c(-10, -10, -10, -15, -15, -15),
    longitude = c(-50, -50, -50, -55, -55, -55),
    municipality = c("Mun1", "Mun1", "Mun1", "Mun2", "Mun2", "Mun2"),
    province = c("Prov1", "Prov1", "Prov1", "Prov2", "Prov2", "Prov2"),
    asset_category = c("agriculture", "agriculture", "agriculture", "commercial building", "commercial building", "commercial building"),
    asset_subtype = c(NA, NA, NA, NA, NA, NA),
    size_in_m2 = c(1000, 1000, 1000, 800, 800, 800),
    share_of_economic_activity = c(0.5, 0.5, 0.5, 0.3, 0.3, 0.3),
    cnae = c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_),
    hazard_name = c(
      "Fire__land_cover__GWL=present__RP=0",
      "Fire__FWI__GWL=present__RP=10",
      "Fire__days_danger_total__GWL=present__RP=10",
      "Fire__land_cover__GWL=present__RP=0",
      "Fire__FWI__GWL=present__RP=10",
      "Fire__days_danger_total__GWL=present__RP=10"
    ),
    hazard_type = c("Fire", "Fire", "Fire", "Fire", "Fire", "Fire"),
    hazard_indicator = c("land_cover", "FWI", "days_danger_total", "land_cover", "FWI", "days_danger_total"),
    hazard_intensity = c(16, 25, 30, 12, 35, 45), # land_cover code, FWI value, days
    hazard_return_period = c(0, 10, 10, 0, 10, 10),
    scenario_code = c("present", "present", "present", "present", "present", "present"),
    scenario_name = c("present", "present", "present", "present", "present", "present"),
    source = c("tif", "csv", "csv", "tif", "csv", "csv"),
    matching_method = c("coordinates", "coordinates", "coordinates", "coordinates", "coordinates", "coordinates"),
    event_id = c("event_1", "event_1", "event_1", "event_1", "event_1", "event_1"),
    event_year = c(2030, 2030, 2030, 2030, 2030, 2030),
    stringsAsFactors = FALSE
  )

  out <- join_fire_damage_factors(fire_assets_long, damage_factors, land_cover_legend)

  # Should pivot to wide format: 2 assets (not 6 rows)
  testthat::expect_equal(nrow(out), 2)

  # Should have all required columns
  required_cols <- c(
    "asset", "company", "hazard_type", "hazard_indicator", "hazard_intensity",
    "damage_factor", "cost_factor", "business_disruption",
    "land_cover_risk", "days_danger_total"
  )
  testthat::expect_true(all(required_cols %in% names(out)))

  # All assets should have Fire hazard_type
  testthat::expect_true(all(out$hazard_type == "Fire"))

  # hazard_indicator should be "FWI" (primary indicator)
  testthat::expect_true(all(out$hazard_indicator == "FWI"))

  # hazard_intensity should contain FWI values (not land_cover codes)
  testthat::expect_true(all(out$hazard_intensity %in% c(25, 35)))
})

testthat::test_that("join_fire_damage_factors joins land_cover_code with legend to get land_cover_risk", {
  base_dir <- get_test_data_dir()
  damage_factors <- read_damage_cost_factors(base_dir)
  land_cover_legend <- read_land_cover_legend(base_dir)

  # Create asset with known land_cover code
  # Find a land_cover code from the legend
  land_cover_code <- land_cover_legend$land_cover_code[1]
  expected_risk <- land_cover_legend$land_cover_risk[land_cover_legend$land_cover_code == land_cover_code][1]

  fire_assets_long <- data.frame(
    asset = c("A1", "A1", "A1"),
    company = c("C1", "C1", "C1"),
    latitude = c(-10, -10, -10),
    longitude = c(-50, -50, -50),
    municipality = c("Mun1", "Mun1", "Mun1"),
    province = c("Prov1", "Prov1", "Prov1"),
    asset_category = c("agriculture", "agriculture", "agriculture"),
    asset_subtype = c(NA, NA, NA),
    size_in_m2 = c(1000, 1000, 1000),
    share_of_economic_activity = c(0.5, 0.5, 0.5),
    cnae = c(NA_real_, NA_real_, NA_real_),
    hazard_name = c(
      "Fire__land_cover__GWL=present__RP=0",
      "Fire__FWI__GWL=present__RP=10",
      "Fire__days_danger_total__GWL=present__RP=10"
    ),
    hazard_type = c("Fire", "Fire", "Fire"),
    hazard_indicator = c("land_cover", "FWI", "days_danger_total"),
    hazard_intensity = c(land_cover_code, 25, 30),
    hazard_return_period = c(0, 10, 10),
    scenario_code = c("present", "present", "present"),
    scenario_name = c("present", "present", "present"),
    source = c("tif", "csv", "csv"),
    matching_method = c("coordinates", "coordinates", "coordinates"),
    event_id = c("event_1", "event_1", "event_1"),
    event_year = c(2030, 2030, 2030),
    stringsAsFactors = FALSE
  )

  out <- join_fire_damage_factors(fire_assets_long, damage_factors, land_cover_legend)

  testthat::expect_equal(nrow(out), 1)
  testthat::expect_equal(out$land_cover_risk, expected_risk, tolerance = 0.001)
})

testthat::test_that("join_fire_damage_factors applies default land_cover_risk 0.50 for assets without coordinates", {
  base_dir <- get_test_data_dir()
  damage_factors <- read_damage_cost_factors(base_dir)
  land_cover_legend <- read_land_cover_legend(base_dir)

  # Create asset without coordinates (NA latitude/longitude)
  fire_assets_long <- data.frame(
    asset = c("A1", "A1", "A1"),
    company = c("C1", "C1", "C1"),
    latitude = c(NA_real_, NA_real_, NA_real_),
    longitude = c(NA_real_, NA_real_, NA_real_),
    municipality = c("Mun1", "Mun1", "Mun1"),
    province = c("Prov1", "Prov1", "Prov1"),
    asset_category = c("agriculture", "agriculture", "agriculture"),
    asset_subtype = c(NA, NA, NA),
    size_in_m2 = c(1000, 1000, 1000),
    share_of_economic_activity = c(0.5, 0.5, 0.5),
    cnae = c(NA_real_, NA_real_, NA_real_),
    hazard_name = c(
      "Fire__land_cover__GWL=present__RP=0",
      "Fire__FWI__GWL=present__RP=10",
      "Fire__days_danger_total__GWL=present__RP=10"
    ),
    hazard_type = c("Fire", "Fire", "Fire"),
    hazard_indicator = c("land_cover", "FWI", "days_danger_total"),
    hazard_intensity = c(NA_real_, 25, 30), # No land_cover extraction
    hazard_return_period = c(0, 10, 10),
    scenario_code = c("present", "present", "present"),
    scenario_name = c("present", "present", "present"),
    source = c("tif", "csv", "csv"),
    matching_method = c("precomputed", "precomputed", "precomputed"),
    event_id = c("event_1", "event_1", "event_1"),
    event_year = c(2030, 2030, 2030),
    stringsAsFactors = FALSE
  )

  out <- join_fire_damage_factors(fire_assets_long, damage_factors, land_cover_legend)

  testthat::expect_equal(nrow(out), 1)
  # Should default to 0.50 for assets without coordinates
  testthat::expect_equal(out$land_cover_risk, 0.50, tolerance = 0.001)
})

testthat::test_that("join_fire_damage_factors caps FWI values at maximum 50", {
  base_dir <- get_test_data_dir()
  damage_factors <- read_damage_cost_factors(base_dir)
  land_cover_legend <- read_land_cover_legend(base_dir)

  # Create asset with FWI > 50
  fire_assets_long <- data.frame(
    asset = c("A1", "A1", "A1"),
    company = c("C1", "C1", "C1"),
    latitude = c(-10, -10, -10),
    longitude = c(-50, -50, -50),
    municipality = c("Mun1", "Mun1", "Mun1"),
    province = c("Prov1", "Prov1", "Prov1"),
    asset_category = c("commercial building", "commercial building", "commercial building"),
    asset_subtype = c(NA, NA, NA),
    size_in_m2 = c(1000, 1000, 1000),
    share_of_economic_activity = c(0.5, 0.5, 0.5),
    cnae = c(NA_real_, NA_real_, NA_real_),
    hazard_name = c(
      "Fire__land_cover__GWL=present__RP=0",
      "Fire__FWI__GWL=present__RP=10",
      "Fire__days_danger_total__GWL=present__RP=10"
    ),
    hazard_type = c("Fire", "Fire", "Fire"),
    hazard_indicator = c("land_cover", "FWI", "days_danger_total"),
    hazard_intensity = c(16, 75, 30), # FWI = 75 (should be capped to 50)
    hazard_return_period = c(0, 10, 10),
    scenario_code = c("present", "present", "present"),
    scenario_name = c("present", "present", "present"),
    source = c("tif", "csv", "csv"),
    matching_method = c("coordinates", "coordinates", "coordinates"),
    event_id = c("event_1", "event_1", "event_1"),
    event_year = c(2030, 2030, 2030),
    stringsAsFactors = FALSE
  )

  out <- join_fire_damage_factors(fire_assets_long, damage_factors, land_cover_legend)

  testthat::expect_equal(nrow(out), 1)
  # FWI should be capped at 50 for damage factor lookup (but hazard_intensity should still show original)
  # The damage_factor should match FWI=50 in the damage factors table
  # We check that damage_factor exists and is not NA (indicating successful lookup at FWI=50)
  testthat::expect_true(!is.na(out$damage_factor))

  # Find damage factor for FWI=50 for commercial building
  fwi_50_factor <- damage_factors |>
    dplyr::filter(
      .data$hazard_type == "Fire",
      .data$hazard_indicator == "FWI",
      .data$asset_category == "commercial building",
      .data$hazard_intensity == 50
    )
  if (nrow(fwi_50_factor) > 0) {
    testthat::expect_equal(out$damage_factor, fwi_50_factor$damage_factor[1], tolerance = 0.001)
  }
})

testthat::test_that("join_fire_damage_factors joins FWI-based damage_factor and cost_factor", {
  base_dir <- get_test_data_dir()
  damage_factors <- read_damage_cost_factors(base_dir)
  land_cover_legend <- read_land_cover_legend(base_dir)

  # Get expected damage and cost factors for FWI=25, commercial building
  expected_factors <- damage_factors |>
    dplyr::filter(
      .data$hazard_type == "Fire",
      .data$hazard_indicator == "FWI",
      .data$asset_category == "commercial building",
      .data$hazard_intensity == 25
    )

  if (nrow(expected_factors) == 0) {
    testthat::skip("Test data missing FWI=25 for commercial building")
  }

  fire_assets_long <- data.frame(
    asset = c("A1", "A1", "A1"),
    company = c("C1", "C1", "C1"),
    latitude = c(-10, -10, -10),
    longitude = c(-50, -50, -50),
    municipality = c("Mun1", "Mun1", "Mun1"),
    province = c("Prov1", "Prov1", "Prov1"),
    asset_category = c("commercial building", "commercial building", "commercial building"),
    asset_subtype = c(NA, NA, NA),
    size_in_m2 = c(1000, 1000, 1000),
    share_of_economic_activity = c(0.5, 0.5, 0.5),
    cnae = c(NA_real_, NA_real_, NA_real_),
    hazard_name = c(
      "Fire__land_cover__GWL=present__RP=0",
      "Fire__FWI__GWL=present__RP=10",
      "Fire__days_danger_total__GWL=present__RP=10"
    ),
    hazard_type = c("Fire", "Fire", "Fire"),
    hazard_indicator = c("land_cover", "FWI", "days_danger_total"),
    hazard_intensity = c(16, 25, 30),
    hazard_return_period = c(0, 10, 10),
    scenario_code = c("present", "present", "present"),
    scenario_name = c("present", "present", "present"),
    source = c("tif", "csv", "csv"),
    matching_method = c("coordinates", "coordinates", "coordinates"),
    event_id = c("event_1", "event_1", "event_1"),
    event_year = c(2030, 2030, 2030),
    stringsAsFactors = FALSE
  )

  out <- join_fire_damage_factors(fire_assets_long, damage_factors, land_cover_legend)

  testthat::expect_equal(nrow(out), 1)
  testthat::expect_equal(out$damage_factor, expected_factors$damage_factor[1], tolerance = 0.001)
  testthat::expect_equal(out$cost_factor, expected_factors$cost_factor[1], tolerance = 0.001)
  # business_disruption should be NA for Fire
  testthat::expect_true(is.na(out$business_disruption))
})

testthat::test_that("join_fire_damage_factors handles missing land_cover_legend (defaults to 0.50)", {
  base_dir <- get_test_data_dir()
  damage_factors <- read_damage_cost_factors(base_dir)

  # Call without land_cover_legend
  fire_assets_long <- data.frame(
    asset = c("A1", "A1", "A1"),
    company = c("C1", "C1", "C1"),
    latitude = c(-10, -10, -10),
    longitude = c(-50, -50, -50),
    municipality = c("Mun1", "Mun1", "Mun1"),
    province = c("Prov1", "Prov1", "Prov1"),
    asset_category = c("agriculture", "agriculture", "agriculture"),
    asset_subtype = c(NA, NA, NA),
    size_in_m2 = c(1000, 1000, 1000),
    share_of_economic_activity = c(0.5, 0.5, 0.5),
    cnae = c(NA_real_, NA_real_, NA_real_),
    hazard_name = c(
      "Fire__land_cover__GWL=present__RP=0",
      "Fire__FWI__GWL=present__RP=10",
      "Fire__days_danger_total__GWL=present__RP=10"
    ),
    hazard_type = c("Fire", "Fire", "Fire"),
    hazard_indicator = c("land_cover", "FWI", "days_danger_total"),
    hazard_intensity = c(16, 25, 30),
    hazard_return_period = c(0, 10, 10),
    scenario_code = c("present", "present", "present"),
    scenario_name = c("present", "present", "present"),
    source = c("tif", "csv", "csv"),
    matching_method = c("coordinates", "coordinates", "coordinates"),
    event_id = c("event_1", "event_1", "event_1"),
    event_year = c(2030, 2030, 2030),
    stringsAsFactors = FALSE
  )

  out <- join_fire_damage_factors(fire_assets_long, damage_factors, land_cover_legend = NULL)

  testthat::expect_equal(nrow(out), 1)
  # Should default to 0.50 when legend is missing
  testthat::expect_equal(out$land_cover_risk, 0.50, tolerance = 0.001)
})

testthat::test_that("join_fire_damage_factors handles agriculture vs commercial/industrial building categories", {
  base_dir <- get_test_data_dir()
  damage_factors <- read_damage_cost_factors(base_dir)
  land_cover_legend <- read_land_cover_legend(base_dir)

  # Create assets with different categories
  fire_assets_long <- data.frame(
    asset = c("A1", "A1", "A1", "A2", "A2", "A2"),
    company = c("C1", "C1", "C1", "C1", "C1", "C1"),
    latitude = c(-10, -10, -10, -15, -15, -15),
    longitude = c(-50, -50, -50, -55, -55, -55),
    municipality = c("Mun1", "Mun1", "Mun1", "Mun2", "Mun2", "Mun2"),
    province = c("Prov1", "Prov1", "Prov1", "Prov2", "Prov2", "Prov2"),
    asset_category = c("agriculture", "agriculture", "agriculture", "commercial building", "commercial building", "commercial building"),
    asset_subtype = c(NA, NA, NA, NA, NA, NA),
    size_in_m2 = c(1000, 1000, 1000, 800, 800, 800),
    share_of_economic_activity = c(0.5, 0.5, 0.5, 0.3, 0.3, 0.3),
    cnae = c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_),
    hazard_name = c(
      "Fire__land_cover__GWL=present__RP=0", "Fire__FWI__GWL=present__RP=10", "Fire__days_danger_total__GWL=present__RP=10",
      "Fire__land_cover__GWL=present__RP=0", "Fire__FWI__GWL=present__RP=10", "Fire__days_danger_total__GWL=present__RP=10"
    ),
    hazard_type = c("Fire", "Fire", "Fire", "Fire", "Fire", "Fire"),
    hazard_indicator = c("land_cover", "FWI", "days_danger_total", "land_cover", "FWI", "days_danger_total"),
    hazard_intensity = c(16, 25, 30, 12, 25, 30),
    hazard_return_period = c(0, 10, 10, 0, 10, 10),
    scenario_code = c("present", "present", "present", "present", "present", "present"),
    scenario_name = c("present", "present", "present", "present", "present", "present"),
    source = c("tif", "csv", "csv", "tif", "csv", "csv"),
    matching_method = c("coordinates", "coordinates", "coordinates", "coordinates", "coordinates", "coordinates"),
    event_id = c("event_1", "event_1", "event_1", "event_1", "event_1", "event_1"),
    event_year = c(2030, 2030, 2030, 2030, 2030, 2030),
    stringsAsFactors = FALSE
  )

  out <- join_fire_damage_factors(fire_assets_long, damage_factors, land_cover_legend)

  testthat::expect_equal(nrow(out), 2)

  # Get damage factors for agriculture and commercial building
  ag_factors <- damage_factors |>
    dplyr::filter(
      .data$hazard_type == "Fire", .data$hazard_indicator == "FWI",
      .data$asset_category == "agriculture", .data$hazard_intensity == 25
    )
  com_factors <- damage_factors |>
    dplyr::filter(
      .data$hazard_type == "Fire", .data$hazard_indicator == "FWI",
      .data$asset_category == "commercial building", .data$hazard_intensity == 25
    )

  if (nrow(ag_factors) > 0 && nrow(com_factors) > 0) {
    ag_asset <- out |> dplyr::filter(.data$asset_category == "agriculture")
    com_asset <- out |> dplyr::filter(.data$asset_category == "commercial building")

    testthat::expect_equal(ag_asset$damage_factor, ag_factors$damage_factor[1], tolerance = 0.001)
    testthat::expect_equal(com_asset$damage_factor, com_factors$damage_factor[1], tolerance = 0.001)
  }
})

testthat::test_that("join_fire_damage_factors sets business_disruption to NA", {
  base_dir <- get_test_data_dir()
  damage_factors <- read_damage_cost_factors(base_dir)
  land_cover_legend <- read_land_cover_legend(base_dir)

  fire_assets_long <- data.frame(
    asset = c("A1", "A1", "A1"),
    company = c("C1", "C1", "C1"),
    latitude = c(-10, -10, -10),
    longitude = c(-50, -50, -50),
    municipality = c("Mun1", "Mun1", "Mun1"),
    province = c("Prov1", "Prov1", "Prov1"),
    asset_category = c("agriculture", "agriculture", "agriculture"),
    asset_subtype = c(NA, NA, NA),
    size_in_m2 = c(1000, 1000, 1000),
    share_of_economic_activity = c(0.5, 0.5, 0.5),
    cnae = c(NA_real_, NA_real_, NA_real_),
    hazard_name = c(
      "Fire__land_cover__GWL=present__RP=0",
      "Fire__FWI__GWL=present__RP=10",
      "Fire__days_danger_total__GWL=present__RP=10"
    ),
    hazard_type = c("Fire", "Fire", "Fire"),
    hazard_indicator = c("land_cover", "FWI", "days_danger_total"),
    hazard_intensity = c(16, 25, 30),
    hazard_return_period = c(0, 10, 10),
    scenario_code = c("present", "present", "present"),
    scenario_name = c("present", "present", "present"),
    source = c("tif", "csv", "csv"),
    matching_method = c("coordinates", "coordinates", "coordinates"),
    event_id = c("event_1", "event_1", "event_1"),
    event_year = c(2030, 2030, 2030),
    stringsAsFactors = FALSE
  )

  out <- join_fire_damage_factors(fire_assets_long, damage_factors, land_cover_legend)

  testthat::expect_equal(nrow(out), 1)
  # business_disruption should be NA for Fire (not used)
  testthat::expect_true(all(is.na(out$business_disruption)))
})
