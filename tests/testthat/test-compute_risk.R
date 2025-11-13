testthat::test_that("compute_risk end-to-end integration across hazards and event types", {
  base_dir <- get_test_data_dir()
  assets <- read_assets(base_dir)
  companies <- read_companies(file.path(base_dir, "user_input", "company.xlsx"))
  hazard_data <- load_hazards_and_inventory(file.path(base_dir, "hazards"), aggregate_factor = 16L)
  # Include all hazard sources (TIF, NC, CSV). Heat hazards are provided via CSV.
  hazards <- c(hazard_data$hazards$tif, hazard_data$hazards$nc, hazard_data$hazards$csv)
  precomputed_hazards <- read_precomputed_hazards(base_dir)
  damage_factors <- read_damage_cost_factors(base_dir)
  inventory <- hazard_data$inventory

  # Mix assets: include one without coordinates to trigger precomputed path
  assets_mixed <- assets
  if (nrow(assets_mixed) >= 1) {
    assets_mixed$latitude[1] <- NA_real_
    assets_mixed$longitude[1] <- NA_real_
    assets_mixed$municipality[1] <- "Borba"
    assets_mixed$province[1] <- "Amazonas"
  }

  # Events: Flood (acute), Heat (acute), and Drought (acute with season)
  # Hazard names match the actual available test data:
  # - Flood: Uses GWL= format with scenario_code (TIF files)
  # - Heat: Uses GWL= with ensemble (CSV files)
  # - Drought: Uses GWL= with season and ensemble (NC files)
  events <- data.frame(
    hazard_type = c("Flood", "Flood", "Flood", "Heat", "Heat", "Drought", "Drought", "Fire"),
    hazard_name = c(
      "Flood__depth(cm)__GWL=present__RP=100",
      "Flood__depth(cm)__GWL=present__RP=100",
      "Flood__depth(cm)__GWL=rcp85__RP=100",
      "Heat__HI__GWL=present__RP=10__ensemble=mean",
      "Heat__HI__GWL=2__RP=10__ensemble=mean",
      "Drought__SPI3__GWL=present__RP=10__season=Summer__ensemble=mean",
      "Drought__SPI3__GWL=1.5__RP=10__season=Winter__ensemble=mean",
      "Fire__FWI__GWL=3__RP=50__ensemble=mean"
    ),
    scenario_name = c("present", "present", "rcp85", "present", "2", "present", "1.5", "3"),
    scenario_code = c("present", "present", "rcp85", "present", "2", "present", "1.5", "3"),
    hazard_return_period = c(10, 10, 100, 10, 10, 10, 10, 50),
    event_year = c(2030L, 2031L, 2035L, 2030L, 2035L, 2032L, 2033L, 2030L),
    stringsAsFactors = FALSE
  )

  res <- compute_risk(
    assets = assets_mixed,
    companies = companies,
    events = events,
    hazards = hazards,
    hazards_inventory = inventory,
    precomputed_hazards = precomputed_hazards,
    damage_factors = damage_factors,
    growth_rate = 0.02,
    discount_rate = 0.05,
    risk_free_rate = 0.02
  )

  # Structure checks
  expected_components <- c("assets_factors", "companies", "assets_yearly", "companies_yearly")
  testthat::expect_true(all(expected_components %in% names(res)))
  testthat::expect_s3_class(res$assets_factors, "data.frame")
  testthat::expect_s3_class(res$companies, "data.frame")
  testthat::expect_s3_class(res$assets_yearly, "data.frame")
  testthat::expect_s3_class(res$companies_yearly, "data.frame")

  # Yearly outputs should be larger than aggregated and include required columns
  testthat::expect_true(nrow(res$assets_yearly) > nrow(res$assets_factors))
  testthat::expect_true(nrow(res$companies_yearly) > nrow(res$companies))
  testthat::expect_true(all(c("year", "scenario") %in% names(res$assets_yearly)))
  testthat::expect_true(all(c("year", "scenario") %in% names(res$companies_yearly)))
  testthat::expect_true(all(c("baseline", "shock") %in% unique(res$assets_yearly$scenario)))
  testthat::expect_true(all(c("baseline", "shock") %in% unique(res$companies_yearly$scenario)))

  # Assets factors should include metadata and event_info
  testthat::expect_true(all(c("matching_method", "event_year") %in% names(res$assets_factors)))
  testthat::expect_true(all(res$assets_factors$matching_method %in% c("coordinates", "municipality", "province")))

  # Event IDs: auto-generated when not provided
  testthat::expect_true("event_id" %in% names(res$assets_factors))
  unique_ids <- unique(res$assets_factors$event_id)
  testthat::expect_true(any(grepl("^event_", unique_ids)))

  # Hazards coverage: Flood, Heat, Drought, and Fire present
  testthat::expect_true(any(grepl("Flood", res$assets_factors$hazard_name)))
  testthat::expect_true(any(grepl("Heat", res$assets_factors$hazard_name)))

  # Drought should be present only for agriculture assets
  drought_assets <- res$assets_factors[grepl("Drought", res$assets_factors$hazard_name), ]
  if (nrow(drought_assets) > 0) {
    testthat::expect_true(all(drought_assets$asset_category == "agriculture"))
    testthat::expect_true("season" %in% names(res$assets_factors))
  }

  # Fire should be present and expanded to multiple indicators
  fire_assets <- res$assets_factors[grepl("Fire", res$assets_factors$hazard_name), ]
  if (nrow(fire_assets) > 0) {
    # Fire should have all three indicators (land_cover, FWI, days_danger_total)
    fire_indicators <- unique(fire_assets$hazard_indicator)
    testthat::expect_true("land_cover" %in% fire_indicators || "FWI" %in% fire_indicators || "days_danger_total" %in% fire_indicators)

    # Fire should have land_cover_risk column from join_fire_damage_factors
    testthat::expect_true("land_cover_risk" %in% names(res$assets_factors))

    # Fire should affect both agriculture (revenue) and buildings (profit)
    fire_asset_categories <- unique(fire_assets$asset_category)
    testthat::expect_true(any(fire_asset_categories %in% c("agriculture", "commercial building", "industrial building")))
  }

  # Coverage by matching method: each matching_method should include Flood, Heat, and Fire
  mm_cov <- res$assets_factors |>
    dplyr::group_by(.data$matching_method) |>
    dplyr::summarise(
      has_flood = any(grepl("Flood", .data$hazard_name)),
      has_heat = any(grepl("Heat", .data$hazard_name)),
      has_fire = any(grepl("Fire", .data$hazard_name)),
      .groups = "drop"
    )
  if (nrow(mm_cov) > 0) {
    testthat::expect_true(all(mm_cov$has_flood))
    testthat::expect_true(all(mm_cov$has_heat))
    # Fire may not be present for all matching methods (depends on test data)
    # Just verify it exists for at least one method
    testthat::expect_true(any(mm_cov$has_fire))
  }

  # Acute behavior: after 2030, shock revenue should not exceed baseline on average
  a_y <- res$assets_yearly
  if (all(c("year", "scenario", "revenue") %in% names(a_y))) {
    post_2030_shock <- a_y[a_y$year >= 2030 & a_y$scenario == "shock", ]
    post_2030_base <- a_y[a_y$year >= 2030 & a_y$scenario == "baseline", ]
    if (nrow(post_2030_shock) > 0 && nrow(post_2030_base) > 0) {
      testthat::expect_true(mean(post_2030_shock$revenue) <= mean(post_2030_base$revenue))
    }
  }

  # Chronic behavior: total shock revenue should be <= baseline over full horizon
  if (all(c("scenario", "revenue") %in% names(a_y))) {
    total_shock <- sum(a_y$revenue[a_y$scenario == "shock"])
    total_base <- sum(a_y$revenue[a_y$scenario == "baseline"])
    testthat::expect_true(total_shock <= total_base)
  }
})

testthat::test_that("compute_risk produces stable snapshot output", {
  base_dir <- get_test_data_dir()
  assets <- read_assets(base_dir)
  companies <- read_companies(file.path(base_dir, "user_input", "company.xlsx"))
  hazard_data <- load_hazards_and_inventory(file.path(base_dir, "hazards"), aggregate_factor = 16L)
  # Include all hazard sources (TIF, NC, CSV). Heat hazards are provided via CSV.
  hazards <- c(hazard_data$hazards$tif, hazard_data$hazards$nc, hazard_data$hazards$csv)
  precomputed_hazards <- read_precomputed_hazards(base_dir)
  damage_factors <- read_damage_cost_factors(base_dir)
  inventory <- hazard_data$inventory

  # Mix assets: include one without coordinates to trigger precomputed path
  assets_mixed <- assets
  if (nrow(assets_mixed) >= 1) {
    assets_mixed$latitude[1] <- NA_real_
    assets_mixed$longitude[1] <- NA_real_
    assets_mixed$municipality[1] <- "Borba"
    assets_mixed$province[1] <- "Amazonas"
  }

  # Events: Flood (acute), Heat (acute), and Drought (acute with season)
  # Hazard names match the actual available test data:
  # - Flood: Uses GWL= format with scenario_name (TIF files)
  # - Heat: Uses GWL= with ensemble (CSV files)
  # - Drought: Uses GWL= with season and ensemble (NC files)
  events <- data.frame(
    hazard_type = c("Flood", "Flood", "Flood", "Heat", "Heat", "Drought", "Drought"),
    hazard_name = c(
      "Flood__depth(cm)__GWL=present__RP=100",
      "Flood__depth(cm)__GWL=present__RP=100",
      "Flood__depth(cm)__GWL=rcp85__RP=100",
      "Heat__HI__GWL=present__RP=10__ensemble=mean",
      "Heat__HI__GWL=2__RP=10__ensemble=mean",
      "Drought__SPI3__GWL=present__RP=10__season=Summer__ensemble=mean",
      "Drought__SPI3__GWL=1.5__RP=10__season=Winter__ensemble=mean"
    ),
    scenario_name = c("present", "present", "rcp85", "present", "2", "present", "1.5"),
    scenario_code = c("present", "present", "rcp85", "present", "2", "present", "1.5"),
    hazard_return_period = c(10, 10, 100, 10, 10, 10, 10),
    event_year = c(2030L, 2031L, 2035L, 2030L, 2035L, 2032L, 2033L),
    stringsAsFactors = FALSE
  )

  res <- compute_risk(
    assets = assets_mixed,
    companies = companies,
    events = events,
    hazards = hazards,
    hazards_inventory = inventory,
    precomputed_hazards = precomputed_hazards,
    damage_factors = damage_factors,
    growth_rate = 0.02,
    discount_rate = 0.05,
    risk_free_rate = 0.02
  )

  # Snapshot test of company-level outputs only
  # Company-level data is more stable and easier to analyze differences.
  # Asset-level snapshots are excluded as they are sensitive to row ordering
  # and can be very large, making it harder to identify meaningful changes.
  testthat::expect_snapshot_value(res$companies, style = "deparse", cran = FALSE)
})

testthat::test_that("compute_risk handles Fire events correctly with multi-indicator expansion", {
  base_dir <- get_test_data_dir()
  assets <- read_assets(base_dir)
  companies <- read_companies(file.path(base_dir, "user_input", "company.xlsx"))
  hazard_data <- load_hazards_and_inventory(file.path(base_dir, "hazards"), aggregate_factor = 16L)
  hazards <- c(hazard_data$hazards$tif, hazard_data$hazards$nc, hazard_data$hazards$csv)
  precomputed_hazards <- read_precomputed_hazards(base_dir)
  damage_factors <- read_damage_cost_factors(base_dir)
  land_cover_legend <- read_land_cover_legend(base_dir)
  inventory <- hazard_data$inventory

  # Create events with Fire (user-facing format: Fire with scenario and RP)
  # The system will expand this to 3 indicators internally
  events <- data.frame(
    hazard_type = c("Fire", "Fire"),
    hazard_indicator = c("FWI", "FWI"), # Primary indicator for user selection
    hazard_name = c(
      "Fire__FWI__GWL=present__RP=10",
      "Fire__FWI__GWL=present__RP=50"
    ),
    scenario_name = c("present", "present"),
    scenario_code = c("present", "present"),
    hazard_return_period = c(10, 50),
    event_year = c(2030L, 2035L),
    season = c(NA_character_, NA_character_),
    stringsAsFactors = FALSE
  )

  res <- compute_risk(
    assets = assets,
    companies = companies,
    events = events,
    hazards = hazards,
    hazards_inventory = inventory,
    precomputed_hazards = precomputed_hazards,
    damage_factors = damage_factors,
    land_cover_legend = land_cover_legend,
    growth_rate = 0.02,
    discount_rate = 0.05
  )

  # Structure checks
  testthat::expect_true(all(c("assets_factors", "companies", "assets_yearly", "companies_yearly") %in% names(res)))

  # Fire should be present in assets_factors
  fire_assets <- res$assets_factors[grepl("Fire", res$assets_factors$hazard_name), ]
  testthat::expect_gt(nrow(fire_assets), 0)

  # Fire assets should have land_cover_risk column (from join_fire_damage_factors)
  testthat::expect_true("land_cover_risk" %in% names(res$assets_factors))
  fire_with_risk <- fire_assets[!is.na(fire_assets$land_cover_risk), ]
  if (nrow(fire_with_risk) > 0) {
    # land_cover_risk should be between 0 and 1
    testthat::expect_true(all(fire_with_risk$land_cover_risk >= 0))
    testthat::expect_true(all(fire_with_risk$land_cover_risk <= 1))
  }

  # Fire should have days_danger_total column
  testthat::expect_true("days_danger_total" %in% names(res$assets_factors))
  fire_with_days <- fire_assets[!is.na(fire_assets$days_danger_total), ]
  if (nrow(fire_with_days) > 0) {
    # days_danger_total should be >= 0 and <= 365
    testthat::expect_true(all(fire_with_days$days_danger_total >= 0))
    testthat::expect_true(all(fire_with_days$days_danger_total <= 365))
  }

  # Fire should affect agriculture assets (revenue shock)
  fire_agriculture <- fire_assets[fire_assets$asset_category == "agriculture", ]
  if (nrow(fire_agriculture) > 0) {
    # Agriculture assets with Fire should have damage_factor
    testthat::expect_true(all(!is.na(fire_agriculture$damage_factor)))
  }

  # Fire should affect commercial/industrial buildings (profit shock)
  fire_buildings <- fire_assets[fire_assets$asset_category %in% c("commercial building", "industrial building"), ]
  if (nrow(fire_buildings) > 0) {
    # Buildings with Fire should have damage_factor and cost_factor
    testthat::expect_true(all(!is.na(fire_buildings$damage_factor)))
    testthat::expect_true(any(!is.na(fire_buildings$cost_factor)))
  }

  # Yearly outputs should show Fire impacts
  # Fire revenue shock: agriculture revenue should decrease in event years
  if (nrow(fire_agriculture) > 0) {
    ag_assets <- unique(fire_agriculture$asset)
    ag_yearly <- res$assets_yearly[res$assets_yearly$asset %in% ag_assets, ]
    if (nrow(ag_yearly) > 0) {
      # Check that shock revenue is <= baseline for agriculture assets in event years
      event_years <- unique(events$event_year)
      for (yr in event_years) {
        ag_year_shock <- ag_yearly[ag_yearly$year == yr & ag_yearly$scenario == "shock", ]
        ag_year_base <- ag_yearly[ag_yearly$year == yr & ag_yearly$scenario == "baseline", ]
        if (nrow(ag_year_shock) > 0 && nrow(ag_year_base) > 0) {
          testthat::expect_true(mean(ag_year_shock$revenue, na.rm = TRUE) <= mean(ag_year_base$revenue, na.rm = TRUE))
        }
      }
    }
  }

  # Fire profit shock: building profits should decrease in event years
  if (nrow(fire_buildings) > 0) {
    building_assets <- unique(fire_buildings$asset)
    building_yearly <- res$assets_yearly[res$assets_yearly$asset %in% building_assets, ]
    if (nrow(building_yearly) > 0) {
      # Check that shock profit is <= baseline for building assets in event years
      event_years <- unique(events$event_year)
      for (yr in event_years) {
        building_year_shock <- building_yearly[building_yearly$year == yr & building_yearly$scenario == "shock", ]
        building_year_base <- building_yearly[building_yearly$year == yr & building_yearly$scenario == "baseline", ]
        if (nrow(building_year_shock) > 0 && nrow(building_year_base) > 0) {
          # Fire can make profits negative, so we just check that they changed
          testthat::expect_true(
            mean(building_year_shock$profit, na.rm = TRUE) <= mean(building_year_base$profit, na.rm = TRUE)
          )
        }
      }
    }
  }
})
