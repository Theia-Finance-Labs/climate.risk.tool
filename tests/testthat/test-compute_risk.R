testthat::test_that("compute_risk end-to-end integration across hazards and event types", {
  base_dir <- get_test_data_dir()
  assets <- read_assets(base_dir)
  companies <- read_companies(file.path(base_dir, "user_input", "company.xlsx"))
  hazard_data <- load_hazards_and_inventory(file.path(base_dir, "hazards"), aggregate_factor = 16L)
  # Include all hazard sources (TIF, NC, CSV). Compound hazards are provided via CSV.
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

  # Events: FloodTIF (acute), Compound (acute), and Drought (acute with season)
  # Hazard names match the actual available test data:
  # - FloodTIF: Uses GWL= format with scenario_name (TIF files)
  # - Compound: Uses GWL= with ensemble (CSV files)
  # - Drought: Uses GWL= with season and ensemble (NC files)
  events <- data.frame(
    hazard_type = c("FloodTIF", "FloodTIF", "FloodTIF", "Compound", "Compound", "Drought", "Drought", "Fire"),
    hazard_name = c(
      "FloodTIF__depth(cm)__GWL=CurrentClimate__RP=10",
      "FloodTIF__depth(cm)__GWL=CurrentClimate__RP=10",
      "FloodTIF__depth(cm)__GWL=RCP8.5__RP=100",
      "Compound__HI__GWL=present__RP=10__ensemble=mean",
      "Compound__HI__GWL=2__RP=10__ensemble=mean",
      "Drought__SPI3__GWL=present__RP=10__season=Summer__ensemble=mean",
      "Drought__SPI3__GWL=1.5__RP=10__season=Winter__ensemble=mean",
      "Fire__FWI__GWL=3__RP=50__ensemble=mean"
    ),
    scenario_name = c("CurrentClimate", "CurrentClimate", "RCP8.5", "present", "2", "present", "1.5", "3"),
    scenario_code = c("pc", "pc", "rcp85", "present", "2", "present", "1.5", "3"),
    hazard_return_period = c(10, 10, 100, 10, 10, 10, 10, 50),
    event_year = c(2030L, 2031L, 2035L, 2030L, 2035L, 2032L, 2033L, 2030L),
    season = c(NA, NA, NA, NA, NA, "Summer", "Winter", NA), # Season only for Drought
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
    net_profit_margin = 0.1,
    discount_rate = 0.05
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

  # Hazards coverage: Flood, Compound, and Drought present
  testthat::expect_true(any(grepl("FloodTIF", res$assets_factors$hazard_name)))
  testthat::expect_true(any(grepl("Compound", res$assets_factors$hazard_name)))

  # Drought should be present only for agriculture assets
  drought_assets <- res$assets_factors[grepl("Drought", res$assets_factors$hazard_name), ]
  if (nrow(drought_assets) > 0) {
    testthat::expect_true(all(drought_assets$asset_category == "agriculture"))
    testthat::expect_true("season" %in% names(res$assets_factors))
  }

  # Coverage by matching method: each matching_method should include FloodTIF and Compound
  mm_cov <- res$assets_factors |>
    dplyr::group_by(.data$matching_method) |>
    dplyr::summarise(
      has_flood = any(grepl("FloodTIF", .data$hazard_name)),
      has_compound = any(grepl("Compound", .data$hazard_name)),
      .groups = "drop"
    )
  if (nrow(mm_cov) > 0) {
    testthat::expect_true(all(mm_cov$has_flood))
    testthat::expect_true(all(mm_cov$has_compound))
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
  # Include all hazard sources (TIF, NC, CSV). Compound hazards are provided via CSV.
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

  # Events: FloodTIF (acute), Compound (acute), and Drought (acute with season)
  # Hazard names match the actual available test data:
  # - FloodTIF: Uses GWL= format with scenario_name (TIF files)
  # - Compound: Uses GWL= with ensemble (CSV files)
  # - Drought: Uses GWL= with season and ensemble (NC files)
  events <- data.frame(
    hazard_type = c("FloodTIF", "FloodTIF", "FloodTIF", "Compound", "Compound", "Drought", "Drought"),
    hazard_name = c(
      "FloodTIF__depth(cm)__GWL=CurrentClimate__RP=10",
      "FloodTIF__depth(cm)__GWL=CurrentClimate__RP=10",
      "FloodTIF__depth(cm)__GWL=RCP8.5__RP=100",
      "Compound__HI__GWL=present__RP=10__ensemble=mean",
      "Compound__HI__GWL=2__RP=10__ensemble=mean",
      "Drought__SPI3__GWL=present__RP=10__season=Summer__ensemble=mean",
      "Drought__SPI3__GWL=1.5__RP=10__season=Winter__ensemble=mean"
    ),
    scenario_name = c("CurrentClimate", "CurrentClimate", "RCP8.5", "present", "2", "present", "1.5"),
    scenario_code = c("pc", "pc", "rcp85", "present", "2", "present", "1.5"),
    hazard_return_period = c(10, 10, 100, 10, 10, 10, 10),
    event_year = c(2030L, 2031L, 2035L, 2030L, 2035L, 2032L, 2033L),
    season = c(NA, NA, NA, NA, NA, "Summer", "Winter"), # Season only for Drought
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
    net_profit_margin = 0.1,
    discount_rate = 0.05
  )

  # Snapshot test of each dataframe
  testthat::expect_snapshot_value(res$assets_factors, style = "deparse", cran = FALSE)
  testthat::expect_snapshot_value(res$companies, style = "deparse", cran = FALSE)
  testthat::expect_snapshot_value(res$assets_yearly, style = "deparse", cran = FALSE)
  testthat::expect_snapshot_value(res$companies_yearly, style = "deparse", cran = FALSE)
})
