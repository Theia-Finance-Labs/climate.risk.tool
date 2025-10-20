testthat::test_that("compute_risk end-to-end integration across hazards and event types", {
  base_dir <- get_test_data_dir()
  assets <- read_assets(base_dir)
  companies <- read_companies(file.path(base_dir, "user_input", "company.csv"))
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

  # Events: FloodTIF (acute + chronic) and Compound (acute); omit event_id so it is auto-generated
  events <- data.frame(
    hazard_type = c("FloodTIF", "FloodTIF", "Compound", "Compound"),
    hazard_name = c(
      "FloodTIF__Flood Height__GWL=CurrentClimate__RP=10",
      "FloodTIF__Flood Height__GWL=CurrentClimate__RP=10",
      # Compound CSV hazards use indicator 'HI' (Heat Index) in our data
      "Compound__HI__GWL=present__RP=10__ensemble=mean",
      "Compound__HI__GWL=3__RP=10__ensemble=mean"
    ),
    scenario_name = c("CurrentClimate", "CurrentClimate", "present", "3"),
    scenario_code = c("pc", "pc", "present", "3"),
    hazard_return_period = c(10, 10, 10, 10),
    event_year = c(2030L, NA_integer_, 2030L, 2035L),
    chronic = c(FALSE, TRUE, FALSE, FALSE),
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
  testthat::expect_true(all(c("matching_method", "hazard_return_period", "event_year", "chronic") %in% names(res$assets_factors)))
  testthat::expect_true(all(res$assets_factors$matching_method %in% c("coordinates", "municipality", "province")))
  testthat::expect_true(all(!is.na(res$assets_factors$hazard_return_period)))
  testthat::expect_true(all(res$assets_factors$chronic %in% c(TRUE, FALSE)))

  # Event IDs: auto-generated when not provided
  testthat::expect_true("event_id" %in% names(res$assets_factors))
  unique_ids <- unique(res$assets_factors$event_id)
  testthat::expect_true(any(grepl("^event_", unique_ids)))

  # Hazards coverage: Flood present
  testthat::expect_true(any(grepl("FloodTIF", res$assets_factors$hazard_name)))
  testthat::expect_true(any(grepl("Compound", res$assets_factors$hazard_name)))

  # Coverage by matching method: each matching_method should include both FloodTIF and Compound
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
    post_2030_base  <- a_y[a_y$year >= 2030 & a_y$scenario == "baseline", ]
    if (nrow(post_2030_shock) > 0 && nrow(post_2030_base) > 0) {
      testthat::expect_true(mean(post_2030_shock$revenue) <= mean(post_2030_base$revenue))
    }
  }

  # Chronic behavior: total shock revenue should be <= baseline over full horizon
  if (all(c("scenario", "revenue") %in% names(a_y))) {
    total_shock <- sum(a_y$revenue[a_y$scenario == "shock"])
    total_base  <- sum(a_y$revenue[a_y$scenario == "baseline"])
    testthat::expect_true(total_shock <= total_base)
  }
})
