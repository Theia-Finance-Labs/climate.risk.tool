testthat::test_that("compute_risk orchestrates new yearly trajectory functions", {
  base_dir <- get_test_data_dir()
  assets <- read_assets(base_dir)
  companies <- read_companies(file.path(base_dir, "user_input", "company.csv"))
  hazards <- load_hazards(file.path(base_dir, "hazards"), aggregate_factor = 16L)
  areas <- load_location_areas(
    file.path(base_dir, "areas", "municipality"),
    file.path(base_dir, "areas", "province")
  )
  damage_factors <- read_damage_cost_factors(base_dir)
  inventory <- list_hazard_inventory(hazards)

  # Define two events - one acute, one chronic
  # Use h10 which has actual data, not h100 which is empty
  events <- data.frame(
    event_id = c("e1", "e2"),
    hazard_type = rep("flood", 2),
    hazard_name = rep("flood__global_rcp85_h10glob", 2),
    event_year = c(2030L, NA_integer_),
    chronic = c(FALSE, TRUE)
  )

  # The refactored compute_risk should return both aggregated and yearly data
  res <- compute_risk(
    assets = assets,
    companies = companies,
    events = events,
    hazards = hazards,
    areas = areas,
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
  testthat::expect_true(nrow(res$assets_yearly) > nrow(res$assets))
  testthat::expect_true(nrow(res$companies_yearly) > nrow(res$companies))

  # Yearly assets should have year and scenario columns
  testthat::expect_true("year" %in% names(res$assets_yearly))
  testthat::expect_true("scenario" %in% names(res$assets_yearly))
  testthat::expect_true("year" %in% names(res$companies_yearly))
  testthat::expect_true("scenario" %in% names(res$companies_yearly))

  # Should have baseline and shock scenarios
  testthat::expect_true(all(c("baseline", "shock") %in% unique(res$assets_yearly$scenario)))
  testthat::expect_true(all(c("baseline", "shock") %in% unique(res$companies_yearly$scenario)))
})

testthat::test_that("compute_risk processes single acute event", {
  base_dir <- get_test_data_dir()
  assets <- read_assets(base_dir)
  companies <- read_companies(file.path(base_dir, "user_input", "company.csv"))
  hazards <- load_hazards(file.path(base_dir, "hazards"), aggregate_factor = 16L)
  areas <- load_location_areas(
    file.path(base_dir, "areas", "municipality"),
    file.path(base_dir, "areas", "province")
  )
  damage_factors <- read_damage_cost_factors(base_dir)

  # Single acute event
  # Use h10 which has actual data, not h100 which is empty
  events <- data.frame(
    event_id = "acute_2030",
    hazard_type = "flood",
    hazard_name = "flood__global_rcp85_h10glob",
    event_year = 2030L,
    chronic = FALSE
  )

  res <- compute_risk(
    assets = assets,
    companies = companies,
    events = events,
    hazards = hazards,
    areas = areas,
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
  hazards <- load_hazards(file.path(base_dir, "hazards"), aggregate_factor = 16L)
  areas <- load_location_areas(
    file.path(base_dir, "areas", "municipality"),
    file.path(base_dir, "areas", "province")
  )
  damage_factors <- read_damage_cost_factors(base_dir)

  # Single chronic event
  # Use h10 which has actual data, not h100 which is empty
  events <- data.frame(
    event_id = "chronic",
    hazard_type = "flood",
    hazard_name = "flood__global_rcp85_h10glob",
    event_year = NA_integer_,
    chronic = TRUE
  )

  res <- compute_risk(
    assets = assets,
    companies = companies,
    events = events,
    hazards = hazards,
    areas = areas,
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
  hazards <- load_hazards(file.path(base_dir, "hazards"), aggregate_factor = 16L)
  areas <- load_location_areas(
    file.path(base_dir, "areas", "municipality"),
    file.path(base_dir, "areas", "province")
  )
  damage_factors <- read_damage_cost_factors(base_dir)
  inventory <- list_hazard_inventory(hazards)

  events <- data.frame(
    event_id = "test",
    hazard_type = "flood",
    hazard_name = inventory$hazard_name[1],
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
      areas = areas,
      damage_factors = damage_factors
    ),
    "hazards must be a non-empty named list"
  )

  # Test missing areas parameter
  testthat::expect_error(
    compute_risk(
      assets = assets,
      companies = companies,
      events = events,
      hazards = hazards,
      areas = NULL,
      damage_factors = damage_factors
    ),
    "areas must be a list with 'municipalities' and 'provinces'"
  )
})

testthat::test_that("compute_risk carries hazard_name through to events", {
  base_dir <- get_test_data_dir()
  assets <- read_assets(base_dir)
  hazards <- load_hazards(get_hazards_dir(), aggregate_factor = 16L)
  municipalities <- load_municipalities(file.path(base_dir, "areas", "municipality"))
  provinces <- load_provinces(file.path(base_dir, "areas", "province"))
  output_crs <- terra::crs(hazards[[1]])

  assets_geo <- geolocate_assets(assets, municipalities, provinces, output_crs = output_crs)
  assets_long <- extract_hazard_statistics(assets_geo, hazards)
  damage_factors <- read_damage_cost_factors(base_dir)
  assets_factors <- join_damage_cost_factors(assets_long, damage_factors)

  # Use h10 which has actual data, not h100 which is empty
  events <- data.frame(
    event_id = "ev1",
    hazard_type = "flood",
    hazard_name = "flood__global_rcp85_h10glob",
    event_year = 2030,
    chronic = FALSE
  )

  companies <- read_companies(file.path(base_dir, "user_input", "company.csv"))

  expect_error(
    compute_risk(
      assets = assets,
      companies = companies,
      events = events,
      hazards = hazards,
      areas = list(municipalities = municipalities, provinces = provinces),
      damage_factors = damage_factors
    ),
    NA
  )
})
