testthat::test_that("compute_hazard_events returns long format asset-hazard intensities", {
  # Use test data directory
  base_dir <- system.file("tests", "tests_data", package = "climate.risk.tool")
  testthat::skip_if_not(dir.exists(base_dir), "Test data directory not found")
  testthat::skip_if_not(dir.exists(file.path(base_dir, "hazards")), "hazards directory not found")
  testthat::skip_if_not(dir.exists(file.path(base_dir, "areas")), "areas directory not found")

  assets <- read_assets(base_dir)
  hazards <- load_hazards(file.path(base_dir, "hazards"))
  areas <- load_location_areas(
    file.path(base_dir, "areas", "municipality"),
    file.path(base_dir, "areas", "province")
  )

  # Define two simple events (canonical schema: hazard_type + scenario)
  parts <- strsplit(names(hazards), "__", fixed = TRUE)
  hazard_type_vec <- vapply(parts, function(p) if (length(p) >= 1) p[[1]] else NA_character_, character(1))
  scenario_vec <- vapply(parts, function(p) if (length(p) >= 2) p[[2]] else NA_character_, character(1))
  events <- data.frame(
    event_id = c("e1", "e2"),
    hazard_type = c(hazard_type_vec[1], hazard_type_vec[2]),
    scenario = c(scenario_vec[1], scenario_vec[2]),
    event_year = c(2030L, NA_integer_),
    chronic = c(FALSE, TRUE),
    stringsAsFactors = FALSE
  )

  res <- compute_hazard_events(
    assets = assets,
    hazards = hazards,
    areas = areas,
    events = events,
    damage_factors = file.path(base_dir, "damage_and_cost_factors.csv"),
    verbose = FALSE
  )

  testthat::expect_s3_class(res, "data.frame")
  testthat::expect_true(all(c("asset", "event_id", "hazard_key") %in% names(res)))
})

testthat::test_that("compute_financials_from_assets consumes asset scenarios", {
  # Minimal synthetic asset scenarios
  assets_scenarios <- data.frame(
    asset = c("A1", "A1"),
    company = c("C1", "C1"),
    scenario = c("baseline", "shock"),
    share_of_economic_activity = c(1, 0.9)
  )
  companies <- data.frame(company_name = "C1", revenues = 100)

  res <- compute_financials_from_assets(
    assets_scenarios = assets_scenarios,
    companies = companies,
    growth_rate = 0.02,
    net_profit_margin = 0.1,
    discount_rate = 0.05
  )

  testthat::expect_true(all(c("assets", "companies") %in% names(res)))
})


