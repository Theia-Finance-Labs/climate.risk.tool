testthat::test_that("compute_risk orchestrates new split functions", {
  base_dir <- get_test_data_dir()
  testthat::skip_if_not(dir.exists(base_dir), "Test data missing")
  assets <- read_assets(base_dir)
  companies <- read_companies(file.path(base_dir, "user_input", "company.csv"))
  hazards <- load_hazards(file.path(base_dir, "hazards"))
  areas <- load_location_areas(
    file.path(base_dir, "areas", "municipality"),
    file.path(base_dir, "areas", "province")
  )
  damage_factors <- file.path(base_dir, "damage_and_cost_factors.csv")

  # Define two events
  events <- data.frame(
    event_id = c("e1", "e2"),
    hazard_type = c("floods", "floods"),
    scenario = c("amazonas_global_rcp85_h100glob", "amazonas_global_rcp85_h100glob"),
    event_year = c(2030L, NA_integer_),
    chronic = c(FALSE, TRUE)
  )

  # The refactored compute_risk should accept optional events and delegate
  res <- compute_risk(
    assets = assets,
    companies = companies,
    hazards = hazards,
    areas = areas,
    damage_factors = damage_factors,
    events = events,
    growth_rate = 0.02,
    net_profit_margin = 0.1,
    discount_rate = 0.05,
    verbose = FALSE
  )
  testthat::expect_true(all(c("assets", "companies") %in% names(res)))
})


