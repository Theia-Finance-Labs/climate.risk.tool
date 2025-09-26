testthat::test_that("compute_hazard_events prepares assets with geospatial data and damage factors", {
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
  damage_factors <- read_damage_cost_factors(base_dir)

  res <- compute_hazard_events(
    assets = assets,
    hazards = hazards,
    areas = areas,
    damage_factors = damage_factors
  )

  testthat::expect_s3_class(res, "data.frame")
  # Should have required long format columns
  required_cols <- c("asset", "hazard_name", "hazard_type", "hazard_intensity", "damage_factor", "cost_factor")
  testthat::expect_true(all(required_cols %in% names(res)))
  
  # Should have damage and cost factors
  testthat::expect_true(is.numeric(res$damage_factor))
  testthat::expect_true(is.numeric(res$cost_factor))
  
  # Should be in long format (more rows than original assets)
  testthat::expect_gte(nrow(res), nrow(assets))
})
