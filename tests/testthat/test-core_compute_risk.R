testthat::test_that('compute_risk executes complete pipeline', {
  # Use test data directory
  base_dir <- system.file("tests", "tests_data", package = "climate.risk.tool")
  
  # Skip if test data not available
  testthat::skip_if_not(dir.exists(base_dir), "Test data directory not found")
  testthat::skip_if_not(dir.exists(file.path(base_dir, "user_input")), "user_input directory not found")
  testthat::skip_if_not(dir.exists(file.path(base_dir, "hazards")), "hazards directory not found")
  testthat::skip_if_not(dir.exists(file.path(base_dir, "areas")), "areas directory not found")
  testthat::skip_if_not(file.exists(file.path(base_dir, "damage_and_cost_factors.csv")), "damage_and_cost_factors.csv not found")
  
  # Load data first
  assets <- read_assets(base_dir)
  companies <- read_companies(file.path(base_dir, "user_input", "company.csv"))
  hazards <- load_hazards(file.path(base_dir, "hazards"))
  areas <- load_location_areas(
    file.path(base_dir, "areas", "municipality"),
    file.path(base_dir, "areas", "province")
  )
  damage_factors_path <- file.path(base_dir, "damage_and_cost_factors.csv")
  
  # Run the complete pipeline with minimal verbosity for testing
  result <- compute_risk(
    assets = assets,
    companies = companies,
    hazards = hazards,
    areas = areas,
    damage_factors = damage_factors_path,
    shock_year = 2030,
    growth_rate = 0.02,
    net_profit_margin = 0.1,
    discount_rate = 0.05,
    verbose = FALSE
  )
  
  # Test structure of results
  testthat::expect_type(result, "list")
  testthat::expect_true(all(c("assets", "companies", "intermediate") %in% names(result)))
  
  # Test that we got data frames for final results
  testthat::expect_s3_class(result$assets, "data.frame")
  testthat::expect_s3_class(result$companies, "data.frame")
  
  # Test intermediate results structure
  testthat::expect_type(result$intermediate, "list")
  testthat::expect_true(length(result$intermediate) >= 16)  # Should have at least 16 steps
  
  # Test that final company results have expected columns
  expected_company_cols <- c("company", "NPV_baseline", "NPV_shock", 
                           "PD_baseline", "PD_shock", 
                           "Expected_loss_baseline", "Expected_loss_shock")
  testthat::expect_true(all(expected_company_cols %in% names(result$companies)))
  
  # Test that final asset results have NPV columns
  testthat::expect_true(any(grepl("NPV_", names(result$assets))))
  
  # Test basic data integrity
  testthat::expect_gte(nrow(result$companies), 1)  # Should have at least one company
})

testthat::test_that('compute_risk handles invalid inputs gracefully', {
  # Test with invalid assets
  testthat::expect_error(
    compute_risk(
      assets = "invalid",
      companies = data.frame(),
      hazards = list(),
      areas = list(),
      damage_factors = "invalid_path.csv",
      shock_year = 2030,
      verbose = FALSE
    ),
    "assets must be a non-empty data.frame"
  )
  
  # Test with empty hazards
  testthat::expect_error(
    compute_risk(
      assets = data.frame(x = 1),
      companies = data.frame(y = 1),
      hazards = list(),
      areas = list(municipalities = list(), provinces = list()),
      damage_factors = "invalid_path.csv",
      shock_year = 2030,
      verbose = FALSE
    ),
    "hazards must be a non-empty"
  )
  
  # Test with invalid areas
  testthat::expect_error(
    compute_risk(
      assets = data.frame(x = 1),
      companies = data.frame(y = 1),
      hazards = list(hazard1 = "dummy"),
      areas = list(),
      damage_factors = "invalid_path.csv",
      shock_year = 2030,
      verbose = FALSE
    ),
    "areas must be a list"
  )
})

testthat::test_that('compute_risk parameters work correctly', {
  # Use test data directory
  base_dir <- system.file("tests", "tests_data", package = "climate.risk.tool")
  
  # Skip if test data not available
  testthat::skip_if_not(dir.exists(base_dir), "Test data directory not found")
  testthat::skip_if_not(dir.exists(file.path(base_dir, "user_input")), "user_input directory not found")
  testthat::skip_if_not(dir.exists(file.path(base_dir, "hazards")), "hazards directory not found")
  testthat::skip_if_not(dir.exists(file.path(base_dir, "areas")), "areas directory not found")
  testthat::skip_if_not(file.exists(file.path(base_dir, "damage_and_cost_factors.csv")), "damage_and_cost_factors.csv not found")
  
  # Load data first
  assets <- read_assets(base_dir)
  companies <- read_companies(file.path(base_dir, "user_input", "company.csv"))
  hazards <- load_hazards(file.path(base_dir, "hazards"))
  areas <- load_location_areas(
    file.path(base_dir, "areas", "municipality"),
    file.path(base_dir, "areas", "province")
  )
  damage_factors_path <- file.path(base_dir, "damage_and_cost_factors.csv")
  
  # Test with different parameter values
  result <- compute_risk(
    assets = assets,
    companies = companies,
    hazards = hazards,
    areas = areas,
    damage_factors = damage_factors_path,
    shock_year = 2025,
    growth_rate = 0.03,
    net_profit_margin = 0.15,
    discount_rate = 0.07,
    verbose = FALSE
  )
  
  # Should still return valid structure
  testthat::expect_type(result, "list")
  testthat::expect_true(all(c("assets", "companies", "intermediate") %in% names(result)))
})
