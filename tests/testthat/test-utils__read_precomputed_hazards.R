# Tests for function: read_precomputed_hazards

# Contract:
# - read_precomputed_hazards(base_dir) -> data.frame
# - Reads precomputed_adm_hazards.csv from base_dir/
# - Returns data frame with columns: region, adm_level, scenario_code, scenario_name, 
#   hazard_return_period, hazard_type, min, max, mean, median, p2_5, p5, p95, p97_5
# - adm_level values: "ADM1" (province), "ADM2" (municipality)
# - Used to look up hazard statistics for assets matched by municipality or province name


testthat::test_that("read_precomputed_hazards loads CSV and returns expected structure", {
  base_dir <- get_test_data_dir()
  precomputed <- read_precomputed_hazards(base_dir)
  
  # Should return a data frame
  testthat::expect_true(is.data.frame(precomputed))
  testthat::expect_gt(nrow(precomputed), 0)
  
  # Should have required columns
  required_cols <- c("region", "adm_level", "scenario_code", "scenario_name", 
                     "hazard_return_period", "hazard_type", "min", "max", "mean", 
                     "median", "p2_5", "p5", "p95", "p97_5")
  testthat::expect_true(all(required_cols %in% names(precomputed)))
  
  # adm_level should be ADM1 or ADM2
  testthat::expect_true(all(precomputed$adm_level %in% c("ADM1", "ADM2")))
  
  # Numeric columns should be numeric
  numeric_cols <- c("min", "max", "mean", "median", "p2_5", "p5", "p95", "p97_5", "hazard_return_period")
  for (col in numeric_cols) {
    testthat::expect_true(is.numeric(precomputed[[col]]))
  }
})


testthat::test_that("read_precomputed_hazards contains both ADM1 and ADM2 data", {
  base_dir <- get_test_data_dir()
  precomputed <- read_precomputed_hazards(base_dir)
  
  # Should have both province (ADM1) and municipality (ADM2) data
  adm_levels <- unique(precomputed$adm_level)
  testthat::expect_true("ADM1" %in% adm_levels)
  testthat::expect_true("ADM2" %in% adm_levels)
})


testthat::test_that("read_precomputed_hazards can lookup specific region hazards", {
  base_dir <- get_test_data_dir()
  precomputed <- read_precomputed_hazards(base_dir)
  
  # Test lookup for a known province (ADM1)
  amazonas_data <- precomputed |>
    dplyr::filter(.data$region == "Amazonas", .data$adm_level == "ADM1")
  testthat::expect_gt(nrow(amazonas_data), 0)
  
  # Should have flood data
  flood_data <- amazonas_data |>
    dplyr::filter(.data$hazard_type == "flood")
  testthat::expect_gt(nrow(flood_data), 0)
  
  # Mean should be a valid number
  testthat::expect_true(is.numeric(flood_data$mean[1]))
  testthat::expect_false(is.na(flood_data$mean[1]))
})

