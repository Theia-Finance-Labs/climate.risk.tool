testthat::test_that("extract_hazard_statistics returns standardized columns", {
  hazards_data <- load_hazards_and_inventory(
    hazards_dir = get_hazards_dir(),
    hazard_indicators_dir = get_hazard_indicators_dir(),
    aggregate_factor = 16L
  )

  assets <- read_assets(get_test_data_dir("user_input"))
  precomputed <- read_precomputed_hazards(get_test_data_dir())

  results <- extract_hazard_statistics(
    assets_df = assets,
    hazards = hazards_data$hazards,
    hazards_inventory = hazards_data$inventory,
    precomputed_hazards = precomputed,
    aggregation_method = "mean"
  )

  testthat::expect_true(is.data.frame(results))
  testthat::expect_gt(nrow(results), 0)
  testthat::expect_true(all(c("hazard_indicator", "hazard_intensity", "gwl", "return_period") %in% names(results)))
})


