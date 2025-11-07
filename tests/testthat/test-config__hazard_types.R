# Tests for hazard type configuration functions

# Contract:
# - get_hazard_type_config() returns configuration for all hazard types
# - is_multi_indicator_hazard(hazard_type) returns TRUE for Fire, FALSE for others
# - get_primary_indicator(hazard_type) returns primary indicator name
# - get_required_indicators(hazard_type) returns all required indicators


testthat::test_that("get_hazard_type_config returns Fire configuration with 3 indicators", {
  config <- get_hazard_type_config()

  testthat::expect_true("Fire" %in% names(config))

  fire_config <- config$Fire
  testthat::expect_equal(length(fire_config$indicators), 3)
  testthat::expect_true("land_cover" %in% fire_config$indicators)
  testthat::expect_true("FWI" %in% fire_config$indicators)
  testthat::expect_true("days_danger_total" %in% fire_config$indicators)
  testthat::expect_equal(fire_config$primary_indicator, "FWI")
})

testthat::test_that("is_multi_indicator_hazard correctly identifies Fire as multi-indicator", {
  testthat::expect_true(is_multi_indicator_hazard("Fire"))
  testthat::expect_false(is_multi_indicator_hazard("Flood"))
  testthat::expect_false(is_multi_indicator_hazard("Heat"))
  testthat::expect_false(is_multi_indicator_hazard("Drought"))
})

testthat::test_that("get_primary_indicator returns FWI for Fire", {
  primary <- get_primary_indicator("Fire")
  testthat::expect_equal(primary, "FWI")
})

testthat::test_that("get_primary_indicator returns correct primary indicator for single-indicator hazards", {
  testthat::expect_equal(get_primary_indicator("Flood"), "depth(cm)")
  testthat::expect_equal(get_primary_indicator("Heat"), "HI")
  testthat::expect_equal(get_primary_indicator("Drought"), "SPI3")
})

testthat::test_that("get_required_indicators returns all 3 indicators for Fire", {
  indicators <- get_required_indicators("Fire")

  testthat::expect_equal(length(indicators), 3)
  testthat::expect_true("land_cover" %in% indicators)
  testthat::expect_true("FWI" %in% indicators)
  testthat::expect_true("days_danger_total" %in% indicators)
})

testthat::test_that("get_required_indicators returns single indicator for single-indicator hazards", {
  flood_indicators <- get_required_indicators("Flood")
  testthat::expect_equal(length(flood_indicators), 1)
  testthat::expect_equal(flood_indicators, "depth(cm)")

  compound_indicators <- get_required_indicators("Heat")
  testthat::expect_equal(length(compound_indicators), 1)
  testthat::expect_equal(compound_indicators, "HI")

  drought_indicators <- get_required_indicators("Drought")
  testthat::expect_equal(length(drought_indicators), 1)
  testthat::expect_equal(drought_indicators, "SPI3")
})

testthat::test_that("get_required_indicators returns NULL for unknown hazard type", {
  indicators <- get_required_indicators("UnknownHazard")
  testthat::expect_null(indicators)
})

testthat::test_that("get_primary_indicator returns NA for unknown hazard type", {
  primary <- get_primary_indicator("UnknownHazard")
  testthat::expect_true(is.na(primary))
})

testthat::test_that("is_multi_indicator_hazard returns FALSE for unknown hazard type", {
  testthat::expect_false(is_multi_indicator_hazard("UnknownHazard"))
})
