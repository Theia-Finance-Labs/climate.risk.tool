testthat::test_that("filter_hazards_by_events selects indicator keys via inventory", {
  hazards <- list(
    "heat_index__hi__return_period=10__scenario_name=present__ensemble=mean" = terra::rast(ncols = 10, nrows = 10),
    "spi3__spi3__return_period=5__scenario_name=present__ensemble=mean" = terra::rast(ncols = 10, nrows = 10)
  )

  hazards_inventory <- tibble::tibble(
    hazard_type = c("Heat", "Drought"),
    hazard_indicator = c("hi", "spi3"),
    scenario_name = c("present", "present"),
    return_period = c(10, 5),
    hazard_name = c("Heat__hi__present__10__mean", "Drought__spi3__present__5__mean"),
    indicator_key = names(hazards)
  )

  hazard_configs <- list(
    Heat = list(indicators = list(hi = list(index = c("scenario_name", "return_period")))),
    Drought = list(indicators = list(spi3 = list(index = c("scenario_name", "return_period"))))
  )

  events <- data.frame(
    hazard_type = c("Heat"),
    scenario_name = c("present"),
    return_period = c(10),
    event_year = c(2030)
  )

  result <- filter_hazards_by_events(hazards, events, hazards_inventory, hazard_configs)

  expect_equal(length(result), 1)
  expect_true("heat_index__hi__return_period=10__scenario_name=present__ensemble=mean" %in% names(result))
})

testthat::test_that("filter_hazards_by_events expands multi-indicator hazards", {
  hazards <- list(
    "fwi__fwi__return_period=10__scenario_name=present__ensemble=mean" = terra::rast(ncols = 10, nrows = 10),
    "days_danger_total__days_danger_total__return_period=10__scenario_name=present__ensemble=mean" = terra::rast(ncols = 10, nrows = 10)
  )

  hazards_inventory <- tibble::tibble(
    hazard_type = c("Fire", "Fire"),
    hazard_indicator = c("fwi", "days_danger_total"),
    scenario_name = c("present", "present"),
    return_period = c(10, 10),
    hazard_name = c("Fire__fwi__present__10__mean", "Fire__days_danger_total__present__10__mean"),
    indicator_key = names(hazards)
  )

  hazard_configs <- list(
    Fire = list(indicators = list(
      fwi = list(index = c("scenario_name", "return_period")),
      days_danger_total = list(index = c("scenario_name", "return_period"))
    ))
  )

  events <- data.frame(
    hazard_type = c("Fire"),
    scenario_name = c("present"),
    return_period = c(10),
    event_year = c(2030)
  )

  result <- filter_hazards_by_events(hazards, events, hazards_inventory, hazard_configs)

  expect_equal(length(result), 2)
  expect_true(all(names(hazards) %in% names(result)))
})
