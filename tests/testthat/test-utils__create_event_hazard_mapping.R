testthat::test_that("create_event_hazard_mapping returns inventory hazard_name", {
  hazards_inventory <- tibble::tibble(
    hazard_type = c("Drought", "Fire", "Fire"),
    hazard_indicator = c("standardized_precipitation_index_3", "fire_weather_index", "days_danger_total"),
    scenario_name = c("1.5", "1.5", "1.5"),
    return_period = c(5, 5, 5),
    season = c("Autumn", NA_character_, NA_character_),
    indicator_key = c(
      "spi3__spi3__return_period=5__gwl=1.5__season=Autumn__ensemble=mean",
      "fwi__fwi__return_period=5__gwl=1.5__ensemble=mean",
      "days_danger_total__days_danger_total__return_period=5__gwl=1.5__ensemble=mean"
    ),
    hazard_name = c(
      "Drought__standardized_precipitation_index_3__return_period=5__gwl=1.5__season=Autumn__ensemble=mean",
      "Fire__fire_weather_index__return_period=5__gwl=1.5__ensemble=mean",
      "Fire__days_danger_total__return_period=5__gwl=1.5__ensemble=mean"
    )
  )

  hazard_configs <- list(
    Drought = list(indicators = list(
      standardized_precipitation_index_3 = list(index = c("gwl", "return_period", "season"))
    )),
    Fire = list(indicators = list(
      fire_weather_index = list(index = c("gwl", "return_period")),
      days_danger_total = list(index = c("gwl", "return_period"))
    ))
  )

  events <- tibble::tibble(
    event_id = c("ev1", "ev2"),
    hazard_type = c("Drought", "Fire"),
    scenario_name = c("1.5", "1.5"),
    return_period = c(5, 5),
    season = c("Autumn", NA_character_),
    event_year = c(2030, 2030)
  )

  mapping <- create_event_hazard_mapping(events, hazards_inventory, hazard_configs)

  by_key <- mapping |>
    dplyr::select("indicator_key", "hazard_name") |>
    dplyr::distinct()

  expected <- hazards_inventory |>
    dplyr::select("indicator_key", "hazard_name") |>
    dplyr::distinct()

  testthat::expect_equal(
    dplyr::arrange(by_key, .data$indicator_key),
    dplyr::arrange(expected, .data$indicator_key)
  )
})
