testthat::test_that("mod_hazards_events_ui renders static placeholders and controls", {
  ui <- mod_hazards_events_ui("hz")
  html <- htmltools::renderTags(ui)$html
  testthat::expect_true(grepl("hz-events_ui", html))
  testthat::expect_true(grepl("Hazard events", html))
  testthat::expect_true(grepl("hz-upload_hazard_config", html))
  testthat::expect_true(grepl("hz-download_config", html))
})
testthat::test_that("mod_hazards_events_server loads events via load_config function", {
  testthat::skip_on_ci()
  testthat::skip_if_not_installed("shiny")

  inventory_df <- tibble::tibble(
    hazard_type = c("Heat"),
    hazard_indicator = c("HI"),
    scenario_name = c("GWL=2.0"),
    hazard_return_period = c(50),
    hazard_name = c("Heat__HI__GWL=2.0__RP=50")
  )

  config_df <- tibble::tibble(
    hazard_type = "Heat",
    scenario_name = "GWL=2.0",
    hazard_return_period = 50,
    event_year = 2040,
    season = NA_character_
  )

  tmp <- tempfile(fileext = ".xlsx")
  writexl::write_xlsx(config_df, tmp)

  shiny::testServer(mod_hazards_events_server, args = list(
    id = "hz",
    hazards_inventory = shiny::reactive(inventory_df)
  ), {
    ret <- session$returned
    load_config_fn <- ret$load_config
    load_config_fn(tmp)

    ev <- events_rv()
    testthat::expect_equal(nrow(ev), 1)
    testthat::expect_equal(ev$hazard_type[1], "Heat")
    testthat::expect_equal(ev$scenario_name[1], "GWL=2.0")
    testthat::expect_equal(ev$hazard_return_period[1], 50)
    testthat::expect_equal(ev$event_year[1], 2040L)
    testthat::expect_true(is.na(ev$season[1]))
  })
})


testthat::test_that("mod_hazards_events_server exposes events reactive", {
  testthat::skip_on_ci()
  testthat::skip_if_not_installed("shiny")
  shiny::testServer(mod_hazards_events_server, args = list(id = "hz", hazards_inventory = shiny::reactive({
    # Minimal fake inventory with required columns including hazard_indicator
    data.frame(
      key = c("flood__rcp85_h100glob", "flood__rcp85_h10glob"),
      hazard_type = c("Flood", "Flood"),
      hazard_indicator = c("depth(cm)", "depth(cm)"),
      scenario_name = c("RCP8.5", "RCP8.5"),
      hazard_return_period = c(100, 10),
      scenario_code = c("rcp85", "rcp85"),
      hazard_name = c("flood__rcp85_h100glob", "flood__rcp85_h10glob"),
      stringsAsFactors = FALSE
    )
  })), {
    # Initially, one form is present (k=1)
    testthat::expect_equal(counter(), 1L)

    # No events added yet
    testthat::expect_equal(nrow(events_rv()), 0)

    # Provide selections for the first event (including hazard_indicator)
    session$setInputs("hazard_type_1" = "Flood")
    session$setInputs("scenario_name_1" = "RCP8.5")
    session$setInputs("return_period_1" = 100)
    session$setInputs("year_1" = 2030)

    # First click of add_event should save the current event and increment counter
    session$setInputs(add_event = 1)

    testthat::expect_equal(nrow(events_rv()), 1)
    testthat::expect_equal(counter(), 2L)

    # Add a second event
    session$setInputs("hazard_type_2" = "Flood")
    session$setInputs("scenario_name_2" = "RCP8.5")
    session$setInputs("return_period_2" = 10)
    session$setInputs("year_2" = 2035)
    session$setInputs(add_event = 2)

    ret <- session$returned
    ev_fun <- ret$events
    ev <- ev_fun()
    testthat::expect_true(is.data.frame(ev))
    testthat::expect_gte(nrow(ev), 2) # Now expecting 2 events
  })
})

testthat::test_that("mod_hazards_events_server shows only one form at a time", {
  testthat::skip_on_ci()
  testthat::skip_if_not_installed("shiny")
  shiny::testServer(mod_hazards_events_server, args = list(id = "hz", hazards_inventory = shiny::reactive({
    # Minimal fake inventory with required columns including hazard_indicator
    data.frame(
      key = c("flood__rcp85_h100glob", "flood__rcp85_h10glob"),
      hazard_type = c("flood", "flood"),
      hazard_indicator = c("depth(cm)", "depth(cm)"),
      scenario_name = c("RCP8.5", "RCP8.5"),
      hazard_return_period = c(100, 10),
      scenario_code = c("rcp85", "rcp85"),
      hazard_name = c("flood__rcp85_h100glob", "flood__rcp85_h10glob"),
      stringsAsFactors = FALSE
    )
  })), {
    # Initially, one form is present (hazard_type_1, hazard_indicator_1, scenario_name_1, return_period_1)
    ui_html <- htmltools::renderTags(output$events_ui)$html
    testthat::expect_true(grepl("hazard_type_1", ui_html))
    testthat::expect_false(grepl("hazard_type_2", ui_html))

    # Add first event
    session$setInputs("hazard_type_1" = "flood")
    session$setInputs("hazard_indicator_1" = "depth(cm)")
    session$setInputs("scenario_name_1" = "RCP8.5")
    session$setInputs("return_period_1" = 100)
    session$setInputs("year_1" = 2030)
    session$setInputs(add_event = 1) # This is now the first 'add_event' click

    # After adding event, should still show only one form (now hazard_type_2, not hazard_type_1)
    ui_html <- htmltools::renderTags(output$events_ui)$html
    testthat::expect_true(grepl("hazard_type_2", ui_html))
    testthat::expect_false(grepl("hazard_type_1", ui_html))
  })
})

testthat::test_that("mod_hazards_events_server captures season for Drought events", {
  testthat::skip_on_ci()
  testthat::skip_if_not_installed("shiny")
  shiny::testServer(mod_hazards_events_server, args = list(id = "hz", hazards_inventory = shiny::reactive({
    # Inventory with Drought hazard
    data.frame(
      key = c("Drought__SPI3__GWL=present__RP=10__ensemble=median"),
      hazard_type = c("Drought"),
      hazard_indicator = c("SPI3"),
      scenario_name = c("present"),
      scenario_code = c("present"),
      hazard_return_period = c(10),
      hazard_name = c("Drought__SPI3__GWL=present__RP=10__ensemble=median"),
      stringsAsFactors = FALSE
    )
  })), {
    # Set up drought event with season
    session$setInputs("hazard_type_1" = "Drought")
    session$setInputs("hazard_indicator_1" = "SPI3")
    session$setInputs("scenario_name_1" = "present")
    session$setInputs("return_period_1" = 10)
    session$setInputs("season_1" = "Summer")
    session$setInputs("year_1" = 2030)
    session$setInputs(add_event = 1)

    # Check that event was created with season
    ev <- events_rv()
    testthat::expect_equal(nrow(ev), 1)
    testthat::expect_equal(ev$season[1], "Summer")
    testthat::expect_equal(ev$hazard_type[1], "Drought")
  })
})

testthat::test_that("mod_hazards_events_server sets season to NA for non-Drought events", {
  testthat::skip_on_ci()
  testthat::skip_if_not_installed("shiny")
  shiny::testServer(mod_hazards_events_server, args = list(id = "hz", hazards_inventory = shiny::reactive({
    # Inventory with Flood hazard
    data.frame(
      key = c("flood__rcp85_h100glob"),
      hazard_type = c("Flood"),
      hazard_indicator = c("depth(cm)"),
      scenario_name = c("RCP8.5"),
      scenario_code = c("rcp85"),
      hazard_return_period = c(100),
      hazard_name = c("flood__rcp85_h100glob"),
      stringsAsFactors = FALSE
    )
  })), {
    # Set up flood event (no season should be captured)
    session$setInputs("hazard_type_1" = "Flood")
    session$setInputs("scenario_name_1" = "RCP8.5")
    session$setInputs("return_period_1" = 100)
    session$setInputs("year_1" = 2030)
    session$setInputs(add_event = 1)

    # Check that event was created with NA season
    ev <- events_rv()
    testthat::expect_equal(nrow(ev), 1)
    testthat::expect_true(is.na(ev$season[1]))
    testthat::expect_equal(ev$hazard_type[1], "Flood")
  })
})
