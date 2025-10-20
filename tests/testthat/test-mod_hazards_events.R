testthat::test_that("mod_hazards_events_ui renders controls", {
  ui <- mod_hazards_events_ui("hz")
  html <- htmltools::renderTags(ui)$html
  testthat::expect_true(grepl("hz-add_event", html))
  testthat::expect_true(grepl("Hazard events", html))
})

testthat::test_that("mod_hazards_events_server exposes events reactive", {
  testthat::skip_on_ci()
  testthat::skip_if_not_installed("shiny")
  shiny::testServer(mod_hazards_events_server, args = list(id = "hz", hazards_inventory = shiny::reactive({
    # Minimal fake inventory with required columns including hazard_indicator
    data.frame(
      key = c("flood__rcp85_h100glob", "flood__rcp85_h10glob"),
      hazard_type = c("flood", "flood"),
      hazard_indicator = c("Flood Height", "Flood Height"),
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
    session$setInputs("hazard_type_1" = "flood")
    session$setInputs("hazard_indicator_1" = "Flood Height")
    session$setInputs("scenario_name_1" = "RCP8.5")
    session$setInputs("return_period_1" = 100)
    session$setInputs("chronic_1" = FALSE)
    session$setInputs("year_1" = 2030)

    # First click of add_event should save the current event and increment counter
    session$setInputs(add_event = 1)

    testthat::expect_equal(nrow(events_rv()), 1)
    testthat::expect_equal(counter(), 2L)

    # Add a second event
    session$setInputs("hazard_type_2" = "flood")
    session$setInputs("hazard_indicator_2" = "Flood Height")
    session$setInputs("scenario_name_2" = "RCP8.5")
    session$setInputs("return_period_2" = 10)
    session$setInputs("chronic_2" = TRUE)
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
      hazard_indicator = c("Flood Height", "Flood Height"),
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
    session$setInputs("hazard_indicator_1" = "Flood Height")
    session$setInputs("scenario_name_1" = "RCP8.5")
    session$setInputs("return_period_1" = 100)
    session$setInputs("chronic_1" = FALSE)
    session$setInputs("year_1" = 2030)
    session$setInputs(add_event = 1) # This is now the first 'add_event' click

    # After adding event, should still show only one form (now hazard_type_2, not hazard_type_1)
    ui_html <- htmltools::renderTags(output$events_ui)$html
    testthat::expect_true(grepl("hazard_type_2", ui_html))
    testthat::expect_false(grepl("hazard_type_1", ui_html))
  })
})
