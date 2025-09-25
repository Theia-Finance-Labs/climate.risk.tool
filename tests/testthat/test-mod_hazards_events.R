testthat::test_that("mod_hazards_events_ui renders controls", {
  ui <- mod_hazards_events_ui("hz")
  html <- htmltools::renderTags(ui)$html
  testthat::expect_true(grepl("hz-add_event", html))
  testthat::expect_true(grepl("Hazard events", html))
})

testthat::test_that("mod_hazards_events_server exposes events reactive", {
  shiny::testServer(mod_hazards_events_server, args = list(id = "hz", hazards_inventory = shiny::reactive({
    # Minimal fake inventory
    data.frame(
      key = c("floods__1in10", "floods__1in100"),
      hazard_type = c("floods", "floods"),
      scenario = c("1in10", "1in100"),
      stringsAsFactors = FALSE
    )
  })), {
    # Initially, one form is present (k=1)
    testthat::expect_equal(counter(), 1L)
    
    # No events added yet
    testthat::expect_equal(nrow(events_rv()), 0)
    
    # Provide selections for the first event
    session$setInputs("hazard_1" = "floods")
    session$setInputs("scenario_1" = "1in10")
    session$setInputs("chronic_1" = FALSE)
    session$setInputs("year_1" = 2030)
    
    # First click of add_event should save the current event and increment counter
    session$setInputs(add_event = 1)
    
    testthat::expect_equal(nrow(events_rv()), 1)
    testthat::expect_equal(counter(), 2L)
    
    # Add a second event
    session$setInputs("hazard_2" = "floods")
    session$setInputs("scenario_2" = "1in100")
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
  shiny::testServer(mod_hazards_events_server, args = list(id = "hz", hazards_inventory = shiny::reactive({
    # Minimal fake inventory
    data.frame(
      key = c("floods__1in10", "floods__1in100"),
      hazard_type = c("floods", "floods"),
      scenario = c("1in10", "1in100"),
      stringsAsFactors = FALSE
    )
  })), {
    # Initially, one form is present (hazard_1)
    ui_html <- htmltools::renderTags(output$events_ui)$html
    testthat::expect_true(grepl("hazard_1", ui_html))
    testthat::expect_false(grepl("hazard_2", ui_html))
    
    # Add first event
    session$setInputs("hazard_1" = "floods")
    session$setInputs("scenario_1" = "1in10")
    session$setInputs("chronic_1" = FALSE)
    session$setInputs("year_1" = 2030)
    session$setInputs(add_event = 1) # This is now the first 'add_event' click
    
    # After adding event, should still show only one form (now hazard_2, not hazard_1)
    ui_html <- htmltools::renderTags(output$events_ui)$html
    testthat::expect_true(grepl("hazard_2", ui_html))
    testthat::expect_false(grepl("hazard_1", ui_html))
  })
})


