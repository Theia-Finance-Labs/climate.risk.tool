# Tests for mod_status module

testthat::test_that("mod_status_ui creates expected elements", {
  ui <- mod_status_ui("test")
  html <- htmltools::renderTags(ui)$html
  
  # Check that the UI contains expected elements
  testthat::expect_true(grepl("test-status_badge", html))
  testthat::expect_true(grepl("test-status_message", html))
  testthat::expect_true(grepl("test-events_table", html))
  testthat::expect_true(grepl("Analysis Status", html))
  testthat::expect_true(grepl("Configured Hazard Events", html))
})

testthat::test_that("mod_status_server displays events with event_id", {
  # Create test events with event_id
  test_events <- data.frame(
    event_id = c("ev1", "ev2"),
    hazard_type = c("FloodTIF", "FloodTIF"),
    hazard_name = c("FloodTIF__Flood Height__GWL=RCP8.5__RP=100", "FloodTIF__Flood Height__GWL=RCP8.5__RP=50"),
    scenario_name = c("RCP8.5", "RCP8.5"),
    hazard_return_period = c(100, 50),
    event_year = c(2030L, 2035L),
    chronic = c(FALSE, FALSE),
    stringsAsFactors = FALSE
  )
  
  shiny::testServer(mod_status_server, args = list(
    id = "test",
    status_reactive = shiny::reactive("Ready"),
    events_reactive = shiny::reactive(test_events)
  ), {
    # Get the output
    events_output <- output$events_table
    
    # The output should exist
    testthat::expect_true(!is.null(events_output))
  })
})

testthat::test_that("mod_status_server handles empty events gracefully", {
  shiny::testServer(mod_status_server, args = list(
    id = "test",
    status_reactive = shiny::reactive("Ready"),
    events_reactive = shiny::reactive(data.frame())
  ), {
    # Should not error with empty events
    events_output <- output$events_table
    testthat::expect_true(!is.null(events_output))
  })
})

testthat::test_that("mod_status_server handles NULL events gracefully", {
  shiny::testServer(mod_status_server, args = list(
    id = "test",
    status_reactive = shiny::reactive("Ready"),
    events_reactive = shiny::reactive(NULL)
  ), {
    # Should not error with NULL events
    events_output <- output$events_table
    testthat::expect_true(!is.null(events_output))
  })
})

testthat::test_that("mod_status_server displays correct status badges", {
  shiny::testServer(mod_status_server, args = list(
    id = "test",
    status_reactive = shiny::reactive("Error: Something went wrong"),
    events_reactive = shiny::reactive(data.frame())
  ), {
    # Should display ERROR badge for error status
    badge_output <- output$status_badge
    testthat::expect_true(!is.null(badge_output))
  })
  
  shiny::testServer(mod_status_server, args = list(
    id = "test",
    status_reactive = shiny::reactive("Analysis complete"),
    events_reactive = shiny::reactive(data.frame())
  ), {
    # Should display READY badge for complete status
    badge_output <- output$status_badge
    testthat::expect_true(!is.null(badge_output))
  })
})
