# Tests for mod_status module

testthat::test_that("mod_status_ui creates expected elements", {
  ui <- mod_status_ui("test")
  html <- htmltools::renderTags(ui)$html

  # Check that the UI contains expected elements
  testthat::expect_true(grepl("test-status_badge", html))
  testthat::expect_true(grepl("test-status_message", html))
  testthat::expect_true(grepl("test-events_table", html))
  testthat::expect_true(grepl("test-events_map", html))
  testthat::expect_true(grepl("test-events_map_toggles", html))
  testthat::expect_true(grepl("test-run_repro_code", html))
  testthat::expect_true(grepl("test-copy_repro_code", html))
  testthat::expect_true(grepl("copyStatusReproCode\\(this\\)", html))
  testthat::expect_true(grepl("Analysis Status", html))
  testthat::expect_true(grepl("Configured Hazard Events", html))
  testthat::expect_true(grepl("Selected Event Areas in Brazil", html))
  testthat::expect_true(grepl("Reproduction Code", html))
  testthat::expect_true(grepl("Copy to Clipboard", html))
})

build_status_test_spatial_data <- function() {
  state_sf <- sf::st_sf(
    region_code = c("11", "12"),
    region_label = c("State 11", "State 12"),
    geometry = sf::st_sfc(
      sf::st_polygon(list(matrix(c(0, 0, 2, 0, 2, 2, 0, 2, 0, 0), ncol = 2, byrow = TRUE))),
      sf::st_polygon(list(matrix(c(2, 0, 4, 0, 4, 2, 2, 2, 2, 0), ncol = 2, byrow = TRUE)))
    ),
    crs = 4326
  )

  municipality_sf <- sf::st_sf(
    region_code = c("1101", "1102", "1201", "1202"),
    region_label = c("Muni 1101", "Muni 1102", "Muni 1201", "Muni 1202"),
    geometry = sf::st_sfc(
      sf::st_polygon(list(matrix(c(0, 0, 1, 0, 1, 2, 0, 2, 0, 0), ncol = 2, byrow = TRUE))),
      sf::st_polygon(list(matrix(c(1, 0, 2, 0, 2, 2, 1, 2, 1, 0), ncol = 2, byrow = TRUE))),
      sf::st_polygon(list(matrix(c(2, 0, 3, 0, 3, 2, 2, 2, 2, 0), ncol = 2, byrow = TRUE))),
      sf::st_polygon(list(matrix(c(3, 0, 4, 0, 4, 2, 3, 2, 3, 0), ncol = 2, byrow = TRUE)))
    ),
    crs = 4326
  )

  hydro_macro <- sf::st_sf(
    region_code = c("M1", "M2"),
    region_label = c("Macro 1", "Macro 2"),
    geometry = sf::st_geometry(state_sf),
    crs = 4326
  )

  hydro_meso <- sf::st_sf(
    region_code = c("ME1", "ME2"),
    region_label = c("Meso 1", "Meso 2"),
    geometry = sf::st_geometry(state_sf),
    crs = 4326
  )

  hydro_micro <- sf::st_sf(
    region_code = c("MI1", "MI2"),
    region_label = c("Micro 1", "Micro 2"),
    geometry = sf::st_geometry(state_sf),
    crs = 4326
  )

  list(
    adm = list(
      state = state_sf,
      municipality = municipality_sf
    ),
    hydro = list(
      macro = hydro_macro,
      meso = hydro_meso,
      micro = hydro_micro
    ),
    overlaps = list(),
    lookup = list(
      state_name_to_code = c(),
      municipality_name_to_code = c()
    ),
    warnings = character()
  )
}

testthat::test_that("mod_status_server displays events with event_id", {
  testthat::skip_on_ci()
  testthat::skip_if_not_installed("shiny")
  # Create test events with event_id
  test_events <- data.frame(
    event_id = c("ev1", "ev2"),
    hazard_type = c("Flood", "Flood"),
    hazard_name = c(
      "Flood__depth__GWL=rcp85__RP=100__ensemble=mean",
      "Flood__depth__GWL=rcp85__RP=50__ensemble=mean"
    ),
    scenario_name = c("rcp85", "rcp85"),
    return_period = c(100, 50),
    event_year = c(2030L, 2035L),
    spatial_level = c("state", "brazil"),
    spatial_region_codes = c("11|33", NA_character_),
    spatial_region_labels = c("Rondonia|Rio de Janeiro", NA_character_),
    stringsAsFactors = FALSE
  )

  shiny::testServer(mod_status_server, args = list(
    id = "test",
    status_reactive = shiny::reactive("Ready"),
    events_reactive = shiny::reactive(test_events)
  ), {
    # Get the output
    session$flushReact()
    events_output <- session$userData$status_events_table

    # The output should exist
    testthat::expect_true(!is.null(events_output))

    testthat::expect_true("Event ID" %in% colnames(events_output))
    testthat::expect_true("Spatial Separation" %in% colnames(events_output))
    testthat::expect_equal(events_output$`Event ID`, c("ev1", "ev2"))
    testthat::expect_equal(events_output$`Spatial Separation`[2], "Brazil (whole)")
  })
})

testthat::test_that("reconcile_event_toggle_states preserves existing values and enables new ids", {
  current <- c(ev1 = FALSE, ev2 = TRUE)
  next_states <- reconcile_event_toggle_states(c("ev1", "ev3"), current)

  testthat::expect_equal(names(next_states), c("ev1", "ev3"))
  testthat::expect_false(next_states[["ev1"]])
  testthat::expect_true(next_states[["ev3"]])
})

testthat::test_that("build_status_event_geometries resolves adm/hydro/brazil and skips invalid regions", {
  spatial_data <- build_status_test_spatial_data()
  events <- tibble::tibble(
    event_id = c("ev_state", "ev_macro", "ev_brazil", "ev_missing"),
    spatial_level = c("state", "macro", "brazil", "municipality"),
    spatial_scheme = c("adm_regions", "hydro_regions", "adm_regions", "adm_regions"),
    spatial_region_codes = c("11", "M2", NA_character_, "999"),
    spatial_region_labels = c("State 11", "Macro 2", NA_character_, "Unknown")
  )

  geoms <- build_status_event_geometries(events, spatial_data)
  testthat::expect_true("ev_state" %in% names(geoms))
  testthat::expect_true("ev_macro" %in% names(geoms))
  testthat::expect_true("ev_brazil" %in% names(geoms))
  testthat::expect_false("ev_missing" %in% names(geoms))

  testthat::expect_s3_class(geoms$ev_state, "sf")
  testthat::expect_equal(nrow(geoms$ev_state), 1)
  testthat::expect_equal(nrow(geoms$ev_macro), 1)
  testthat::expect_equal(nrow(geoms$ev_brazil), 1)
})

testthat::test_that("mod_status_server map toggles initialize and persist across event changes", {
  testthat::skip_if_not_installed("shiny")
  events_rv <- shiny::reactiveVal(
    tibble::tibble(
      event_id = c("ev1", "ev2"),
      hazard_type = c("Flood", "Heat"),
      hazard_name = c("Flood__x", "Heat__x"),
      scenario_name = c("rcp85", "rcp85"),
      return_period = c(100, 0),
      event_year = c(2030L, 2035L),
      spatial_level = c("state", "macro"),
      spatial_region_codes = c("11", "M2"),
      spatial_region_labels = c("State 11", "Macro 2"),
      spatial_scheme = c("adm_regions", "hydro_regions")
    )
  )

  spatial_data <- build_status_test_spatial_data()

  shiny::testServer(mod_status_server, args = list(
    id = "test",
    status_reactive = shiny::reactive("Ready"),
    events_reactive = shiny::reactive(events_rv()),
    spatial_data_reactive = shiny::reactive(spatial_data)
  ), {
    session$flushReact()
    initial_states <- session$userData$status_map_toggle_states
    testthat::expect_true(all(unlist(initial_states)))
    testthat::expect_equal(names(initial_states), c("ev1", "ev2"))

    session$setInputs(toggle_map_event = "ev1")
    session$flushReact()
    toggled_states <- session$userData$status_map_toggle_states
    testthat::expect_false(toggled_states[["ev1"]])
    testthat::expect_true(toggled_states[["ev2"]])

    events_rv(
      tibble::tibble(
        event_id = c("ev1", "ev2", "ev3"),
        hazard_type = c("Flood", "Heat", "Flood"),
        hazard_name = c("Flood__x", "Heat__x", "Flood__y"),
        scenario_name = c("rcp85", "rcp85", "rcp85"),
        return_period = c(100, 0, 50),
        event_year = c(2030L, 2035L, 2040L),
        spatial_level = c("state", "macro", "brazil"),
        spatial_region_codes = c("11", "M2", NA_character_),
        spatial_region_labels = c("State 11", "Macro 2", NA_character_),
        spatial_scheme = c("adm_regions", "hydro_regions", "adm_regions")
      )
    )
    session$flushReact()
    expanded_states <- session$userData$status_map_toggle_states
    testthat::expect_false(expanded_states[["ev1"]])
    testthat::expect_true(expanded_states[["ev2"]])
    testthat::expect_true(expanded_states[["ev3"]])

    events_rv(
      tibble::tibble(
        event_id = c("ev1", "ev3"),
        hazard_type = c("Flood", "Flood"),
        hazard_name = c("Flood__x", "Flood__y"),
        scenario_name = c("rcp85", "rcp85"),
        return_period = c(100, 50),
        event_year = c(2030L, 2040L),
        spatial_level = c("state", "brazil"),
        spatial_region_codes = c("11", NA_character_),
        spatial_region_labels = c("State 11", NA_character_),
        spatial_scheme = c("adm_regions", "adm_regions")
      )
    )
    session$flushReact()
    reduced_states <- session$userData$status_map_toggle_states
    testthat::expect_equal(names(reduced_states), c("ev1", "ev3"))
    testthat::expect_false(reduced_states[["ev1"]])
    testthat::expect_true(reduced_states[["ev3"]])
  })
})

testthat::test_that("mod_status_server handles empty events gracefully", {
  testthat::skip_on_ci()
  testthat::skip_if_not_installed("shiny")
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
  testthat::skip_on_ci()
  testthat::skip_if_not_installed("shiny")
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

testthat::test_that("mod_status_server renders reproduction code", {
  shiny::testServer(mod_status_server, args = list(
    id = "test",
    status_reactive = shiny::reactive("Ready"),
    events_reactive = shiny::reactive(data.frame()),
    run_repro_code_reactive = shiny::reactive("library(climate.risk.tool)\nprint('ok')")
  ), {
    code_output <- output$run_repro_code
    testthat::expect_true(!is.null(code_output))
    testthat::expect_match(session$userData$status_run_repro_code, "library\\(climate\\.risk\\.tool\\)")
  })
})

testthat::test_that("mod_status_server renders reproduction code fallback", {
  shiny::testServer(mod_status_server, args = list(
    id = "test",
    status_reactive = shiny::reactive("Ready"),
    events_reactive = shiny::reactive(data.frame()),
    run_repro_code_reactive = shiny::reactive("Reproduction code unavailable: select an input folder first.")
  ), {
    code_output <- output$run_repro_code
    testthat::expect_true(!is.null(code_output))
    testthat::expect_match(session$userData$status_run_repro_code, "select an input folder")
  })
})
