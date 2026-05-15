# Tests for mod_results_assets module

testthat::test_that("mod_results_assets_ui creates expected elements", {
  ui <- mod_results_assets_ui("test")
  html <- htmltools::renderTags(ui)$html

  # Check that the UI contains expected elements
  testthat::expect_true(grepl("test-hazard_tables", html))
  testthat::expect_true(grepl("Asset Exposures", html))
})

testthat::test_that("mod_results_assets_server renders event-specific tables with metadata and empty event panels", {
  testthat::skip_on_ci()
  testthat::skip_if_not_installed("shiny")
  test_assets_factors <- data.frame(
    asset = c("A1", "A2"),
    company = c("TestCo", "TestCo"),
    event_id = c("ev1", "ev2"),
    matching_method = c("coordinates", "coordinates"),
    return_period = c(10, 10),
    event_year = c(2030, 2040),
    hazard_type = c("Flood", "Flood"),
    hazard_name = c("Flood__RP10", "Flood__RP10"),
    scenario_name = c("SSP2-4.5", "SSP2-4.5"),
    state = c("Rondonia", "Sao Paulo"),
    state_code = c("11", "35"),
    state_name = c("Rondonia", "Sao Paulo"),
    municipality = c("Ariquemes", NA_character_),
    municipality_code = c("1100023", NA_character_),
    municipality_name = c("Ariquemes", NA_character_),
    fwi = c(1.5, NA_real_),
    depth = c(NA_real_, 2.5),
    damage_factor = c(0.1, 0.2),
    cost_factor = c(1000, 2000),
    share_of_economic_activity = c(0.6, 0.4),  # A1 is 0.6 (Fire), A2 is 0.4 (Flood)
    sector = c("06", "35"),  # Keep sector column as string with leading zero
    cnae = c(6, 35),  # Add cnae column for sector metadata lookup
    stringsAsFactors = FALSE
  )

  run_events <- tibble::tibble(
    event_id = c("ev2", "ev1", "ev3"),
    hazard_type = c("Flood", "Flood", "Drought"),
    hazard_name = c("Flood__RP10", "Flood__RP10", "Drought__RP50"),
    scenario_name = c("SSP2-4.5", "SSP2-4.5", "SSP5-8.5"),
    return_period = c(10, 10, 50),
    event_year = c(2040, 2030, 2045),
    spatial_level = c("state", "municipality", "brazil"),
    spatial_region_codes = c("35|11", "3550308", NA_character_),
    spatial_region_labels = c("Sao Paulo|Rondonia", "Sao Paulo", NA_character_)
  )

  cnae_exposure <- tibble::tibble(
    cnae = c(6, 35),
    description = c("Oil and Gas Extraction", "Hydropower Generation"),
    lp_exposure = c("median", "low")
  )

  test_results <- list(
    assets_factors = test_assets_factors
  )

  shiny::testServer(mod_results_assets_server, args = list(
    id = "test",
    results_reactive = shiny::reactive(test_results),
    cnae_exposure_reactive = shiny::reactive(cnae_exposure),
    events_reactive = shiny::reactive(run_events)
  ), {
    hazard_ui <- output$hazard_tables
    testthat::expect_false(is.null(hazard_ui))
    hazard_ui_html <- htmltools::renderTags(hazard_ui)$html

    testthat::expect_true(grepl("ev2 \\| Flood \\| 2040", hazard_ui_html))
    testthat::expect_true(grepl("ev1 \\| Flood \\| 2030", hazard_ui_html))
    testthat::expect_true(grepl("ev3 \\| Drought \\| 2045", hazard_ui_html))
    testthat::expect_true(grepl("Event ID:", hazard_ui_html))
    testthat::expect_true(grepl("Spatial Separation:", hazard_ui_html))
    testthat::expect_true(grepl("States: Sao Paulo, Rondonia", hazard_ui_html))

    table_data <- session$userData$hazard_tables_data
    testthat::expect_length(table_data, 3)

    table_one <- table_data[[1]]
    table_two <- table_data[[2]]
    table_three <- table_data[[3]]

    testthat::expect_true(is.data.frame(table_one))
    testthat::expect_true(is.data.frame(table_two))
    testthat::expect_true(is.data.frame(table_three))

    # Ensure grouping is per event (ordered by run events, not hazard label)
    testthat::expect_true("event_id" %in% colnames(table_one))
    testthat::expect_true(all(unique(table_one$event_id) == "ev2"))
    testthat::expect_true(all(unique(table_two$event_id) == "ev1"))
    testthat::expect_equal(nrow(table_three), 0)
    testthat::expect_true("sector" %in% colnames(table_one))
    testthat::expect_true(all(table_one$sector == "Hydropower Generation"))
    testthat::expect_false("sector_name" %in% colnames(table_one))
    testthat::expect_true("sector_code" %in% colnames(table_one))
    testthat::expect_true(all(table_one$sector_code == "35"))
    testthat::expect_true("share_of_economic_activity" %in% colnames(table_one))
    testthat::expect_true(all(table_one$share_of_economic_activity == "40.0%"))
    testthat::expect_true(all(c("state", "state_code", "municipality", "municipality_code") %in% colnames(table_one)))
    testthat::expect_equal(table_one$state_code[1], "35")
    testthat::expect_equal(table_one$state[1], "35 - Sao Paulo")

    display_tables <- session$userData$hazard_tables_display_data
    testthat::expect_length(display_tables, 3)
    testthat::expect_false("event_id" %in% names(display_tables[[1]]))
    testthat::expect_false("hazard_name" %in% names(display_tables[[1]]))
    testthat::expect_false("hazard_type" %in% names(display_tables[[1]]))
    testthat::expect_false("scenario_name" %in% names(display_tables[[1]]))
    testthat::expect_false("event_year" %in% names(display_tables[[1]]))
    testthat::expect_false(any(c("state_code", "state_name", "municipality_code", "municipality_name") %in% names(display_tables[[1]])))
    testthat::expect_false(any(c("state_code", "state_name", "municipality_code", "municipality_name") %in% names(display_tables[[2]])))
    testthat::expect_equal(display_tables[[1]]$state[1], "35 - Sao Paulo")
    testthat::expect_equal(display_tables[[2]]$municipality[1], "1100023 - Ariquemes")

    download_data <- assets_download_data()
    testthat::expect_s3_class(download_data, "data.frame")
    testthat::expect_true("event_id" %in% colnames(download_data))
    testthat::expect_true("hazard_name" %in% colnames(download_data))
    testthat::expect_true("hazard_type" %in% colnames(download_data))
    testthat::expect_true("scenario_name" %in% colnames(download_data))
    testthat::expect_true("event_year" %in% colnames(download_data))
    testthat::expect_true("sector_name" %in% colnames(download_data))
    testthat::expect_true(all(c("state_code", "state_name", "municipality_code", "municipality_name") %in% colnames(download_data)))
    testthat::expect_setequal(
      unique(download_data$sector_name),
      c("Oil and Gas Extraction", "Hydropower Generation")
    )
  })
})

testthat::test_that("mod_results_assets_server handles NULL results gracefully", {
  testthat::skip_on_ci()
  testthat::skip_if_not_installed("shiny")
  shiny::testServer(mod_results_assets_server, args = list(
    id = "test",
    results_reactive = shiny::reactive(NULL)
  ), {
    hazard_ui <- output$hazard_tables
    testthat::expect_false(is.null(hazard_ui))
  })
})

testthat::test_that("mod_results_assets_server handles results without assets_factors data", {
  testthat::skip_on_ci()
  testthat::skip_if_not_installed("shiny")
  test_results <- list(
    companies = data.frame(company = "TestCo")
    # No assets_factors data
  )

  shiny::testServer(mod_results_assets_server, args = list(
    id = "test",
    results_reactive = shiny::reactive(test_results)
  ), {
    hazard_ui <- output$hazard_tables
    testthat::expect_false(is.null(hazard_ui))
  })
})
