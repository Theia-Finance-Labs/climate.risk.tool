# Tests for mod_results_assets module

testthat::test_that("mod_results_assets_ui creates expected elements", {
  ui <- mod_results_assets_ui("test")
  html <- htmltools::renderTags(ui)$html

  # Check that the UI contains expected elements
  testthat::expect_true(grepl("test-hazard_tables", html))
  testthat::expect_true(grepl("Asset Exposures", html))
})

testthat::test_that("mod_results_assets_server renders hazard-specific tables with CNAE descriptions", {
  testthat::skip_on_ci()
  testthat::skip_if_not_installed("shiny")
  test_assets_factors <- data.frame(
    asset = c("A1", "A2"),
    company = c("TestCo", "TestCo"),
    event_id = c("ev1", "ev2"),
    matching_method = c("coordinates", "coordinates"),
    hazard_return_period = c(50, 10),  # A1 is RP50 (Fire), A2 is RP10 (Flood)
    event_year = c(2030, 2040),
    hazard_type = c("fire", "flood"),  # A1 is fire, A2 is flood
    hazard_name = c("Fire__RP50", "Flood__RP10"),  # A1 is Fire, A2 is Flood
    hazard_intensity = c(1.5, 2.5),
    damage_factor = c(0.1, 0.2),
    cost_factor = c(1000, 2000),
    share_of_economic_activity = c(0.6, 0.4),  # A1 is 0.6 (Fire), A2 is 0.4 (Flood)
    sector = c("06", "35"),  # Keep sector column as string with leading zero
    cnae = c(6, 35),  # Add cnae column for sector metadata lookup
    stringsAsFactors = FALSE
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
    cnae_exposure_reactive = shiny::reactive(cnae_exposure)
  ), {
    hazard_ui <- output$hazard_tables
    testthat::expect_false(is.null(hazard_ui))

    table_data <- session$userData$hazard_tables_data
    testthat::expect_length(table_data, 2)

    table_one <- table_data[[1]]
    table_two <- table_data[[2]]

    testthat::expect_true(is.data.frame(table_one))
    testthat::expect_true(is.data.frame(table_two))

    # Ensure event_id column is present and data filtered per hazard
    testthat::expect_true("event_id" %in% colnames(table_one))
    testthat::expect_true(all(unique(table_one$hazard_name) == "Fire__RP50"))
    testthat::expect_true(all(unique(table_two$hazard_name) == "Flood__RP10"))
    testthat::expect_true("sector" %in% colnames(table_one))
    testthat::expect_true(all(table_one$sector == "06"))
    testthat::expect_false("sector_name" %in% colnames(table_one))
    testthat::expect_true("sector_code" %in% colnames(table_one))
    testthat::expect_true(all(table_one$sector_code == "06"))
    testthat::expect_true("share_of_economic_activity" %in% colnames(table_one))
    testthat::expect_true(all(table_one$share_of_economic_activity == "60.0%"))

    download_data <- assets_download_data()
    testthat::expect_s3_class(download_data, "data.frame")
    testthat::expect_true("sector_name" %in% colnames(download_data))
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
