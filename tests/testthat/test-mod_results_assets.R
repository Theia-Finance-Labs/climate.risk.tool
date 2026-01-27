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
    return_period = c(50, 10),  # A1 is RP50 (Fire), A2 is RP10 (Flood)
    event_year = c(2030, 2040),
    hazard_type = c("Fire", "Flood"),  # A1 is fire, A2 is flood
    hazard_name = c("Fire__RP50", "Flood__RP10"),  # A1 is Fire, A2 is Flood
    fwi = c(1.5, NA_real_),
    depth = c(NA_real_, 2.5),
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

testthat::test_that("mod_results_assets_server uses indexing indicator for multi-indicator hazards", {
  testthat::skip_on_ci()
  testthat::skip_if_not_installed("shiny")
  
  # Simulate Fire multi-indicator hazard with 3 indicators: land_cover, fire_weather_index, days_danger_total
  # The indexing indicator should be fire_weather_index
  test_assets_factors <- data.frame(
    asset = c("A1", "A1", "A1"),
    company = c("TestCo", "TestCo", "TestCo"),
    event_id = c("fire_ev1", "fire_ev1", "fire_ev1"),
    matching_method = c("coordinates", "coordinates", "coordinates"),
    return_period = c(100, 100, 100),
    event_year = c(2030, 2030, 2030),
    hazard_type = c("Fire", "Fire", "Fire"),
    # Three different hazard_names for the same event (multi-indicator)
    hazard_name = c(
      "Fire__land_cover__return_period=0__scenario_name=present__ensemble=mean",
      "Fire__fire_weather_index__return_period=100__gwl=3__ensemble=mean",
      "Fire__days_danger_total__return_period=100__gwl=3__ensemble=mean"
    ),
    indicator_key = c(
      "land_cover__land_cover__return_period=0__scenario_name=present__ensemble=mean",
      "fire_weather_index__fwi__return_period=100__gwl=3__ensemble=mean",
      "days_danger_total__days_danger_total__return_period=100__gwl=3__ensemble=mean"
    ),
    land_cover = c(5, NA_real_, NA_real_),
    fwi = c(NA_real_, 45.2, NA_real_),
    days_danger_total = c(NA_real_, NA_real_, 120),
    damage_factor = c(0.1, 0.1, 0.1),
    share_of_economic_activity = c(0.6, 0.6, 0.6),
    stringsAsFactors = FALSE
  )
  
  test_results <- list(
    assets_factors = test_assets_factors
  )
  
  # Mock hazard configs to specify fire_weather_index as indexing indicator
  mock_hazard_configs <- list(
    Fire = list(
      index_indicator = "fire_weather_index"
    )
  )
  
  shiny::testServer(mod_results_assets_server, args = list(
    id = "test",
    results_reactive = shiny::reactive(test_results),
    hazard_configs_reactive = shiny::reactive(mock_hazard_configs)
  ), {
    # Extract unique hazards should return ONE hazard for the Fire event
    # and it should use the indexing indicator's hazard_name
    hazard_ui <- output$hazard_tables
    testthat::expect_false(is.null(hazard_ui))
    
    table_data <- session$userData$hazard_tables_data
    # Should have exactly 1 table (one per event, not one per indicator)
    testthat::expect_length(table_data, 1)
    
    table_one <- table_data[[1]]
    testthat::expect_true(is.data.frame(table_one))
    
    # The hazard label should be the indexing indicator's hazard_name
    # NOT the alphabetically first one (land_cover)
    testthat::expect_true(
      all(grepl("fire_weather_index", table_one$hazard_name, ignore.case = TRUE))
    )
  })
})
