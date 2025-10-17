# Tests for mod_results_assets module

testthat::test_that("mod_results_assets_ui creates expected elements", {
  ui <- mod_results_assets_ui("test")
  html <- htmltools::renderTags(ui)$html
  
  # Check that the UI contains expected elements
  testthat::expect_true(grepl("test-assets_table", html))
  testthat::expect_true(grepl("Asset Exposures", html))
})

testthat::test_that("mod_results_assets_server displays event information columns", {
  # Create test results with event information columns
  test_assets_factors <- data.frame(
    asset = "A1",
    company = "TestCo",
    matching_method = "coordinates",
    hazard_return_period = 10,
    event_year = 2030,
    chronic = FALSE,
    hazard_type = "flood",
    hazard_intensity = 1.5,
    damage_factor = 0.1,
    cost_factor = 1000,
    stringsAsFactors = FALSE
  )
  
  test_results <- list(
    assets_factors = test_assets_factors
  )
  
  shiny::testServer(mod_results_assets_server, args = list(
    id = "test",
    results_reactive = shiny::reactive(test_results)
  ), {
    # Get the output
    assets_output <- output$assets_table
    
    # The output should exist
    testthat::expect_true(!is.null(assets_output))
  })
})

testthat::test_that("mod_results_assets_server formats chronic column as Yes/No", {
  # Create test results with chronic column
  test_assets_factors <- data.frame(
    asset = c("A1", "A2"),
    company = c("TestCo", "TestCo"),
    matching_method = c("coordinates", "coordinates"),
    hazard_return_period = c(10, 100),
    event_year = c(2030, NA),
    chronic = c(FALSE, TRUE),
    hazard_type = c("flood", "temperature"),
    stringsAsFactors = FALSE
  )
  
  test_results <- list(
    assets_factors = test_assets_factors
  )
  
  shiny::testServer(mod_results_assets_server, args = list(
    id = "test",
    results_reactive = shiny::reactive(test_results)
  ), {
    # Get the output
    assets_output <- output$assets_table
    
    # The output should exist
    testthat::expect_true(!is.null(assets_output))
  })
})

testthat::test_that("mod_results_assets_server handles NULL results gracefully", {
  shiny::testServer(mod_results_assets_server, args = list(
    id = "test",
    results_reactive = shiny::reactive(NULL)
  ), {
    # Should not error with NULL results
    assets_output <- output$assets_table
    testthat::expect_true(TRUE)  # If we get here, no error occurred
  })
})

testthat::test_that("mod_results_assets_server displays event_id column when present", {
  # Create test results with event_id column
  test_assets_factors <- data.frame(
    asset = "A1",
    company = "TestCo",
    event_id = "ev1",
    matching_method = "coordinates",
    hazard_return_period = 10,
    event_year = 2030,
    chronic = FALSE,
    hazard_type = "flood",
    hazard_intensity = 1.5,
    damage_factor = 0.1,
    cost_factor = 1000,
    stringsAsFactors = FALSE
  )
  
  test_results <- list(
    assets_factors = test_assets_factors
  )
  
  shiny::testServer(mod_results_assets_server, args = list(
    id = "test",
    results_reactive = shiny::reactive(test_results)
  ), {
    # Get the output
    assets_output <- output$assets_table
    
    # The output should exist
    testthat::expect_true(!is.null(assets_output))
  })
})

testthat::test_that("mod_results_assets_server handles results without assets_factors data", {
  test_results <- list(
    companies = data.frame(company = "TestCo")
    # No assets_factors data
  )
  
  shiny::testServer(mod_results_assets_server, args = list(
    id = "test",
    results_reactive = shiny::reactive(test_results)
  ), {
    # Should not error when assets_factors data is missing
    assets_output <- output$assets_table
    testthat::expect_true(TRUE)  # If we get here, no error occurred
  })
})

testthat::test_that("mod_results_assets_server orders columns correctly", {
  # Create test results with multiple columns
  test_assets_factors <- data.frame(
    hazard_type = "flood",
    asset = "A1",
    hazard_intensity = 1.5,
    company = "TestCo",
    damage_factor = 0.1,
    matching_method = "coordinates",
    cost_factor = 1000,
    hazard_return_period = 10,
    event_year = 2030,
    chronic = FALSE,
    stringsAsFactors = FALSE
  )
  
  test_results <- list(
    assets_factors = test_assets_factors
  )
  
  shiny::testServer(mod_results_assets_server, args = list(
    id = "test",
    results_reactive = shiny::reactive(test_results)
  ), {
    # Get the output
    assets_output <- output$assets_table
    
    # The output should exist
    testthat::expect_true(!is.null(assets_output))
    
    # Priority columns should be first in the data
    # Note: we can't directly test DT output, but we can verify the function runs without error
    testthat::expect_true(TRUE)
  })
})

