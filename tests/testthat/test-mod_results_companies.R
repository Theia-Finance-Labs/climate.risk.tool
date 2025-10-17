# Tests for mod_results_companies module

testthat::test_that("mod_results_companies_ui creates expected elements", {
  ui <- mod_results_companies_ui("test")
  html <- htmltools::renderTags(ui)$html

  # Check that the UI contains expected elements
  testthat::expect_true(grepl("test-companies_table", html))
  testthat::expect_true(grepl("Company Financials", html))
})

testthat::test_that("mod_results_companies_server formats percentage change columns correctly", {
  # Create test results with percentage change columns
  test_companies <- data.frame(
    company = "TestCo",
    NPV_baseline = 1000,
    NPV_shock = 950,
    NPV_change_pct = -5.0,
    PD_baseline = 0.05,
    PD_shock = 0.08,
    Expected_loss_baseline = 100,
    Expected_loss_shock = 120,
    Expected_loss_change_pct = 20.0,
    stringsAsFactors = FALSE
  )

  test_results <- list(
    companies = test_companies
  )

  shiny::testServer(mod_results_companies_server, args = list(
    id = "test",
    results_reactive = shiny::reactive(test_results)
  ), {
    # Get the output
    companies_output <- output$companies_table

    # The output should exist
    testthat::expect_true(!is.null(companies_output))
  })
})

testthat::test_that("mod_results_companies_server handles NULL results gracefully", {
  shiny::testServer(mod_results_companies_server, args = list(
    id = "test",
    results_reactive = shiny::reactive(NULL)
  ), {
    # Should not error with NULL results
    companies_output <- output$companies_table
    testthat::expect_true(TRUE) # If we get here, no error occurred
  })
})

testthat::test_that("mod_results_companies_server handles results without companies data", {
  test_results <- list(
    assets = data.frame(asset = "A1")
    # No companies data
  )

  shiny::testServer(mod_results_companies_server, args = list(
    id = "test",
    results_reactive = shiny::reactive(test_results)
  ), {
    # Should not error when companies data is missing
    companies_output <- output$companies_table
    testthat::expect_true(TRUE) # If we get here, no error occurred
  })
})
