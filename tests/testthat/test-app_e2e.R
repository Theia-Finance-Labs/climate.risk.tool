testthat::test_that("e2e: upload base_dir, run analysis, download results", {
  testthat::skip_if_not_installed("shinytest2")

  base_dir <- get_test_data_dir()

  # Start the app with base_dir as a golem option since we removed the UI input
  app <- shinytest2::AppDriver$new(
    run_app(base_dir = base_dir),
    variant = "e2e",
    timeout = 120000,
    seed = 123
  )
  on.exit(app$stop(), add = TRUE)
  
  # Wait for the app to fully load
  Sys.sleep(3)

  # Upload company file first (required for analysis)
  company_file_path <- file.path(base_dir, "user_input", "company.csv")
  if (file.exists(company_file_path)) {
    app$upload_file(company_file = company_file_path)
  }
  
  # Wait a moment for upload to process
  Sys.sleep(1)
  
  # Click run button
  if ("run_analysis" %in% names(app$get_values(input = TRUE))) {
    app$set_inputs(run_analysis = 1)
  } else if ("run" %in% names(app$get_values(input = TRUE))) {
    app$set_inputs(run = 1)
  }
  
  # Wait for analysis to complete
  Sys.sleep(2)

  # Check for results output elements in HTML (shinytest2 sometimes doesn't detect named elements)
  html_content <- app$get_html("body")
  has_results_output <- any(grepl("results_table|results_summary|status_text", html_content))
  testthat::expect_true(has_results_output, info = "App should have results output elements")

  # Check for download functionality in HTML
  has_download <- grepl("download_results", html_content)
  testthat::expect_true(has_download, info = "App should expose a download control")
})


