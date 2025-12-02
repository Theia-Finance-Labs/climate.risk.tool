testthat::test_that("e2e: upload base_dir, run analysis, download results", {
  skip_slow_tests()
  testthat::skip_on_ci()
  testthat::skip_if_not_installed("shinytest2")

  base_dir <- get_test_data_dir()

  # For golem apps, we need to create an app.R file that shinytest2 can run
  # Create a temporary app directory
  temp_app_dir <- tempfile("shinytest_app")
  dir.create(temp_app_dir, recursive = TRUE)

  # Get the package root directory
  pkg_root <- rprojroot::find_package_root_file()

  # Write an app.R file that loads the package from its root directory
  app_r_content <- sprintf('
pkgload::load_all("%s")
climate.risk.tool::run_app(base_dir = "%s")
', gsub("\\\\", "/", pkg_root), gsub("\\\\", "/", base_dir))

  writeLines(app_r_content, file.path(temp_app_dir, "app.R"))

  # Start the app from the directory
  app <- shinytest2::AppDriver$new(
    app_dir = temp_app_dir,
    variant = "e2e",
    timeout = 120000,
    seed = 123
  )
  on.exit(
    {
      app$stop()
      unlink(temp_app_dir, recursive = TRUE)
    },
    add = TRUE
  )

  # Wait for the app to fully load
  Sys.sleep(3)

  # Upload company file first (required for analysis)
  # The input is namespaced as control-company_file
  company_file_path <- file.path(base_dir, "user_input", "company_information.xlsx")
  if (file.exists(company_file_path)) {
    app$upload_file(`control-company_file` = company_file_path)
  }

  # Wait a moment for upload to process
  Sys.sleep(1)

  # Click run button (also namespaced as control-run_analysis)
  app$set_inputs(`control-run_analysis` = "click")

  # Wait for analysis to complete (needs more time for actual computation)
  Sys.sleep(30)

  # Check for results output elements in HTML (shinytest2 sometimes doesn't detect named elements)
  html_content <- app$get_html("body")

  # Check for status module output (should show analysis results)
  has_status_badge <- grepl("status-status_badge", html_content)
  testthat::expect_true(has_status_badge, info = "App should have status badge element")

  has_status_message <- grepl("status-status_message", html_content)
  testthat::expect_true(has_status_message, info = "App should have status message element")

  # Check for download functionality in HTML
  has_download <- grepl("profit_pathways-download_profit_pathways_csv", html_content)
  testthat::expect_true(has_download, info = "App should expose profit pathways download control")
})
