testthat::test_that("app_ui exposes upload, run, download controls", {
  ui <- app_ui(request = NULL)
  html <- htmltools::renderTags(ui)$html

  # Namespaced module IDs
  testthat::expect_true(grepl("control-company_file", html))
  testthat::expect_true(grepl("control-run_analysis", html))
  testthat::expect_true(grepl("control-download_results", html))
})


