testthat::test_that("app_ui exposes upload, run, download controls", {
  ui <- app_ui(request = NULL)
  html <- htmltools::renderTags(ui)$html

  # Namespaced module IDs
  testthat::expect_true(grepl("control-select_folder", html))
  testthat::expect_true(grepl("control-run_analysis", html))
  testthat::expect_true(grepl("profit_pathways-download_profit_pathways_csv", html))
})
