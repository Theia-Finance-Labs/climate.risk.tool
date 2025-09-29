testthat::test_that("app_ui exposes upload, run, download controls", {
  ui <- app_ui(request = NULL)
  # The initial UI may be a placeholder; these expectations define the contract
  # we want to implement. Adjust selectors/ids in code to satisfy these tests.

  # Render the UI and inspect HTML
  html <- htmltools::renderTags(ui)$html

  # Expect a file input for company CSV upload
  expect_true(grepl("id=\"company_file\"|name=\"company_file\"", html),
              info = "Expect a fileInput control for company CSV upload")

  # Expect a run button
  expect_true(grepl("id=\"run_analysis\"|id=\"run\"", html),
              info = "Expect a button to trigger analysis")

  # Expect a download button/output
  expect_true(grepl("id=\"download_results\"|id=\"download\"", html),
              info = "Expect a download control for results")

})


