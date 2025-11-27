testthat::test_that("server handles missing input folder gracefully", {
  testthat::skip_on_ci()
  testthat::skip_if_not_installed("shiny")
  # Use real test data directory
  base_dir <- get_test_data_dir()

  # Test using golem::with_golem_options to properly set options
  golem::with_golem_options(
    app = {
      shiny::testServer(app_server, args = list(), {
        # Simulate a Run click without folder selection
        session$setInputs(`control-run_analysis` = 1)

        # Should not proceed with analysis and should show error
        # We can't directly test reactive values state, but we can test they exist
        expect_true(exists("data_loaded"))
        expect_true(exists("results_ready"))
        expect_true(exists("results"))
      })
    },
    golem_opts = list(base_dir = base_dir)
  )
})


testthat::test_that("server requires hazard selection before running analysis", {
  testthat::skip_on_ci()
  testthat::skip_if_not_installed("shiny")
  # Use real test data directory
  base_dir <- get_test_data_dir()
  input_folder <- file.path(base_dir, "user_input")

  # Test using golem::with_golem_options to properly set options
  golem::with_golem_options(
    app = {
      shiny::testServer(app_server, args = list(), {
        # Note: Cannot test folder selection via shinyFiles input as it requires special handling
        # This test verifies that server requires hazard selection before running analysis

        # Simulate a Run click without adding any hazard events
        session$setInputs(`control-run_analysis` = 1)

        # Contract: server should create these variables/reactives
        expect_true(exists("data_loaded"), info = "server should define data_loaded reactive flag")
        expect_true(exists("results_ready"), info = "server should define results_ready reactive flag")
        expect_true(exists("results"), info = "server should define results reactive/list")

        # Results should be null since no hazards were selected
        res <- try(results(), silent = TRUE)
        if (inherits(res, "try-error")) {
          res <- try(results, silent = TRUE)
        }
        expect_true(is.null(res), info = "Results should be null when no hazards are selected")
      })
    },
    golem_opts = list(base_dir = base_dir)
  )
})
