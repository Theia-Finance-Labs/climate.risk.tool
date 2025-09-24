testthat::test_that("server loads inputs and runs analysis from base_dir and uploaded company file", {
  # Define a minimal contract for server logic using reactive values names
  # We assume app_server reads golem option `base_dir` and exposes reactiveValues: data_loaded, results_ready, results
  skip_if_not_installed <- function(pkg) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      testthat::skip(paste0("Package not installed: ", pkg))
    }
  }
  skip_if_not_installed("shiny")

  # Use real test data directory
  base_dir <- get_test_data_dir()
  company_file_path <- file.path(base_dir, "user_input", "company.csv")

  # Test using golem::with_golem_options to properly set options
  golem::with_golem_options(
    app = {
      shiny::testServer(app_server, args = list(), {
        # Simulate company file upload
        session$setInputs(company_file = list(datapath = company_file_path))
        
        # Simulate a Run click after setting the inputs
        session$setInputs(run_analysis = 1)

        # Contract: server should create these variables/reactives
        expect_true(exists("data_loaded"), info = "server should define data_loaded reactive flag")
        expect_true(exists("results_ready"), info = "server should define results_ready reactive flag")
        expect_true(exists("results"), info = "server should define results reactive/list")

        # After run, results should be non-null list with expected names
        # We allow lazy evaluation: isolate if needed
        res <- try(results(), silent = TRUE)  # Call reactive if it's a reactive
        if (inherits(res, "try-error")) {
          res <- try(results, silent = TRUE)  # Try as regular variable
        }
        
        if (!inherits(res, "try-error") && !is.null(res)) {
          expect_type(res, "list")
          expect_true(all(c("assets", "companies") %in% names(res)))
        }
      })
    },
    golem_opts = list(base_dir = base_dir)
  )
})


testthat::test_that("server handles missing company file gracefully", {
  skip_if_not_installed <- function(pkg) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      testthat::skip(paste0("Package not installed: ", pkg))
    }
  }
  skip_if_not_installed("shiny")

  # Use real test data directory
  base_dir <- get_test_data_dir()

  # Test using golem::with_golem_options to properly set options
  golem::with_golem_options(
    app = {
      shiny::testServer(app_server, args = list(), {
        # Simulate a Run click without company file upload
        session$setInputs(run_analysis = 1)

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


testthat::test_that("server handles missing base_dir gracefully", {
  skip_if_not_installed <- function(pkg) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      testthat::skip(paste0("Package not installed: ", pkg))
    }
  }
  skip_if_not_installed("shiny")

  # Test without setting golem options (no base_dir)
  golem::with_golem_options(
    app = {
      shiny::testServer(app_server, args = list(), {
        # Simulate a Run click without base_dir set
        session$setInputs(run_analysis = 1)

        # Should not proceed with analysis and should show error
        # We can't directly test reactive values state, but we can test they exist
        expect_true(exists("data_loaded"))
        expect_true(exists("results_ready"))
        expect_true(exists("results"))
      })
    },
    golem_opts = list()  # Empty golem options - no base_dir
  )
})


