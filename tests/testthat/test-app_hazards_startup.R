testthat::test_that("hazards inventory is available at startup", {
  skip_if_not_installed <- function(pkg) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      testthat::skip(paste0("Package not installed: ", pkg))
    }
  }
  skip_if_not_installed("shiny")
  base_dir <- get_test_data_dir()

  # Set environment variable for testing
  Sys.setenv(CLIMATE_RISK_BASE_DIR = base_dir)

  golem::with_golem_options(
    app = {
      shiny::testServer(app_server, args = list(), {
        # Check that hazards_inventory function exists and is callable
        testthat::expect_true(is.function(control$hazards_inventory))
        inv <- control$hazards_inventory()
        testthat::expect_true(is.data.frame(inv))
        testthat::expect_gt(nrow(inv), 0)
      })
    },
    golem_opts = list()
  )
})

testthat::test_that("control module exposes hazards inventory and aggregation_factor", {
  skip_if_not_installed <- function(pkg) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      testthat::skip(paste0("Package not installed: ", pkg))
    }
  }
  skip_if_not_installed("shiny")
  base_dir <- get_test_data_dir()

  # Set environment variable for testing
  Sys.setenv(CLIMATE_RISK_BASE_DIR = base_dir)

  golem::with_golem_options(
    app = {
      shiny::testServer(app_server, args = list(), {
        # Check that hazards_inventory function exists and is callable
        testthat::expect_true(is.function(control$hazards_inventory))
        inv <- control$hazards_inventory()
        testthat::expect_true(is.data.frame(inv))
        testthat::expect_gt(nrow(inv), 0)
      })
    },
    golem_opts = list()
  )
})
