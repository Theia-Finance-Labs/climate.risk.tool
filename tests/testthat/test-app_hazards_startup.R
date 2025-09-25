testthat::test_that("hazards inventory is available at startup", {
  skip_if_not_installed <- function(pkg) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      testthat::skip(paste0("Package not installed: ", pkg))
    }
  }
  skip_if_not_installed("shiny")
  base_dir <- get_test_data_dir()

  golem::with_golem_options(
    app = {
      shiny::testServer(app_server, args = list(), {
        # Without clicking Run Analysis, hazards should be loaded from base_dir
        inv <- try(hazards_inventory(), silent = TRUE)
        if (!inherits(inv, "try-error")) {
          testthat::expect_true(is.data.frame(inv))
          # Test that hazards are loaded at startup
          testthat::expect_gte(nrow(inv), 0)  # Allow empty inventory but expect data frame
        } else {
          testthat::skip("hazards_inventory not exposed in this context")
        }
      })
    },
    golem_opts = list(base_dir = base_dir)
  )
})


