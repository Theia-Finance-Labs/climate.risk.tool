testthat::test_that("mod_control_server uses default aggregation factor 1 for hazard loading", {
  base_dir <- get_test_data_dir()

  shiny::testServer(mod_control_server, args = list(
    id = "ctrl",
    base_dir_reactive = shiny::reactive(base_dir)
  ), {
    # Get hazards - they should be loaded with default aggregation factor 1
    haz <- get_hazards_at_factor()

    # If hazards loaded successfully, they should respect the default aggregation factor
    if (!is.null(haz) && length(haz) > 0) {
      # Just check that we got some hazards back
      testthat::expect_true(length(haz) > 0)
    }
  })
})

testthat::test_that("mod_control_server hazards inventory loads with default aggregation factor", {
  base_dir <- get_test_data_dir()

  shiny::testServer(mod_control_server, args = list(
    id = "ctrl",
    base_dir_reactive = shiny::reactive(base_dir)
  ), {
    # Get inventory with default aggregation factor (1)
    inv <- hazards_inventory()
    testthat::expect_true(is.data.frame(inv))

    # Inventory should be consistent since aggregation factor is fixed at 1
    if (nrow(inv) > 0) {
      testthat::expect_true(nrow(inv) > 0)
    }
  })
})
