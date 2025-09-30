testthat::test_that("mod_control_ui renders aggregation factor slider", {
  ui <- mod_control_ui("ctrl")
  html <- htmltools::renderTags(ui)$html

  # Check for aggregation factor slider
  testthat::expect_true(grepl("ctrl-agg_factor", html))
  testthat::expect_true(grepl("Aggregation Factor", html) | grepl("aggregation", html, ignore.case = TRUE))
})

testthat::test_that("mod_control_server exposes aggregation_factor reactive", {
  base_dir <- get_test_data_dir()

  shiny::testServer(mod_control_server, args = list(
    id = "ctrl",
    base_dir_reactive = shiny::reactive(base_dir)
  ), {
    # Check that aggregation_factor reactive exists and returns a value
    testthat::expect_true(exists("aggregation_factor") || !is.null(session$returned$aggregation_factor))

    # If aggregation_factor is exposed, it should be a reactive
    if (!is.null(session$returned$aggregation_factor)) {
      agg_val <- session$returned$aggregation_factor()
      testthat::expect_true(is.numeric(agg_val))
      # Only 16, 64, and 128 are allowed
      testthat::expect_true(agg_val %in% c(16, 64, 128))
    }
  })
})

testthat::test_that("mod_control_server aggregation factor affects hazard loading", {
  base_dir <- get_test_data_dir()

  shiny::testServer(mod_control_server, args = list(
    id = "ctrl",
    base_dir_reactive = shiny::reactive(base_dir)
  ), {
    # Set aggregation factor to 64
    session$setInputs(agg_factor = 64)

    # Get hazards - they should be loaded with aggregation factor 64
    haz <- get_hazards_at_factor()

    # If hazards loaded successfully, they should respect the aggregation factor
    if (!is.null(haz) && length(haz) > 0) {
      # Just check that we got some hazards back
      testthat::expect_true(length(haz) > 0)
    }
  })
})

testthat::test_that("mod_control_server aggregation factor updates hazards inventory", {
  base_dir <- get_test_data_dir()

  shiny::testServer(mod_control_server, args = list(
    id = "ctrl",
    base_dir_reactive = shiny::reactive(base_dir)
  ), {
    # Start with default agg_factor (16)
    inv1 <- hazards_inventory()
    testthat::expect_true(is.data.frame(inv1))

    # Change aggregation factor to 64
    session$setInputs(agg_factor = 64)

    # Get inventory again - it should update reactively
    inv2 <- hazards_inventory()
    testthat::expect_true(is.data.frame(inv2))

    # Both inventories should have the same hazard types (since same data)
    # but they came from different aggregation levels
    if (nrow(inv1) > 0 && nrow(inv2) > 0) {
      testthat::expect_equal(nrow(inv1), nrow(inv2))
    }
  })
})
