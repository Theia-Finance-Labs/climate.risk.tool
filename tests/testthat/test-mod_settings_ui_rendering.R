testthat::test_that("mod_settings_ui rendering handles mappings and shocks", {
  testthat::skip_if_not_installed("shiny")
  
  # Mock config with mapping without intensity match and with shocks
  hazard_configs <- list(
    Flood = list(
      indicators = list(
        depth = list(
          file = "flood.nc",
          variable = "depth",
          index = c("gwl"),
          agg = "median"
        )
      ),
      mappings = list(
        no_intensity = list(
          file = "test.csv",
          join = list(
            on_assets = c("cnae")
            # on_indicator_intensity is missing/empty
          )
        ),
        with_intensity = list(
          file = "test2.csv",
          intensity_match = "closest",
          join = list(
            on_indicator_intensity = c("depth")
          )
        )
      ),
      shocks = list(
        revenue = list(
          equations = list(
            list(name = "test_shock", equation = "revenue * 0.9")
          )
        )
      )
    )
  )

  # Run server in a test session to get the UI output
  shiny::testServer(mod_settings_server, args = list(
    id = "settings",
    base_dir_reactive = shiny::reactive("temp"),
    hazard_configs_reactive = shiny::reactive(hazard_configs),
    inventory_reactive = shiny::reactive(NULL)
  ), {
    ui_output <- output$settings_body
    html <- paste(as.character(ui_output), collapse = "\n")
    
    # 1. Check if formulas (shocks) are displayed
    testthat::expect_true(grepl("Shocks", html))
    testthat::expect_true(grepl("test_shock", html))
    testthat::expect_true(grepl("revenue \\* 0.9", html))
    
    # 2. Check intensity match visibility
    # For 'no_intensity' mapping, intensity match dropdown should NOT be displayed
    testthat::expect_false(grepl("mapping_intensity_match__Flood__no_intensity", html))
    
    # For 'with_intensity' mapping, it SHOULD be displayed
    testthat::expect_true(grepl("mapping_intensity_match__Flood__with_intensity", html))
    
    # 3. Check alignment style (min-width: 200px)
    testthat::expect_true(grepl("min-width: 200px", html))
  })
})
