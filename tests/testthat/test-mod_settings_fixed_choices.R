testthat::test_that("mod_settings uses indicator file dimensions for fixed choices", {
  testthat::skip_if_not_installed("shiny")
  testthat::skip_if_not_installed("ncdf4")

  temp_dir <- tempfile("settings_base_")
  hazards_dir <- file.path(temp_dir, "hazards", "config")
  indicators_dir <- file.path(temp_dir, "hazards", "indicators")
  dir.create(hazards_dir, recursive = TRUE)
  dir.create(indicators_dir, recursive = TRUE)

  # Create a small NetCDF file with an ensemble dimension
  nc_path <- file.path(indicators_dir, "hi.nc")
  lon <- ncdf4::ncdim_def("lon", "degrees_east", vals = c(0, 1))
  lat <- ncdf4::ncdim_def("lat", "degrees_north", vals = c(0, 1))
  ensemble <- ncdf4::ncdim_def("ensemble", "", vals = c(1, 2))
  v <- ncdf4::ncvar_def("hi", "", dim = list(lon, lat, ensemble), missval = NA_real_)
  nc <- ncdf4::nc_create(nc_path, list(v))
  on.exit(ncdf4::nc_close(nc), add = TRUE)
  ncdf4::ncvar_put(nc, v, array(1, dim = c(2, 2, 2)))

  # Hazard config referencing the NetCDF file
  config <- list(
    indicators = list(
      hi = list(
        file = "hi.nc",
        variable = "hi",
        index = c("scenario_name"),
        fixed = list(ensemble = "mean"),
        agg = "closest"
      )
    )
  )
  yaml::write_yaml(config, file.path(hazards_dir, "Heat.yml"))

  hazard_configs <- load_hazard_configs(hazards_dir)

  shiny::testServer(mod_settings_server, args = list(
    id = "settings",
    base_dir_reactive = shiny::reactive(temp_dir),
    hazard_configs_reactive = shiny::reactive(hazard_configs),
    inventory_reactive = shiny::reactive(tibble::tibble())
  ), {
    ui_output <- output$settings_body
    html <- paste(as.character(ui_output), collapse = "\n")

    # The fixed ensemble dropdown should include both mean and median
    testthat::expect_true(grepl("mean", html))
    testthat::expect_true(grepl("median", html))
  })
})
