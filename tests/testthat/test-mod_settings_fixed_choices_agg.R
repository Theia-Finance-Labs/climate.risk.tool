testthat::test_that("mod_settings uses aggregated indicator file when base file missing", {
  testthat::skip_if_not_installed("shiny")
  testthat::skip_if_not_installed("ncdf4")

  temp_dir <- tempfile("settings_base_")
  hazards_dir <- file.path(temp_dir, "hazards", "config")
  indicators_dir <- file.path(temp_dir, "hazards", "indicators")
  dir.create(hazards_dir, recursive = TRUE)
  dir.create(indicators_dir, recursive = TRUE)

  # Create an aggregated NetCDF file with ensemble dim
  nc_path <- file.path(indicators_dir, "fwi__agg16.nc")
  lon <- ncdf4::ncdim_def("lon", "degrees_east", vals = c(0, 1))
  lat <- ncdf4::ncdim_def("lat", "degrees_north", vals = c(0, 1))
  ensemble <- ncdf4::ncdim_def("ensemble", "", vals = c(1, 2, 3, 4))
  v <- ncdf4::ncvar_def("fwi", "", dim = list(lon, lat, ensemble), missval = NA_real_)
  nc <- ncdf4::nc_create(nc_path, list(v))
  on.exit(ncdf4::nc_close(nc), add = TRUE)
  ncdf4::ncvar_put(nc, v, array(1, dim = c(2, 2, 4)))

  # Hazard config references base file (missing), should fall back to agg file
  config <- list(
    indicators = list(
      fwi = list(
        file = "fwi.nc",
        variable = "fwi",
        index = c("scenario_name"),
        fixed = list(ensemble = "mean"),
        agg = "closest"
      )
    )
  )
  yaml::write_yaml(config, file.path(hazards_dir, "Fire.yml"))

  hazard_configs <- load_hazard_configs(hazards_dir)

  shiny::testServer(mod_settings_server, args = list(
    id = "settings",
    base_dir_reactive = shiny::reactive(temp_dir),
    hazard_configs_reactive = shiny::reactive(hazard_configs),
    inventory_reactive = shiny::reactive(tibble::tibble())
  ), {
    ui_output <- output$settings_body
    html <- paste(as.character(ui_output), collapse = "\n")

    # Should map 1..4 to mean, median, p10, p90
    testthat::expect_true(grepl("mean", html))
    testthat::expect_true(grepl("median", html))
    testthat::expect_true(grepl("p10", html))
    testthat::expect_true(grepl("p90", html))
  })
})
