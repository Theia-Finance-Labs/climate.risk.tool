testthat::test_that("load_hazards_events_config defaults spatial separation for legacy files", {
  inventory_df <- tibble::tibble(
    hazard_type = "Heat",
    hazard_indicator = "heat_index",
    scenario_name = "1.5",
    return_period = 10,
    hazard_name = "Heat__heat_index__return_period=10__gwl=1.5__ensemble=mean",
    indicator_key = "heat_index__hi__return_period=10__gwl=1.5__ensemble=mean"
  )

  hazard_configs <- list(
    Heat = list(
      spatial_separation_scheme = "adm_regions",
      index_indicator = "heat_index",
      indicators = list(
        heat_index = list(index = c("gwl", "return_period"))
      )
    )
  )

  cfg_df <- tibble::tibble(
    hazard_type = "Heat",
    gwl = "1.5",
    return_period = 10,
    event_year = 2030
  )

  tmp <- tempfile(fileext = ".xlsx")
  writexl::write_xlsx(cfg_df, tmp)

  loaded <- load_hazards_events_config(tmp, hazard_configs, inventory_df)

  testthat::expect_equal(nrow(loaded), 1)
  testthat::expect_equal(loaded$spatial_level[1], "brazil")
  testthat::expect_true(is.na(loaded$spatial_region_codes[1]))
  testthat::expect_true(is.na(loaded$spatial_region_labels[1]))
  testthat::expect_equal(loaded$spatial_scheme[1], "adm_regions")
})


testthat::test_that("load_hazards_events_config preserves explicit spatial columns", {
  inventory_df <- tibble::tibble(
    hazard_type = "Flood",
    hazard_indicator = "flood_depth",
    scenario_name = "rcp85",
    return_period = 100,
    hazard_name = "Flood__flood_depth__return_period=100__scenario_name=rcp85__ensemble=mean",
    indicator_key = "flood_depth__flood_depth_cm__return_period=100__scenario_name=rcp85__ensemble=mean"
  )

  hazard_configs <- list(
    Flood = list(
      spatial_separation_scheme = "hydro_regions",
      index_indicator = "flood_depth",
      indicators = list(
        flood_depth = list(index = c("scenario_name", "return_period"))
      )
    )
  )

  cfg_df <- tibble::tibble(
    hazard_type = "Flood",
    scenario_name = "rcp85",
    return_period = 100,
    event_year = 2035,
    spatial_level = "micro",
    spatial_region_codes = "10316084|11151279",
    spatial_region_labels = "Mearim|Medio Grande",
    spatial_scheme = "hydro_regions"
  )

  tmp <- tempfile(fileext = ".xlsx")
  writexl::write_xlsx(cfg_df, tmp)

  loaded <- load_hazards_events_config(tmp, hazard_configs, inventory_df)

  testthat::expect_equal(nrow(loaded), 1)
  testthat::expect_equal(loaded$spatial_level[1], "micro")
  testthat::expect_equal(loaded$spatial_region_codes[1], "10316084|11151279")
  testthat::expect_equal(loaded$spatial_region_labels[1], "Mearim|Medio Grande")
  testthat::expect_equal(loaded$spatial_scheme[1], "hydro_regions")
})
