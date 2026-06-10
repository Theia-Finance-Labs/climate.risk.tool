testthat::test_that("build_run_repro_code creates runnable script for current selections", {
  events <- tibble::tibble(
    event_id = "ev1",
    hazard_type = "Flood",
    hazard_indicator = "flood_depth",
    hazard_name = "Flood__flood_depth__scenario_name=rcp85__return_period=100__ensemble=mean",
    scenario_name = "rcp85",
    return_period = 100,
    event_year = 2030L,
    season = NA_character_,
    spatial_level = "state",
    spatial_region_codes = "11|33",
    spatial_region_labels = "Rondonia|Rio de Janeiro",
    spatial_scheme = "adm"
  )

  base_dir <- "/tmp/base dir"
  input_folder <- "/tmp/input's folder"

  code <- build_run_repro_code(list(
    base_dir = base_dir,
    input_folder = input_folder,
    events = events,
    growth_rate = 0.02,
    discount_rate = 0.05,
    risk_free_rate = 0.03
  ))

  testthat::expect_match(code, "library\\(climate\\.risk\\.tool\\)")
  testthat::expect_match(code, "library\\(dplyr\\)")
  testthat::expect_match(code, "library\\(sf\\)")
  testthat::expect_match(
    code,
    paste0("base_dir <- ", format_r_scalar(normalizePath(base_dir, winslash = "/", mustWork = FALSE)))
  )
  testthat::expect_match(
    code,
    paste0("input_folder <- ", format_r_scalar(normalizePath(input_folder, winslash = "/", mustWork = FALSE)))
  )
  testthat::expect_match(code, "growth_rate = 0\\.02")
  testthat::expect_match(code, "discount_rate = 0\\.05")
  testthat::expect_match(code, "risk_free_rate = 0\\.03")
  testthat::expect_match(code, "event_id = c\\(\"ev1\"\\)")
  testthat::expect_match(code, "spatial_region_codes = c\\(\"11\\|33\"\\)")
  testthat::expect_match(code, "print\\(utils::head\\(results\\$companies\\)\\)", fixed = FALSE)
})

testthat::test_that("build_run_repro_code returns helpful fallback messages", {
  events <- tibble::tibble(
    event_id = "ev1",
    hazard_type = "Flood"
  )

  testthat::expect_match(
    build_run_repro_code(NULL),
    "will appear here"
  )
  testthat::expect_match(
    build_run_repro_code(list(input_folder = "/tmp/input", events = events)),
    "set a base directory"
  )
  testthat::expect_match(
    build_run_repro_code(list(base_dir = "/tmp/base", events = events)),
    "select an input folder"
  )
  testthat::expect_match(
    build_run_repro_code(list(base_dir = "/tmp/base", input_folder = "/tmp/input", events = tibble::tibble())),
    "add at least one hazard event"
  )
})
