testthat::test_that("load_geo_code_mapping loads IBGE codes from areas mapping", {
  base_dir <- get_test_data_dir()

  mapping <- load_geo_code_mapping(base_dir)

  testthat::expect_s3_class(mapping, "data.frame")
  testthat::expect_true(all(c(
    "adm_level",
    "code",
    "state_code",
    "name",
    "name_normalized",
    "state_name",
    "state_name_normalized"
  ) %in% names(mapping)))
  testthat::expect_true(any(mapping$adm_level == "ADM1"))
  testthat::expect_true(any(mapping$adm_level == "ADM2"))

  ariq <- mapping |>
    dplyr::filter(.data$adm_level == "ADM2", .data$code == "1100023") |>
    dplyr::slice_head(n = 1)

  testthat::expect_equal(ariq$name_normalized, "Ariquemes")
  testthat::expect_equal(ariq$state_code, "11")
})

testthat::test_that("read_assets maps IBGE codes to normalized names", {
  base_dir <- get_test_data_dir()
  input_dir <- file.path(base_dir, "temp_input_codes")
  dir.create(input_dir, showWarnings = FALSE, recursive = TRUE)
  on.exit(unlink(input_dir, recursive = TRUE), add = TRUE)

  assets_df <- tibble::tibble(
    asset = c("A1", "A2"),
    company = c("C1", "C1"),
    share_of_economic_activity = c(0.5, 0.5),
    asset_category = c("commercial building", "commercial building"),
    municipality = c(1100023, NA_real_), # Ariquemes (ADM2)
    state = c(11, 35) # Rondonia + Sao Paulo
  )

  writexl::write_xlsx(
    list(asset_information = assets_df),
    path = file.path(input_dir, "asset_information.xlsx")
  )

  assets <- read_assets(input_dir)

  testthat::expect_equal(assets$municipality[1], "Ariquemes")
  testthat::expect_equal(assets$state[1], "Rondonia")
  testthat::expect_equal(assets$state[2], "Sao Paulo")
})

testthat::test_that("load_mapping_from_config attaches state_code when state column exists", {
  base_dir <- get_test_data_dir()
  hazards_dir <- file.path(base_dir, "hazards", "config")
  hazard_configs <- load_hazard_configs(hazards_dir)

  drought_mapping <- load_mapping_from_config(base_dir, hazard_configs, "Drought", "drought_sensitivity")

  testthat::expect_true("state_code" %in% names(drought_mapping))

  sao_paulo_row <- drought_mapping |>
    dplyr::filter(.data$state == "Sao Paulo") |>
    dplyr::slice_head(n = 1)
  testthat::expect_equal(sao_paulo_row$state_code, "35")
})
