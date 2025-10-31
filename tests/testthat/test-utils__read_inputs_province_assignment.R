# Tests for province assignment in read_assets

testthat::test_that("read_assets normalizes province and municipality names", {
  base_dir <- get_test_data_dir()

  # Create test assets with accented names
  test_assets_path <- file.path(base_dir, "user_input", "asset_information.xlsx")

  # Read the actual assets (they should have normalized names after reading)
  assets <- read_assets(base_dir)

  # Check that province and municipality are character columns
  testthat::expect_true("province" %in% names(assets))
  testthat::expect_true("municipality" %in% names(assets))
  testthat::expect_type(assets$province, "character")
  testthat::expect_type(assets$municipality, "character")

  # If there are non-NA provinces/municipalities, they should not contain accented characters
  if (any(!is.na(assets$province))) {
    # Check that names are ASCII (no accented characters)
    province_chars <- paste(assets$province[!is.na(assets$province)], collapse = "")
    testthat::expect_true(all(charToRaw(province_chars) < 128),
      info = "Province names should be ASCII (no accents)"
    )
  }

  if (any(!is.na(assets$municipality))) {
    municipality_chars <- paste(assets$municipality[!is.na(assets$municipality)], collapse = "")
    testthat::expect_true(all(charToRaw(municipality_chars) < 128),
      info = "Municipality names should be ASCII (no accents)"
    )
  }
})

testthat::test_that("read_damage_cost_factors normalizes province names", {
  base_dir <- get_test_data_dir()

  damage_factors <- read_damage_cost_factors(base_dir)

  testthat::expect_true("province" %in% names(damage_factors))

  # Check that Compound rows have normalized province names (no accents)
  compound_rows <- damage_factors |> dplyr::filter(.data$hazard_type == "Compound")

  if (nrow(compound_rows) > 0) {
    # Get non-dash provinces
    provinces_to_check <- compound_rows$province[compound_rows$province != "-" & !is.na(compound_rows$province)]

    if (length(provinces_to_check) > 0) {
      # All province characters should be ASCII
      province_chars <- paste(provinces_to_check, collapse = "")
      testthat::expect_true(all(charToRaw(province_chars) < 128),
        info = "Damage factor province names should be ASCII (no accents)"
      )
    }
  }
})

testthat::test_that("read_precomputed_hazards normalizes region names", {
  base_dir <- get_test_data_dir()

  precomputed <- read_precomputed_hazards(base_dir)

  testthat::expect_true("region" %in% names(precomputed))

  # Check that region names are normalized (no accents)
  if (any(!is.na(precomputed$region))) {
    region_chars <- paste(precomputed$region[!is.na(precomputed$region)], collapse = "")
    testthat::expect_true(all(charToRaw(region_chars) < 128),
      info = "Precomputed hazard region names should be ASCII (no accents)"
    )
  }
})

testthat::test_that("assign_province_to_assets assigns province via coordinates", {
  base_dir <- get_test_data_dir()

  # Create test data with coordinates but no province
  test_assets <- data.frame(
    asset = c("A1", "A2"),
    company = c("C1", "C2"),
    latitude = c(-15.7801, -23.5505), # Brasilia, Sao Paulo
    longitude = c(-47.9292, -46.6333),
    municipality = c(NA, NA),
    province = c(NA, NA),
    asset_category = c("commercial building", "commercial building"),
    size_in_m2 = c(1000, 800),
    share_of_economic_activity = c(0.5, 0.3),
    stringsAsFactors = FALSE
  )

  # Skip if area files don't exist
  province_path <- file.path(base_dir, "areas", "province", "geoBoundaries-BRA-ADM1_simplified.geojson")
  testthat::skip_if_not(file.exists(province_path), "Province boundaries file not found")

  result <- assign_province_to_assets(test_assets, base_dir)

  # Both assets should have province assigned
  testthat::expect_true(all(!is.na(result$province)))
  testthat::expect_equal(nrow(result), nrow(test_assets))
})

testthat::test_that("assign_province_to_assets assigns province via municipality names", {
  base_dir <- get_test_data_dir()

  muni_path <- file.path(base_dir, "areas", "municipality", "geoBoundaries-BRA-ADM2_simplified.geojson")
  prov_path <- file.path(base_dir, "areas", "province", "geoBoundaries-BRA-ADM1_simplified.geojson")
  testthat::skip_if_not(file.exists(muni_path) && file.exists(prov_path), "ADM1/ADM2 boundaries not found")

  # Pick a municipality name known to exist in ADM2 file and its province (ADM1)
  # Ariquemes is a municipality in Rondonia (ADM1)
  test_assets <- data.frame(
    asset = c("X1"),
    company = c("C1"),
    latitude = NA_real_,
    longitude = NA_real_,
    municipality = c("Ariquemes"),
    province = NA_character_,
    asset_category = c("commercial building"),
    size_in_m2 = c(1000),
    share_of_economic_activity = c(0.5),
    stringsAsFactors = FALSE
  )

  result <- suppressWarnings(assign_province_to_assets(test_assets, base_dir))

  testthat::expect_true(!is.na(result$province[1]))
  # Province should be Rondonia (ASCII normalized)
  testthat::expect_equal(tolower(result$province[1]), "rondonia")
})
