# Tests for state assignment in read_assets

testthat::test_that("read_assets normalizes state and municipality names", {
  base_dir <- get_test_data_dir()

  # Create test assets with accented names
  test_assets_path <- file.path(base_dir, "user_input", "asset_information.xlsx")

  # Read the actual assets (they should have normalized names after reading)
  assets <- read_assets(base_dir)

  # Check that state and municipality are character columns
  testthat::expect_true("state" %in% names(assets))
  testthat::expect_true("municipality" %in% names(assets))
  testthat::expect_type(assets$state, "character")
  testthat::expect_type(assets$municipality, "character")

  # If there are non-NA states/municipalities, they should not contain accented characters
  if (any(!is.na(assets$state))) {
    # Check that names are ASCII (no accented characters)
    state_chars <- paste(assets$state[!is.na(assets$state)], collapse = "")
    testthat::expect_true(all(charToRaw(state_chars) < 128),
      info = "State names should be ASCII (no accents)"
    )
  }

  if (any(!is.na(assets$municipality))) {
    municipality_chars <- paste(assets$municipality[!is.na(assets$municipality)], collapse = "")
    testthat::expect_true(all(charToRaw(municipality_chars) < 128),
      info = "Municipality names should be ASCII (no accents)"
    )
  }
})

testthat::test_that("read_damage_cost_factors normalizes state names", {
  base_dir <- get_test_data_dir()

  damage_factors <- read_damage_cost_factors(base_dir)

  testthat::expect_true("state" %in% names(damage_factors))

  # Check that Heat rows have normalized state names (no accents)
  compound_rows <- damage_factors |> dplyr::filter(.data$hazard_type == "Heat")

  if (nrow(compound_rows) > 0) {
    # Get non-dash states
    states_to_check <- compound_rows$state[compound_rows$state != "-" & !is.na(compound_rows$state)]

    if (length(states_to_check) > 0) {
      # All state characters should be ASCII
      state_chars <- paste(states_to_check, collapse = "")
      testthat::expect_true(all(charToRaw(state_chars) < 128),
        info = "Damage factor state names should be ASCII (no accents)"
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

testthat::test_that("assign_state_to_assets assigns state via coordinates", {
  base_dir <- get_test_data_dir()

  # Create test data with coordinates but no province
  test_assets <- data.frame(
    asset = c("A1", "A2"),
    company = c("C1", "C2"),
    latitude = c(-15.7801, -23.5505), # Brasilia, Sao Paulo
    longitude = c(-47.9292, -46.6333),
    municipality = c(NA, NA),
    state = c(NA, NA),
    asset_category = c("commercial building", "commercial building"),
    size_in_m2 = c(1000, 800),
    share_of_economic_activity = c(0.5, 0.3),
    stringsAsFactors = FALSE
  )

  # Skip if area files don't exist
  state_path <- file.path(base_dir, "areas", "state", "geoBoundaries-BRA-ADM1_simplified.geojson")
  testthat::skip_if_not(file.exists(state_path), "State boundaries file not found")

  result <- assign_state_to_assets(test_assets, base_dir)

  # Both assets should have state assigned
  testthat::expect_true(all(!is.na(result$state)))
  testthat::expect_equal(nrow(result), nrow(test_assets))
})

testthat::test_that("assign_state_to_assets assigns state via municipality names", {
  base_dir <- get_test_data_dir()

  muni_path <- file.path(base_dir, "areas", "municipality", "geoBoundaries-BRA-ADM2_simplified.geojson")
  state_path <- file.path(base_dir, "areas", "state", "geoBoundaries-BRA-ADM1_simplified.geojson")
  testthat::skip_if_not(file.exists(muni_path) && file.exists(state_path), "ADM1/ADM2 boundaries not found")

  # Pick a municipality name known to exist in ADM2 file and its province (ADM1)
  # Ariquemes is a municipality in Rondonia (ADM1)
  test_assets <- data.frame(
    asset = c("X1"),
    company = c("C1"),
    latitude = NA_real_,
    longitude = NA_real_,
    municipality = c("Ariquemes"),
    state = NA_character_,
    asset_category = c("commercial building"),
    size_in_m2 = c(1000),
    share_of_economic_activity = c(0.5),
    stringsAsFactors = FALSE
  )

  result <- suppressWarnings(assign_state_to_assets(test_assets, base_dir))

  testthat::expect_true(!is.na(result$state[1]))
  # State should be Rondonia (ASCII normalized)
  testthat::expect_equal(tolower(result$state[1]), "rondonia")
})
