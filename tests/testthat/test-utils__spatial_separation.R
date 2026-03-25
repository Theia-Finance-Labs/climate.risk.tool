testthat::test_that("apply_spatial_separation handles municipality selection and state-only fallback", {
  square <- function(xmin, ymin, xmax, ymax) {
    sf::st_polygon(list(matrix(c(
      xmin, ymin,
      xmax, ymin,
      xmax, ymax,
      xmin, ymax,
      xmin, ymin
    ), byrow = TRUE, ncol = 2)))
  }

  adm1 <- sf::st_sf(
    shapeID = c("s1", "s2"),
    geometry = sf::st_sfc(
      square(0, 0, 1, 1),
      square(1, 0, 2, 1)
    ),
    crs = 4326
  )

  adm2 <- sf::st_sf(
    shapeID = c("m1", "m2"),
    geometry = sf::st_sfc(
      square(0, 0, 1, 1),
      square(1, 0, 2, 1)
    ),
    crs = 4326
  )

  adm_codes <- tibble::tibble(
    code = c("01", "02", "0100001", "0200001"),
    name = c("State One", "State Two", "Municipality One", "Municipality Two"),
    adm = c("adm1", "adm1", "adm2", "adm2"),
    shapeID = c("s1", "s2", "m1", "m2")
  )

  assets_with_events <- tibble::tibble(
    asset = c("A_coord_in", "A_coord_out", "A_state_only", "A_muni_match", "A_flood"),
    company = "C1",
    latitude = c(0.5, 1.5, NA, NA, 1.5),
    longitude = c(0.5, 1.5, NA, NA, 1.5),
    state_code = c(NA, NA, "01", "01", NA),
    municipality_code = c(NA, NA, NA, "0100001", NA),
    state = c("State One", "State Two", "State One", "State One", "State Two"),
    municipality = c(NA, NA, NA, "Municipality One", NA),
    hazard_type = c("Fire", "Fire", "Fire", "Fire", "Flood"),
    hazard_name = c("FireA", "FireA", "FireA", "FireA", "FloodA"),
    event_id = c("ev1", "ev1", "ev1", "ev1", "ev2")
  )

  split <- apply_spatial_separation(
    assets_with_events = assets_with_events,
    spatial_separation = list(
      enabled = TRUE,
      level = "municipality",
      selected_codes = "0100001",
      hazard_types = c("Heat", "Drought", "Fire")
    ),
    adm1_boundaries = adm1,
    adm2_boundaries = adm2,
    adm_codes = adm_codes
  )

  exposed_assets <- unique(split$exposed$asset)
  status_assets <- unique(split$status$asset)

  testthat::expect_true("A_coord_in" %in% exposed_assets)
  testthat::expect_true("A_muni_match" %in% exposed_assets)
  testthat::expect_true("A_flood" %in% exposed_assets) # non-targeted hazard remains unaffected

  testthat::expect_true("A_coord_out" %in% status_assets)
  testthat::expect_true("A_state_only" %in% status_assets)

  insufficient_msg <- split$status |>
    dplyr::filter(.data$asset == "A_state_only") |>
    dplyr::pull(.data$spatial_status)

  testthat::expect_equal(
    insufficient_msg,
    "Insufficient location data available. Less granular spatial separation necessary"
  )
})


testthat::test_that("apply_spatial_separation handles state code matching for non-coordinate assets", {
  adm1 <- sf::st_sf(
    shapeID = "s1",
    geometry = sf::st_sfc(sf::st_polygon(list(matrix(c(
      0, 0,
      1, 0,
      1, 1,
      0, 1,
      0, 0
    ), byrow = TRUE, ncol = 2))), crs = 4326)
  )

  adm2 <- sf::st_sf(
    shapeID = "m1",
    geometry = sf::st_sfc(sf::st_polygon(list(matrix(c(
      0, 0,
      1, 0,
      1, 1,
      0, 1,
      0, 0
    ), byrow = TRUE, ncol = 2))), crs = 4326)
  )

  adm_codes <- tibble::tibble(
    code = c("01", "0100001"),
    name = c("State One", "Municipality One"),
    adm = c("adm1", "adm2"),
    shapeID = c("s1", "m1")
  )

  assets_with_events <- tibble::tibble(
    asset = c("A_state_match", "A_state_out"),
    company = "C1",
    latitude = NA_real_,
    longitude = NA_real_,
    state_code = c("01", "02"),
    municipality_code = NA_character_,
    hazard_type = "Fire",
    hazard_name = "FireA",
    event_id = "ev1"
  )

  split <- apply_spatial_separation(
    assets_with_events = assets_with_events,
    spatial_separation = list(
      enabled = TRUE,
      level = "state",
      selected_codes = "01",
      hazard_types = c("Heat", "Drought", "Fire")
    ),
    adm1_boundaries = adm1,
    adm2_boundaries = adm2,
    adm_codes = adm_codes
  )

  testthat::expect_true("A_state_match" %in% split$exposed$asset)
  out_row <- split$status |> dplyr::filter(.data$asset == "A_state_out")
  testthat::expect_equal(out_row$spatial_status[[1]], "Not exposed to selected hazard event")
})
