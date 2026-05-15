testthat::test_that("evaluate_spatial_separation defaults to no separation when spatial columns are absent", {
  assets_with_events <- tibble::tibble(
    asset = c("A1", "A2"),
    event_id = c("ev1", "ev1"),
    hazard_type = c("Heat", "Heat"),
    latitude = c(-10.0, -11.0),
    longitude = c(-47.0, -48.0)
  )

  events <- tibble::tibble(
    event_id = "ev1",
    hazard_type = "Heat"
  )

  result <- evaluate_spatial_separation(
    assets_with_events = assets_with_events,
    events = events,
    hazard_configs = list(Heat = list(spatial_separation_scheme = "adm_regions"))
  )

  testthat::expect_true(all(result$spatial_included))
  testthat::expect_true(all(is.na(result$spatial_exposure_status)))
  testthat::expect_true(all(result$spatial_multiplier == 1))
})

testthat::test_that("ADM municipality spatial selection marks state-only assets as insufficient", {
  assets_with_events <- tibble::tibble(
    asset = c("A1", "A2"),
    event_id = c("ev1", "ev1"),
    hazard_type = c("Heat", "Heat"),
    latitude = c(NA_real_, NA_real_),
    longitude = c(NA_real_, NA_real_),
    municipality_code = c("1100023", NA_character_),
    municipality = c("Ariquemes", NA_character_),
    state_code = c("11", "11"),
    state = c("Rondonia", "Rondonia")
  )

  events <- tibble::tibble(
    event_id = "ev1",
    hazard_type = "Heat",
    spatial_scheme = "adm_regions",
    spatial_level = "municipality",
    spatial_region_codes = "1100023",
    spatial_region_labels = "Ariquemes"
  )

  hazard_configs <- list(
    Heat = list(spatial_separation_scheme = "adm_regions")
  )

  result <- evaluate_spatial_separation(
    assets_with_events = assets_with_events,
    events = events,
    hazard_configs = hazard_configs
  )

  testthat::expect_equal(result$spatial_included[result$asset == "A1"], TRUE)
  testthat::expect_true(is.na(result$spatial_exposure_status[result$asset == "A1"]))

  testthat::expect_equal(result$spatial_included[result$asset == "A2"], FALSE)
  testthat::expect_equal(
    result$spatial_exposure_status[result$asset == "A2"],
    spatial_status_insufficient()
  )
})


testthat::test_that("Flood micro spatial selection applies municipality overlap multiplier", {
  assets_with_events <- tibble::tibble(
    asset = "F1",
    event_id = "ev1",
    hazard_type = "Flood",
    latitude = NA_real_,
    longitude = NA_real_,
    municipality_code = "3304557",
    municipality = "Rio de Janeiro",
    state_code = "33",
    state = "Rio de Janeiro"
  )

  events <- tibble::tibble(
    event_id = "ev1",
    hazard_type = "Flood",
    spatial_scheme = "hydro_regions",
    spatial_level = "micro",
    spatial_region_codes = "R4",
    spatial_region_labels = "Region 4"
  )

  hazard_configs <- list(
    Flood = list(spatial_separation_scheme = "hydro_regions")
  )

  spatial_data <- list(
    adm = list(state = NULL, municipality = NULL),
    hydro = list(macro = NULL, meso = NULL, micro = NULL),
    overlaps = list(
      municipality_micro = tibble::tibble(
        source_code = "3304557",
        target_code = "R4",
        fraction = 0.75
      )
    ),
    lookup = list(state_name_to_code = c(), municipality_name_to_code = c())
  )

  result <- evaluate_spatial_separation(
    assets_with_events = assets_with_events,
    events = events,
    hazard_configs = hazard_configs,
    spatial_separation_data = spatial_data
  )

  testthat::expect_equal(result$spatial_included[1], TRUE)
  testthat::expect_equal(result$spatial_multiplier[1], 0.75)
  testthat::expect_true(is.na(result$spatial_exposure_status[1]))
})


testthat::test_that("Hydro municipality selection falls back to geometry overlap when overlap tables are missing", {
  testthat::skip_if_not_installed("sf")

  municipality_poly <- sf::st_polygon(list(rbind(
    c(0, 0), c(10, 0), c(10, 10), c(0, 10), c(0, 0)
  )))
  meso_1 <- sf::st_polygon(list(rbind(
    c(0, 0), c(3, 0), c(3, 10), c(0, 10), c(0, 0)
  )))
  meso_2 <- sf::st_polygon(list(rbind(
    c(3, 0), c(9.99, 0), c(9.99, 10), c(3, 10), c(3, 0)
  )))

  assets_with_events <- tibble::tibble(
    asset = "M1",
    event_id = "ev1",
    hazard_type = "Flood",
    latitude = NA_real_,
    longitude = NA_real_,
    municipality_code = "1100023",
    municipality = "Ariquemes",
    state_code = "11",
    state = "Rondonia"
  )

  events <- tibble::tibble(
    event_id = "ev1",
    hazard_type = "Flood",
    spatial_scheme = "hydro_regions",
    spatial_level = "meso",
    spatial_region_codes = "M1|M2",
    spatial_region_labels = "Meso 1|Meso 2"
  )

  hazard_configs <- list(
    Flood = list(spatial_separation_scheme = "hydro_regions")
  )

  spatial_data <- list(
    adm = list(
      state = sf::st_sf(
        region_code = "11",
        region_label = "Rondonia",
        geometry = sf::st_sfc(municipality_poly, crs = 4326)
      ),
      municipality = sf::st_sf(
        region_code = "1100023",
        region_label = "Ariquemes",
        geometry = sf::st_sfc(municipality_poly, crs = 4326)
      )
    ),
    hydro = list(
      macro = NULL,
      meso = sf::st_sf(
        region_code = c("M1", "M2"),
        region_label = c("Meso 1", "Meso 2"),
        geometry = sf::st_sfc(meso_1, meso_2, crs = 4326)
      ),
      micro = NULL
    ),
    overlaps = list(),
    lookup = list(state_name_to_code = c(), municipality_name_to_code = c())
  )

  warnings <- character()
  result <- withCallingHandlers(
    evaluate_spatial_separation(
      assets_with_events = assets_with_events,
      events = events,
      hazard_configs = hazard_configs,
      spatial_separation_data = spatial_data
    ),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  fallback_warnings <- warnings[grepl("Hydro overlap table .* Falling back to runtime geometry overlaps", warnings)]
  testthat::expect_equal(length(fallback_warnings), 1)
  testthat::expect_equal(result$spatial_included[1], TRUE)
  testthat::expect_gt(result$spatial_multiplier[1], 0.99)
  testthat::expect_lt(result$spatial_multiplier[1], 1)
  testthat::expect_true(is.na(result$spatial_exposure_status[1]))
})

testthat::test_that("Hydro state micro selection falls back to geometry overlap", {
  testthat::skip_if_not_installed("sf")

  state_poly <- sf::st_polygon(list(rbind(
    c(0, 0), c(2, 0), c(2, 1), c(0, 1), c(0, 0)
  )))
  micro_left <- sf::st_polygon(list(rbind(
    c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0)
  )))

  assets_with_events <- tibble::tibble(
    asset = "F2",
    event_id = "ev1",
    hazard_type = "Flood",
    latitude = NA_real_,
    longitude = NA_real_,
    municipality_code = NA_character_,
    municipality = NA_character_,
    state_code = "53",
    state = "Distrito Federal"
  )

  events <- tibble::tibble(
    event_id = "ev1",
    hazard_type = "Flood",
    spatial_scheme = "hydro_regions",
    spatial_level = "micro",
    spatial_region_codes = "R6",
    spatial_region_labels = "Region 6"
  )

  hazard_configs <- list(
    Flood = list(spatial_separation_scheme = "hydro_regions")
  )

  spatial_data <- list(
    adm = list(
      state = sf::st_sf(
        region_code = "53",
        region_label = "Distrito Federal",
        geometry = sf::st_sfc(state_poly, crs = 4326)
      ),
      municipality = NULL
    ),
    hydro = list(
      macro = NULL,
      meso = NULL,
      micro = sf::st_sf(
        region_code = "R6",
        region_label = "Region 6",
        geometry = sf::st_sfc(micro_left, crs = 4326)
      )
    ),
    overlaps = list(),
    lookup = list(state_name_to_code = c(), municipality_name_to_code = c())
  )

  warnings <- character()
  result <- withCallingHandlers(
    evaluate_spatial_separation(
      assets_with_events = assets_with_events,
      events = events,
      hazard_configs = hazard_configs,
      spatial_separation_data = spatial_data
    ),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  testthat::expect_true(any(grepl("Hydro overlap table for state -> micro is missing or incomplete", warnings)))

  testthat::expect_equal(result$spatial_included[1], TRUE)
  testthat::expect_equal(result$spatial_multiplier[1], 0.5, tolerance = 0.01)
  testthat::expect_true(is.na(result$spatial_exposure_status[1]))
})

testthat::test_that("Hydro fallback reports insufficient when source geometry is unavailable", {
  assets_with_events <- tibble::tibble(
    asset = "F2",
    event_id = "ev1",
    hazard_type = "Flood",
    latitude = NA_real_,
    longitude = NA_real_,
    municipality_code = NA_character_,
    municipality = NA_character_,
    state_code = "53",
    state = "Distrito Federal"
  )

  events <- tibble::tibble(
    event_id = "ev1",
    hazard_type = "Flood",
    spatial_scheme = "hydro_regions",
    spatial_level = "micro",
    spatial_region_codes = "R6",
    spatial_region_labels = "Region 6"
  )

  hazard_configs <- list(
    Flood = list(spatial_separation_scheme = "hydro_regions")
  )

  spatial_data <- list(
    adm = list(
      state = NULL,
      municipality = NULL
    ),
    hydro = list(macro = NULL, meso = NULL, micro = NULL),
    overlaps = list(),
    lookup = list(state_name_to_code = c(), municipality_name_to_code = c())
  )

  warnings <- character()
  result <- withCallingHandlers(
    evaluate_spatial_separation(
      assets_with_events = assets_with_events,
      events = events,
      hazard_configs = hazard_configs,
      spatial_separation_data = spatial_data
    ),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  testthat::expect_true(any(grepl("Hydro overlap table for state -> micro is missing or incomplete", warnings)))
  testthat::expect_equal(result$spatial_included[1], FALSE)
  testthat::expect_equal(result$spatial_exposure_status[1], spatial_status_insufficient())
})

testthat::test_that("repair_spatial_layer_geometries fixes invalid polygons and warns", {
  testthat::skip_if_not_installed("sf")

  invalid_poly <- sf::st_polygon(list(rbind(
    c(0, 0),
    c(1, 1),
    c(1, 0),
    c(0, 1),
    c(0, 0)
  )))
  invalid_sf <- sf::st_sf(
    region_code = "R1",
    geometry = sf::st_sfc(invalid_poly, crs = 4326)
  )

  testthat::expect_false(sf::st_is_valid(invalid_sf)[[1]])

  testthat::expect_warning(
    repaired <- climate.risk.tool:::repair_spatial_layer_geometries(
      invalid_sf,
      "test layer"
    ),
    "\\[spatial_separation\\] Repaired 1 invalid geometry in test layer\\."
  )

  testthat::expect_true(all(sf::st_is_valid(repaired$data)))
  testthat::expect_match(repaired$warnings[[1]], "Repaired 1 invalid geometry")
})

testthat::test_that("safe_spatial_region_join falls back to planar join when s2 fails", {
  testthat::skip_if_not_installed("sf")

  invalid_poly <- sf::st_polygon(list(rbind(
    c(0, 0),
    c(1, 1),
    c(1, 0),
    c(0, 1),
    c(0, 0)
  )))
  region_sf <- sf::st_sf(
    region_code = "R1",
    geometry = sf::st_sfc(invalid_poly, crs = 4326)
  )
  pts_sf <- sf::st_as_sf(
    tibble::tibble(asset = c("inside", "outside"), longitude = c(0.75, 1.5), latitude = c(0.75, 1.5)),
    coords = c("longitude", "latitude"),
    crs = 4326
  )

  testthat::expect_warning(
    joined <- climate.risk.tool:::safe_spatial_region_join(
      pts_sf = pts_sf,
      selected_layer = region_sf,
      layer_name = "test macro layer"
    ),
    "Falling back to planar spatial join for test macro layer because s2 rejected the geometry"
  )

  testthat::expect_s3_class(joined, "sf")
  testthat::expect_equal(nrow(joined), 2)
  testthat::expect_true("region_code" %in% names(joined))
})
