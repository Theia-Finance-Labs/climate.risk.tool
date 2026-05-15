testthat::test_that("set_events_spatial_separation returns unchanged events when filters are NULL", {
  events <- tibble::tibble(
    event_id = c("ev1", "ev2"),
    hazard_type = c("Flood", "Heat"),
    event_year = c(2030L, 2035L)
  )

  out <- set_events_spatial_separation(events, spatial_filters = NULL)
  testthat::expect_identical(out, events)
})

testthat::test_that("set_events_spatial_separation applies shorthand level mapping", {
  events <- tibble::tibble(
    event_id = c("ev1", "ev2"),
    hazard_type = c("Flood", "Heat"),
    event_year = c(2030L, 2035L)
  )

  out <- set_events_spatial_separation(
    events = events,
    spatial_filters = list(
      ev1 = list(micro = c("R1", "R2")),
      ev2 = list(state = c("11", "33"))
    )
  )

  testthat::expect_equal(out$spatial_level[out$event_id == "ev1"], "micro")
  testthat::expect_equal(out$spatial_scheme[out$event_id == "ev1"], "hydro_regions")
  testthat::expect_equal(out$spatial_region_codes[out$event_id == "ev1"], "R1|R2")

  testthat::expect_equal(out$spatial_level[out$event_id == "ev2"], "state")
  testthat::expect_equal(out$spatial_scheme[out$event_id == "ev2"], "adm_regions")
  testthat::expect_equal(out$spatial_region_codes[out$event_id == "ev2"], "11|33")
})

testthat::test_that("set_events_spatial_separation supports explicit payload and labels", {
  events <- tibble::tibble(
    event_id = "ev1",
    hazard_type = "Heat",
    event_year = 2030L
  )

  out <- set_events_spatial_separation(
    events = events,
    spatial_filters = list(
      ev1 = list(
        level = "municipality",
        codes = c("1100023", "1200401"),
        labels = c("Ariquemes", "Rio Branco")
      )
    )
  )

  testthat::expect_equal(out$spatial_level, "municipality")
  testthat::expect_equal(out$spatial_scheme, "adm_regions")
  testthat::expect_equal(out$spatial_region_codes, "1100023|1200401")
  testthat::expect_equal(out$spatial_region_labels, "Ariquemes|Rio Branco")
})

testthat::test_that("set_events_spatial_separation errors on unknown event_id when strict", {
  events <- tibble::tibble(
    event_id = "ev1",
    hazard_type = "Heat",
    event_year = 2030L
  )

  testthat::expect_error(
    set_events_spatial_separation(
      events = events,
      spatial_filters = list(ev999 = list(state = "11")),
      strict = TRUE
    ),
    "unknown event_id"
  )
})

