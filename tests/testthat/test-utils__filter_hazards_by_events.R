testthat::test_that("filter_hazards_by_events handles NetCDF hazards with exact matching", {
  # Create mock NetCDF hazards
  hazards <- list(
    "Drought__spi3__GWL=present__RP=5__ensemble=mean" = terra::rast(ncols = 10, nrows = 10),
    "Heat__hi__GWL=2__RP=10__ensemble=mean" = terra::rast(ncols = 10, nrows = 10),
    "Flood__depth__GWL=present__RP=100__ensemble=mean" = terra::rast(ncols = 10, nrows = 10)
  )

  # Define events (only request 2 out of 3)
  events <- data.frame(
    hazard_name = c("Drought__spi3__GWL=present__RP=5__ensemble=mean", "Heat__hi__GWL=2__RP=10__ensemble=mean"),
    event_year = c(2030, 2040)
  )

  # Filter
  result <- filter_hazards_by_events(hazards, events)

  # Should return only the 2 requested hazards
  expect_equal(length(result), 2)
  expect_true("Drought__spi3__GWL=present__RP=5__ensemble=mean" %in% names(result))
  expect_true("Heat__hi__GWL=2__RP=10__ensemble=mean" %in% names(result))
  expect_false("Flood__depth__GWL=present__RP=100__ensemble=mean" %in% names(result))
})

testthat::test_that("filter_hazards_by_events matches NC hazards with ensemble suffix", {
  # Create mock NC hazards with ensemble suffix (as they are actually loaded)
  hazards <- list(
    "Drought__spi3__GWL=present__RP=5__ensemble=mean" = terra::rast(ncols = 10, nrows = 10),
    "Heat__Frost__GWL=2__RP=10__ensemble=mean" = terra::rast(ncols = 10, nrows = 10)
  )

  # Define events (use the full hazard name with ensemble suffix)
  events <- data.frame(
    hazard_name = c("Drought__spi3__GWL=present__RP=5__ensemble=mean"),
    event_year = c(2030)
  )

  # Filter
  result <- filter_hazards_by_events(hazards, events)

  # Should match the exact hazard name
  expect_equal(length(result), 1)
  expect_true("Drought__spi3__GWL=present__RP=5__ensemble=mean" %in% names(result))

  # Heat hazards should not be included
  expect_false(any(grepl("Heat__Frost", names(result))))
})

testthat::test_that("filter_hazards_by_events matches exact ensemble when specified", {
  # Create mock NC hazards with mean ensemble only (current implementation behavior)
  hazards <- list(
    "Compound__fwi__GWL=3__RP=10__ensemble=mean" = terra::rast(ncols = 10, nrows = 10)
  )

  # Define events (includes ensemble suffix - should match exactly)
  events <- data.frame(
    hazard_name = c("Compound__fwi__GWL=3__RP=10__ensemble=mean"),
    event_year = c(2050)
  )

  # Filter
  result <- filter_hazards_by_events(hazards, events)

  # Should match exactly the specified ensemble (no expansion)
  expect_equal(length(result), 1)
  expect_true("Compound__fwi__GWL=3__RP=10__ensemble=mean" %in% names(result))
})

testthat::test_that("filter_hazards_by_events handles multiple NC events correctly", {
  # Create mock NC hazards for multiple events (only mean ensemble loaded)
  hazards <- list(
    "Drought__spi3__GWL=present__RP=5__ensemble=mean" = terra::rast(ncols = 10, nrows = 10),
    "Heat__Frost__GWL=2__RP=10__ensemble=mean" = terra::rast(ncols = 10, nrows = 10),
    "Compound__fwi__GWL=3__RP=5__ensemble=mean" = terra::rast(ncols = 10, nrows = 10)
  )

  # Define events (use full hazard names with ensemble suffix)
  events <- data.frame(
    hazard_name = c("Drought__spi3__GWL=present__RP=5__ensemble=mean", "Heat__Frost__GWL=2__RP=10__ensemble=mean"),
    event_year = c(2030, 2040)
  )

  # Filter
  result <- filter_hazards_by_events(hazards, events)

  # Should match only mean ensemble variants (current implementation behavior)
  expect_equal(length(result), 2)

  # Check Drought mean variant
  expect_true("Drought__spi3__GWL=present__RP=5__ensemble=mean" %in% names(result))

  # Check Heat mean variant
  expect_true("Heat__Frost__GWL=2__RP=10__ensemble=mean" %in% names(result))

  # Compound should not be included
  expect_false(any(grepl("Compound__fwi", names(result))))
})

testthat::test_that("filter_hazards_by_events maps requested ensemble=median to loaded ensemble=mean", {
  hazards <- list(
    "Heat__hi__GWL=2__RP=10__ensemble=mean" = terra::rast(ncols = 10, nrows = 10),
    "Drought__spi3__GWL=present__RP=5__ensemble=mean" = terra::rast(ncols = 10, nrows = 10)
  )

  # Event requests median, but only mean is loaded in hazards
  events <- data.frame(
    hazard_name = c("Heat__hi__GWL=2__RP=10__ensemble=median"),
    event_year = c(2030)
  )

  result <- filter_hazards_by_events(hazards, events)

  expect_equal(length(result), 1)
  expect_true("Heat__hi__GWL=2__RP=10__ensemble=mean" %in% names(result))
})

testthat::test_that("filter_hazards_by_events maps missing ensemble suffix to loaded ensemble=mean", {
  hazards <- list(
    "Flood__depth__GWL=present__RP=100__ensemble=mean" = terra::rast(ncols = 10, nrows = 10),
    "Heat__hi__GWL=present__RP=10__ensemble=mean" = terra::rast(ncols = 10, nrows = 10)
  )

  # Events omit ensemble; hazards are loaded with __ensemble=mean
  events <- data.frame(
    hazard_name = c("Flood__depth__GWL=present__RP=100"),
    event_year = c(2030)
  )

  result <- filter_hazards_by_events(hazards, events)

  expect_equal(length(result), 1)
  expect_true("Flood__depth__GWL=present__RP=100__ensemble=mean" %in% names(result))
})
