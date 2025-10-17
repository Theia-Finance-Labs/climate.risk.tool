testthat::test_that("filter_hazards_by_events returns all hazards when events is not a dataframe", {
  # Create mock hazards
  hazards <- list(
    "hazard1" = terra::rast(ncols = 10, nrows = 10),
    "hazard2" = terra::rast(ncols = 10, nrows = 10),
    "hazard3" = terra::rast(ncols = 10, nrows = 10)
  )
  
  # Call with non-dataframe events
  result <- filter_hazards_by_events(hazards, events = NULL)
  
  # Should return all hazards unchanged
  expect_equal(length(result), 3)
  expect_equal(names(result), names(hazards))
})

testthat::test_that("filter_hazards_by_events handles TIF hazards with exact matching", {
  # Create mock TIF hazards
  hazards <- list(
    "flood__rcp85_h10glob" = terra::rast(ncols = 10, nrows = 10),
    "drought__rcp45_h5" = terra::rast(ncols = 10, nrows = 10),
    "heat__rcp85_h10" = terra::rast(ncols = 10, nrows = 10)
  )
  
  # Define events (only request 2 out of 3)
  events <- data.frame(
    hazard_name = c("flood__rcp85_h10glob", "heat__rcp85_h10"),
    event_year = c(2030, 2040),
    chronic = c(FALSE, FALSE)
  )
  
  # Filter
  result <- filter_hazards_by_events(hazards, events)
  
  # Should return only the 2 requested hazards
  expect_equal(length(result), 2)
  expect_true("flood__rcp85_h10glob" %in% names(result))
  expect_true("heat__rcp85_h10" %in% names(result))
  expect_false("drought__rcp45_h5" %in% names(result))
})

testthat::test_that("filter_hazards_by_events matches NC hazards with ensemble suffix", {
  # Create mock NC hazards with ensemble suffix (as they are actually loaded)
  hazards <- list(
    "Drought__CDD__GWL=present__RP=5__ensemble=mean" = terra::rast(ncols = 10, nrows = 10),
    "Heat__Frost__GWL=2__RP=10__ensemble=mean" = terra::rast(ncols = 10, nrows = 10)
  )
  
  # Define events (use the full hazard name with ensemble suffix)
  events <- data.frame(
    hazard_name = c("Drought__CDD__GWL=present__RP=5__ensemble=mean"),
    event_year = c(2030),
    chronic = c(FALSE)
  )
  
  # Filter
  result <- filter_hazards_by_events(hazards, events)
  
  # Should match the exact hazard name
  expect_equal(length(result), 1)
  expect_true("Drought__CDD__GWL=present__RP=5__ensemble=mean" %in% names(result))
  
  # Heat hazards should not be included
  expect_false(any(grepl("Heat__Frost", names(result))))
})

testthat::test_that("filter_hazards_by_events matches exact ensemble when specified", {
  # Create mock NC hazards with mean ensemble only (current implementation behavior)
  hazards <- list(
    "Compound__FWI__GWL=3__RP=10__ensemble=mean" = terra::rast(ncols = 10, nrows = 10)
  )
  
  # Define events (includes ensemble suffix - should match exactly)
  events <- data.frame(
    hazard_name = c("Compound__FWI__GWL=3__RP=10__ensemble=mean"),
    event_year = c(2050),
    chronic = c(FALSE)
  )
  
  # Filter
  result <- filter_hazards_by_events(hazards, events)
  
  # Should match exactly the specified ensemble (no expansion)
  expect_equal(length(result), 1)
  expect_true("Compound__FWI__GWL=3__RP=10__ensemble=mean" %in% names(result))
})

testthat::test_that("filter_hazards_by_events handles multiple NC events correctly", {
  # Create mock NC hazards for multiple events
  hazards <- list(
    "Drought__CDD__GWL=present__RP=5__ensemble=mean" = terra::rast(ncols = 10, nrows = 10),
    "Drought__CDD__GWL=present__RP=5__ensemble=median" = terra::rast(ncols = 10, nrows = 10),
    "Drought__CDD__GWL=present__RP=5__ensemble=p10" = terra::rast(ncols = 10, nrows = 10),
    "Drought__CDD__GWL=present__RP=5__ensemble=p90" = terra::rast(ncols = 10, nrows = 10),
    "Heat__Frost__GWL=2__RP=10__ensemble=mean" = terra::rast(ncols = 10, nrows = 10),
    "Heat__Frost__GWL=2__RP=10__ensemble=median" = terra::rast(ncols = 10, nrows = 10),
    "Heat__Frost__GWL=2__RP=10__ensemble=p10" = terra::rast(ncols = 10, nrows = 10),
    "Heat__Frost__GWL=2__RP=10__ensemble=p90" = terra::rast(ncols = 10, nrows = 10),
    "Compound__FWI__GWL=3__RP=5__ensemble=mean" = terra::rast(ncols = 10, nrows = 10),
    "Compound__FWI__GWL=3__RP=5__ensemble=median" = terra::rast(ncols = 10, nrows = 10),
    "Compound__FWI__GWL=3__RP=5__ensemble=p10" = terra::rast(ncols = 10, nrows = 10),
    "Compound__FWI__GWL=3__RP=5__ensemble=p90" = terra::rast(ncols = 10, nrows = 10)
  )
  
  # Define events (use full hazard names with ensemble suffix)
  events <- data.frame(
    hazard_name = c("Drought__CDD__GWL=present__RP=5__ensemble=mean", "Heat__Frost__GWL=2__RP=10__ensemble=mean"),
    event_year = c(2030, 2040),
    chronic = c(FALSE, FALSE)
  )
  
  # Filter
  result <- filter_hazards_by_events(hazards, events)
  
  # Should match only mean ensemble variants (current implementation behavior)
  expect_equal(length(result), 2)
  
  # Check Drought mean variant only
  expect_true("Drought__CDD__GWL=present__RP=5__ensemble=mean" %in% names(result))
  expect_false("Drought__CDD__GWL=present__RP=5__ensemble=median" %in% names(result))
  expect_false("Drought__CDD__GWL=present__RP=5__ensemble=p10" %in% names(result))
  expect_false("Drought__CDD__GWL=present__RP=5__ensemble=p90" %in% names(result))
  
  # Check Heat mean variant only
  expect_true("Heat__Frost__GWL=2__RP=10__ensemble=mean" %in% names(result))
  expect_false("Heat__Frost__GWL=2__RP=10__ensemble=median" %in% names(result))
  expect_false("Heat__Frost__GWL=2__RP=10__ensemble=p10" %in% names(result))
  expect_false("Heat__Frost__GWL=2__RP=10__ensemble=p90" %in% names(result))
  
  # Compound should not be included
  expect_false(any(grepl("Compound__FWI", names(result))))
})

testthat::test_that("filter_hazards_by_events handles mixed TIF and NC hazards", {
  # Create mock mixed hazards
  hazards <- list(
    # TIF hazards (no ensemble suffix)
    "flood__rcp85_h10glob" = terra::rast(ncols = 10, nrows = 10),
    # NC hazards (with ensemble variants)
    "Drought__CDD__GWL=present__RP=5__ensemble=mean" = terra::rast(ncols = 10, nrows = 10),
    "Drought__CDD__GWL=present__RP=5__ensemble=median" = terra::rast(ncols = 10, nrows = 10),
    "Drought__CDD__GWL=present__RP=5__ensemble=p10" = terra::rast(ncols = 10, nrows = 10),
    "Drought__CDD__GWL=present__RP=5__ensemble=p90" = terra::rast(ncols = 10, nrows = 10)
  )
  
  # Define events (mix of TIF and NC with full names)
  events <- data.frame(
    hazard_name = c("flood__rcp85_h10glob", "Drought__CDD__GWL=present__RP=5__ensemble=mean"),
    event_year = c(2030, 2040),
    chronic = c(FALSE, FALSE)
  )
  
  # Filter
  result <- filter_hazards_by_events(hazards, events)
  
  # Should return 1 TIF + 1 NC mean variant = 2 total (current implementation behavior)
  expect_equal(length(result), 2)
  
  # TIF exact match
  expect_true("flood__rcp85_h10glob" %in% names(result))
  
  # NC matches only mean ensemble variant
  expect_true("Drought__CDD__GWL=present__RP=5__ensemble=mean" %in% names(result))
  expect_false("Drought__CDD__GWL=present__RP=5__ensemble=median" %in% names(result))
  expect_false("Drought__CDD__GWL=present__RP=5__ensemble=p10" %in% names(result))
  expect_false("Drought__CDD__GWL=present__RP=5__ensemble=p90" %in% names(result))
})
