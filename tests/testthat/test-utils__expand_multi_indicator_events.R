# Tests for expand_multi_indicator_events

# Contract:
# - expand_multi_indicator_events(events, hazards_inventory)
# - Expands multi-indicator hazards (Fire) into multiple events (one per indicator)
# - Single-indicator hazards pass through unchanged
# - All expanded events share same event_id and event_year
# - Preserves event metadata (season, etc.)


testthat::test_that("expand_multi_indicator_events expands single Fire event into 3 indicator events", {
  base_dir <- get_test_data_dir()
  hazards_data <- load_hazards_and_inventory(get_hazards_dir(), aggregate_factor = 16L)
  hazards_inventory <- hazards_data$inventory

  # Create a single Fire event (user-selected)
  events <- tibble::tibble(
    event_id = "event_fire_1",
    hazard_type = "Fire",
    hazard_indicator = "FWI", # Primary indicator
    hazard_name = "Fire__FWI__GWL=present__RP=10",
    scenario_name = "present",
    scenario_code = "present",
    hazard_return_period = 10,
    event_year = 2030,
    season = NA_character_
  )

  expanded <- expand_multi_indicator_events(events, hazards_inventory)

  # Should expand to 3 events (one per indicator)
  testthat::expect_equal(nrow(expanded), 3)

  # All should have same event_id and event_year
  testthat::expect_equal(length(unique(expanded$event_id)), 1)
  testthat::expect_equal(expanded$event_id[1], "event_fire_1")
  testthat::expect_equal(length(unique(expanded$event_year)), 1)
  testthat::expect_equal(expanded$event_year[1], 2030)

  # Should have all three indicators
  indicators <- unique(expanded$hazard_indicator)
  testthat::expect_true("land_cover" %in% indicators)
  testthat::expect_true("FWI" %in% indicators)
  testthat::expect_true("days_danger_total" %in% indicators)
})

testthat::test_that("expand_multi_indicator_events sets correct scenario/RP for each Fire indicator", {
  base_dir <- get_test_data_dir()
  hazards_data <- load_hazards_and_inventory(get_hazards_dir(), aggregate_factor = 16L)
  hazards_inventory <- hazards_data$inventory

  # Create Fire event with user-selected scenario/RP
  events <- tibble::tibble(
    event_id = "event_fire_1",
    hazard_type = "Fire",
    hazard_indicator = "FWI",
    hazard_name = "Fire__FWI__GWL=present__RP=10",
    scenario_name = "present",
    scenario_code = "present",
    hazard_return_period = 10,
    event_year = 2030,
    season = NA_character_
  )

  expanded <- expand_multi_indicator_events(events, hazards_inventory)

  # land_cover should use static scenario/RP from inventory
  land_cover_event <- expanded |> dplyr::filter(.data$hazard_indicator == "land_cover")
  testthat::expect_equal(nrow(land_cover_event), 1)
  # Should have hazard_name from inventory for land_cover (check if "land_cover" appears in the hazard_name)
  testthat::expect_true(grepl("land_cover", land_cover_event$hazard_name[1], fixed = TRUE))

  # FWI and days_danger_total should use user-selected scenario/RP
  fwi_event <- expanded |> dplyr::filter(.data$hazard_indicator == "FWI")
  testthat::expect_equal(nrow(fwi_event), 1)
  testthat::expect_equal(fwi_event$scenario_name[1], "present")
  testthat::expect_equal(fwi_event$hazard_return_period[1], 10)

  days_event <- expanded |> dplyr::filter(.data$hazard_indicator == "days_danger_total")
  testthat::expect_equal(nrow(days_event), 1)
  testthat::expect_equal(days_event$scenario_name[1], "present")
  testthat::expect_equal(days_event$hazard_return_period[1], 10)
})

testthat::test_that("expand_multi_indicator_events passes through single-indicator hazards unchanged", {
  base_dir <- get_test_data_dir()
  hazards_data <- load_hazards_and_inventory(get_hazards_dir(), aggregate_factor = 16L)
  hazards_inventory <- hazards_data$inventory

  # Create single-indicator hazards (Flood, Compound, Drought)
  events <- tibble::tibble(
    event_id = c("event_flood_1", "event_compound_1", "event_drought_1"),
    hazard_type = c("Flood", "Compound", "Drought"),
    hazard_indicator = c("depth(cm)", "HI", "SPI3"),
    hazard_name = c("Flood__depth(cm)__GWL=RCP8.5__RP=10", "Compound__HI__GWL=present__RP=10", "Drought__SPI3__GWL=present__RP=10"),
    scenario_name = c("RCP8.5", "present", "present"),
    scenario_code = c("rcp85", "present", "present"),
    hazard_return_period = c(10, 10, 10),
    event_year = c(2030, 2030, 2030),
    season = c(NA_character_, NA_character_, "Summer")
  )

  expanded <- expand_multi_indicator_events(events, hazards_inventory)

  # Should have same number of events (no expansion)
  testthat::expect_equal(nrow(expanded), 3)

  # All should keep original event_id
  testthat::expect_true(all(events$event_id %in% expanded$event_id))
  testthat::expect_true(all(expanded$event_id %in% events$event_id))

  # Season should be preserved
  drought_event <- expanded |> dplyr::filter(.data$hazard_type == "Drought")
  testthat::expect_equal(drought_event$season[1], "Summer")
})

testthat::test_that("expand_multi_indicator_events preserves event metadata", {
  base_dir <- get_test_data_dir()
  hazards_data <- load_hazards_and_inventory(get_hazards_dir(), aggregate_factor = 16L)
  hazards_inventory <- hazards_data$inventory

  # Create Fire event with metadata
  events <- tibble::tibble(
    event_id = "event_fire_1",
    hazard_type = "Fire",
    hazard_indicator = "FWI",
    hazard_name = "Fire__FWI__GWL=present__RP=10",
    scenario_name = "present",
    scenario_code = "present",
    hazard_return_period = 10,
    event_year = 2030,
    season = "Summer" # Preserve this
  )

  expanded <- expand_multi_indicator_events(events, hazards_inventory)

  # All expanded events should have same event_year and season
  testthat::expect_true(all(expanded$event_year == 2030))
  testthat::expect_true(all(expanded$season == "Summer"))
  testthat::expect_true(all(expanded$event_id == "event_fire_1"))
})

testthat::test_that("expand_multi_indicator_events handles mixed multi and single-indicator events", {
  base_dir <- get_test_data_dir()
  hazards_data <- load_hazards_and_inventory(get_hazards_dir(), aggregate_factor = 16L)
  hazards_inventory <- hazards_data$inventory

  # Mix of Fire (multi) and Flood (single)
  events <- tibble::tibble(
    event_id = c("event_fire_1", "event_flood_1"),
    hazard_type = c("Fire", "Flood"),
    hazard_indicator = c("FWI", "depth(cm)"),
    hazard_name = c("Fire__FWI__GWL=present__RP=10", "Flood__depth(cm)__GWL=RCP8.5__RP=10"),
    scenario_name = c("present", "RCP8.5"),
    scenario_code = c("present", "rcp85"),
    hazard_return_period = c(10, 10),
    event_year = c(2030, 2030),
    season = c(NA_character_, NA_character_)
  )

  expanded <- expand_multi_indicator_events(events, hazards_inventory)

  # Should have 3 Fire events + 1 Flood = 4 total
  testthat::expect_equal(nrow(expanded), 4)

  # Fire should be expanded to 3 indicators
  fire_events <- expanded |> dplyr::filter(.data$hazard_type == "Fire")
  testthat::expect_equal(nrow(fire_events), 3)

  # Flood should remain as 1
  flood_events <- expanded |> dplyr::filter(.data$hazard_type == "Flood")
  testthat::expect_equal(nrow(flood_events), 1)
})

testthat::test_that("expand_multi_indicator_events handles empty events", {
  base_dir <- get_test_data_dir()
  hazards_data <- load_hazards_and_inventory(get_hazards_dir(), aggregate_factor = 16L)
  hazards_inventory <- hazards_data$inventory

  # Empty events
  events <- tibble::tibble(
    event_id = character(0),
    hazard_type = character(0),
    hazard_indicator = character(0),
    hazard_name = character(0),
    scenario_name = character(0),
    scenario_code = character(0),
    hazard_return_period = numeric(0),
    event_year = integer(0),
    season = character(0)
  )

  expanded <- expand_multi_indicator_events(events, hazards_inventory)

  # Should return empty
  testthat::expect_equal(nrow(expanded), 0)
})

