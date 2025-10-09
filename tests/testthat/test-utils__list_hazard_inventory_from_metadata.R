# Test: list_hazard_inventory_from_metadata

test_that("list_hazard_inventory_from_metadata creates inventory from metadata", {
  # Create sample metadata
  metadata <- tibble::tibble(
    hazard_file = c("global_pc_h10glob.tif", "global_pc_h100glob.tif"),
    hazard_type = c("flood", "flood"),
    scenario_code = c("pc", "pc"),
    scenario_name = c("CurrentClimate", "CurrentClimate"),
    hazard_return_period = c(10, 100)
  )
  
  inventory <- list_hazard_inventory_from_metadata(metadata)
  
  # Should return a tibble with expected columns
  expect_true(tibble::is_tibble(inventory))
  expect_true("hazard_type" %in% names(inventory))
  expect_true("scenario_name" %in% names(inventory))
  expect_true("hazard_return_period" %in% names(inventory))
  expect_true("hazard_name" %in% names(inventory))
})

test_that("list_hazard_inventory_from_metadata creates unique hazard names", {
  metadata <- tibble::tibble(
    hazard_file = c("global_pc_h10glob.tif", "global_rcp85_h100glob.tif"),
    hazard_type = c("flood", "flood"),
    scenario_code = c("pc", "rcp85"),
    scenario_name = c("CurrentClimate", "RCP8.5"),
    hazard_return_period = c(10, 100)
  )
  
  inventory <- list_hazard_inventory_from_metadata(metadata)
  
  # Hazard names should be unique
  expect_equal(length(unique(inventory$hazard_name)), nrow(inventory))
  
  # Hazard names should incorporate scenario and return period
  expect_true(all(nzchar(inventory$hazard_name)))
})

test_that("list_hazard_inventory_from_metadata preserves all metadata columns", {
  metadata <- tibble::tibble(
    hazard_file = c("global_pc_h10glob.tif"),
    hazard_type = c("flood"),
    scenario_code = c("pc"),
    scenario_name = c("CurrentClimate"),
    hazard_return_period = c(10)
  )
  
  inventory <- list_hazard_inventory_from_metadata(metadata)
  
  # Should preserve key filtering columns
  expect_true("hazard_type" %in% names(inventory))
  expect_true("scenario_name" %in% names(inventory))
  expect_true("hazard_return_period" %in% names(inventory))
  expect_equal(inventory$hazard_type[1], "flood")
  expect_equal(inventory$scenario_name[1], "CurrentClimate")
  expect_equal(inventory$hazard_return_period[1], 10)
})

test_that("list_hazard_inventory_from_metadata creates consistent naming", {
  metadata <- tibble::tibble(
    hazard_file = c("global_pc_h10glob.tif", "global_pc_h100glob.tif", "global_rcp85_h10glob.tif"),
    hazard_type = c("flood", "flood", "flood"),
    scenario_code = c("pc", "pc", "rcp85"),
    scenario_name = c("CurrentClimate", "CurrentClimate", "RCP8.5"),
    hazard_return_period = c(10, 100, 10)
  )
  
  inventory <- list_hazard_inventory_from_metadata(metadata)
  
  # Names should follow pattern: hazard_type__scenario_code_hXXXglob
  expect_true(all(grepl("flood__", inventory$hazard_name)))
  expect_true(any(grepl("pc_h10glob", inventory$hazard_name)))
  expect_true(any(grepl("pc_h100glob", inventory$hazard_name)))
  expect_true(any(grepl("rcp85_h10glob", inventory$hazard_name)))
})

test_that("list_hazard_inventory_from_metadata handles multiple hazard types", {
  metadata <- tibble::tibble(
    hazard_file = c("flood1.tif", "heat1.tif"),
    hazard_type = c("flood", "heat"),
    scenario_code = c("pc", "pc"),
    scenario_name = c("CurrentClimate", "CurrentClimate"),
    hazard_return_period = c(10, 10)
  )
  
  inventory <- list_hazard_inventory_from_metadata(metadata)
  
  # Should have both hazard types
  expect_equal(length(unique(inventory$hazard_type)), 2)
  expect_true("flood" %in% inventory$hazard_type)
  expect_true("heat" %in% inventory$hazard_type)
})

