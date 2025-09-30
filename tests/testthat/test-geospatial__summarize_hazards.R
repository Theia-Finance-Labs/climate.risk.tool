# Tests for function 4: summarize_hazards

# Contract:
# - summarize_hazards(assets_with_hazard_cells)
# - Returns long format with hazard_name, hazard_type, hazard_intensity columns
# - Each row represents one asset-hazard combination


testthat::test_that("summarize_hazards returns long format with hazard data", {
  base_dir <- get_test_data_dir()
  assets <- read_assets(base_dir)
  hazards <- load_hazards(get_hazards_dir())
  municipalities <- load_municipalities(file.path(base_dir, "areas", "municipality"))
  provinces <- load_provinces(file.path(base_dir, "areas", "province"))
  assets_geo <- geolocate_assets(assets, hazards, municipalities, provinces)

  out_cells <- cutout_hazards(assets_geo, hazards)
  out_long <- summarize_hazards(out_cells)

  # Check that summarize_hazards returns long format with required columns
  required_cols <- c("asset", "hazard_name", "hazard_type", "hazard_intensity")
  testthat::expect_true(all(required_cols %in% names(out_long)))
  testthat::expect_true(is.character(out_long$hazard_name))
  testthat::expect_true(is.character(out_long$hazard_type))
  testthat::expect_true(is.numeric(out_long$hazard_intensity))

  # Should have more rows than original assets (long format)
  testthat::expect_gte(nrow(out_long), nrow(assets_geo))

  # Check that hazard_type is extracted correctly from hazard_name
  if (nrow(out_long) > 0) {
    testthat::expect_true(all(!is.na(out_long$hazard_type)))
    testthat::expect_true(all(nzchar(out_long$hazard_type)))
  }
})
