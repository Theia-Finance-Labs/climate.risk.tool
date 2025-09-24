# Tests for function 4: summarize_hazards

# Contract:
# - summarize_hazards(assets_with_hazard_cells)
# - Returns one mean column per hazard raster, numeric, length equals nrow(assets)


testthat::test_that("summarize_hazards returns one numeric mean per hazard column", {
  base_dir <- get_test_data_dir()
  res <- read_inputs(base_dir)
  hazards <- load_hazards(get_hazards_dir())
  municipalities <- load_municipalities(file.path(base_dir, "areas", "municipality"))
  provinces <- load_provinces(file.path(base_dir, "areas", "province"))
  assets_geo <- geolocate_assets(res$assets, hazards, municipalities, provinces)

  out_cells <- cutout_hazards(assets_geo, hazards)
  out_mean <- summarize_hazards(out_cells)

  # Check that summarize_hazards adds mean columns for each hazard
  added <- setdiff(names(out_mean), names(out_cells))
  testthat::expect_gt(length(added), 0)
  testthat::expect_true(all(vapply(out_mean[added], is.numeric, logical(1))))
  testthat::expect_equal(nrow(out_mean), nrow(assets_geo))
})
