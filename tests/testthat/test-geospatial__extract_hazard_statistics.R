# Tests for function: extract_hazard_statistics

# Contract:
# - extract_hazard_statistics(assets_with_geometry, hazards)
# - Returns long format with hazard_name, hazard_type, and multiple statistic columns
# - Each row represents one asset-hazard combination with all computed statistics


testthat::test_that("extract_hazard_statistics returns long format with hazard statistics", {

  base_dir <- get_test_data_dir()
  assets <- read_assets(base_dir)
  hazards <- load_hazards(get_hazards_dir(), aggregate_factor = 16L)
  municipalities <- load_municipalities(file.path(base_dir, "areas", "municipality"))
  provinces <- load_provinces(file.path(base_dir, "areas", "province"))
  assets_geo <- geolocate_assets(assets, hazards, municipalities, provinces)

  out <- extract_hazard_statistics(assets_geo, hazards, use_exactextractr = TRUE)

  # Should have required long format columns
  required_cols <- c("asset", "hazard_name", "hazard_type", "hazard_mean", "hazard_median", "hazard_max")
  testthat::expect_true(all(required_cols %in% names(out)))

  # Should be in long format (more rows than original assets)
  testthat::expect_gte(nrow(out), nrow(assets_geo))

  # Should have numeric hazard statistics
  testthat::expect_true(is.numeric(out$hazard_mean))
  testthat::expect_true(is.numeric(out$hazard_median))
  testthat::expect_true(is.numeric(out$hazard_max))
})


testthat::test_that("extract_hazard_statistics optimizes by grouping municipality and province methods", {
  # Create a small test dataset with known geolocation methods
  base_dir <- get_test_data_dir()
  assets <- read_assets(base_dir)
  hazards <- load_hazards(get_hazards_dir(), aggregate_factor = 16L)
  municipalities <- load_municipalities(file.path(base_dir, "areas", "municipality"))
  provinces <- load_provinces(file.path(base_dir, "areas", "province"))

  # Create test data with 6 assets using different geolocation methods
  df <- assets[1:6, , drop = FALSE]

  # Assets 1-2: Same municipality (should get same hazard values)
  df$latitude[1:2] <- NA_real_
  df$longitude[1:2] <- NA_real_
  df$municipality[1:2] <- "Borba"
  df$province[1:2] <- "Amazonas"

  # Assets 3-4: Same province (should get same hazard values)
  df$latitude[3:4] <- NA_real_
  df$longitude[3:4] <- NA_real_
  df$municipality[3:4] <- NA_character_
  df$province[3:4] <- "Amazonas"

  # Assets 5-6: Different coordinates (should get different hazard values)
  df$latitude[5] <- -3.0
  df$longitude[5] <- -60.0
  df$municipality[5] <- NA_character_
  df$province[5] <- NA_character_

  df$latitude[6] <- -1.0 # Much more different coordinates
  df$longitude[6] <- -58.0
  df$municipality[6] <- NA_character_
  df$province[6] <- NA_character_

  assets_geo <- geolocate_assets(df, hazards, municipalities, provinces)

  # Verify geolocation methods are as expected
  testthat::expect_equal(assets_geo$geolocation_method[1:2], c("municipality", "municipality"))
  testthat::expect_equal(assets_geo$geolocation_method[3:4], c("province", "province"))
  testthat::expect_equal(assets_geo$geolocation_method[5:6], c("coordinates", "coordinates"))

  out <- extract_hazard_statistics(assets_geo, hazards, use_exactextractr = TRUE)

  # Test that assets with same municipality/province get same summarized means
  # Group by hazard_name and check that assets with same municipality/province have same means
  for (hazard_name in unique(out$hazard_name)) {
    hazard_subset <- out[out$hazard_name == hazard_name, ]

    # Skip if hazard_name has __mean suffix (shouldn't happen with new logic)
    if (grepl("__mean$", hazard_name)) next

    # Assets 1-2 (same municipality) should have identical mean values
    mean_1_2 <- hazard_subset$hazard_mean[hazard_subset$asset %in% df$asset[1:2]]
    testthat::expect_equal(mean_1_2[1], mean_1_2[2],
      info = paste("Municipality assets should have same", hazard_name, "mean")
    )

    # Assets 3-4 (same province) should have identical mean values
    mean_3_4 <- hazard_subset$hazard_mean[hazard_subset$asset %in% df$asset[3:4]]
    testthat::expect_equal(mean_3_4[1], mean_3_4[2],
      info = paste("Province assets should have same", hazard_name, "mean")
    )

    # Assets 5-6 (different coordinates) should tend to have different means when not NA
    mean_5_6 <- hazard_subset$hazard_mean[hazard_subset$asset %in% df$asset[5:6]]
    if (length(mean_5_6) == 2 && !is.na(mean_5_6[1]) && !is.na(mean_5_6[2])) {
      testthat::expect_false(isTRUE(all.equal(mean_5_6[1], mean_5_6[2])),
        info = paste("Coordinate assets should have different", hazard_name, "means")
      )
    }
  }
})


testthat::test_that("extract_hazard_statistics handles missing geolocation_method column", {
  base_dir <- get_test_data_dir()
  assets <- read_assets(base_dir)
  hazards <- load_hazards(get_hazards_dir(), aggregate_factor = 16L)

  # Create assets with geometry but without geolocation_method column
  assets_with_geom <- assets[1:2, , drop = FALSE]
  # Add fake geometry column
  assets_with_geom$geometry <- sf::st_sfc(
    sf::st_polygon(list(matrix(c(0, 0, 0, 1, 1, 1, 1, 0, 0, 0), ncol = 2, byrow = TRUE))),
    sf::st_polygon(list(matrix(c(0, 0, 0, 1, 1, 1, 1, 0, 0, 0), ncol = 2, byrow = TRUE))),
    crs = terra::crs(hazards[[1]])
  )

  testthat::expect_error(
    extract_hazard_statistics(assets_with_geom, hazards, use_exactextractr = TRUE),
    "Input dataframe must have a 'geolocation_method' column"
  )
})
