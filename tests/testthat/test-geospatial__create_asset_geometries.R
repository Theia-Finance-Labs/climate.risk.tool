# Tests for function: create_asset_geometries

# Contract:
# - create_asset_geometries(assets_df, default_buffer_size_m = 1111, output_crs = 4326)
# - Simplified function that ONLY handles coordinate-based geolocation
# - For assets WITH lat/lon: creates point geometry with buffer based on size_in_m2 or default
# - For assets WITHOUT lat/lon: raises an error (these will be handled via precomputed lookups)
# - Returns original columns + geometry (polygon) and centroid (point) columns in the specified output CRS
# - Row count preserved


testthat::test_that("create_asset_geometries handles assets with coordinates", {
  base_dir <- get_test_data_dir()
  assets <- read_assets(file.path(base_dir, "user_input"))

  # Filter to only assets with coordinates
  assets_with_coords <- assets |>
    dplyr::filter(!is.na(.data$latitude), !is.na(.data$longitude))

  out <- create_asset_geometries(assets_with_coords)

  # Should preserve row count
  testthat::expect_equal(nrow(out), nrow(assets_with_coords))

  # Should have geometry and centroid columns
  testthat::expect_true(all(c("geometry", "centroid") %in% names(out)))

  # Geometry should be polygons, centroid should be points
  testthat::expect_s3_class(out$geometry, "sfc")
  testthat::expect_s3_class(out$centroid, "sfc")

  geom_types <- unique(sf::st_geometry_type(out$geometry))
  cent_types <- unique(sf::st_geometry_type(out$centroid))
  testthat::expect_true(all(geom_types %in% c("POLYGON", "MULTIPOLYGON")))
  testthat::expect_true(all(cent_types %in% c("POINT")))
})

testthat::test_that("create_asset_geometries buffer: row uses size_in_m2 if given, default buffer otherwise", {
  base_dir <- get_test_data_dir()
  assets <- read_assets(file.path(base_dir, "user_input"))

  # Create 3 test assets: one with large size, one with small size, one with NA size
  df <- assets[1:3, , drop = FALSE]

  # Asset 1: 10,000 m²
  df$latitude[1] <- -3.0
  df$longitude[1] <- -60.0
  df$size_in_m2[1] <- 10000

  # Asset 2: 5,000 m²
  df$latitude[2] <- -3.5
  df$longitude[2] <- -60.5
  df$size_in_m2[2] <- 5000

  # Asset 3: size NA; should use default buffer
  df$latitude[3] <- -4.0
  df$longitude[3] <- -61.0
  df$size_in_m2[3] <- NA_real_

  default_buffer <- 1500 # custom default
  out <- create_asset_geometries(df, default_buffer_size_m = default_buffer, output_crs = 3857)

  areas <- as.numeric(sf::st_area(out$geometry))
  # Expected area for size-in-m2 is just size; for NA, it is pi * r^2
  expected_area_1 <- 10000
  expected_area_2 <- 5000
  expected_area_3 <- pi * default_buffer^2

  # Perform checks at the end:
  # 1. Area for asset 1 > asset 2 (due to larger size_in_m2)
  testthat::expect_gt(areas[1], areas[2])

  # 2. Asset 1 and 2: check area is close to size_in_m2 (tolerance 10%)
  testthat::expect_true(abs(areas[1] - expected_area_1) / expected_area_1 < 0.1)
  testthat::expect_true(abs(areas[2] - expected_area_2) / expected_area_2 < 0.1)

  # 3. Asset 3: check area is close to default buffer area
  testthat::expect_true(abs(areas[3] - expected_area_3) / expected_area_3 < 0.1)
})


testthat::test_that("create_asset_geometries raises error for assets without coordinates", {
  base_dir <- get_test_data_dir()
  assets <- read_assets(file.path(base_dir, "user_input"))

  # Create test asset without coordinates
  df <- assets[1, , drop = FALSE]
  df$latitude[1] <- NA_real_
  df$longitude[1] <- NA_real_
  df$municipality[1] <- "Borba"
  df$state[1] <- "Amazonas"

  # Should raise error with informative message
  testthat::expect_error(
    create_asset_geometries(df),
    regexp = "coordinates.*required|latitude.*longitude"
  )
})


testthat::test_that("create_asset_geometries respects output_crs parameter", {
  base_dir <- get_test_data_dir()
  assets <- read_assets(file.path(base_dir, "user_input"))

  # Create test asset
  df <- assets[1, , drop = FALSE]
  df$latitude[1] <- -3.0
  df$longitude[1] <- -60.0

  # Test with WGS84 (EPSG:4326)
  out_wgs84 <- create_asset_geometries(df, output_crs = 4326)
  testthat::expect_equal(sf::st_crs(out_wgs84$geometry)$epsg, 4326)

  # Test with Web Mercator (EPSG:3857)
  out_3857 <- create_asset_geometries(df, output_crs = 3857)
  testthat::expect_equal(sf::st_crs(out_3857$geometry)$epsg, 3857)
})
