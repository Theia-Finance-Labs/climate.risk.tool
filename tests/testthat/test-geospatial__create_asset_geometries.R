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
  assets <- read_assets(base_dir)
  
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


testthat::test_that("create_asset_geometries respects size_in_m2 for buffer sizing", {
  base_dir <- get_test_data_dir()
  assets <- read_assets(base_dir)
  
  # Create two test assets with different sizes
  df <- assets[1:2, , drop = FALSE]
  df$latitude[1] <- -3.0
  df$longitude[1] <- -60.0
  df$size_in_m2[1] <- 10000  # 10,000 m²
  
  df$latitude[2] <- -3.5
  df$longitude[2] <- -60.5
  df$size_in_m2[2] <- 5000   # 5,000 m²
  
  out <- create_asset_geometries(df, output_crs = 3857)  # Use metric CRS for area comparison
  
  # Calculate areas
  area_1 <- as.numeric(sf::st_area(out$geometry[1]))
  area_2 <- as.numeric(sf::st_area(out$geometry[2]))
  
  # Larger size_in_m2 should produce larger area
  testthat::expect_gt(area_1, area_2)
  
  # Areas should be approximately equal to size_in_m2 (within 10% tolerance due to buffer approximation)
  testthat::expect_true(abs(area_1 - 10000) / 10000 < 0.1)
  testthat::expect_true(abs(area_2 - 5000) / 5000 < 0.1)
})


testthat::test_that("create_asset_geometries uses default buffer when size_in_m2 is missing", {
  base_dir <- get_test_data_dir()
  assets <- read_assets(base_dir)
  
  # Create test asset without size_in_m2
  df <- assets[1, , drop = FALSE]
  df$latitude[1] <- -3.0
  df$longitude[1] <- -60.0
  df$size_in_m2[1] <- NA_real_
  
  default_buffer <- 1500  # Use custom default
  out <- create_asset_geometries(df, default_buffer_size_m = default_buffer, output_crs = 3857)
  
  # Calculate area (should be approximately pi * buffer^2)
  area <- as.numeric(sf::st_area(out$geometry[1]))
  expected_area <- pi * default_buffer^2
  
  # Allow 10% tolerance
  testthat::expect_true(abs(area - expected_area) / expected_area < 0.1)
})


testthat::test_that("create_asset_geometries raises error for assets without coordinates", {
  base_dir <- get_test_data_dir()
  assets <- read_assets(base_dir)
  
  # Create test asset without coordinates
  df <- assets[1, , drop = FALSE]
  df$latitude[1] <- NA_real_
  df$longitude[1] <- NA_real_
  df$municipality[1] <- "Borba"
  df$province[1] <- "Amazonas"
  
  # Should raise error with informative message
  testthat::expect_error(
    create_asset_geometries(df),
    regexp = "coordinates.*required|latitude.*longitude"
  )
})


testthat::test_that("create_asset_geometries respects output_crs parameter", {
  base_dir <- get_test_data_dir()
  assets <- read_assets(base_dir)
  
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


testthat::test_that("create_asset_geometries handles mixed cases correctly", {
  base_dir <- get_test_data_dir()
  assets <- read_assets(base_dir)
  
  # Create dataset with mix of valid and invalid cases
  df <- assets[1:3, , drop = FALSE]
  
  # Asset 1: has coordinates - VALID
  df$latitude[1] <- -3.0
  df$longitude[1] <- -60.0
  df$size_in_m2[1] <- 10000
  
  # Asset 2: has coordinates - VALID
  df$latitude[2] <- -3.5
  df$longitude[2] <- -60.5
  df$size_in_m2[2] <- 5000
  
  # Asset 3: no coordinates - INVALID (should error)
  df$latitude[3] <- NA_real_
  df$longitude[3] <- NA_real_
  df$municipality[3] <- "Borba"
  
  # Should raise error because asset 3 has no coordinates
  testthat::expect_error(
    create_asset_geometries(df),
    regexp = "coordinates.*required|latitude.*longitude|asset.*3"
  )
  
  # But if we filter to only valid ones, should work
  df_valid <- df |> dplyr::filter(!is.na(.data$latitude), !is.na(.data$longitude))
  out <- create_asset_geometries(df_valid)
  testthat::expect_equal(nrow(out), 2)
  testthat::expect_true(all(c("geometry", "centroid") %in% names(out)))
})

