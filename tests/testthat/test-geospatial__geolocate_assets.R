# Tests for function: geolocate_assets

# Contract:
# - geolocate_assets(assets_df, municipalities_areas, provinces_areas)
# - Priority: if latitude/longitude present -> use point -> polygon lookup
#   else if municipality present -> match within ADM2 names
#   else if province present -> match within ADM1 names
# - Returns original columns + geometry (polygon) and centroid (POINT) columns in WGS84 (EPSG:4326)
# - Row count preserved


testthat::test_that("geolocate_assets adds geometry and centroid, preserves rows", {
  base_dir <- get_test_data_dir()
  assets <- read_assets(base_dir)
  hazards <- load_hazards(get_hazards_dir(), aggregate_factor = 16L)
  municipalities <- load_municipalities(file.path(base_dir, "areas", "municipality"))
  provinces <- load_provinces(file.path(base_dir, "areas", "province"))
  output_crs <- terra::crs(hazards[[1]])
  out <- geolocate_assets(assets, municipalities, provinces, output_crs = output_crs)

  testthat::expect_equal(nrow(out), nrow(assets))
  testthat::expect_true(all(c("geometry", "centroid", "geolocation_method") %in% names(out)))
})


testthat::test_that("geolocate_assets uses geoloc > municipality > province priority", {
  base_dir <- get_test_data_dir()
  assets <- read_assets(base_dir)
  hazards <- load_hazards(get_hazards_dir(), aggregate_factor = 16L)
  municipalities <- load_municipalities(file.path(base_dir, "areas", "municipality"))
  provinces <- load_provinces(file.path(base_dir, "areas", "province"))

  # Craft five records with different priority scenarios
  df <- assets[1:5, , drop = FALSE]

  # Row 1: has lat/long and municipality/province - should use lat/long (priority 1)
  df$latitude[1] <- -3.0
  df$longitude[1] <- -60.0
  df$municipality[1] <- "Borba"
  df$province[1] <- "Amazonas"
  df$size_in_m2[1] <- 10000 # 10,000 m² for testing size-based buffer

  # Row 2: no lat/long, has municipality that exists in test data - should use municipality (priority 2)
  df$latitude[2] <- NA_real_
  df$longitude[2] <- NA_real_
  df$municipality[2] <- "Borba"
  df$province[2] <- "Amazonas"

  # Row 3: no lat/long, no municipality, only province - should use province (priority 3)
  df$latitude[3] <- NA_real_
  df$longitude[3] <- NA_real_
  df$municipality[3] <- NA_character_
  df$province[3] <- "Amazonas"

  # Row 4: no lat/long, has municipality and province - should use municipality (priority 2)
  df$latitude[4] <- NA_real_
  df$longitude[4] <- NA_real_
  df$municipality[4] <- "Borba"
  df$province[4] <- "Amazonas"

  # Row 5: has lat/long, municipality, and province - should use lat/long (priority 1)
  df$latitude[5] <- -2.5
  df$longitude[5] <- -59.5
  df$municipality[5] <- "Borba"
  df$province[5] <- "Amazonas"
  df$size_in_m2[5] <- 5000 # 5,000 m² for testing different size
  output_crs <- terra::crs(hazards[[1]])
  out <- geolocate_assets(df, municipalities, provinces, output_crs = output_crs)

  testthat::expect_true(all(c("geometry", "centroid", "geolocation_method") %in% names(out)))
  testthat::expect_false(any(is.na(out$centroid)))

  # Test that methods are correctly assigned based on priority
  testthat::expect_equal(out$geolocation_method[1], "coordinates") # Row 1: has lat/lon
  testthat::expect_equal(out$geolocation_method[2], "municipality") # Row 2: no lat/lon, has municipality
  testthat::expect_equal(out$geolocation_method[3], "province") # Row 3: no lat/lon, no municipality, has province
  testthat::expect_equal(out$geolocation_method[4], "municipality") # Row 4: no lat/lon, has municipality (priority over province)
  testthat::expect_equal(out$geolocation_method[5], "coordinates") # Row 5: has lat/lon (priority over municipality/province)

  # Test that different methods produce different geometries
  # Row 1 and Row 5 should have different centroids (different lat/lon)
  centroid_1 <- sf::st_coordinates(out$centroid[1])
  centroid_5 <- sf::st_coordinates(out$centroid[5])
  testthat::expect_false(identical(centroid_1, centroid_5))

  # Test that size_in_m2 affects buffer size for lat/lon cases
  # Calculate areas of row 1 and row 5 geometries (both use lat/lon but different sizes)
  area_1 <- as.numeric(sf::st_area(out$geometry[1]))
  area_5 <- as.numeric(sf::st_area(out$geometry[5]))
  # Row 1 (10,000 m²) should have larger area than Row 5 (5,000 m²)
  testthat::expect_gt(area_1, area_5)

  # Test that rows 2 and 4 (both using "Borba" municipality) have similar centroids
  centroid_2 <- sf::st_coordinates(out$centroid[2])
  centroid_4 <- sf::st_coordinates(out$centroid[4])
  # Allow for small numerical differences
  testthat::expect_true(all(abs(centroid_2 - centroid_4) < 0.01))
})


testthat::test_that("geolocate_assets returns valid sfc types in WGS84 CRS", {
  base_dir <- get_test_data_dir()
  assets <- read_assets(base_dir)
  hazards <- load_hazards(get_hazards_dir(), aggregate_factor = 16L)
  municipalities <- load_municipalities(file.path(base_dir, "areas", "municipality"))
  provinces <- load_provinces(file.path(base_dir, "areas", "province"))
  output_crs <- terra::crs(hazards[[1]])
  out <- geolocate_assets(assets, municipalities, provinces, output_crs = output_crs)

  # geometry should be polygons/multipolygons; centroid should be points
  # Check that we have sfc geometry columns
  testthat::expect_s3_class(out$geometry, "sfc")
  testthat::expect_s3_class(out$centroid, "sfc")

  # Check the geometry types in the sfc objects
  geom_types <- unique(sf::st_geometry_type(out$geometry))
  cent_types <- unique(sf::st_geometry_type(out$centroid))
  testthat::expect_true(all(geom_types %in% c("POLYGON", "MULTIPOLYGON")))
  testthat::expect_true(all(cent_types %in% c("POINT")))

  # Check that CRS is WGS84 (EPSG:4326)
  geom_crs <- sf::st_crs(out$geometry)
  testthat::expect_equal(geom_crs$epsg, 4326)
})

#' Test that geolocate_assets handles assets without valid location data
#'
#' This test ensures that geolocate_assets provides a meaningful error message
#' when an asset cannot be geolocated.

testthat::test_that("geolocate_assets handles assets without valid location data", {
  # Load test data
  base_dir <- get_test_data_dir()

  # Load required data
  assets <- read_assets(base_dir)
  areas <- load_location_areas(
    file.path(base_dir, "areas", "municipality"),
    file.path(base_dir, "areas", "province")
  )

  # Create asset with no valid location data
  bad_asset <- assets[1, , drop = FALSE]
  bad_asset$latitude <- NA_real_
  bad_asset$longitude <- NA_real_
  bad_asset$municipality <- NA_character_
  bad_asset$province <- NA_character_

  # Test that geolocate_assets provides a meaningful error message
  testthat::expect_error(
    geolocate_assets(bad_asset, areas$municipalities, areas$provinces),
    regexp = "Failed to geolocate asset"
  )
})
