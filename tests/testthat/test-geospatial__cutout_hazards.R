# Tests for function: cutout_hazards

# Contract:
# - cutout_hazards(assets_with_geometry, hazards)
# - For each loaded hazard, add a list column with all pixel values within the polygon
# - Non-empty list columns added, names derived from hazard names


testthat::test_that("cutout_hazards adds one list column per loaded hazard", {
  skip_slow_tests()
  
  base_dir <- get_test_data_dir()
  assets <- read_assets(base_dir)
  hazards <- load_hazards(get_hazards_dir())
  municipalities <- load_municipalities(file.path(base_dir, "areas", "municipality"))
  provinces <- load_provinces(file.path(base_dir, "areas", "province"))
  assets_geo <- geolocate_assets(assets, hazards, municipalities, provinces)

  out <- cutout_hazards(assets_geo, hazards)

  # Added columns should be at least the number of hazards
  added <- setdiff(names(out), names(assets_geo))
  testthat::expect_gte(length(added), length(hazards))
  testthat::expect_true(all(vapply(out[added], is.list, logical(1))))
})


testthat::test_that("cutout_hazards optimizes by grouping municipality and province methods", {
  skip_slow_tests()
  # Create a small test dataset with known geolocation methods
  base_dir <- get_test_data_dir()
  assets <- read_assets(base_dir)
  hazards <- load_hazards(get_hazards_dir())
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
  
  df$latitude[6] <- -1.0  # Much more different coordinates
  df$longitude[6] <- -58.0
  df$municipality[6] <- NA_character_
  df$province[6] <- NA_character_
  
  assets_geo <- geolocate_assets(df, hazards, municipalities, provinces)
  
  # Verify geolocation methods are as expected
  testthat::expect_equal(assets_geo$geolocation_method[1:2], c("municipality", "municipality"))
  testthat::expect_equal(assets_geo$geolocation_method[3:4], c("province", "province"))
  testthat::expect_equal(assets_geo$geolocation_method[5:6], c("coordinates", "coordinates"))
  
  out <- cutout_hazards(assets_geo, hazards)
  
  # Test that assets with same municipality get same hazard pixel values
  hazard_names <- names(hazards)
  for (hazard_name in hazard_names) {
    if (hazard_name %in% names(out)) {
      # Assets 1-2 (same municipality) should have identical pixel value lists
      testthat::expect_equal(out[[hazard_name]][[1]], out[[hazard_name]][[2]], 
                           info = paste("Municipality assets should have same", hazard_name, "pixel values"))
      
      # Assets 3-4 (same province) should have identical pixel value lists
      testthat::expect_equal(out[[hazard_name]][[3]], out[[hazard_name]][[4]],
                           info = paste("Province assets should have same", hazard_name, "pixel values"))
      
      # Assets 5-6 (different coordinates) should have different pixel value lists
      # Only test if both have non-empty pixel lists
      pixels5 <- out[[hazard_name]][[5]]
      pixels6 <- out[[hazard_name]][[6]]
      if (length(pixels5) > 0 && length(pixels6) > 0) {
        testthat::expect_false(identical(pixels5, pixels6),
                             info = paste("Coordinate assets should have different", hazard_name, "pixel values"))
      }
    }
  }
})


testthat::test_that("cutout_hazards handles missing geolocation_method column", {
  skip_slow_tests()
  base_dir <- get_test_data_dir()
  assets <- read_assets(base_dir)
  hazards <- load_hazards(get_hazards_dir())
  
  # Create assets with geometry but without geolocation_method column
  assets_with_geom <- assets[1:2, , drop = FALSE]
  # Add fake geometry column
  assets_with_geom$geometry <- sf::st_sfc(
    sf::st_polygon(list(matrix(c(0,0,0,1,1,1,1,0,0,0), ncol=2, byrow=TRUE))),
    sf::st_polygon(list(matrix(c(0,0,0,1,1,1,1,0,0,0), ncol=2, byrow=TRUE))),
    crs = terra::crs(hazards[[1]])
  )
  
  testthat::expect_error(
    cutout_hazards(assets_with_geom, hazards),
    "Input dataframe must have a 'geolocation_method' column"
  )
})
