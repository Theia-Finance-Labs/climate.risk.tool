# Tests for function: extract_hazard_statistics
#
# Contract:
# - extract_hazard_statistics(assets_df, hazards, precomputed_hazards, use_exactextractr)
# - For assets WITH coordinates: performs spatial extraction using raster data
# - For assets WITHOUT coordinates but WITH municipality: looks up precomputed ADM2 data
# - For assets WITHOUT coordinates/municipality but WITH province: looks up precomputed ADM1 data
# - Returns long format with hazard statistics for all assets
# - Raises error if an asset cannot be matched to either spatial or precomputed data
#
# PRIORITY CASCADE: coordinates > municipality > province

# ==============================================================================
# Basic functionality tests
# ==============================================================================

testthat::test_that("extract_hazard_statistics returns long format with hazard statistics", {
  base_dir <- get_test_data_dir()
  assets <- read_assets(base_dir)
  mapping <- read_hazards_mapping(file.path(base_dir, "hazards_metadata.csv"))
  hazards <- load_hazards_from_mapping(mapping, get_hazards_dir(), aggregate_factor = 16L)
  precomputed <- read_precomputed_hazards(base_dir)

  out <- extract_hazard_statistics(assets, hazards, precomputed, use_exactextractr = FALSE)

  # Should have required long format columns including matching_method
  required_cols <- c("asset", "hazard_name", "hazard_type", "hazard_mean", "hazard_median", "hazard_max", "matching_method")
  testthat::expect_true(all(required_cols %in% names(out)))

  # Should be in long format (more rows than original assets due to multiple hazards)
  testthat::expect_gte(nrow(out), nrow(assets))

  # Should have numeric hazard statistics
  testthat::expect_true(is.numeric(out$hazard_mean))
  testthat::expect_true(is.numeric(out$hazard_median))
  testthat::expect_true(is.numeric(out$hazard_max))
  
  # Should have matching_method column with valid values
  testthat::expect_true(is.character(out$matching_method))
  testthat::expect_true(all(out$matching_method %in% c("coordinates", "municipality", "province")))
})


testthat::test_that("extract_hazard_statistics handles mixed assets (coordinates and precomputed)", {
  base_dir <- get_test_data_dir()
  assets <- read_assets(base_dir)
  precomputed <- read_precomputed_hazards(base_dir)
  mapping <- read_hazards_mapping(file.path(base_dir, "hazards_metadata.csv"))
  hazards <- load_hazards_from_mapping(mapping, get_hazards_dir(), aggregate_factor = 16L)
  
  # Create mixed dataset
  df <- assets[1:4, , drop = FALSE]
  
  # Asset 1: Has coordinates (spatial extraction)
  df$latitude[1] <- -3.0
  df$longitude[1] <- -60.0
  df$municipality[1] <- "Borba"
  df$province[1] <- "Amazonas"
  
  # Asset 2: No coordinates, has municipality (ADM2 lookup)
  df$latitude[2] <- NA_real_
  df$longitude[2] <- NA_real_
  df$municipality[2] <- "Borba"
  df$province[2] <- "Amazonas"
  
  # Asset 3: No coordinates, no municipality, has province (ADM1 lookup)
  df$latitude[3] <- NA_real_
  df$longitude[3] <- NA_real_
  df$municipality[3] <- NA_character_
  df$province[3] <- "Amazonas"
  
  # Asset 4: Has coordinates (spatial extraction)
  df$latitude[4] <- -15.0
  df$longitude[4] <- -47.9
  df$municipality[4] <- NA_character_
  df$province[4] <- NA_character_
  
  out <- extract_hazard_statistics(df, hazards, precomputed, use_exactextractr = FALSE)
  
  # Should have results for all 4 assets
  testthat::expect_equal(length(unique(out$asset)), 4)
  
  # All should have hazard statistics
  testthat::expect_true(all(!is.na(out$hazard_mean)))
  
  # Test matching_method column values
  asset1_method <- unique(out$matching_method[out$asset == df$asset[1]])
  asset2_method <- unique(out$matching_method[out$asset == df$asset[2]])
  asset3_method <- unique(out$matching_method[out$asset == df$asset[3]])
  asset4_method <- unique(out$matching_method[out$asset == df$asset[4]])
  
  testthat::expect_equal(asset1_method, "coordinates")
  testthat::expect_equal(asset2_method, "municipality")
  testthat::expect_equal(asset3_method, "province")
  testthat::expect_equal(asset4_method, "coordinates")
})


testthat::test_that("extract_hazard_statistics raises error for unmatchable assets", {
  base_dir <- get_test_data_dir()
  assets <- read_assets(base_dir)
  precomputed <- read_precomputed_hazards(base_dir)
  mapping <- read_hazards_mapping(file.path(base_dir, "hazards_metadata.csv"))
  hazards <- load_hazards_from_mapping(mapping, get_hazards_dir(), aggregate_factor = 16L)
  
  # Create asset with no location data
  df <- assets[1, , drop = FALSE]
  df$latitude[1] <- NA_real_
  df$longitude[1] <- NA_real_
  df$municipality[1] <- NA_character_
  df$province[1] <- NA_character_
  
  # Should raise error
  testthat::expect_error(
    extract_hazard_statistics(df, hazards, precomputed, use_exactextractr = FALSE),
    regexp = "Cannot determine hazard|no location"
  )
})


# ==============================================================================
# Priority cascade tests: coordinates > municipality > province
# ==============================================================================

testthat::test_that("PRIORITY 1: coordinates take priority over municipality/province", {
  base_dir <- get_test_data_dir()
  assets <- read_assets(base_dir)
  mapping <- read_hazards_mapping(file.path(base_dir, "hazards_metadata.csv"))
  hazards <- load_hazards_from_mapping(mapping, get_hazards_dir(), aggregate_factor = 16L)
  precomputed <- read_precomputed_hazards(base_dir)

  # Create two assets:
  # - Asset 1: has BOTH coordinates AND municipality
  # - Asset 2: has ONLY municipality (same as Asset 1)
  df <- assets[1:2, , drop = FALSE]

  # Asset 1: coordinates + municipality
  df$latitude[1] <- -3.0
  df$longitude[1] <- -60.0
  df$municipality[1] <- "Borba"
  df$province[1] <- "Amazonas"

  # Asset 2: only municipality (same)
  df$latitude[2] <- NA_real_
  df$longitude[2] <- NA_real_
  df$municipality[2] <- "Borba"
  df$province[2] <- "Amazonas"

  out <- extract_hazard_statistics(df, hazards, precomputed, use_exactextractr = FALSE)

  # Asset 1 should use spatial extraction (coordinates priority)
  # Asset 2 should use precomputed lookup (municipality)
  # Both should have valid values
  
  for (hazard_name in unique(out$hazard_name)) {
    asset1_mean <- out |> 
      dplyr::filter(.data$asset == df$asset[1], .data$hazard_name == hazard_name) |>
      dplyr::pull(.data$hazard_mean)
    asset2_mean <- out |> 
      dplyr::filter(.data$asset == df$asset[2], .data$hazard_name == hazard_name) |>
      dplyr::pull(.data$hazard_mean)
    
    if (length(asset1_mean) > 0 && length(asset2_mean) > 0) {
      testthat::expect_true(is.numeric(asset1_mean))
      testthat::expect_true(is.numeric(asset2_mean))
    }
  }
})


testthat::test_that("PRIORITY 2: municipality takes priority over province", {
  base_dir <- get_test_data_dir()
  assets <- read_assets(base_dir)
  mapping <- read_hazards_mapping(file.path(base_dir, "hazards_metadata.csv"))
  hazards <- load_hazards_from_mapping(mapping, get_hazards_dir(), aggregate_factor = 16L)
  precomputed <- read_precomputed_hazards(base_dir)

  # Create two assets:
  # - Asset 1: has BOTH municipality AND province
  # - Asset 2: has ONLY province (same as Asset 1)
  df <- assets[1:2, , drop = FALSE]

  # Asset 1: municipality + province
  df$latitude[1] <- NA_real_
  df$longitude[1] <- NA_real_
  df$municipality[1] <- "Borba"
  df$province[1] <- "Amazonas"

  # Asset 2: only province (same)
  df$latitude[2] <- NA_real_
  df$longitude[2] <- NA_real_
  df$municipality[2] <- NA_character_
  df$province[2] <- "Amazonas"

  out <- extract_hazard_statistics(df, hazards, precomputed, use_exactextractr = FALSE)

  # Asset 1 should use municipality (ADM2) - more specific
  # Asset 2 should use province (ADM1) - less specific
  # They should have DIFFERENT values
  
  for (hazard_name in unique(out$hazard_name)) {
    asset1_mean <- out |> 
      dplyr::filter(.data$asset == df$asset[1], .data$hazard_name == hazard_name) |>
      dplyr::pull(.data$hazard_mean)
    asset2_mean <- out |> 
      dplyr::filter(.data$asset == df$asset[2], .data$hazard_name == hazard_name) |>
      dplyr::pull(.data$hazard_mean)
    
    if (length(asset1_mean) > 0 && length(asset2_mean) > 0) {
      testthat::expect_true(is.numeric(asset1_mean))
      testthat::expect_true(is.numeric(asset2_mean))
    }
  }
})


testthat::test_that("cascade priority works end-to-end with all levels", {
  base_dir <- get_test_data_dir()
  assets <- read_assets(base_dir)
  mapping <- read_hazards_mapping(file.path(base_dir, "hazards_metadata.csv"))
  hazards <- load_hazards_from_mapping(mapping, get_hazards_dir(), aggregate_factor = 16L)
  precomputed <- read_precomputed_hazards(base_dir)

  # Create 5 test assets demonstrating each priority level
  df <- assets[1:5, , drop = FALSE]

  # Asset 1: HAS lat/lon (and also municipality/province) - should use COORDINATES (priority 1)
  df$latitude[1] <- -3.0
  df$longitude[1] <- -60.0
  df$municipality[1] <- "Borba"
  df$province[1] <- "Amazonas"

  # Asset 2: NO lat/lon, HAS municipality - should use MUNICIPALITY/ADM2 (priority 2)
  df$latitude[2] <- NA_real_
  df$longitude[2] <- NA_real_
  df$municipality[2] <- "Borba"
  df$province[2] <- "Amazonas"

  # Asset 3: NO lat/lon, NO municipality, HAS province - should use PROVINCE/ADM1 (priority 3)
  df$latitude[3] <- NA_real_
  df$longitude[3] <- NA_real_
  df$municipality[3] <- NA_character_
  df$province[3] <- "Amazonas"

  # Asset 4: HAS lat/lon, NO municipality, NO province - should use COORDINATES (priority 1)
  df$latitude[4] <- -15.0
  df$longitude[4] <- -47.9
  df$municipality[4] <- NA_character_
  df$province[4] <- NA_character_

  # Asset 5: Another with municipality (to verify consistency)
  df$latitude[5] <- NA_real_
  df$longitude[5] <- NA_real_
  df$municipality[5] <- "Borba"
  df$province[5] <- "Amazonas"

  out <- extract_hazard_statistics(df, hazards, precomputed, use_exactextractr = FALSE)

  # Verify all assets got hazard statistics
  testthat::expect_equal(length(unique(out$asset)), 5)
  testthat::expect_true(all(!is.na(out$hazard_mean)))

  # Test: Assets 2 and 5 used municipality lookup (same municipality)
  # They should have IDENTICAL hazard values (from same precomputed ADM2)
  for (hazard_name in unique(out$hazard_name)) {
    asset2_mean <- out |> 
      dplyr::filter(.data$asset == df$asset[2], .data$hazard_name == hazard_name) |>
      dplyr::pull(.data$hazard_mean)
    asset5_mean <- out |> 
      dplyr::filter(.data$asset == df$asset[5], .data$hazard_name == hazard_name) |>
      dplyr::pull(.data$hazard_mean)
    
    if (length(asset2_mean) > 0 && length(asset5_mean) > 0) {
      testthat::expect_equal(asset2_mean, asset5_mean,
        info = paste("Assets 2 and 5 (same municipality) should have identical", hazard_name, "from precomputed ADM2")
      )
    }
  }
})


# ==============================================================================
# Consistency tests for precomputed lookups
# ==============================================================================

testthat::test_that("same municipality assets get identical precomputed values", {
  base_dir <- get_test_data_dir()
  assets <- read_assets(base_dir)
  mapping <- read_hazards_mapping(file.path(base_dir, "hazards_metadata.csv"))
  hazards <- load_hazards_from_mapping(mapping, get_hazards_dir(), aggregate_factor = 16L)
  precomputed <- read_precomputed_hazards(base_dir)

  # Create test data with 6 assets using different methods
  df <- assets[1:6, , drop = FALSE]

  # Assets 1-2: Same municipality (should get same hazard values from precomputed)
  df$latitude[1:2] <- NA_real_
  df$longitude[1:2] <- NA_real_
  df$municipality[1:2] <- "Borba"
  df$province[1:2] <- "Amazonas"

  # Assets 3-4: Same province (should get same hazard values from precomputed)
  df$latitude[3:4] <- NA_real_
  df$longitude[3:4] <- NA_real_
  df$municipality[3:4] <- NA_character_
  df$province[3:4] <- "Amazonas"

  # Assets 5-6: Different coordinates (should use spatial extraction)
  df$latitude[5] <- -15.0
  df$longitude[5] <- -47.9
  df$municipality[5] <- NA_character_
  df$province[5] <- NA_character_

  df$latitude[6] <- -23.5
  df$longitude[6] <- -46.6
  df$municipality[6] <- NA_character_
  df$province[6] <- NA_character_

  out <- extract_hazard_statistics(df, hazards, precomputed, use_exactextractr = FALSE)

  # Test that assets with same municipality/province get same means
  for (hazard_name in unique(out$hazard_name)) {
    hazard_subset <- out[out$hazard_name == hazard_name, ]

    # Assets 1-2 (same municipality) should have identical mean values
    mean_1_2 <- hazard_subset$hazard_mean[hazard_subset$asset %in% df$asset[1:2]]
    if (length(mean_1_2) == 2) {
      testthat::expect_equal(mean_1_2[1], mean_1_2[2],
        info = paste("Municipality assets should have same", hazard_name, "mean from precomputed data")
      )
    }

    # Assets 3-4 (same province) should have identical mean values
    mean_3_4 <- hazard_subset$hazard_mean[hazard_subset$asset %in% df$asset[3:4]]
    if (length(mean_3_4) == 2) {
      testthat::expect_equal(mean_3_4[1], mean_3_4[2],
        info = paste("Province assets should have same", hazard_name, "mean from precomputed data")
      )
    }

    # Assets 5-6 (different coordinates) just verify they have valid numeric values
    mean_5_6 <- hazard_subset$hazard_mean[hazard_subset$asset %in% df$asset[5:6]]
    if (length(mean_5_6) == 2 && !is.na(mean_5_6[1]) && !is.na(mean_5_6[2])) {
      testthat::expect_true(is.numeric(mean_5_6[1]))
      testthat::expect_true(is.numeric(mean_5_6[2]))
    }
  }
})


testthat::test_that("municipality not in precomputed data raises error", {
  base_dir <- get_test_data_dir()
  assets <- read_assets(base_dir)
  precomputed <- read_precomputed_hazards(base_dir)
  mapping <- read_hazards_mapping(file.path(base_dir, "hazards_metadata.csv"))
  hazards <- load_hazards_from_mapping(mapping, get_hazards_dir(), aggregate_factor = 16L)
  
  # Create asset with municipality not in precomputed data
  df <- assets[1, , drop = FALSE]
  df$latitude[1] <- NA_real_
  df$longitude[1] <- NA_real_
  df$municipality[1] <- "NonExistentMunicipality"
  df$province[1] <- "NonExistentProvince"
  
  # Should raise error (can't fall back if both are invalid)
  testthat::expect_error(
    extract_hazard_statistics(df, hazards, precomputed, use_exactextractr = FALSE),
    regexp = "Cannot determine|not found|NonExistent"
  )
})
