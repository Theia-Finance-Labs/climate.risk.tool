testthat::test_that("format_hover_text creates HTML with asset info", {
  html <- format_hover_text("Asset1", 5.5)
  
  testthat::expect_true(grepl("Asset1", html, fixed = TRUE))
  testthat::expect_true(grepl("5.5", html, fixed = TRUE))
  testthat::expect_true(grepl("Hazard Intensity", html, fixed = TRUE))
  testthat::expect_true(grepl("<div", html, fixed = TRUE))
  testthat::expect_true(grepl("</div>", html, fixed = TRUE))
})

testthat::test_that("format_hover_text includes additional info", {
  html <- format_hover_text(
    "Asset1", 
    3.2, 
    list("Category" = "agriculture", "Province" = "Bahia")
  )
  
  testthat::expect_true(grepl("Asset1", html, fixed = TRUE))
  testthat::expect_true(grepl("3.2", html, fixed = TRUE))
  testthat::expect_true(grepl("agriculture", html, fixed = TRUE))
  testthat::expect_true(grepl("Bahia", html, fixed = TRUE))
})

testthat::test_that("create_color_palette returns correct number of colors", {
  colors <- create_color_palette(5)
  
  testthat::expect_length(colors, 5)
  testthat::expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", colors)))
})

testthat::test_that("create_color_palette handles single category", {
  colors <- create_color_palette(1)
  
  testthat::expect_length(colors, 1)
  testthat::expect_equal(colors, "#3498db")
})

testthat::test_that("prepare_profit_trajectories filters by scenario", {
  assets_yearly <- tibble::tibble(
    asset = c("A1", "A1", "A2", "A2"),
    scenario = c("baseline", "ev1", "baseline", "ev1"),
    year = c(2025, 2025, 2025, 2025),
    profit = c(100, 80, 200, 150)
  )
  
  baseline_data <- prepare_profit_trajectories(assets_yearly, "baseline")
  
  testthat::expect_equal(nrow(baseline_data), 2)
  testthat::expect_equal(baseline_data$asset, c("A1", "A2"))
  testthat::expect_equal(baseline_data$profit, c(100, 200))
  testthat::expect_false("scenario" %in% names(baseline_data))
})

testthat::test_that("prepare_profit_trajectories handles empty data", {
  result <- prepare_profit_trajectories(NULL, "baseline")
  
  testthat::expect_s3_class(result, "tbl_df")
  testthat::expect_equal(nrow(result), 0)
  testthat::expect_true("asset" %in% names(result))
  testthat::expect_true("year" %in% names(result))
  testthat::expect_true("profit" %in% names(result))
})

testthat::test_that("compute_portfolio_summary calculates totals correctly", {
  companies_df <- tibble::tibble(
    company = c("C1", "C2"),
    Expected_loss_baseline = c(1000, 2000),
    Expected_loss_shock = c(1500, 2500)
  )
  
  summary <- compute_portfolio_summary(companies_df)
  
  testthat::expect_equal(nrow(summary), 3)
  testthat::expect_equal(summary$metric, c("Baseline", "Shock", "Difference"))
  testthat::expect_equal(summary$value[1], 3000)  # 1000 + 2000
  testthat::expect_equal(summary$value[2], 4000)  # 1500 + 2500
  testthat::expect_equal(summary$value[3], 1000)  # 4000 - 3000
})

testthat::test_that("compute_portfolio_summary handles long format data", {
  companies_df <- tibble::tibble(
    company = c("C1", "C1", "C2", "C2"),
    scenario = c("baseline", "ev1", "baseline", "ev1"),
    Expected_loss = c(1000, 1500, 2000, 2500)
  )
  
  summary <- compute_portfolio_summary(companies_df)
  
  testthat::expect_equal(nrow(summary), 3)
  testthat::expect_equal(summary$value[1], 3000)  # baseline: 1000 + 2000
  testthat::expect_equal(summary$value[2], 4000)  # shock: 1500 + 2500
})

testthat::test_that("compute_portfolio_summary handles empty data", {
  summary <- compute_portfolio_summary(NULL)
  
  testthat::expect_s3_class(summary, "tbl_df")
  testthat::expect_equal(nrow(summary), 0)
})

testthat::test_that("extract_hazard_data retrieves hazard from list", {
  hazards <- list(
    "hazard1" = "data1",
    "hazard2" = "data2"
  )
  
  result <- extract_hazard_data("hazard1", hazards)
  testthat::expect_equal(result, "data1")
  
  result2 <- extract_hazard_data("hazard2", hazards)
  testthat::expect_equal(result2, "data2")
})

testthat::test_that("extract_hazard_data returns NULL for missing hazard", {
  hazards <- list(
    "hazard1" = "data1"
  )
  
  result <- extract_hazard_data("missing", hazards)
  testthat::expect_null(result)
})

testthat::test_that("get_hazard_type identifies raster correctly", {
  # Create a simple SpatRaster for testing
  testthat::skip_if_not_installed("terra")
  
  rast <- terra::rast(ncols = 10, nrows = 10, vals = 1:100)
  result <- get_hazard_type(rast)
  
  testthat::expect_equal(result, "raster")
})

testthat::test_that("get_hazard_type identifies points correctly", {
  df <- data.frame(lat = c(1, 2), lon = c(3, 4), value = c(5, 6))
  result <- get_hazard_type(df)
  
  testthat::expect_equal(result, "points")
})

testthat::test_that("prepare_asset_overlay handles empty assets_factors", {
  result <- prepare_asset_overlay(NULL, "hazard1", NULL, NULL)
  
  testthat::expect_type(result, "list")
  testthat::expect_null(result$geolocated)
  testthat::expect_null(result$municipalities)
  testthat::expect_null(result$provinces)
})

testthat::test_that("prepare_asset_overlay separates by matching method", {
  assets_factors <- tibble::tibble(
    asset = c("A1", "A2", "A3"),
    hazard_name = c("hazard1", "hazard1", "hazard1"),
    matching_method = c("coordinates", "municipality", "province"),
    lat = c(-15.7, NA, NA),
    lon = c(-47.9, NA, NA),
    hazard_intensity = c(5, 3, 4),
    municipality = c(NA, "Brasilia", NA),
    province = c(NA, NA, "Goias")
  )
  
  result <- prepare_asset_overlay(assets_factors, "hazard1", NULL, NULL)
  
  testthat::expect_type(result, "list")
  testthat::expect_true(!is.null(result$geolocated))
  testthat::expect_equal(nrow(result$geolocated), 1)
  testthat::expect_s3_class(result$geolocated, "sf")
})

