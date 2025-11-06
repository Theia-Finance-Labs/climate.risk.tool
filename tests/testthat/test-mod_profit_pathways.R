testthat::test_that("mod_profit_pathways_ui includes download control", {
  ui <- mod_profit_pathways_ui("test")
  html <- htmltools::renderTags(ui)$html

  testthat::expect_true(grepl("test-download_profit_pathways_csv", html))
  testthat::expect_true(grepl("test-download_profit_pathways_excel", html))
  testthat::expect_true(grepl("Log scale requires positive profits", html))
})

testthat::test_that("mod_profit_pathways_server enriches metadata and download data", {
  testthat::skip_on_ci()
  testthat::skip_if_not_installed("shiny")

  assets_factors <- data.frame(
    asset = c("A1", "A2"),
    company = c("CompA", "CompB"),
    share_of_economic_activity = c(0.6, 0.4),
    asset_category = c("industrial", "agriculture"),
    asset_subtype = c("manufacturing", "crop"),
    sector = c("10", "01"),
    stringsAsFactors = FALSE
  )

  cnae_exposure <- tibble::tibble(
    cnae = c(10, 1),
    description = c("Manufacturing", "Agriculture"),
    lp_exposure = c("median", "high")
  )

  assets_yearly <- data.frame(
    asset = rep(c("A1", "A2"), each = 4),
    company = rep(c("CompA", "CompB"), each = 4),
    year = rep(c(2025, 2026), times = 4),
    scenario = rep(c("baseline", "shock"), times = 4),
    profit = c(1000, 1020, 950, 970, 800, 820, 760, 780),
    stringsAsFactors = FALSE
  )

  test_results <- list(
    assets_factors = assets_factors,
    assets_yearly = assets_yearly
  )

  shiny::testServer(mod_profit_pathways_server, args = list(
    id = "test",
    results_reactive = shiny::reactive(test_results),
    cnae_exposure_reactive = shiny::reactive(cnae_exposure)
  ), {
    session$flushReact()
    assets_table <- session$userData$profit_pathways_assets_table
    testthat::expect_false(is.null(assets_table))
    testthat::expect_true("Share of economic activity" %in% colnames(assets_table))
    testthat::expect_true(all(assets_table$Sector %in% c("Manufacturing", "Agriculture")))

    download_data <- session$userData$download_profit_pathways_data()
    testthat::expect_false(is.null(download_data))
    testthat::expect_true(all(c("share_of_economic_activity", "sector_name", "sector_code") %in% names(download_data)))
    testthat::expect_true(all(download_data$sector_code %in% c("10", "01")))
  })
})
testthat::test_that("create_profit_plot retains non-positive profits on log scale", {
  testthat::skip_if_not_installed("plotly")

  test_data <- data.frame(
    asset = c("A", "A", "B"),
    year = c(2020, 2021, 2020),
    profit = c(100, 0, -50)
  )

  plot_obj <- create_profit_plot(
    data = test_data,
    highlighted_assets = character(0),
    title = "Test",
    log_scale = TRUE
  )

  built_plot <- plotly::plotly_build(plot_obj)

  trace_a <- built_plot$x$data[[1]]
  trace_b <- built_plot$x$data[[2]]

  trace_a_x <- as.numeric(trace_a$x)
  trace_a_y <- as.numeric(trace_a$y)
  trace_a_custom <- as.numeric(trace_a$customdata)
  trace_a_text <- as.character(trace_a$text)

  testthat::expect_equal(trace_a_x, c(2020, 2021))
  testthat::expect_length(trace_a_y, 2)
  testthat::expect_true(all(trace_a_y > 0))
  testthat::expect_equal(trace_a_custom, c(100, 0))
  testthat::expect_true(all(trace_a_text == ""))

  trace_b_x <- as.numeric(trace_b$x)
  trace_b_y <- as.numeric(trace_b$y)
  trace_b_custom <- as.numeric(trace_b$customdata)
  trace_b_text <- as.character(trace_b$text)

  testthat::expect_equal(trace_b_x, 2020)
  testthat::expect_length(trace_b_y, 1)
  testthat::expect_true(all(trace_b_y > 0))
  testthat::expect_equal(trace_b_custom, -50)
  testthat::expect_true(all(trace_b_text == ""))
})

