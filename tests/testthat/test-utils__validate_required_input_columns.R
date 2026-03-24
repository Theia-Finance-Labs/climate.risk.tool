testthat::test_that("validate_required_input_columns returns no errors for full schema", {
  catalog <- get_input_columns_catalog()

  assets_df <- as.data.frame(
    stats::setNames(
      rep(list(NA), length(catalog$assets_required)),
      catalog$assets_required
    )
  )
  companies_df <- as.data.frame(
    stats::setNames(
      rep(list(NA), length(catalog$companies_required)),
      catalog$companies_required
    )
  )

  validation_results <- list(errors = character(), warnings = character())
  validation_results <- climate.risk.tool:::validate_required_input_columns(
    assets_df = assets_df,
    companies_df = companies_df,
    validation_results = validation_results
  )

  testthat::expect_length(validation_results$errors, 0)
})

testthat::test_that("validate_required_input_columns reports missing columns", {
  assets_df <- data.frame(asset = "A1")

  validation_results <- list(errors = character(), warnings = character())
  validation_results <- climate.risk.tool:::validate_required_input_columns(
    assets_df = assets_df,
    companies_df = NULL,
    validation_results = validation_results
  )

  testthat::expect_true(
    any(grepl("Assets table is missing required column", validation_results$errors))
  )
})
