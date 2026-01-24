testthat::test_that("evaluate_hazard_shock identifies duplicate assets in single equation", {
  assets_event <- tibble::tibble(
    asset = c("A1", "A1", "A2"),
    revenue = c(100, 100, 200),
    damage_factor = c(0.1, 0.1, 0.2)
  )
  
  hazard_config <- list(
    shocks = list(
      revenue = list(
        equations = list(
          eq1 = list(
            equation = "revenue * (1 - damage_factor)"
          )
        )
      )
    )
  )
  
  testthat::expect_error(
    climate.risk.tool:::evaluate_hazard_shock(assets_event, hazard_config, "revenue"),
    "produced multiple values for assets: A1"
  )
})

testthat::test_that("evaluate_hazard_shock identifies overlapping equations", {
  assets_event <- tibble::tibble(
    asset = c("A1", "A2"),
    revenue = c(100, 200),
    damage_factor = c(0.1, 0.2)
  )
  
  hazard_config <- list(
    shocks = list(
      revenue = list(
        equations = list(
          eq1 = list(
            when = "revenue > 50",
            equation = "revenue * 0.9"
          ),
          eq2 = list(
            when = "revenue > 150",
            equation = "revenue * 0.8"
          )
        )
      )
    )
  )
  
  testthat::expect_error(
    climate.risk.tool:::evaluate_hazard_shock(assets_event, hazard_config, "revenue"),
    "Multiple shock equations produced values for assets: A2"
  )
})

testthat::test_that("evaluate_hazard_shock works with non-overlapping equations", {
  assets_event <- tibble::tibble(
    asset = c("A1", "A2"),
    revenue = c(100, 200),
    damage_factor = c(0.1, 0.2)
  )
  
  hazard_config <- list(
    shocks = list(
      revenue = list(
        equations = list(
          eq1 = list(
            when = "revenue <= 150",
            equation = "revenue * 0.9"
          ),
          eq2 = list(
            when = "revenue > 150",
            equation = "revenue * 0.8"
          )
        )
      )
    )
  )
  
  result <- climate.risk.tool:::evaluate_hazard_shock(assets_event, hazard_config, "revenue")
  testthat::expect_equal(nrow(result), 2)
  testthat::expect_equal(result$shock_value, c(90, 160))
})
