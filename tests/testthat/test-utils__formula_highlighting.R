testthat::test_that("highlight_formula colors indicator, mapping, input, and constants", {
  text <- "pmax(0, revenue * (1 - damage_factor) + hi + L0)"
  highlighted <- climate.risk.tool:::highlight_formula(
    text = text,
    indicator_vars = "hi",
    mapping_vars = "damage_factor",
    constant_vars = "L0",
    input_vars = "revenue"
  )

  testthat::expect_true(grepl("color: #002776", highlighted)) # indicator
  testthat::expect_true(grepl("color: #009C3B", highlighted)) # mapping
  testthat::expect_true(grepl("color: #9333ea", highlighted)) # input/asset
  testthat::expect_true(grepl("color: #64748b", highlighted)) # constant
})

testthat::test_that("highlight_formula matches full words only", {
  text <- "indebted + debt"
  highlighted <- climate.risk.tool:::highlight_formula(
    text = text,
    indicator_vars = character(0),
    mapping_vars = character(0),
    constant_vars = character(0),
    input_vars = "debt"
  )

  testthat::expect_true(grepl("debt</span>", highlighted))
  testthat::expect_false(grepl("indebt</span>", highlighted))
})

testthat::test_that("highlight_formula prefers longer variable names", {
  text <- "revenues - revenue"
  highlighted <- climate.risk.tool:::highlight_formula(
    text = text,
    indicator_vars = character(0),
    mapping_vars = character(0),
    constant_vars = character(0),
    input_vars = c("revenue", "revenues")
  )

  testthat::expect_true(grepl(">revenues</span>", highlighted))
  testthat::expect_true(grepl(">revenue</span>", highlighted))
})
