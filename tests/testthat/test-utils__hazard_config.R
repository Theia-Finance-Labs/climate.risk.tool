testthat::test_that("read_hazard_config parses indicators and mappings", {
  temp_dir <- tempfile("hazard_config_")
  dir.create(temp_dir, recursive = TRUE, showWarnings = FALSE)
  config_path <- file.path(temp_dir, "hazard.yml")

  yaml_text <- c(
    "name: Flood",
    "",
    "indicators:",
    "  depth:",
    "    file: flood_depth.nc",
    "    variable: depth",
    "    index: [gwl, return_period]",
    "    agg: median",
    "",
    "mappings:",
    "  damage_and_cost_factors:",
    "    file: damage_and_cost_factors.csv",
    "    join:",
    "      on_intensity: [depth]",
    "      on_hazard: [return_period]",
    "      on_assets: [asset_category]"
  )
  writeLines(yaml_text, config_path)

  config <- read_hazard_config(config_path)

  testthat::expect_equal(config$name, "Flood")
  testthat::expect_true("depth" %in% names(config$indicators))
  testthat::expect_equal(config$indicators$depth$file, "flood_depth.nc")
  testthat::expect_equal(config$indicators$depth$variable, "depth")
  testthat::expect_equal(config$indicators$depth$index, c("gwl", "return_period"))
  testthat::expect_equal(config$indicators$depth$agg, "median")
  testthat::expect_equal(config$primary_indicator, "depth")

  testthat::expect_true("damage_and_cost_factors" %in% names(config$mappings))
  join_keys <- config$mappings$damage_and_cost_factors$join
  testthat::expect_equal(join_keys$on_intensity, "depth")
  testthat::expect_equal(join_keys$on_hazard, "return_period")
  testthat::expect_equal(join_keys$on_assets, "asset_category")
})

testthat::test_that("load_hazard_configs reads configs from hazards folder", {
  temp_dir <- tempfile("hazards_root_")
  dir.create(temp_dir, recursive = TRUE, showWarnings = FALSE)

  flood_dir <- file.path(temp_dir, "Flood")
  heat_dir <- file.path(temp_dir, "Heat")
  dir.create(flood_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(heat_dir, recursive = TRUE, showWarnings = FALSE)

  writeLines(c("name: Flood", "indicators: {depth: {file: flood_depth.nc, variable: depth, agg: mean}}"), file.path(flood_dir, "hazard.yml"))
  writeLines(c("name: Heat", "indicators: {hi: {file: hi.nc, variable: hi, agg: mean}}"), file.path(heat_dir, "hazard.yml"))

  registry <- load_hazard_configs(temp_dir)

  testthat::expect_true(is.list(registry))
  testthat::expect_true(all(c("Flood", "Heat") %in% names(registry)))
  testthat::expect_equal(registry$Flood$name, "Flood")
  testthat::expect_equal(registry$Heat$name, "Heat")
})
testthat::test_that("load_hazard_configs reads hazard.yml and normalizes defaults", {
  temp_dir <- tempfile("hazard_config_")
  hazards_dir <- file.path(temp_dir, "hazards")
  flood_dir <- file.path(hazards_dir, "Flood")
  dir.create(flood_dir, recursive = TRUE)

  config <- list(
    name = "Flood",
    indicators = list(
      depth = list(
        file = "flood_depth.nc",
        variable = "depth",
        index = c("gwl", "return_period"),
        agg = "median"
      )
    ),
    mappings = list(
      damage_and_cost_factors = list(
        file = "damage_and_cost_factors.csv",
        join = list(
          on_intensity = c("depth"),
          on_hazard = c("return_period"),
          on_assets = c("asset_category")
        )
      )
    )
  )

  yaml::write_yaml(config, file.path(flood_dir, "hazard.yml"))

  configs <- load_hazard_configs(hazards_dir)

  testthat::expect_true(is.list(configs))
  testthat::expect_true("Flood" %in% names(configs))

  flood_cfg <- configs[["Flood"]]
  testthat::expect_equal(flood_cfg$name, "Flood")
  testthat::expect_true("depth" %in% names(flood_cfg$indicators))

  depth_cfg <- flood_cfg$indicators$depth
  testthat::expect_equal(depth_cfg$key, "depth")
  testthat::expect_equal(depth_cfg$file, "flood_depth.nc")
  testthat::expect_equal(depth_cfg$variable, "depth")
  testthat::expect_equal(depth_cfg$index, c("gwl", "return_period"))
  testthat::expect_equal(depth_cfg$agg, "median")
  testthat::expect_false(depth_cfg$categorical)
  testthat::expect_true(is.list(depth_cfg$fixed))

  mapping_cfg <- flood_cfg$mappings$damage_and_cost_factors
  testthat::expect_equal(mapping_cfg$file, "damage_and_cost_factors.csv")
  testthat::expect_equal(mapping_cfg$join$on_intensity, c("depth"))
  testthat::expect_equal(mapping_cfg$join$on_hazard, c("return_period"))
  testthat::expect_equal(mapping_cfg$join$on_assets, c("asset_category"))
  testthat::expect_equal(mapping_cfg$intensity_match, "exact")
})

testthat::test_that("load_hazard_configs errors on missing required keys", {
  temp_dir <- tempfile("hazard_config_missing_")
  hazards_dir <- file.path(temp_dir, "hazards")
  drought_dir <- file.path(hazards_dir, "Drought")
  dir.create(drought_dir, recursive = TRUE)

  yaml::write_yaml(list(name = "Drought"), file.path(drought_dir, "hazard.yml"))

  testthat::expect_error(
    load_hazard_configs(hazards_dir),
    regexp = "indicators"
  )
})

testthat::test_that("load_hazard_configs allows mappings without join keys", {
  temp_dir <- tempfile("hazard_config_join_")
  hazards_dir <- file.path(temp_dir, "hazards")
  heat_dir <- file.path(hazards_dir, "Heat")
  dir.create(heat_dir, recursive = TRUE)

  config <- list(
    name = "Heat",
    indicators = list(
      days_hot_total = list(
        file = "hi.nc",
        variable = "days_hot_total",
        index = c("gwl", "return_period"),
        fixed = list(ensemble = "mean"),
        agg = "closest"
      )
    ),
    mappings = list(
      exposure_links = list(
        file = "exposure_factors.csv"
      )
    )
  )

  yaml::write_yaml(config, file.path(heat_dir, "hazard.yml"))

  configs <- load_hazard_configs(hazards_dir)
  mapping_cfg <- configs$Heat$mappings$exposure_links
  testthat::expect_true(is.list(mapping_cfg$join))
  testthat::expect_equal(mapping_cfg$join$on_intensity, character(0))
})

