testthat::test_that("mod_settings_server saves and resets overrides", {
  testthat::skip_if_not_installed("shiny")

  safe_id <- function(value) {
    gsub("[^A-Za-z0-9_]", "_", value)
  }

  temp_dir <- tempfile("settings_base_")
  hazards_dir <- file.path(temp_dir, "hazards", "config")
  dir.create(hazards_dir, recursive = TRUE)

  config <- list(
    primary_indicator = "depth",
    indicators = list(
      depth = list(
        file = "revealed_depth",
        variable = "depth",
        index = c("gwl", "return_period"),
        fixed = list(ensemble = "mean"),
        agg = "median",
        categorical = FALSE
      )
    ),
    mappings = list(
      damage_and_cost_factors = list(
        file = "damage_and_cost_factors.csv",
        intensity_match = "closest",
        join = list(
          on_intensity = c("depth"),
          on_hazard = c("return_period"),
          on_assets = c("asset_category")
        )
      )
    )
  )
  yaml::write_yaml(config, file.path(hazards_dir, "Flood.yml"))

  hazard_configs <- load_hazard_configs(hazards_dir)
  inventory <- tibble::tibble(
    hazard_type = c("Flood", "Flood"),
    hazard_indicator = c("depth", "depth"),
    ensemble = c("mean", "median")
  )

  shiny::testServer(mod_settings_server, args = list(
    id = "settings",
    base_dir_reactive = shiny::reactive(temp_dir),
    hazard_configs_reactive = shiny::reactive(hazard_configs),
    inventory_reactive = shiny::reactive(inventory)
  ), {
    session$flushReact()

    hazard_id <- safe_id("Flood")
    indicator_id <- safe_id("depth")
    mapping_id <- safe_id("damage_and_cost_factors")
    fixed_id <- safe_id("ensemble")

    inputs <- list()
    inputs[[paste0("indicator_agg__", hazard_id, "__", indicator_id)]] <- "mean"
    inputs[[paste0("fixed__", hazard_id, "__", indicator_id, "__", fixed_id)]] <- "median"
    inputs[[paste0("mapping_intensity_match__", hazard_id, "__", mapping_id)]] <- "exact"
    inputs[["save_overrides"]] <- 1
    do.call(session$setInputs, inputs)
    session$flushReact()

    override_path <- file.path(temp_dir, "hazards", "config_overrides.yml")
    testthat::expect_true(file.exists(override_path))

    overrides <- yaml::read_yaml(override_path)
    testthat::expect_equal(overrides$Flood$indicators$depth$agg, "mean")
    testthat::expect_equal(overrides$Flood$indicators$depth$fixed$ensemble, "median")
    testthat::expect_equal(overrides$Flood$mappings$damage_and_cost_factors$intensity_match, "exact")
    testthat::expect_true(is.null(overrides$Flood$primary_indicator))

    session$setInputs(reset_overrides = 1)
    session$flushReact()

    testthat::expect_true(file.exists(override_path))
    wiped <- yaml::read_yaml(override_path)
    testthat::expect_true(is.null(wiped) || (is.list(wiped) && length(wiped) == 0L))
  })
})
