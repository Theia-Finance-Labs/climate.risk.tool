testthat::test_that("compute_risk filters hazards by exact hazard_name", {
  base_dir <- get_test_data_dir()
  assets <- read_assets(base_dir)
  companies <- read_companies(file.path(base_dir, "user_input", "company.csv"))
  hazards <- load_hazards(file.path(base_dir, "hazards"), aggregate_factor = 16L)
  areas <- load_location_areas(
    file.path(base_dir, "areas", "municipality"),
    file.path(base_dir, "areas", "province")
  )
  damage_factors <- read_damage_cost_factors(base_dir)

  available <- names(hazards)

  # Pick two distinct hazard names
  selected_two <- unique(available)[1:2]

  # Events with exact hazard_name
  events <- data.frame(
    event_id = c("e1", "e2"),
    hazard_type = c(
      strsplit(selected_two[1], "__", fixed = TRUE)[[1]][1],
      strsplit(selected_two[2], "__", fixed = TRUE)[[1]][1]
    ),
    hazard_name = selected_two,
    event_year = c(2030L, 2035L),
    chronic = c(FALSE, TRUE)
  )

  res <- compute_risk(
    assets = assets,
    companies = companies,
    events = events,
    hazards = hazards,
    areas = areas,
    damage_factors = damage_factors
  )

  # Exposures in assets_factors should include exactly the two hazard names
  testthat::expect_true("assets_factors" %in% names(res))
  hnames <- unique(res$assets_factors$hazard_name)
  testthat::expect_true(all(selected_two %in% hnames))
  testthat::expect_true(all(hnames %in% selected_two))
})
