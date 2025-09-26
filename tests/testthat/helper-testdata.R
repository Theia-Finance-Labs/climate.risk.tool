# Test Fixtures and Helpers for locating test data and common checks

# Helpers for locating test data and common checks
get_test_data_dir <- function(...) {
  file.path(testthat::test_path(".."), "tests_data", ...)
}


get_hazards_dir <- function() {
  use_mini <- Sys.getenv("USE_MINI_HAZARDS", "TRUE") == "TRUE"
  if (use_mini) {
    mini_dir <- get_test_data_dir("hazards_mini")
    if (!dir.exists(mini_dir)) {
      # Generate mini hazards if they don't exist
      generate_mini_hazards_if_needed()
    }
    return(mini_dir)
  }
  get_test_data_dir("hazards")
}

# Create a temporary directory for generated mini test datasets if needed
get_test_scratch_dir <- function() {
  scratch <- file.path(tempdir(), "climate.risk.tool_tests")
  if (!dir.exists(scratch)) dir.create(scratch, recursive = TRUE, showWarnings = FALSE)
  scratch
}

# Generate mini hazards automatically if needed
generate_mini_hazards_if_needed <- function() {
  mini_dir <- get_test_data_dir("hazards_mini")
  if (!dir.exists(mini_dir)) {
    dir.create(mini_dir, recursive = TRUE, showWarnings = FALSE)
  }

  # Downscale entire hazard rasters to lower resolution, preserving full extent
  input_dir <- get_test_data_dir("hazards")
  factor <- as.integer(Sys.getenv("MINI_HAZARDS_FACTOR", "64"))
  # Ensure package functions are available when tests source helpers
  if (!exists("downscale_hazard_dir")) {
    devtools::load_all(quiet = TRUE)
  }
  downscale_hazard_dir(input_dir, mini_dir, factor = factor, overwrite = TRUE)
}

trySuppressWarnings <- function(expr) {
  suppressWarnings(try(expr, silent = TRUE))
}

list_hazard_files <- function() {
  sort(Sys.glob(file.path(get_hazards_dir(), "*.tif")))
}

hazard_factor_path <- function() {
  get_test_data_dir("damage_and_cost_factors.csv")
}

has_pkg <- function(pkg) {
  requireNamespace(pkg, quietly = TRUE)
}

# Skip slow tests unless explicitly requested
skip_slow_tests <- function() {
  skip_on_ci <- Sys.getenv("CI") != ""
  skip_slow <- Sys.getenv("SKIP_SLOW_TESTS", "TRUE") == "TRUE"
  
  if (skip_on_ci || skip_slow) {
    testthat::skip("Skipping slow test (set SKIP_SLOW_TESTS=FALSE to run)")
  }
}

# Time a test and skip if it takes too long (for development)
timed_test <- function(test_name, test_code, max_seconds = 60) {
  start_time <- Sys.time()
  result <- test_code
  elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  
  if (elapsed > max_seconds) {
    message(sprintf("Test '%s' took %.2f seconds (> %d seconds threshold)", 
                   test_name, elapsed, max_seconds))
  }
  
  result
}

# Helper to build baseline and shock datasets for scenario tests
create_baseline_and_shock <- function() {
  td <- get_test_data_dir()
  assets <- read_assets(td)
  companies <- read_companies(file.path(td, "user_input", "company.csv"))
  shocked <- assets
  shocked$share_of_economic_activity <- pmax(0, shocked$share_of_economic_activity * 0.9)
  list(baseline = assets, shocked = shocked, companies = companies)
}

# Helper to create yearly scenario data for testing
create_yearly_scenarios <- function() {
  yearly_baseline <- data.frame(
    asset = c("A1", "A1", "A2", "A2"),
    company = c("C1", "C1", "C1", "C1"),
    year = c(2025, 2026, 2025, 2026),
    baseline_revenue = c(1000, 1020, 800, 816),
    baseline_profit = c(100, 102, 80, 81.6)
  )
  
  yearly_shocked <- data.frame(
    asset = c("A1", "A1", "A2", "A2"),
    company = c("C1", "C1", "C1", "C1"),
    year = c(2025, 2026, 2025, 2026),
    shocked_revenue = c(1000, 979, 800, 783),
    shocked_profit = c(100, 97.9, 80, 78.3)
  )
  
  companies <- data.frame(
    company_name = "C1",
    revenues = 1000
  )
  
  list(baseline = yearly_baseline, shocked = yearly_shocked, companies = companies)
}

# Helper to create discounted asset data for company-level tests
create_discounted_assets <- function() {
  data.frame(
    asset = c("A1", "A1", "A2", "A2"),
    company = c("C1", "C1", "C1", "C1"),
    scenario = c("baseline", "shock", "baseline", "shock"),
    discounted_net_profit = c(100, 95, 80, 76)
  )
}
