# Test Fixtures and Helpers for locating test data and common checks

# Helpers for locating test data and common checks
get_test_data_dir <- function(...) {
  # Resolve absolute path to tests/tests_data reliably in all environments (including R CMD check)
  testthat_dir <- testthat::test_path()
  tests_dir <- normalizePath(file.path(testthat_dir, ".."), winslash = "/", mustWork = TRUE)
  base_dir <- normalizePath(file.path(tests_dir, "tests_data"), winslash = "/", mustWork = TRUE)
  file.path(base_dir, ...)
}


get_hazards_dir <- function() {
  get_test_data_dir("hazards")
}

trySuppressWarnings <- function(expr) {
  suppressWarnings(try(expr, silent = TRUE))
}

# Skip slow tests unless explicitly requested
skip_slow_tests <- function() {
  skip_slow <- Sys.getenv("SKIP_SLOW_TESTS", "TRUE") == "TRUE"

  if (skip_slow) {
    testthat::skip("Skipping slow test (set SKIP_SLOW_TESTS=FALSE to run)")
  }
}

# Time a test and skip if it takes too long (for development)
timed_test <- function(test_name, test_code, max_seconds = 60) {
  start_time <- Sys.time()
  result <- test_code
  elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))

  if (elapsed > max_seconds) {
    message(sprintf(
      "Test '%s' took %.2f seconds (> %d seconds threshold)",
      test_name, elapsed, max_seconds
    ))
  }

  result
}


# Helper to create yearly scenario data for testing
create_yearly_scenarios <- function() {
  yearly_baseline <- data.frame(
    asset = c("A1", "A1", "A2", "A2"),
    company = c("C1", "C1", "C1", "C1"),
    year = c(2025, 2026, 2025, 2026),
    revenue = c(1000, 1020, 800, 816),
    profit = c(100, 102, 80, 81.6)
  )

  yearly_shocked <- data.frame(
    asset = c("A1", "A1", "A2", "A2"),
    company = c("C1", "C1", "C1", "C1"),
    year = c(2025, 2026, 2025, 2026),
    revenue = c(1000, 979, 800, 783),
    profit = c(100, 97.9, 80, 78.3)
  )

  companies <- data.frame(
    company = "C1",
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
