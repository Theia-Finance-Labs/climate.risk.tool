# Script to time individual test files
library(devtools)

# Get all test files
test_files <- list.files("tests/testthat", pattern = "^test-.*\\.R$", full.names = TRUE)
test_files <- test_files[!grepl("_snaps|helper", test_files)] # Exclude snapshots and helpers

# Initialize results vector
results <- c()

cat("Running individual test files with timing...\n\n")

for (test_file in test_files) {
  test_name <- basename(test_file)

  # Time the test execution
  start_time <- Sys.time()

  tryCatch(
    {
      # Run the specific test file
      devtools::test_file(test_file)
      success <- TRUE
    },
    error = function(e) {
      cat(sprintf("ERROR in %s: %s\n", test_name, e$message))
      success <- FALSE
    }
  )

  end_time <- Sys.time()
  duration <- as.numeric(difftime(end_time, start_time, units = "secs"))

  # Store result
  results <- c(results, sprintf(
    "%-50s: %6.2f seconds %s",
    test_name,
    duration,
    ifelse(success, "✓", "✗")
  ))

  # Progress indicator
  cat(sprintf("Completed: %s (%.2fs)\n", test_name, duration))
}

# Sort by duration (slowest first)
durations <- as.numeric(sapply(strsplit(results, ":"), function(x) as.numeric(trimws(strsplit(x[2], "seconds")[[1]][1]))))
results_sorted <- results[order(durations, decreasing = TRUE)]

cat("\n\n=== TIMING RESULTS (Slowest First) ===\n")
for (result in results_sorted) {
  cat(result, "\n")
}

cat("\n=== SUMMARY ===\n")
total_time <- sum(durations)
cat(sprintf("Total time for all tests: %.2f seconds\n", total_time))
cat(sprintf("Number of test files: %d\n", length(test_files)))
cat(sprintf("Average time per test file: %.2f seconds\n", total_time / length(test_files)))

# Save results to file for reference
writeLines(results_sorted, "test_timing_results.txt")
cat("\nResults also saved to test_timing_results.txt\n")
