# Process Flood Maps for Brazil
# This script reads global flood maps from tests/tests_data/hazards_world/flood/
# and extracts data for Brazil using administrative boundaries,
# then saves the processed files to tests/tests_data/hazards/

# Set encoding to UTF-8
Sys.setlocale("LC_ALL", "en_US.UTF-8")

# Load required libraries
suppressPackageStartupMessages({
  library(terra)
  library(sf)
  library(dplyr)
})

# Define paths relative to project root
# Use getwd() if here package is not available
if (requireNamespace("here", quietly = TRUE)) {
  project_root <- here::here()
} else {
  project_root <- getwd()
}

hazards_world_dir <- file.path(project_root, "workspace", "hazards_world")
brazil_boundaries_file <- file.path(project_root, "tests", "tests_data", "areas", "province", "geoBoundaries-BRA-ADM1_simplified.geojson")
output_base_dir <- file.path(project_root, "tests", "tests_data", "hazards")

# Create output base directory if it doesn't exist
if (!dir.exists(output_base_dir)) {
  dir.create(output_base_dir, recursive = TRUE)
  cat("Created output base directory:", output_base_dir, "\n")
}

# Check if input directories exist
if (!dir.exists(hazards_world_dir)) {
  stop("Input directory not found: ", hazards_world_dir)
}

if (!file.exists(brazil_boundaries_file)) {
  stop("Brazil boundaries file not found: ", brazil_boundaries_file)
}

# Read Brazil administrative boundaries (ADM1 - States)
cat("Loading Brazil boundaries from:", brazil_boundaries_file, "\n")
brazil_boundaries <- sf::st_read(brazil_boundaries_file, quiet = TRUE)

# Print information about the boundaries
cat("Brazil boundaries loaded:\n")
cat("- Number of administrative units:", nrow(brazil_boundaries), "\n")
cat("- CRS:", sf::st_crs(brazil_boundaries)$input, "\n")
cat("- Bounding box:\n")
print(sf::st_bbox(brazil_boundaries))

# Create a union of all Brazilian states for efficient processing
brazil_union <- sf::st_union(brazil_boundaries)
cat("- Created union of all Brazilian territories\n")

# Get list of all .tif files recursively, maintaining relative paths
flood_files <- list.files(hazards_world_dir, pattern = "\\.tif$", full.names = TRUE, recursive = TRUE)
# Also get relative paths for maintaining directory structure
flood_files_rel <- list.files(hazards_world_dir, pattern = "\\.tif$", full.names = FALSE, recursive = TRUE)

# Sort so aggregated files (with __agg128) are processed first
order_idx <- order(grepl("__agg128", flood_files), decreasing = TRUE)
flood_files <- flood_files[order_idx]
flood_files_rel <- flood_files_rel[order_idx]

if (length(flood_files) == 0) {
  stop("No .tif files found in: ", hazards_world_dir)
}

cat("\nFound", length(flood_files), "hazard map files:\n")
for (i in seq_along(flood_files)) {
  cat("-", flood_files_rel[i], "\n")
}

# Process each flood map
cat("\n=== Processing Hazard Maps ===\n")

for (i in seq_along(flood_files)) {
  flood_file <- flood_files[i]
  flood_file_rel <- flood_files_rel[i]
  
  cat("\nProcessing:", flood_file_rel, "\n")
  
  # Generate output filename maintaining directory structure
  base_name <- tools::file_path_sans_ext(basename(flood_file))
  output_subdir <- file.path(output_base_dir, dirname(flood_file_rel))
  output_file <- file.path(output_subdir, paste0(base_name, "_brazil.tif"))
  
  # Create output subdirectory if it doesn't exist
  if (!dir.exists(output_subdir)) {
    dir.create(output_subdir, recursive = TRUE)
    cat("- Created output directory:", output_subdir, "\n")
  }
  
  # Skip if file already exists
  if (file.exists(output_file)) {
    cat("- Output file already exists, skipping:", basename(output_file), "\n")
    next
  }
  
  tryCatch({
    # Read the flood map raster
    flood_raster <- terra::rast(flood_file)
    
    # Print raster information
    cat("- Original raster dimensions:", nrow(flood_raster), "x", ncol(flood_raster), "\n")
    cat("- Original CRS:", terra::crs(flood_raster), "\n")
    cat("- Original extent:\n")
    print(terra::ext(flood_raster))
    
    # Transform Brazil boundaries to match the raster CRS
    brazil_transformed <- sf::st_transform(brazil_union, terra::crs(flood_raster))
    
    # Get bounding box for efficient cropping
    brazil_bbox <- sf::st_bbox(brazil_transformed)
    cat("- Brazil bounding box in raster CRS:\n")
    print(brazil_bbox)
    
    # Crop the raster to Brazil's bounding box first (for efficiency)
    flood_cropped <- terra::crop(flood_raster, brazil_bbox)
    cat("- Cropped raster dimensions:", nrow(flood_cropped), "x", ncol(flood_cropped), "\n")
    
    # Mask the raster using Brazil's boundaries
    flood_brazil <- terra::mask(flood_cropped, terra::vect(brazil_transformed))
    
    # Generate output filename (already defined above)
    # base_name and output_file already set
    
    # Write the clipped raster
    terra::writeRaster(flood_brazil, output_file, overwrite = TRUE)
    
    cat("- Output saved to:", basename(output_file), "\n")
    cat("- Final raster dimensions:", nrow(flood_brazil), "x", ncol(flood_brazil), "\n")
    
    # Calculate some basic statistics
    flood_values <- terra::values(flood_brazil)
    flood_values <- flood_values[!is.na(flood_values)]
    
    if (length(flood_values) > 0) {
      cat("- Statistics for Brazil:\n")
      cat("  - Min value:", min(flood_values), "\n")
      cat("  - Max value:", max(flood_values), "\n")
      cat("  - Mean value:", round(mean(flood_values), 4), "\n")
      cat("  - Non-zero cells:", sum(flood_values > 0), "\n")
      cat("  - Total valid cells:", length(flood_values), "\n")
      cat("  - Percentage of non-zero cells:", round(100 * sum(flood_values > 0) / length(flood_values), 2), "%\n")
    } else {
      cat("- Warning: No valid data found in Brazil region\n")
    }
    
    # Calculate file size reduction
    original_size <- file.size(flood_file) / (1024 * 1024) # Size in MB
    processed_size <- file.size(output_file) / (1024 * 1024) # Size in MB
    reduction_percent <- round(100 * (1 - processed_size / original_size), 1)
    
    cat("- File size: ", sprintf("%.2f MB", original_size), " -> ", sprintf("%.2f MB", processed_size), 
        " (", reduction_percent, "% reduction)\n", sep = "")
    
  }, error = function(e) {
    cat("ERROR processing", basename(flood_file), ":", e$message, "\n")
  })
}

cat("\n=== Processing Complete ===\n")
cat("Output files saved in:", output_base_dir, "\n")

# List the output files recursively
output_files <- list.files(output_base_dir, pattern = "\\.tif$", full.names = TRUE, recursive = TRUE)
output_files_rel <- list.files(output_base_dir, pattern = "\\.tif$", full.names = FALSE, recursive = TRUE)

if (length(output_files) > 0) {
  cat("\nGenerated files:\n")
  total_size <- 0
  for (i in seq_along(output_files)) {
    file_size <- file.size(output_files[i]) / (1024 * 1024) # Size in MB
    total_size <- total_size + file_size
    cat("-", output_files_rel[i], sprintf(" (%.2f MB)\n", file_size))
  }
  cat("Total size of processed files:", sprintf("%.2f MB\n", total_size))
} else {
  cat("\nNo output files were generated.\n")
}

cat("\nScript completed successfully!\n")
