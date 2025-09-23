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
  
  # Load test data to get asset geometries
  base_dir <- get_test_data_dir()
  res <- read_inputs(base_dir)
  hazards_full <- load_hazards(get_test_data_dir("hazards"))
  municipalities <- load_municipalities(file.path(base_dir, "areas", "municipality"))
  provinces <- load_provinces(file.path(base_dir, "areas", "province"))
  assets_geo <- geolocate_assets(res$assets, hazards_full, municipalities, provinces)
  
  # Generate mini hazards
  geoms <- sf::st_sf(geometry = assets_geo$geometry)
  generate_mini_hazards(hazards_dir = get_test_data_dir("hazards"), geometries_sf = geoms, 
                       buffer_m = 10000, out_dir = mini_dir, margin_degrees = 0.2, aggregate_factor = 100)
}

# Generate cropped mini hazard rasters around provided geometries while preserving
# both zero/empty and positive flood areas. Returns directory containing mini rasters.
# geometries_sf: sf object (polygons or points) in CRS of hazards (will be reprojected if needed)
generate_mini_hazards <- function(hazards_dir = get_hazards_dir(), geometries_sf, buffer_m = 5000, out_dir = NULL, margin_degrees = 0.2, aggregate_factor = 100) {
  if (is.null(out_dir)) {
    out_dir <- file.path(get_test_scratch_dir(), "mini_hazards")
  }
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  tif_files <- list.files(hazards_dir, pattern = "\\.tif$", full.names = TRUE)
  if (length(tif_files) == 0) stop("No .tif in hazards_dir")
  for (tif in tif_files) {
    r <- terra::rast(tif)
    # Reproject input geometries to raster CRS and compute centroids
    g <- sf::st_transform(geometries_sf, terra::crs(r))
    centroids <- sf::st_centroid(g)
    cp <- terra::vect(centroids)
    # Sample raster at centroids to find positive and zero/NA locations
    samp <- suppressWarnings(terra::extract(r, cp))
    vals <- as.numeric(samp[, 2])
    pos_idx <- which(vals > 0)
    empty_idx <- which(is.na(vals) | vals <= 0)
    # Default: use union of geometries if we couldn't find diversity
    use_union <- length(pos_idx) == 0 || length(empty_idx) == 0
    if (use_union) {
      gbuf <- sf::st_buffer(sf::st_union(sf::st_geometry(g)), dist = buffer_m)
      v <- terra::vect(gbuf)
      ext <- terra::ext(v)
      # Add a small degree margin
      ext <- terra::ext(
        terra::xmin(ext) - margin_degrees,
        terra::xmax(ext) + margin_degrees,
        terra::ymin(ext) - margin_degrees,
        terra::ymax(ext) + margin_degrees
      )
    } else {
      # Create small extent around one positive and one empty centroid
      pts <- centroids[c(pos_idx[1], empty_idx[1]), ]
      crd <- sf::st_coordinates(pts)
      xmin_ <- min(crd[, 1]) - margin_degrees
      xmax_ <- max(crd[, 1]) + margin_degrees
      ymin_ <- min(crd[, 2]) - margin_degrees
      ymax_ <- max(crd[, 2]) + margin_degrees
      ext <- terra::ext(xmin_, xmax_, ymin_, ymax_)
    }
    rc <- trySuppressWarnings(terra::crop(r, ext))
    if (inherits(rc, "try-error")) next
    # Coarsen to reduce number of cells for faster extraction
    if (aggregate_factor > 1) {
      rc <- terra::aggregate(rc, fact = aggregate_factor, fun = mean, na.rm = TRUE)
    }
    out_path <- file.path(out_dir, basename(tif))
    terra::writeRaster(rc, out_path, overwrite = TRUE)
  }
  out_dir
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
  res <- read_inputs(td)
  assets <- res$assets
  shocked <- assets
  shocked$share_of_economic_activity <- pmax(0, shocked$share_of_economic_activity * 0.9)
  list(baseline = assets, shocked = shocked, companies = res$companies)
}
