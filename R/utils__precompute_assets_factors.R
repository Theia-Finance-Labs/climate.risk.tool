#' Precompute assets factors for efficient risk analysis
#'
#' @title Precompute and cache assets factors for climate risk analysis
#' @description Precomputes the expensive geospatial operations (geolocation, hazard cutout, 
#'   summarization, and damage factor joining) and saves the results to disk. This allows
#'   the main compute_risk function to run much faster by loading precomputed data instead
#'   of performing these operations every time.
#'
#' @param assets Data frame containing asset information (from read_assets())
#' @param hazards Named list of SpatRaster objects (from load_hazards())
#' @param areas List containing municipalities and provinces named lists (from load_location_areas())
#' @param damage_factors Data frame with damage and cost factors, or path to CSV file
#' @param hazards_dir Character string specifying the hazards directory where precomputed results will be saved
#' @param progress_callback Optional function to call with progress updates (processed, total, message)
#' @param force_recompute Logical. If TRUE, recompute even if cached file exists (default: FALSE)
#'
#' @return Character string path to the saved precomputed assets factors file
#'
#' @details
#' The function performs the following operations:
#' 1. Checks if precomputed file exists and is complete
#' 2. If not complete or force_recompute=TRUE, performs geospatial operations:
#'    - Geolocates assets using lat/lon > municipality > province priority
#'    - Cuts out hazard values for each asset geometry
#'    - Summarizes hazards in long format (one row per asset-hazard combination)
#'    - Joins damage and cost factors
#' 3. Saves results to hazards_dir/assets_factors_precomputed.rds
#' 4. Returns path to saved file
#'
#' The precomputed file contains a data frame with all assets enriched with:
#' - Geometry and centroid information
#' - Hazard intensity values for each hazard scenario
#' - Damage and cost factors for each asset-hazard combination
#'
#' @examples
#' \dontrun{
#' # Load required data
#' base_dir <- system.file("tests_data", package = "climate.risk.tool")
#' assets <- read_assets(base_dir)
#' hazards <- load_hazards(file.path(base_dir, "hazards"))
#' areas <- load_location_areas(
#'   file.path(base_dir, "areas", "municipality"),
#'   file.path(base_dir, "areas", "province")
#' )
#' damage_factors <- read_damage_cost_factors(base_dir)
#'
#' # Precompute assets factors
#' precomputed_file <- precompute_assets_factors(
#'   assets = assets,
#'   hazards = hazards,
#'   areas = areas,
#'   damage_factors = damage_factors,
#'   hazards_dir = file.path(base_dir, "hazards")
#' )
#'
#' # Use in compute_risk
#' results <- compute_risk(
#'   assets = assets,
#'   companies = companies,
#'   precomputed_assets_factors = precomputed_file,
#'   events = events
#' )
#' }
#' @export
precompute_assets_factors <- function(assets,
                                    hazards,
                                    areas,
                                    damage_factors,
                                    hazards_dir,
                                    progress_callback = NULL,
                                    force_recompute = FALSE) {
  
  # Validate inputs
  if (!is.data.frame(assets) || nrow(assets) == 0) {
    stop("assets must be a non-empty data.frame (from read_assets())")
  }
  if (!is.list(hazards) || length(hazards) == 0) {
    stop("hazards must be a non-empty named list of SpatRaster objects (from load_hazards())")
  }
  if (!is.list(areas) || !all(c("municipalities", "provinces") %in% names(areas))) {
    stop("areas must be a list with 'municipalities' and 'provinces' elements (from load_location_areas())")
  }
  if (!dir.exists(hazards_dir)) {
    stop("hazards_dir must be an existing directory: ", hazards_dir)
  }
  
  # Handle damage_factors parameter (can be data.frame or path)
  if (is.character(damage_factors)) {
    if (!file.exists(damage_factors)) {
      stop("damage_factors file not found: ", damage_factors)
    }
    damage_factors_df <- read_damage_cost_factors(dirname(damage_factors))
  } else if (is.data.frame(damage_factors)) {
    damage_factors_df <- damage_factors
  } else {
    stop("damage_factors must be a data.frame or path to CSV file")
  }
  
  # Define output file path
  output_file <- file.path(hazards_dir, "assets_factors_precomputed.rds")
  
  # Check if precomputed file exists and is complete
  if (!force_recompute && file.exists(output_file)) {
    tryCatch({
      cached_data <- readRDS(output_file)
      
      # Check if cached data is complete by comparing dimensions
      expected_rows <- nrow(assets) * length(hazards)
      if (is.data.frame(cached_data) && nrow(cached_data) >= expected_rows) {
        if (!is.null(progress_callback)) {
          progress_callback(nrow(cached_data), nrow(cached_data), "Using cached assets factors")
        }
        message("✅ [precompute_assets_factors] Using cached assets factors (", nrow(cached_data), " records)")
        return(output_file)
      }
    }, error = function(e) {
      message("⚠️ [precompute_assets_factors] Cached file corrupted, recomputing: ", e$message)
    })
  }
  
  # Calculate total operations for progress tracking
  total_operations <- nrow(assets) * length(hazards)
  processed_operations <- 0
  
  if (!is.null(progress_callback)) {
    progress_callback(0, total_operations, "Starting assets factors computation...")
  }
  
  message("🔄 [precompute_assets_factors] Computing assets factors for ", nrow(assets), " assets and ", length(hazards), " hazards (", total_operations, " total operations)")
  
  # Perform the expensive geospatial operations
  start_time <- Sys.time()
  
  # Step 1: Geolocate assets
  if (!is.null(progress_callback)) {
    progress_callback(processed_operations, total_operations, "Geolocating assets...")
  }
  message("📍 [precompute_assets_factors] Geolocating assets...")
  assets_geo <- geolocate_assets(assets, hazards, areas$municipalities, areas$provinces)
  processed_operations <- processed_operations + nrow(assets)
  
  # Step 2: Cutout hazards
  if (!is.null(progress_callback)) {
    progress_callback(processed_operations, total_operations, "Extracting hazard values...")
  }
  message("🗺️ [precompute_assets_factors] Extracting hazard values...")
  assets_cut <- cutout_hazards(assets_geo, hazards)
  processed_operations <- processed_operations + nrow(assets)
  
  # Step 3: Summarize hazards (this is the most expensive step)
  if (!is.null(progress_callback)) {
    progress_callback(processed_operations, total_operations, "Summarizing hazards...")
  }
  message("📊 [precompute_assets_factors] Summarizing hazards...")
  assets_long <- summarize_hazards(assets_cut)
  processed_operations <- processed_operations + nrow(assets)
  
  # Step 4: Join damage cost factors
  if (!is.null(progress_callback)) {
    progress_callback(processed_operations, total_operations, "Joining damage cost factors...")
  }
  message("🔗 [precompute_assets_factors] Joining damage cost factors...")
  assets_with_factors <- join_damage_cost_factors(assets_long, damage_factors_df)
  processed_operations <- total_operations
  
  # Save results
  if (!is.null(progress_callback)) {
    progress_callback(processed_operations, total_operations, "Saving precomputed results...")
  }
  message("💾 [precompute_assets_factors] Saving precomputed results...")
  
  # Create backup if file exists
  if (file.exists(output_file)) {
    backup_file <- paste0(output_file, ".backup")
    file.copy(output_file, backup_file, overwrite = TRUE)
  }
  
  # Save new results
  saveRDS(assets_with_factors, output_file)
  
  # Remove backup if save was successful
  if (file.exists(paste0(output_file, ".backup"))) {
    file.remove(paste0(output_file, ".backup"))
  }
  
  elapsed_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  
  if (!is.null(progress_callback)) {
    progress_callback(processed_operations, total_operations, paste0("Completed in ", round(elapsed_time, 1), " seconds"))
  }
  
  message("✅ [precompute_assets_factors] Successfully precomputed assets factors (", nrow(assets_with_factors), " records) in ", round(elapsed_time, 1), " seconds")
  message("📁 [precompute_assets_factors] Saved to: ", output_file)
  
  return(output_file)
}

#' Load precomputed assets factors
#'
#' @title Load precomputed assets factors from file
#' @description Loads the precomputed assets factors data from the saved RDS file.
#'
#' @param precomputed_file Character string path to the precomputed assets factors file
#' @return Data frame with precomputed assets factors
#'
#' @details
#' This function loads the precomputed assets factors that were created by precompute_assets_factors().
#' The returned data frame contains all assets enriched with geospatial hazard data and damage/cost factors.
#'
#' @examples
#' \dontrun{
#' # Load precomputed assets factors
#' assets_factors <- load_precomputed_assets_factors("path/to/assets_factors_precomputed.rds")
#' }
#' @export
load_precomputed_assets_factors <- function(precomputed_file) {
  if (!file.exists(precomputed_file)) {
    stop("Precomputed assets factors file not found: ", precomputed_file)
  }
  
  tryCatch({
    assets_factors <- readRDS(precomputed_file)
    if (!is.data.frame(assets_factors)) {
      stop("Precomputed file does not contain a data.frame")
    }
    message("✅ [load_precomputed_assets_factors] Loaded ", nrow(assets_factors), " precomputed records")
    return(assets_factors)
  }, error = function(e) {
    stop("Failed to load precomputed assets factors: ", e$message)
  })
}
