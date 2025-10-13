#' Load hazards (TIF + NC) and build complete inventory
#'
#' @title Load all hazard data and generate inventory
#' @description Self-contained loader that:
#' 1. Auto-discovers `hazards_metadata.csv` for TIF files (if present)
#' 2. Loads TIF rasters from mapping
#' 3. Scans directory tree for NetCDF files and loads them
#' 4. Generates a unified inventory combining TIF and NC metadata
#' 5. Returns both hazards and inventory
#'
#' @param hazards_dir Character. Root directory that contains hazard files and subdirectories
#' @param aggregate_factor Integer >= 1. Aggregation factor for TIF rasters (default: 1)
#' @return A list with two elements:
#'   - `hazards`: Nested list with `tif` and `nc` keys, each containing named SpatRaster objects
#'   - `inventory`: Tibble with columns: hazard_type, scenario_name, hazard_return_period, 
#'     scenario_code, hazard_name, source (either "tif" or "nc")
#' @examples
#' \dontrun{
#' result <- load_hazards_and_inventory(
#'   hazards_dir = file.path(base_dir, "hazards"),
#'   aggregate_factor = 1L
#' )
#' 
#' # Access hazards (flattened for compute)
#' all_hazards <- c(result$hazards$tif, result$hazards$nc)
#' 
#' # Access inventory (for UI dropdowns)
#' inventory <- result$inventory
#' }
#' @export
load_hazards_and_inventory <- function(hazards_dir, aggregate_factor = 1L) {
  message("[load_hazards_and_inventory] Starting hazard loading and inventory...")
  
  # TIF files require a mapping file - if no mapping exists, skip TIF loading entirely
  parent_dir <- dirname(hazards_dir)
  mapping_path <- file.path(parent_dir, "hazards_metadata.csv")
  
  tif_list <- list()
  tif_inventory <- tibble::tibble()
  
  if (file.exists(mapping_path)) {
    message("  Found TIF mapping at: ", mapping_path)
    message("  Attempting to load TIF hazards...")
    mapping_df <- read_hazards_mapping(mapping_path)
    
    tif_list <- load_tif_hazards(
      mapping_df = mapping_df,
      hazards_dir = hazards_dir,
      aggregate_factor = as.integer(aggregate_factor)
    )
    
    # Build TIF inventory only if we actually loaded TIF files
    if (length(tif_list) > 0) {
      # Extract the successfully loaded hazard types from tif_list names
      loaded_names <- names(tif_list)
      
      # Create inventory only for loaded files
      tif_inventory <- mapping_df |>
        dplyr::mutate(
          hazard_name = paste0(
            .data$hazard_type, "__",
            .data$scenario_code, "_h",
            .data$hazard_return_period, "glob"
          ),
          source = "tif"
        ) |>
        dplyr::filter(.data$hazard_name %in% loaded_names) |>
        dplyr::select(
          "hazard_type",
          "hazard_indicator",
          "scenario_name",
          "hazard_return_period",
          "scenario_code",
          "hazard_name",
          "source"
        )
    }
  } else {
    message("  No TIF mapping file found at: ", mapping_path)
    message("  Skipping TIF loading (mapping file required for TIF hazards)")
  }
  
  # Load NC files and build inventory
  nc_result <- load_nc_hazards_with_metadata(hazards_dir = hazards_dir)
  nc_list <- nc_result$hazards
  nc_inventory <- nc_result$inventory
  
  # Combine inventories
  inventory <- dplyr::bind_rows(tif_inventory, nc_inventory)
  
  message("[load_hazards_and_inventory] Complete: ", 
          length(tif_list), " TIF + ", length(nc_list), " NC hazards loaded")
  
  return(list(
    hazards = list(tif = tif_list, nc = nc_list),
    inventory = inventory
  ))
}
