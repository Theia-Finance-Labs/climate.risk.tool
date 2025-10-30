#' Load hazards (TIF + NC + CSV) and build complete inventory
#'
#' @title Load all hazard data and generate inventory
#' @description Self-contained loader that:
#' 1. Auto-discovers `hazards_metadata.csv` for TIF files (if present)
#' 2. Loads TIF rasters from mapping
#' 3. Scans directory tree for NetCDF files and loads them
#' 4. Scans directory tree for CSV files and loads them
#' 5. Validates no mixed file types (tif/nc/csv) in same leaf folder
#' 6. Generates a unified inventory combining TIF, NC, and CSV metadata
#' 7. Returns both hazards and inventory
#'
#' @param hazards_dir Character path to hazards directory containing subdirectories with hazard files
#' @param aggregate_factor Integer >= 1. Aggregation factor for TIF and NC rasters (default: 1)
#' @return A list with three elements:
#'   - `hazards`: Nested list with `tif`, `nc`, and `csv` keys
#'   - `inventory`: Tibble with columns: hazard_type, hazard_indicator, scenario_name,
#'     hazard_return_period, scenario_code, hazard_name (unified format),
#'     ensemble (ensemble variant or NA for TIF), source ("tif", "nc", or "csv")
#' @examples
#' \dontrun{
#' result <- load_hazards_and_inventory(
#'   hazards_dir = file.path(base_dir, "hazards"),
#'   aggregate_factor = 1L
#' )
#'
#' # Access hazards (flattened for compute)
#' all_hazards <- c(result$hazards$tif, result$hazards$nc, result$hazards$csv)
#'
#' # Access inventory (for UI dropdowns)
#' inventory <- result$inventory
#' }
#' @export
load_hazards_and_inventory <- function(hazards_dir, aggregate_factor = 1L) {
  message("[load_hazards_and_inventory] Starting hazard loading and inventory...")

  # Validate no mixed file types in same folder (at leaf directory level)
  validate_no_mixed_hazard_types(hazards_dir)

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
      # Extract the successfully loaded hazard types from tif_list names (old format)
      loaded_names_old_format <- names(tif_list)

      # Create inventory with same structure as NC inventory
      # hazard_name is the unified format (WITHOUT ensemble suffix)
      # TIF files don't have ensemble dimension, so ensemble is NA (will be determined at extraction)
      tif_inventory <- mapping_df |>
        dplyr::mutate(
          # Unified format for inventory (WITHOUT ensemble suffix)
          hazard_name = paste0(
            .data$hazard_type, "__", .data$hazard_indicator,
            "__GWL=", .data$scenario_name,
            "__RP=", .data$hazard_return_period
          ),
          ensemble = NA_character_, # TIF has no pre-computed ensemble
          source = "tif"
        ) |>
        dplyr::select(
          "hazard_type",
          "hazard_indicator",
          "scenario_name",
          "hazard_return_period",
          "scenario_code",
          "hazard_name",
          "ensemble",
          "source"
        )
    }
  } else {
    message("  No TIF mapping file found at: ", mapping_path)
    message("  Skipping TIF loading (mapping file required for TIF hazards)")
  }

  # Load NC files and build inventory
  nc_result <- load_nc_hazards_with_metadata(
    hazards_dir = hazards_dir,
    aggregate_factor = as.integer(aggregate_factor)
  )
  nc_list <- nc_result$hazards
  nc_inventory <- nc_result$inventory

  # Load CSV files and build inventory
  csv_result <- load_csv_hazards_with_metadata(hazards_dir = hazards_dir)
  csv_list <- csv_result$hazards
  csv_inventory <- csv_result$inventory

  # Combine inventories
  inventory <- dplyr::bind_rows(tif_inventory, nc_inventory, csv_inventory)

  message(
    "[load_hazards_and_inventory] Complete: ",
    length(tif_list), " TIF + ", length(nc_list), " NC + ", length(csv_list), " CSV hazards loaded"
  )

  return(list(
    hazards = list(tif = tif_list, nc = nc_list, csv = csv_list),
    inventory = inventory
  ))
}

#' Validate no mixed hazard file types in same folder
#'
#' @description Checks that each leaf directory contains only one type of hazard
#'   file (.tif, .nc, or .csv). Mixed types in the same folder will raise an error.
#'
#' @param hazards_dir Character. Root directory to scan
#' @noRd
validate_no_mixed_hazard_types <- function(hazards_dir) {
  # Find all hazard files
  tif_files <- list.files(hazards_dir, pattern = "\\.tif$", full.names = TRUE, recursive = TRUE)
  nc_files <- list.files(hazards_dir, pattern = "\\.nc$", full.names = TRUE, recursive = TRUE)
  csv_files <- list.files(hazards_dir, pattern = "\\.csv$", full.names = TRUE, recursive = TRUE)

  # Get directories for each file type
  tif_dirs <- unique(dirname(tif_files))
  nc_dirs <- unique(dirname(nc_files))
  csv_dirs <- unique(dirname(csv_files))

  # Check for overlaps
  tif_nc_overlap <- intersect(tif_dirs, nc_dirs)
  tif_csv_overlap <- intersect(tif_dirs, csv_dirs)
  nc_csv_overlap <- intersect(nc_dirs, csv_dirs)

  mixed_dirs <- unique(c(tif_nc_overlap, tif_csv_overlap, nc_csv_overlap))

  if (length(mixed_dirs) > 0) {
    # Build detailed error message
    error_details <- character(0)
    for (dir in mixed_dirs) {
      types_found <- character(0)
      if (dir %in% tif_dirs) types_found <- c(types_found, "tif")
      if (dir %in% nc_dirs) types_found <- c(types_found, "nc")
      if (dir %in% csv_dirs) types_found <- c(types_found, "csv")

      error_details <- c(
        error_details,
        paste0("  - ", dir, ": found ", paste(types_found, collapse = ", "))
      )
    }

    stop(
      "Mixed hazard types detected in the same folder. ",
      "Each folder must contain only one hazard format (tif, nc, or csv).\n",
      "Folders with mixed types:\n",
      paste(error_details, collapse = "\n")
    )
  }
}
