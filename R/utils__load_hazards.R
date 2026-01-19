#' Load hazards (TIF + NC + CSV) and build complete inventory
#'
#' @title Load all hazard data and generate inventory
#' @description Self-contained loader that:
#' 1. Scans directory tree for TIF files (if `hazards_metadata.csv` is present)
#' 2. Scans directory tree for NetCDF files and loads them
#' 3. Scans directory tree for CSV files and loads them
#' 4. Validates no mixed file types (tif/nc/csv) in same leaf folder
#' 5. Generates a unified inventory with hazard metadata
#' 6. Returns both hazards and inventory
#'
#' @param hazards_dir Character path to hazards directory containing subdirectories with hazard files
#' @param aggregate_factor Integer >= 1. Aggregation factor for TIF and NetCDF rasters (default: `NULL`).
#'   When `NULL`, reads the `climate_risk_tool_nc_aggregate_factor` option (default: 1).
#'   Values > 1 spatially aggregate each raster on load so that tests can run with lower resolution.
#' @return A list with two elements:
#'   - `hazards`: Named list of SpatRaster objects (combined from all sources)
#'   - `inventory`: Tibble with columns: hazard_type, hazard_indicator, scenario_name,
#'     hazard_return_period, hazard_name (unified format),
#'     ensemble (ensemble variant), source ("tif", "nc", or "csv")
#' @examples
#' \dontrun{
#' result <- load_hazards_and_inventory(
#'   hazards_dir = file.path(base_dir, "hazards"),
#'   aggregate_factor = 1L
#' )
#'
#' # Access hazards (for compute pipeline)
#' all_hazards <- result$hazards
#'
#' # Access inventory (for UI dropdowns)
#' inventory <- result$inventory
#' }
#' @export
load_hazards_and_inventory <- function(hazards_dir, aggregate_factor = NULL) {
  message("[load_hazards_and_inventory] Starting hazard loading and inventory...")

  if (is.null(aggregate_factor)) {
    aggregate_factor <- getOption("climate_risk_tool_nc_aggregate_factor", 1L)
  }
  aggregate_factor <- as.integer(aggregate_factor)
  if (aggregate_factor < 1) {
    stop("aggregate_factor must be >= 1")
  }

  # Validate no mixed file types in same folder (at leaf directory level)
  validate_no_mixed_hazard_types(hazards_dir)

  # TIF files require a mapping file - if no mapping exists, skip TIF loading entirely
  parent_dir <- dirname(hazards_dir)
  mapping_path <- file.path(parent_dir, "hazards_metadata.csv")

  tif_list <- list()
  tif_inventory <- tibble::tibble(
    hazard_type = character(),
    hazard_indicator = character(),
    scenario_name = character(),
    hazard_return_period = numeric(),
    hazard_name = character(),
    ensemble = character(),
    season = character(),
    source = character()
  )

  if (file.exists(mapping_path)) {
    message("  Found TIF mapping at: ", mapping_path)
    message("  Attempting to load TIF hazards...")
    mapping_df <- read_hazards_mapping(mapping_path)

    tif_list <- load_tif_hazards(
      mapping_df = mapping_df,
      hazards_dir = hazards_dir,
      aggregate_factor = aggregate_factor
    )

    # Build TIF inventory only if we actually loaded TIF files
    if (length(tif_list) > 0) {
      tif_inventory <- mapping_df |>
        dplyr::mutate(
          # Unified format for inventory (WITH ensemble=mean for consistency)
          hazard_name = paste0(
            .data$hazard_type, "__", .data$hazard_indicator,
            "__GWL=", .data$scenario_name,
            "__RP=", .data$hazard_return_period,
            "__ensemble=mean"
          ),
          ensemble = "mean", # TIF has no pre-computed ensemble, default to mean
          season = NA_character_, # TIF has no season dimension
          source = "tif"
        ) |>
        dplyr::select(
          "hazard_type",
          "hazard_indicator",
          "scenario_name",
          "hazard_return_period",
          "hazard_name",
          "ensemble",
          "season",
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
    aggregate_factor = aggregate_factor
  )
  nc_list <- nc_result$hazards
  nc_inventory <- nc_result$inventory

  # Load CSV files and build inventory
  csv_result <- load_csv_hazards_with_metadata(hazards_dir = hazards_dir)
  csv_list <- csv_result$hazards
  csv_inventory <- csv_result$inventory

  # Combine hazards and inventory
  all_hazards <- c(tif_list, nc_list, csv_list)
  inventory <- dplyr::bind_rows(tif_inventory, nc_inventory, csv_inventory)

  message(
    "[load_hazards_and_inventory] Complete: ",
    length(tif_list), " TIF + ",
    length(nc_list), " NetCDF + ",
    length(csv_list), " CSV hazards loaded"
  )

  return(list(
    hazards = all_hazards,
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
  # Normalize hazards_dir
  hazards_dir <- normalizePath(hazards_dir, winslash = "/", mustWork = TRUE)

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
