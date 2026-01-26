#' Load hazards (TIF + NC) and build complete inventory
#'
#' @title Load all hazard data and generate inventory
#' @description Self-contained loader that:
#' 1. Reads hazard configs from `hazards/config/` (one YAML per hazard)
#' 2. Loads NetCDF indicators from `hazards/indicators/` root
#' 3. Loads TIF indicators from `hazards/indicators/<indicator_folder>/` using metadata.csv
#' 4. Generates a unified inventory with hazard metadata
#' 5. Returns both hazards and inventory
#'
#' @param hazards_dir Character path to hazards/config directory containing hazard YAML files
#' @param hazard_indicators_dir Character path to hazards/indicators directory
#' @param hazards_override_path Optional path to a config_overrides.yml file.
#'   When NULL, defaults to hazards_dir/config_overrides.yml. Missing files are ignored.
#' @param aggregate_factor Integer >= 1. Aggregation factor for TIF and NetCDF rasters (default: `NULL`).
#'   When `NULL`, reads the `climate_risk_tool_nc_aggregate_factor` option (default: 1).
#'   Values > 1 spatially aggregate each raster on load so that tests can run with lower resolution.
#' @return A list with two elements:
#'   - `hazards`: Named list of SpatRaster objects (combined from all sources)
#'   - `inventory`: Tibble with columns: hazard_type, hazard_indicator, scenario_name,
#'     return_period, hazard_name (unified format),
#'     ensemble (ensemble variant), source ("tif", "nc", or "csv")
#' @examples
#' \dontrun{
#' result <- load_hazards_and_inventory(
#'   hazards_dir = file.path(base_dir, "hazards", "config"),
#'   hazard_indicators_dir = file.path(base_dir, "hazards", "indicators"),
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
load_hazards_and_inventory <- function(
  hazards_dir,
  hazard_indicators_dir,
  hazards_override_path = NULL,
  aggregate_factor = NULL
) {
  message("[load_hazards_and_inventory] Starting hazard loading and inventory...")

  if (is.null(aggregate_factor)) {
    aggregate_factor <- getOption("climate_risk_tool_nc_aggregate_factor", 1L)
  }
  aggregate_factor <- as.integer(aggregate_factor)
  if (aggregate_factor < 1) {
    stop("aggregate_factor must be >= 1")
  }

  # Load hazard configs
  hazard_configs <- load_hazard_configs(
    hazards_dir = hazards_dir,
    hazards_override_path = hazards_override_path
  )

  tif_list <- list()
  tif_inventory <- tibble::tibble(
    hazard_type = character(),
    hazard_indicator = character(),
    scenario_name = character(),
    return_period = numeric(),
    hazard_name = character(),
    ensemble = character(),
    season = character(),
    source = character()
  )

  # Load indicators defined in hazard configs
  all_hazards <- list()
  inventory <- tibble::tibble()

  for (hazard_type in names(hazard_configs)) {
    hazard_config <- hazard_configs[[hazard_type]]
    for (indicator_key in names(hazard_config$indicators)) {
      indicator <- hazard_config$indicators[[indicator_key]]
      if (indicator$source == "nc") {
        indicator_path <- file.path(hazard_indicators_dir, indicator$file)
        nc_result <- load_nc_hazards_with_metadata(
          indicator_path = indicator_path,
          hazard_type = hazard_type,
          hazard_indicator = indicator_key,
          indicator_config = indicator,
          aggregate_factor = aggregate_factor
        )
        all_hazards <- c(all_hazards, nc_result$hazards)
        inventory <- dplyr::bind_rows(inventory, nc_result$inventory)
      } else if (indicator$source == "tif") {
        indicator_folder <- file.path(hazard_indicators_dir, indicator$file)
        mapping_path <- file.path(indicator_folder, "metadata.csv")
        if (!file.exists(mapping_path)) {
          message("  No TIF metadata file found at: ", mapping_path)
          next
        }
        message("  Found TIF metadata at: ", mapping_path)
        mapping_df <- read_hazards_mapping(mapping_path)

        tif_mapping <- mapping_df |>
          dplyr::filter(.data$hazard_indicator == indicator_key)

        # Handle optional hazard_type column in mapping
        if ("hazard_type" %in% names(tif_mapping)) {
          tif_mapping <- tif_mapping |>
            dplyr::filter(.data$hazard_type == !!hazard_type)
        } else {
          tif_mapping$hazard_type <- hazard_type
        }

        if (nrow(tif_mapping) == 0) {
          next
        }

        tif_list <- load_tif_hazards(
          mapping_df = tif_mapping,
          hazards_dir = indicator_folder,
          aggregate_factor = aggregate_factor
        )

        all_hazards <- c(all_hazards, tif_list)

        if (nrow(tif_mapping) > 0) {
          # Get index configuration for this hazard type
          index_cols <- indicator$index
          
          tif_inventory_rows <- tif_mapping |>
            dplyr::mutate(
              hazard_name = paste0(
                .data$hazard_type, "__", .data$hazard_indicator,
                "__scenario_name=", .data$scenario_name,
                "__RP=", .data$return_period,
                "__ensemble=mean"
              ),
              ensemble = "mean",
              season = NA_character_,
              source = "tif",
              agg = indicator$agg,
              categorical = indicator$categorical
            )
          
          # Ensure all index columns exist in the inventory
          for (idx_col in index_cols) {
            if (!idx_col %in% names(tif_inventory_rows)) {
               # If an index column is missing from TIF mapping, we might have an issue
               # but we'll try to fallback to standard names if they match
               if (idx_col == "gwl" && "scenario_name" %in% names(tif_inventory_rows)) {
                 tif_inventory_rows[[idx_col]] <- tif_inventory_rows$scenario_name
               } else {
                 tif_inventory_rows[[idx_col]] <- NA_character_
               }
            }
          }

          tif_inventory <- tif_inventory_rows |>
            dplyr::select(
              "hazard_type",
              "hazard_indicator",
              "hazard_name",
              "ensemble",
              "season",
              "source",
              "agg",
              "categorical",
              dplyr::any_of(c("scenario_name", "return_period", index_cols))
            )
          inventory <- dplyr::bind_rows(inventory, tif_inventory)
        }
      }
    }
  }

  message(
    "[load_hazards_and_inventory] Complete: ",
    length(all_hazards), " hazard layers loaded"
  )

  return(list(
    hazards = all_hazards,
    inventory = inventory,
    configs = hazard_configs
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
