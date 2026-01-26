#' Build a structured indicator key (implementation detail)
#' @noRd
build_indicator_key <- function(indicator_file, indicator_variable, index_values, ensemble = "mean") {
  file_part <- gsub("/+$", "", as.character(indicator_file))
  var_part <- as.character(indicator_variable)
  
  key <- paste0(file_part, "__", var_part)
  
  # Add index dimensions in specific order if they exist
  # Order: return_period -> gwl -> scenario_name -> season
  if (!is.null(index_values$return_period) && !is.na(index_values$return_period)) {
    val <- index_values$return_period
    if (is.numeric(val) && val == as.integer(val)) val <- as.integer(val)
    key <- paste0(key, "__return_period=", val)
  }
  
  if (!is.null(index_values$gwl) && !is.na(index_values$gwl)) {
    key <- paste0(key, "__gwl=", index_values$gwl)
  }
  
  if (!is.null(index_values$scenario_name) && !is.na(index_values$scenario_name)) {
    key <- paste0(key, "__scenario_name=", index_values$scenario_name)
  }
  
  if (!is.null(index_values$season) && !is.na(index_values$season)) {
    key <- paste0(key, "__season=", index_values$season)
  }
  
  if (!is.null(ensemble) && !is.na(ensemble) && ensemble != "" && ensemble != "NA") {
    key <- paste0(key, "__ensemble=", ensemble)
  }
  
  return(key)
}

#' Build a semantic hazard name (public identifier)
#' @noRd
build_hazard_name <- function(hazard_type, hazard_indicator, index_values, ensemble = "mean") {
  key <- paste0(hazard_type, "__", hazard_indicator)
  
  # Add index dimensions in specific order
  # Order: scenario_name -> return_period -> season
  # We prefer scenario_name over gwl for the semantic name
  
  scen <- if (!is.null(index_values$scenario_name) && !is.na(index_values$scenario_name)) {
    index_values$scenario_name
  } else if (!is.null(index_values$gwl) && !is.na(index_values$gwl)) {
    index_values$gwl
  } else {
    NULL
  }
  
  if (!is.null(scen)) {
    key <- paste0(key, "__", scen)
  }
  
  if (!is.null(index_values$return_period) && !is.na(index_values$return_period)) {
    val <- index_values$return_period
    if (is.numeric(val) && val == as.integer(val)) val <- as.integer(val)
    key <- paste0(key, "__", val)
  }
  
  if (!is.null(index_values$season) && !is.na(index_values$season)) {
    key <- paste0(key, "__", index_values$season)
  }
  
  if (!is.null(ensemble) && !is.na(ensemble) && ensemble != "" && ensemble != "NA") {
    key <- paste0(key, "__", ensemble)
  }
  
  return(key)
}

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

  normalize_indicator_file <- function(x) {
    x <- gsub("/+$", "", as.character(x))
    tools::file_path_sans_ext(x)
  }

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
      
      # Determine source based on file path: if it's a directory, it's TIF-based
      indicator_path <- file.path(hazard_indicators_dir, indicator$file)
      is_dir <- dir.exists(indicator_path)
      
      if (indicator$source == "nc" && !is_dir) {
        nc_result <- load_nc_hazards_with_metadata(
          indicator_path = indicator_path,
          hazard_type = hazard_type,
          hazard_indicator = indicator_key,
          indicator_config = indicator,
          aggregate_factor = aggregate_factor
        )
        all_hazards <- c(all_hazards, nc_result$hazards)
        inventory <- dplyr::bind_rows(inventory, nc_result$inventory)
      } else if (indicator$source == "tif" || is_dir) {
        indicator_folder <- indicator_path
        mapping_path <- file.path(indicator_folder, "metadata.csv")
        
        if (!file.exists(mapping_path)) {
          message("  No TIF metadata file found at: ", mapping_path)
          next
        }
        message("  Found TIF metadata at: ", mapping_path)
        mapping_df <- read_hazards_mapping(mapping_path)

        tif_mapping <- mapping_df |>
          dplyr::filter(.data$hazard_indicator == indicator_key)

        # Fallback: if no rows match indicator_key, try matching by hazard_type
        # this handles cases where metadata.csv uses a generic indicator name (like 'depth')
        # while the config uses a more specific one (like 'flood_depth')
        if (nrow(tif_mapping) == 0 && "hazard_type" %in% names(mapping_df)) {
          tif_mapping <- mapping_df |>
            dplyr::filter(.data$hazard_type == !!hazard_type)
          if (nrow(tif_mapping) > 0) {
            # Update hazard_indicator to match config for consistency in inventory
            tif_mapping$hazard_indicator <- indicator_key
          }
        }

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

        variable_fallback <- if (!is.null(indicator$variable) && nzchar(indicator$variable)) {
          indicator$variable
        } else {
          NA_character_
        }
        if (!"variable" %in% names(tif_mapping)) {
          tif_mapping$variable <- variable_fallback
        } else {
          tif_mapping$variable <- dplyr::coalesce(tif_mapping$variable, variable_fallback)
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
            ensemble = "mean",
            season = NA_character_,
            source = "tif",
            agg = indicator$agg,
            categorical = indicator$categorical,
            variable = indicator$variable
          )
        
        # Build structured hazard_name for each row
        tif_inventory_rows$indicator_key <- purrr::map_chr(seq_len(nrow(tif_inventory_rows)), function(j) {
          row <- tif_inventory_rows[j, ]
          index_values <- list(
            return_period = if ("return_period" %in% names(row)) row$return_period else NA_real_,
            gwl = if ("gwl" %in% names(row)) row$gwl else NA_character_,
            scenario_name = if ("scenario_name" %in% names(row)) row$scenario_name else NA_character_,
            season = if ("season" %in% names(row)) row$season else NA_character_
          )
          
          build_indicator_key(
            indicator_file = basename(indicator_folder),
            indicator_variable = dplyr::coalesce(row$variable, row$hazard_indicator),
            index_values = index_values,
            ensemble = "mean"
          )
        })

        # Build structured hazard_name (semantic)
        tif_inventory_rows$hazard_name <- purrr::map_chr(seq_len(nrow(tif_inventory_rows)), function(j) {
          row <- tif_inventory_rows[j, ]
          index_values <- list(
            return_period = if ("return_period" %in% names(row)) row$return_period else NA_real_,
            gwl = if ("gwl" %in% names(row)) row$gwl else NA_character_,
            scenario_name = if ("scenario_name" %in% names(row)) row$scenario_name else NA_character_,
            season = if ("season" %in% names(row)) row$season else NA_character_
          )
          
          build_hazard_name(
            hazard_type = hazard_type,
            hazard_indicator = indicator_key,
            index_values = index_values,
            ensemble = "mean"
          )
        })
        
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
            "indicator_key",
            "ensemble",
            "season",
            "source",
            "agg",
            "categorical",
            "variable",
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

  indicator_registry <- lapply(names(hazard_configs), function(hazard_type) {
    hazard_config <- hazard_configs[[hazard_type]]
    lapply(names(hazard_config$indicators), function(indicator_key) {
      indicator <- hazard_config$indicators[[indicator_key]]
      tibble::tibble(
        hazard_type = hazard_type,
        hazard_indicator = indicator_key,
        indicator_file = indicator$file,
        indicator_variable = indicator$variable,
        indicator_file_key = normalize_indicator_file(indicator$file)
      )
    }) |>
      dplyr::bind_rows()
  }) |>
    dplyr::bind_rows()

  inventory <- inventory |>
    dplyr::left_join(
      indicator_registry,
      by = c("hazard_type", "hazard_indicator")
    )

  if (!"indicator_variable" %in% names(inventory)) {
    inventory$indicator_variable <- NA_character_
  }

  inventory <- inventory |>
    dplyr::mutate(
      indicator_variable = dplyr::coalesce(.data$indicator_variable, .data$variable, .data$hazard_indicator),
      season = dplyr::na_if(as.character(.data$season), "NA"),
      ensemble = dplyr::na_if(as.character(.data$ensemble), "NA"),
      ensemble = dplyr::na_if(.data$ensemble, ""),
      ensemble = dplyr::coalesce(.data$ensemble, "mean")
    )

  # Ensure hazard_key is available (for backward compatibility if needed, but we use indicator_key)
  inventory <- inventory |>
    dplyr::mutate(hazard_key = .data$indicator_key)

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
