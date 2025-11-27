#' Load CSV hazards and build inventory
#'
#' @description
#' Scans for .csv files in the hazards directory, extracts metadata from the folder
#' structure (hazard_type, hazard_indicator, model_type) and from CSV columns (GWL,
#' return_period, ensemble). For each combination of GWL, return_period, and
#' ensemble value, creates a separate data frame.
#'
#' **CSV file structure:** Expected columns are `ensemble`, `GWL`, `return_period`,
#' `lat`, `lon`, `hazard_indicator`, `hazard_intensity`. Each row represents a
#' geolocated point with a hazard value.
#'
#' **Ensemble dimension:** Only the 'median' ensemble is loaded by default.
#' This avoids iteration over all ensemble values and provides a single representative
#' dataset per hazard scenario.
#'
#' Returns both the loaded data frames and a metadata inventory tibble.
#'
#' @param hazards_dir Character. Root directory that contains `hazards/` subfolders
#' @return List with two elements: `hazards` (named list of data frames) and
#'   `inventory` (tibble with hazard metadata)
#' @noRd
load_csv_hazards_with_metadata <- function(hazards_dir) {
  csv_files <- list.files(hazards_dir, pattern = "\\.csv$", full.names = TRUE, recursive = TRUE)
  if (length(csv_files) == 0) {
    return(list(
      hazards = list(),
      inventory = tibble::tibble()
    ))
  }

  results <- list()
  inventory_rows <- list()

  for (f in csv_files) {
    # Path parsing: {hazards_dir}/{hazard_type}/{hazard_indicator}/{model_type}/{file}.csv
    # Use relative path from hazards_dir for more robust parsing
    hazards_dir_norm <- normalizePath(hazards_dir, winslash = "/")
    f_norm <- normalizePath(f, winslash = "/")
    relative_path <- sub(paste0("^", hazards_dir_norm, "/"), "", f_norm)
    parts <- strsplit(relative_path, "/", fixed = TRUE)[[1]]

    if (length(parts) >= 4) {
      # hazards/{hazard_type}/{hazard_indicator}/{model_type}/file.csv
      hazard_type <- parts[1]
      hazard_indicator <- parts[2]
      model_type <- parts[3]
    } else if (length(parts) == 3) {
      # hazards/{hazard_type}/{hazard_indicator}/file.csv
      hazard_type <- parts[1]
      hazard_indicator <- parts[2]
      model_type <- "ensemble"
    } else if (length(parts) == 2) {
      # hazards/{hazard_type}/file.csv
      hazard_type <- parts[1]
      hazard_indicator <- "indicator"
      model_type <- "ensemble"
    } else {
      hazard_type <- "unknown"
      hazard_indicator <- "indicator"
      model_type <- "ensemble"
    }

    message("  Loading CSV hazard file: ", basename(f))

    # Read CSV file
    csv_data <- tryCatch(
      utils::read.csv(f, stringsAsFactors = FALSE),
      error = function(e) {
        warning("[load_csv_hazards_with_metadata] Failed to read CSV file: ", f, ". Error: ", e$message)
        return(NULL)
      }
    )

    if (is.null(csv_data)) {
      next
    }

    # Validate required columns
    required_cols <- c("ensemble", "GWL", "return_period", "lat", "lon", "hazard_indicator", "hazard_intensity")
    missing_cols <- setdiff(required_cols, names(csv_data))

    if (length(missing_cols) > 0) {
      warning(
        "[load_csv_hazards_with_metadata] CSV file '", basename(f),
        "' is missing required columns: ", paste(missing_cols, collapse = ", "),
        ". Skipping this file."
      )
      next
    }

    # Convert to tibble for easier manipulation
    csv_data <- tibble::as_tibble(csv_data)

    # Check if season column exists (for drought hazards)
    has_season <- "season" %in% names(csv_data)

    # Filter to only 'median' ensemble
    csv_data_median <- csv_data |>
      dplyr::filter(.data$ensemble == "median")

    if (nrow(csv_data_median) == 0) {
      message("    No 'median' ensemble found in CSV file, skipping: ", basename(f))
      next
    }

    # Get unique combinations of (GWL, return_period, ensemble, season)
    if (has_season) {
      unique_combos <- csv_data_median |>
        dplyr::distinct(.data$GWL, .data$return_period, .data$ensemble, .data$season)
    } else {
      unique_combos <- csv_data_median |>
        dplyr::distinct(.data$GWL, .data$return_period, .data$ensemble)
    }

    # Create a separate data frame for each combination
    for (i in seq_len(nrow(unique_combos))) {
      combo <- unique_combos |> dplyr::slice(i)
      gwl_val <- combo$GWL
      rp_val <- combo$return_period
      ens_val <- combo$ensemble
      season_val <- if (has_season) combo$season else NA_character_

      # Filter data for this combination
      if (has_season && !is.na(season_val)) {
        hazard_data <- csv_data_median |>
          dplyr::filter(
            .data$GWL == gwl_val,
            .data$return_period == rp_val,
            .data$ensemble == ens_val,
            .data$season == season_val
          )
      } else {
        hazard_data <- csv_data_median |>
          dplyr::filter(
            .data$GWL == gwl_val,
            .data$return_period == rp_val,
            .data$ensemble == ens_val
          )
      }

      # Create hazard name (unified format with ensemble suffix, include season if present)
      if (has_season && !is.na(season_val)) {
        hazard_name <- paste0(
          hazard_type, "__", hazard_indicator,
          "__GWL=", gwl_val,
          "__RP=", rp_val,
          "__season=", season_val,
          "__ensemble=", ens_val
        )
      } else {
        hazard_name <- paste0(
          hazard_type, "__", hazard_indicator,
          "__GWL=", gwl_val,
          "__RP=", rp_val,
          "__ensemble=", ens_val
        )
      }

      # Store the data frame
      results[[hazard_name]] <- hazard_data

      # Build inventory row
      rp_numeric <- suppressWarnings(as.numeric(rp_val))
      if (is.na(rp_numeric)) rp_numeric <- i

      # Include season column in inventory if present
      if (has_season && !is.na(season_val)) {
        inventory_rows[[length(inventory_rows) + 1]] <- tibble::tibble(
          hazard_type = hazard_type,
          hazard_indicator = hazard_indicator,
          scenario_name = as.character(gwl_val),
          hazard_return_period = rp_numeric,
          hazard_name = hazard_name,
          ensemble = as.character(ens_val),
          season = as.character(season_val),
          source = "csv"
        )
      } else {
        inventory_rows[[length(inventory_rows) + 1]] <- tibble::tibble(
          hazard_type = hazard_type,
          hazard_indicator = hazard_indicator,
          scenario_name = as.character(gwl_val),
          hazard_return_period = rp_numeric,
          hazard_name = hazard_name,
          ensemble = as.character(ens_val),
          season = NA_character_,
          source = "csv"
        )
      }
    }
  }

  # Combine inventory
  inventory <- if (length(inventory_rows) > 0) {
    dplyr::bind_rows(inventory_rows)
  } else {
    tibble::tibble(
      hazard_type = character(),
      hazard_indicator = character(),
      scenario_name = character(),
      hazard_return_period = numeric(),
      hazard_name = character(),
      ensemble = character(),
      season = character(),
      source = character()
    )
  }

  message("[load_csv_hazards_with_metadata] Loaded ", length(results), " CSV hazard datasets")

  return(list(
    hazards = results,
    inventory = inventory
  ))
}
