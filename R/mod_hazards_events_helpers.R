#' @noRd
lookup_hazard_entry <- function(hazard_type_val, index_values, hazard_configs, hazards_inventory) {
  hazard_indicator_val <- get_index_indicator(hazard_configs, hazard_type_val)

  if (is.na(hazard_indicator_val)) {
    return(list(
      hazard_indicator = NA_character_,
      hazard_name = NA_character_
    ))
  }

  if (is.null(hazards_inventory) || nrow(hazards_inventory) == 0) {
    return(list(
      hazard_indicator = hazard_indicator_val,
      hazard_name = NA_character_
    ))
  }

  # Start with primary indicator rows
  filtered <- hazards_inventory |>
    dplyr::filter(
      .data$hazard_type == hazard_type_val,
      .data$hazard_indicator == hazard_indicator_val
    )

  # Dynamically filter by all provided index values
  for (idx_col in names(index_values)) {
    if (idx_col %in% names(filtered)) {
      val <- index_values[[idx_col]]
      # Handle numeric return_period
      if (idx_col == "return_period") val <- as.numeric(val)

      filtered <- filtered |>
        dplyr::filter(.data[[idx_col]] == !!val)
    }
  }

  hazard_name_val <- if (nrow(filtered) > 0) filtered$hazard_name[[1]] else NA_character_

  list(
    hazard_indicator = hazard_indicator_val,
    hazard_name = hazard_name_val
  )
}

#' @noRd
delete_event_by_id <- function(events_df, event_id) {
  if (is.null(events_df) || nrow(events_df) == 0) {
    return(events_df)
  }

  events_df |>
    dplyr::filter(.data$event_id != !!event_id)
}

#' @noRd
load_hazards_events_config <- function(file_path, hazard_configs, hazards_inventory) {
  if (is.null(file_path) || !file.exists(file_path)) {
    return(NULL)
  }

  uploaded <- try(readxl::read_excel(file_path), silent = TRUE)

  if (inherits(uploaded, "try-error") || is.null(uploaded) || nrow(uploaded) == 0) {
    message("[mod_hazards_events] Failed to read hazard configuration from external upload.")
    return(NULL)
  }

  # Check for required columns based on hazard types being loaded
  # We'll be flexible: if it has hazard_type and event_year, we try to resolve indices
  if (!all(c("hazard_type", "event_year") %in% names(uploaded))) {
    message("[mod_hazards_events] External configuration missing required columns: hazard_type, event_year")
    return(NULL)
  }

  # Rename 'gwl' to 'scenario_name' if present for backward compatibility
  if ("gwl" %in% names(uploaded) && !"scenario_name" %in% names(uploaded)) {
    uploaded <- uploaded |> dplyr::rename(scenario_name = "gwl")
  }

  processed <- tibble::as_tibble(uploaded) |>
    dplyr::mutate(
      hazard_type = as.character(.data$hazard_type),
      event_year = as.integer(.data$event_year)
    )

  # Convert all other potential index columns to character for now
  potential_indices <- setdiff(names(processed), c("hazard_type", "event_year", "event_id", "hazard_indicator", "hazard_name"))
  for (col in potential_indices) {
    if (col == "return_period") {
      processed[[col]] <- as.numeric(processed[[col]])
    } else {
      processed[[col]] <- as.character(processed[[col]])
    }
  }

  rows <- split(processed, seq_len(nrow(processed)))

  reconstructed <- purrr::imap_dfr(rows, function(row_df, idx) {
    hazard_type_val <- row_df$hazard_type[[1]]
    event_year_val <- row_df$event_year[[1]]

    # Get index configuration for this hazard type
    index_ind <- get_index_indicator(hazard_configs, hazard_type_val)
    index_cols <- if (!is.na(index_ind)) hazard_configs[[hazard_type_val]]$indicators[[index_ind]]$index else character(0)

    # Collect index values from the row
    index_values <- list()
    for (idx_col in index_cols) {
      # Handle gwl/scenario_name alias
      alt_col <- if (idx_col == "gwl") "scenario_name" else if (idx_col == "scenario_name") "gwl" else NULL

      if (idx_col %in% names(row_df)) {
        index_values[[idx_col]] <- row_df[[idx_col]][[1]]
      } else if (!is.null(alt_col) && alt_col %in% names(row_df)) {
        index_values[[idx_col]] <- row_df[[alt_col]][[1]]
      }
    }

    lookup <- lookup_hazard_entry(
      hazard_type_val = hazard_type_val,
      index_values = index_values,
      hazard_configs = hazard_configs,
      hazards_inventory = hazards_inventory
    )

    hazard_indicator_val <- if ("hazard_indicator" %in% names(row_df) &&
      !is.na(row_df$hazard_indicator[[1]]) &&
      nzchar(row_df$hazard_indicator[[1]])) {
      as.character(row_df$hazard_indicator[[1]])
    } else {
      lookup$hazard_indicator
    }

    hazard_name_val <- if ("hazard_name" %in% names(row_df) &&
      !is.na(row_df$hazard_name[[1]]) &&
      nzchar(row_df$hazard_name[[1]])) {
      as.character(row_df$hazard_name[[1]])
    } else {
      lookup$hazard_name
    }

    if (is.na(hazard_indicator_val) || is.na(hazard_name_val) || hazard_name_val == "") {
      message(
        "[mod_hazards_events] Skipping external upload row; unable to resolve hazard metadata for: ",
        hazard_type_val, " with indices ", paste(names(index_values), index_values, sep = "=", collapse = ", ")
      )
      return(tibble::tibble())
    }

    event_id_val <- if ("event_id" %in% names(row_df) &&
      !is.na(row_df$event_id[[1]]) &&
      nzchar(row_df$event_id[[1]])) {
      as.character(row_df$event_id[[1]])
    } else {
      paste0("ev", idx)
    }

    res_row <- tibble::tibble(
      event_id = event_id_val,
      hazard_type = hazard_type_val,
      hazard_indicator = hazard_indicator_val,
      hazard_name = hazard_name_val,
      event_year = as.integer(event_year_val)
    )

    # Add all index columns
    for (idx_col in index_cols) {
      val <- index_values[[idx_col]]
      if (idx_col == "return_period") val <- as.numeric(val)
      res_row[[idx_col]] <- val
    }

    # Backward compatibility columns
    if (!"season" %in% names(res_row)) {
      res_row$season <- if ("season" %in% names(row_df)) as.character(row_df$season[[1]]) else NA_character_
    }

    if (!"scenario_name" %in% names(res_row)) {
      if ("scenario_name" %in% names(row_df)) {
        res_row$scenario_name <- as.character(row_df$scenario_name[[1]])
      } else if ("gwl" %in% names(res_row)) {
        res_row$scenario_name <- as.character(res_row$gwl)
      } else {
        res_row$scenario_name <- NA_character_
      }
    }

    # Spatial separation columns (backward-compatible defaults)
    if ("spatial_level" %in% names(row_df) &&
      !is.na(row_df$spatial_level[[1]]) &&
      nzchar(as.character(row_df$spatial_level[[1]]))) {
      res_row$spatial_level <- tolower(as.character(row_df$spatial_level[[1]]))
    } else {
      res_row$spatial_level <- "brazil"
    }

    if ("spatial_region_codes" %in% names(row_df) &&
      !is.na(row_df$spatial_region_codes[[1]]) &&
      nzchar(as.character(row_df$spatial_region_codes[[1]]))) {
      res_row$spatial_region_codes <- as.character(row_df$spatial_region_codes[[1]])
    } else {
      res_row$spatial_region_codes <- NA_character_
    }

    if ("spatial_region_labels" %in% names(row_df) &&
      !is.na(row_df$spatial_region_labels[[1]]) &&
      nzchar(as.character(row_df$spatial_region_labels[[1]]))) {
      res_row$spatial_region_labels <- as.character(row_df$spatial_region_labels[[1]])
    } else {
      res_row$spatial_region_labels <- NA_character_
    }

    if ("spatial_scheme" %in% names(row_df) &&
      !is.na(row_df$spatial_scheme[[1]]) &&
      nzchar(as.character(row_df$spatial_scheme[[1]]))) {
      res_row$spatial_scheme <- tolower(as.character(row_df$spatial_scheme[[1]]))
    } else {
      res_row$spatial_scheme <- get_hazard_spatial_scheme(hazard_configs, hazard_type_val)
    }

    return(res_row)
  })

  if (nrow(reconstructed) == 0) {
    message("[mod_hazards_events] External configuration did not contain any valid hazard rows.")
    return(NULL)
  }

  reconstructed
}
