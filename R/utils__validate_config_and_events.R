#' Validate mapping tables against hazard configs
#'
#' @param hazards_dir Character path to hazards/config directory
#' @param hazard_configs Named list of hazard configs
#' @param validation_results List with errors and warnings vectors
#' @return Updated validation_results list
#' @noRd
validate_mapping_tables_against_config <- function(hazards_dir, hazard_configs, validation_results) {
  if (is.null(hazards_dir) || !dir.exists(hazards_dir)) {
    validation_results$errors <- c(validation_results$errors, "hazards_dir does not exist")
    return(validation_results)
  }
  if (is.null(hazard_configs) || length(hazard_configs) == 0) {
    validation_results$errors <- c(validation_results$errors, "hazard_configs is empty")
    return(validation_results)
  }

  mappings_dir <- file.path(dirname(hazards_dir), "mappings")
  if (!dir.exists(mappings_dir)) {
    validation_results$errors <- c(validation_results$errors, "hazards mappings directory does not exist")
    return(validation_results)
  }

  for (hazard_type in names(hazard_configs)) {
    cfg <- hazard_configs[[hazard_type]]
    if (is.null(cfg$mappings) || length(cfg$mappings) == 0) {
      next
    }

    for (mapping_key in names(cfg$mappings)) {
      mapping <- cfg$mappings[[mapping_key]]
      if (is.null(mapping$file) || !nzchar(as.character(mapping$file))) {
        validation_results$errors <- c(
          validation_results$errors,
          paste0("Mapping '", mapping_key, "' for hazard '", hazard_type, "' has no file defined")
        )
        next
      }

      mapping_df <- read_hazard_mapping_table(mappings_dir, mapping$file)
      join_cols <- c(
        mapping$join$on_indicator_intensity,
        mapping$join$on_indicator_index,
        mapping$join$on_assets
      )
      required_cols <- unique(c(unname(join_cols), mapping$variables))
      missing_cols <- setdiff(required_cols, names(mapping_df))
      if (length(missing_cols) > 0) {
        validation_results$errors <- c(
          validation_results$errors,
          paste0(
            "Mapping '", mapping_key, "' for hazard '", hazard_type,
            "' missing columns: ", paste(missing_cols, collapse = ", ")
          )
        )
      }
    }
  }

  validation_results
}

#' Validate events contain required index columns from configs
#'
#' @param events_df Optional events data frame
#' @param hazard_configs Named list of hazard configs
#' @param validation_results List with errors and warnings vectors
#' @return Updated validation_results list
#' @noRd
validate_events_index_columns <- function(events_df, hazard_configs, validation_results) {
  if (is.null(events_df) || nrow(events_df) == 0) {
    return(validation_results)
  }
  if (is.null(hazard_configs) || length(hazard_configs) == 0) {
    validation_results$errors <- c(validation_results$errors, "hazard_configs is empty")
    return(validation_results)
  }
  if (!"hazard_type" %in% names(events_df)) {
    validation_results$errors <- c(validation_results$errors, "events_df must contain hazard_type column")
    return(validation_results)
  }

  hazard_types <- unique(events_df$hazard_type)
  for (hazard_type in hazard_types) {
    if (!hazard_type %in% names(hazard_configs)) {
      next
    }
    idx_indicator <- get_index_indicator(hazard_configs, hazard_type)
    if (is.na(idx_indicator) || is.null(idx_indicator)) {
      next
    }
    index_cols <- hazard_configs[[hazard_type]]$indicators[[idx_indicator]]$index
    if (length(index_cols) == 0) {
      next
    }
    missing_cols <- setdiff(index_cols, names(events_df))
    if (length(missing_cols) > 0) {
      validation_results$errors <- c(
        validation_results$errors,
        paste0(
          "Events table missing required index column(s) for hazard '",
          hazard_type, "': ", paste(missing_cols, collapse = ", ")
        )
      )
    }
  }

  validation_results
}

#' Validate events table for required columns and values
#'
#' @param events_df Events data frame
#' @param validation_results List with errors and warnings vectors
#' @return Updated validation_results list
#' @noRd
validate_events_table <- function(events_df, validation_results) {
  if (is.null(events_df)) {
    return(validation_results)
  }

  if (nrow(events_df) == 0) {
    validation_results$warnings <- c(
      validation_results$warnings,
      "Events table is empty"
    )
    return(validation_results)
  }

  # Required columns for events
  required_cols <- c("hazard_type", "hazard_name", "scenario_name", "return_period", "event_year")
  missing_cols <- setdiff(required_cols, names(events_df))

  if (length(missing_cols) > 0) {
    validation_results$errors <- c(
      validation_results$errors,
      paste0("Events table is missing required column(s): ", paste(missing_cols, collapse = ", "))
    )
    return(validation_results)
  }

  # Check for missing values in required columns
  for (col in required_cols) {
    if (any(is.na(events_df[[col]]))) {
      missing_idx <- which(is.na(events_df[[col]]))
      validation_results$errors <- c(
        validation_results$errors,
        paste0("Events table has missing values in column '", col, "' for rows: ", paste(missing_idx, collapse = ", "))
      )
    }
  }

  return(validation_results)
}
