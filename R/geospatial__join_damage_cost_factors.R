#' Join mapping tables for hazards (internal)
#'
#' @param assets_with_hazards Data frame in long format with asset and hazard information
#'   including hazard_type, hazard_indicator, indicator-specific values, scenario_name, return_period, event_id
#' @param hazard_configs Named list from load_hazards_and_inventory()$configs
#' @param hazards_dir Character path to hazards/config directory
#' @return Data frame with mapping columns joined
#' @noRd
join_damage_cost_factors <- function(assets_with_hazards, hazard_configs, hazards_dir) {
  if (is.null(assets_with_hazards) || nrow(assets_with_hazards) == 0) {
    stop("No hazard assets provided for mapping joins")
  }
  if (is.null(hazard_configs) || length(hazard_configs) == 0) {
    stop("hazard_configs is required for mapping joins")
  }
  if (is.null(hazards_dir) || !dir.exists(hazards_dir)) {
    stop("hazards_dir does not exist: ", hazards_dir)
  }

  mappings_dir <- file.path(dirname(hazards_dir), "mappings")
  if (!dir.exists(mappings_dir)) {
    stop("hazards mappings directory does not exist: ", mappings_dir)
  }

  # CRITICAL: Check for duplicates in input data by (asset, event_id, hazard_type, hazard_indicator)
  # Duplicates should NOT exist here. If they do, we stop to find the real bug.
  dups <- assets_with_hazards |>
    dplyr::count(.data$asset, .data$event_id, .data$hazard_type, .data$hazard_indicator) |>
    dplyr::filter(.data$n > 1)
  
  if (nrow(dups) > 0) {
    stop(
      "[join_damage_cost_factors] Detected ", nrow(dups), " duplicate asset/event/indicator combinations. ",
      "This indicates a bug earlier in the pipeline. ",
      "First duplicate: Asset=", dups$asset[1], ", Event=", dups$event_id[1], ", Indicator=", dups$hazard_indicator[1]
    )
  }

  results <- list()

  for (hazard_type in names(hazard_configs)) {
    hazard_assets <- assets_with_hazards |>
      dplyr::filter(.data$hazard_type == !!hazard_type)

    if (nrow(hazard_assets) == 0) {
      next
    }

    hazard_config <- hazard_configs[[hazard_type]]
    if (is.null(hazard_config$mappings) || length(hazard_config$mappings) == 0) {
      results[[length(results) + 1]] <- hazard_assets
      next
    }

    base_table <- build_indicator_wide(hazard_assets, hazard_config)

    for (mapping_key in names(hazard_config$mappings)) {
      mapping <- hazard_config$mappings[[mapping_key]]
      mapping_df <- read_hazard_mapping_table(mappings_dir, mapping$file)

      intensity_cols <- mapping$join$on_indicator_intensity
      hazard_cols <- mapping$join$on_indicator_index
      asset_cols <- mapping$join$on_assets

      join_cols <- c(intensity_cols, hazard_cols, asset_cols)
      # Remove duplicates but preserve names if possible
      if (length(join_cols) > 0) {
        # Keep first occurrence of each key-value pair
        join_cols <- join_cols[!duplicated(paste0(names(join_cols), "=", join_cols))]
      }

      # Join keys: names are columns in assets (base_table), values are columns in mapping_df
      # If unnamed, the same name is used for both.
      get_left_cols <- function(x) {
        nms <- names(x)
        if (is.null(nms)) return(as.character(x))
        ifelse(nms == "", as.character(x), nms)
      }

      mapping_cols <- unname(join_cols)
      asset_cols_to_check <- get_left_cols(join_cols)

      if (length(join_cols) == 0) {
        stop("Mapping '", mapping_key, "' has no join columns")
      }

      variables <- mapping$variables
      if (!is.null(variables) && length(variables) > 0) {
        keep_cols <- unique(c(mapping_cols, variables))
        missing_in_mapping <- setdiff(keep_cols, names(mapping_df))
        if (length(missing_in_mapping) > 0) {
          stop(
            "Missing selected columns in mapping '", mapping_key, "': ",
            paste(missing_in_mapping, collapse = ", ")
          )
        }
        mapping_df <- mapping_df |>
          dplyr::select(dplyr::all_of(keep_cols))
      }

      missing_in_assets <- setdiff(asset_cols_to_check, names(base_table))
      missing_in_mapping <- setdiff(mapping_cols, names(mapping_df))
      if (length(missing_in_assets) > 0) {
        stop("Missing join columns in assets for mapping '", mapping_key, "': ", paste(missing_in_assets, collapse = ", "))
      }
      if (length(missing_in_mapping) > 0) {
        stop("Missing join columns in mapping '", mapping_key, "': ", paste(missing_in_mapping, collapse = ", "))
      }

      if ("return_period" %in% join_cols) {
        base_table <- base_table |>
          dplyr::mutate(return_period = as.numeric(.data$return_period))
        mapping_df <- mapping_df |>
          dplyr::mutate(return_period = as.numeric(.data$return_period))
      }
      if ("scenario_name" %in% join_cols) {
        base_table <- base_table |>
          dplyr::mutate(scenario_name = as.character(.data$scenario_name))
        mapping_df <- mapping_df |>
          dplyr::mutate(scenario_name = as.character(.data$scenario_name))
      }

      # Save raw intensity values before matching/capping
      # This preserves the original extracted values in [indicator]_raw columns
      if (length(intensity_cols) > 0) {
        for (intensity_col in intensity_cols) {
          if (intensity_col %in% names(base_table)) {
            raw_col_name <- paste0(intensity_col, "_raw")
            base_table[[raw_col_name]] <- base_table[[intensity_col]]
          }
        }
      }
      
      base_table <- apply_intensity_matching(base_table, mapping_df, intensity_cols, mapping$intensity_match)

      # ========================================================================
      # CONFIG-DRIVEN TRANSFORMATIONS
      # Apply mapping asset fallbacks before joining
      # ========================================================================
      fallback_original_cols <- character()
      if (!is.null(mapping$assets_fallbacks) && length(mapping$assets_fallbacks) > 0) {
        left_by_right <- setNames(asset_cols_to_check, mapping_cols)
        mapping_value_sets <- lapply(mapping_cols, function(col) unique(mapping_df[[col]]))
        names(mapping_value_sets) <- mapping_cols

        for (fb_col in names(mapping$assets_fallbacks)) {
          if (!fb_col %in% mapping_cols) {
            next
          }
          left_col <- left_by_right[[fb_col]]
          if (!left_col %in% names(base_table)) {
            next
          }
          fb_def <- mapping$assets_fallbacks[[fb_col]]

          if (!is.null(fb_def$on_missing_or_unknown)) {
            fallback_val <- fb_def$on_missing_or_unknown
            in_mapping <- base_table[[left_col]] %in% mapping_value_sets[[fb_col]]
            missing_or_unknown <- is.na(base_table[[left_col]]) | !in_mapping
            if (any(missing_or_unknown)) {
              original_col <- paste0(left_col, "_original")
              if (!original_col %in% names(base_table)) {
                base_table[[original_col]] <- base_table[[left_col]]
                fallback_original_cols <- unique(c(fallback_original_cols, original_col))
              }
              base_table[[left_col]][missing_or_unknown] <- fallback_val
            }
          }

          # on_unmatched_combination intentionally unsupported
        }
      }

      # Perform the join.
      base_table <- dplyr::left_join(
        base_table,
        mapping_df,
        by = join_cols
      )

      if (all(c("cost_factor.x", "cost_factor.y") %in% names(base_table))) {
        base_table$cost_factor <- dplyr::coalesce(base_table$cost_factor.x, base_table$cost_factor.y)
        base_table$cost_factor.x <- NULL
        base_table$cost_factor.y <- NULL
      }
      
      # Apply defaults for variables that are NA after join
      if (!is.null(mapping$defaults) && length(mapping$defaults) > 0) {
        for (var_name in names(mapping$defaults)) {
          default_value <- mapping$defaults[[var_name]]
          if (var_name %in% names(base_table)) {
            # Fill NA values with the default
            na_mask <- is.na(base_table[[var_name]])
            if (any(na_mask)) {
              base_table[[var_name]][na_mask] <- default_value
              message("  [join_damage_cost_factors] Applied default value '", default_value, 
                      "' to ", sum(na_mask), " NA values in column '", var_name, "' for ", hazard_type)
            }
          }
        }
      }
      
      # Restore original values after join for any fallback columns
      if (length(fallback_original_cols) > 0) {
        for (original_col in fallback_original_cols) {
          restored_col <- sub("_original$", "", original_col)
          if (original_col %in% names(base_table) && restored_col %in% names(base_table)) {
            base_table[[restored_col]] <- base_table[[original_col]]
            base_table[[original_col]] <- NULL
          }
        }
      }
    }

    results[[length(results) + 1]] <- base_table
  }

  if (length(results) == 0) {
    stop("No hazards joined with mapping tables")
  }

  # When combining results from different hazards, we might have different columns
  # from different mapping tables. We use bind_rows which handles this.
  # Each hazard type has been processed separately, so we just combine them.
  combined <- dplyr::bind_rows(results)

  return(combined)
}

#' Build wide indicator table for mapping joins (internal)
#'
#' @param hazard_assets Long-format hazard assets for a single hazard type
#' @param hazard_config Hazard config list for the hazard type
#' @return Data frame with one row per asset/event and indicator columns
#' @noRd
build_indicator_wide <- function(hazard_assets, hazard_config) {
  
  primary_indicator <- hazard_config$primary_indicator
  primary_rows <- hazard_assets |>
    dplyr::filter(.data$hazard_indicator == primary_indicator)

  if (nrow(primary_rows) == 0) {
    message(
      "[build_indicator_wide] Primary indicator '", primary_indicator,
      "' not found in hazard_assets; falling back to all indicators."
    )
    primary_rows <- hazard_assets
  }

  # Use variable names from config if available, otherwise fallback to indicator keys
  indicator_cols <- vapply(names(hazard_config$indicators), function(k) {
    var <- hazard_config$indicators[[k]]$variable
    if (!is.null(var) && nzchar(var)) var else k
  }, character(1))
  
  # Also include indicator keys as fallback in case they are already in hazard_assets
  # but NOT in config variable names
  all_indicator_cols <- unique(c(names(hazard_config$indicators), indicator_cols))
  
  # Build wide indicator table explicitly per indicator to avoid losing values
  indicator_wide <- hazard_assets |>
    dplyr::select("asset", "event_id") |>
    dplyr::distinct()

  for (indicator_key in names(hazard_config$indicators)) {
    indicator_cfg <- hazard_config$indicators[[indicator_key]]
    indicator_var <- indicator_cfg$variable
    if (is.null(indicator_var) || !nzchar(indicator_var)) {
      indicator_var <- indicator_key
    }

    candidate_cols <- intersect(c(indicator_var, indicator_key), names(hazard_assets))
    if (length(candidate_cols) == 0) {
      next
    }

    # FIX: Use !! to force evaluation of loop variable in dplyr context
    # The issue was that .data$hazard_indicator == indicator_key wasn't capturing indicator_key correctly
    indicator_key_val <- indicator_key  # Capture loop variable
    indicator_rows <- hazard_assets |>
      dplyr::filter(.data$hazard_indicator == !!indicator_key_val)

    # Fallbacks: sometimes hazard_indicator uses variable name or indicator_key is stored in indicator_key column
    if (nrow(indicator_rows) == 0 && "hazard_indicator" %in% names(hazard_assets)) {
      indicator_var_val <- indicator_var  # Capture variable
      indicator_rows <- hazard_assets |>
        dplyr::filter(.data$hazard_indicator == !!indicator_var_val)
    }
    if (nrow(indicator_rows) == 0 && "indicator_key" %in% names(hazard_assets)) {
      pattern_val <- paste0("^", indicator_var, "__")  # Capture pattern
      indicator_rows <- hazard_assets |>
        dplyr::filter(grepl(!!pattern_val, .data$indicator_key))
    }
    
    if (nrow(indicator_rows) == 0) {
      next
    }
    
    indicator_vals <- indicator_rows |>
      dplyr::select("asset", "event_id", dplyr::any_of(c(candidate_cols, "hazard_intensity"))) |>
      dplyr::group_by(.data$asset, .data$event_id) |>
      dplyr::summarize(
        dplyr::across(dplyr::any_of(c(candidate_cols, "hazard_intensity")), function(x) {
          non_na <- x[!is.na(x)]
          unique_vals <- unique(non_na)
          if (length(unique_vals) > 1) {
            stop(
              "[build_indicator_wide] Multiple values for indicator '", indicator_key,
              "' and asset '", dplyr::first(.data$asset), "' in event '", dplyr::first(.data$event_id),
              "'. This indicates a bug earlier in the pipeline."
            )
          }
          if (length(unique_vals) == 0) NA else unique_vals[[1]]
        }),
        .groups = "drop"
      )

    # Normalize to the variable name used downstream (fallback to hazard_intensity)
    if (indicator_var %in% names(indicator_vals)) {
      indicator_vals <- indicator_vals |>
        dplyr::select("asset", "event_id", dplyr::all_of(indicator_var))
    } else if (indicator_key %in% names(indicator_vals)) {
      indicator_vals <- indicator_vals |>
        dplyr::rename(!!indicator_var := .data[[indicator_key]]) |>
        dplyr::select("asset", "event_id", dplyr::all_of(indicator_var))
    } else if ("hazard_intensity" %in% names(indicator_vals)) {
      indicator_vals <- indicator_vals |>
        dplyr::rename(!!indicator_var := .data$hazard_intensity) |>
        dplyr::select("asset", "event_id", dplyr::all_of(indicator_var))
    }

    indicator_wide <- dplyr::left_join(
      indicator_wide,
      indicator_vals,
      by = c("asset", "event_id")
    )
  }

  # Get all indicator index dimensions dynamically
  index_indicator <- hazard_config$index_indicator
  if (is.null(index_indicator) || !nzchar(as.character(index_indicator))) {
    index_indicator <- primary_indicator
  }
  
  # Prepare base table from primary rows, keeping all non-indicator columns
  # CRITICAL: Ensure primary_rows has exactly one entry per asset/event
  primary_dups <- primary_rows |>
    dplyr::count(.data$asset, .data$event_id) |>
    dplyr::filter(.data$n > 1)
  
  if (nrow(primary_dups) > 0) {
    stop(
      "[build_indicator_wide] Detected duplicate rows in primary indicator '", primary_indicator,
      "' for asset '", primary_dups$asset[1], "' in event '", primary_dups$event_id[1], "'. ",
      "This indicates a bug earlier in the pipeline."
    )
  }

  base_table <- primary_rows |>
    dplyr::select(-dplyr::any_of(all_indicator_cols))
  
  base_table <- dplyr::left_join(
    base_table,
    indicator_wide,
    by = c("asset", "event_id"),
    relationship = "one-to-one"
  )

  return(base_table)
}

#' Read hazard mapping table (internal)
#'
#' @param mappings_dir Character directory containing mapping tables
#' @param mapping_file Character filename for mapping table
#' @return Tibble with mapping data
#' @noRd
read_hazard_mapping_table <- function(mappings_dir, mapping_file) {
  table_path <- file.path(mappings_dir, mapping_file)
  if (!file.exists(table_path)) {
    stop("Mapping table not found: ", table_path)
  }

  ext <- tolower(tools::file_ext(table_path))
  if (ext == "csv") {
    return(readr::read_csv(table_path, show_col_types = FALSE) |> tibble::as_tibble())
  }
  if (ext %in% c("xlsx", "xls")) {
    return(readxl::read_excel(table_path) |> tibble::as_tibble())
  }

  stop("Unsupported mapping table extension: ", ext)
}

#' Apply intensity matching strategy (internal)
#'
#' @param asset_df Data frame with indicator columns
#' @param mapping_df Mapping table data frame
#' @param intensity_cols Character vector of intensity column names
#' @param match_type Character match type ("exact" or "closest")
#' @return Updated asset_df with intensity columns adjusted
#' @noRd
apply_intensity_matching <- function(asset_df, mapping_df, intensity_cols, match_type) {
  # If no intensity columns, nothing to do
  if (length(intensity_cols) == 0) {
    return(asset_df)
  }
  
  # Default to "closest" matching for continuous intensity values
  if (is.null(match_type)) {
    match_type <- "closest"
  }
  
  # If explicitly set to "exact", skip intensity matching
  if (match_type == "exact") {
    return(asset_df)
  }

  if (match_type != "closest") {
    stop("Unsupported intensity_match: ", match_type)
  }
  if (length(intensity_cols) != 1) {
    stop("closest intensity_match supports exactly one intensity column")
  }

  intensity_col <- intensity_cols[[1]]
  if (!intensity_col %in% names(asset_df) || !intensity_col %in% names(mapping_df)) {
    return(asset_df)
  }

  mapping_vals <- suppressWarnings(as.numeric(mapping_df[[intensity_col]]))
  mapping_vals <- sort(unique(mapping_vals[!is.na(mapping_vals)]))
  if (length(mapping_vals) == 0) {
    return(asset_df)
  }

  asset_vals <- suppressWarnings(as.numeric(asset_df[[intensity_col]]))
  closest_vals <- vapply(asset_vals, function(x) {
    if (is.na(x)) return(NA_real_)
    mapping_vals[which.min(abs(mapping_vals - x))]
  }, numeric(1))

  asset_df[[intensity_col]] <- closest_vals
  return(asset_df)
}
