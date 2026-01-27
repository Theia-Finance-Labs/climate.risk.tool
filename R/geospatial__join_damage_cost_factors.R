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

  # CRITICAL: Deduplicate input data by (asset, event_id, hazard_type, hazard_indicator)
  # This prevents many-to-many issues when the same indicator appears multiple times
  # for the same asset/event (e.g., from spatial + precomputed sources, or extraction bugs)
  assets_with_hazards <- assets_with_hazards |>
    dplyr::group_by(.data$asset, .data$event_id, .data$hazard_type, .data$hazard_indicator) |>
    dplyr::slice(1) |>
    dplyr::ungroup()

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

      base_table <- apply_intensity_matching(base_table, mapping_df, intensity_cols, mapping$intensity_match)

      # Ensure mapping keys are unique to avoid many-to-many joins
      # For numeric columns, take the mean. For character/other, take the first.
      # This prevents duplicate rows if the mapping table has multiple entries for the same join keys.
      mapping_df <- mapping_df |>
        dplyr::group_by(dplyr::across(dplyr::all_of(mapping_cols))) |>
        dplyr::summarize(
          dplyr::across(
            dplyr::setdiff(names(mapping_df), mapping_cols),
            ~ if (is.numeric(.x)) mean(.x, na.rm = TRUE) else dplyr::first(.x)
          ),
          .groups = "drop"
        )

      # Perform the join. We use many-to-one because we just deduplicated mapping_df.
      base_table <- dplyr::left_join(
        base_table,
        mapping_df,
        by = join_cols,
        relationship = "many-to-one"
      )
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
    primary_rows <- hazard_assets |>
      dplyr::slice(1)
  }

  # Use variable names from config if available, otherwise fallback to indicator keys
  indicator_cols <- vapply(names(hazard_config$indicators), function(k) {
    var <- hazard_config$indicators[[k]]$variable
    if (!is.null(var) && nzchar(var)) var else k
  }, character(1))
  
  # Also include indicator keys as fallback in case they are already in hazard_assets
  # but NOT in config variable names
  all_indicator_cols <- unique(c(names(hazard_config$indicators), indicator_cols))
  
  indicator_wide <- hazard_assets |>
    dplyr::select("asset", "event_id", dplyr::any_of(all_indicator_cols)) |>
    dplyr::group_by(.data$asset, .data$event_id) |>
    dplyr::summarize(
      dplyr::across(dplyr::any_of(all_indicator_cols), ~ mean(.x, na.rm = TRUE)),
      .groups = "drop"
    )

  # Get all indicator index dimensions dynamically
  index_indicator <- hazard_config$index_indicator
  if (is.null(index_indicator) || !nzchar(as.character(index_indicator))) {
    index_indicator <- primary_indicator
  }
  
  # Prepare base table from primary rows, keeping all non-indicator columns
  # CRITICAL: Ensure we deduplicate primary_rows by (asset, event_id) before proceeding
  # This prevents duplicate rows if primary_rows has multiple entries per asset/event
  base_table <- primary_rows |>
    dplyr::select(-dplyr::any_of(all_indicator_cols)) |>
    dplyr::distinct(.data$asset, .data$event_id, .keep_all = TRUE)

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
  if (length(intensity_cols) == 0 || is.null(match_type) || match_type == "exact") {
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

