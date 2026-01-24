#' Join mapping tables for hazards (internal)
#'
#' @param assets_with_hazards Data frame in long format with asset and hazard information
#'   including hazard_type, hazard_indicator, indicator-specific values, gwl, return_period, event_id
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

      join_cols <- unique(c(intensity_cols, hazard_cols, asset_cols))
      if (length(join_cols) == 0) {
        stop("Mapping '", mapping_key, "' has no join columns")
      }

      variables <- mapping$variables
      if (!is.null(variables) && length(variables) > 0) {
        keep_cols <- unique(c(join_cols, variables))
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

      missing_in_assets <- setdiff(join_cols, names(base_table))
      missing_in_mapping <- setdiff(join_cols, names(mapping_df))
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
      if ("gwl" %in% join_cols) {
        base_table <- base_table |>
          dplyr::mutate(gwl = as.character(.data$gwl))
        mapping_df <- mapping_df |>
          dplyr::mutate(gwl = as.character(.data$gwl))
      }

      base_table <- apply_intensity_matching(base_table, mapping_df, intensity_cols, mapping$intensity_match)

      base_table <- dplyr::left_join(
        base_table,
        mapping_df,
        by = join_cols,
        relationship = "many-to-many"
      )
    }

    results[[length(results) + 1]] <- base_table
  }

  if (length(results) == 0) {
    stop("No hazards joined with mapping tables")
  }

  return(dplyr::bind_rows(results))
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

  indicator_cols <- names(hazard_config$indicators)
  indicator_wide <- hazard_assets |>
    dplyr::select("asset", "event_id", dplyr::any_of(indicator_cols)) |>
    dplyr::group_by(.data$asset, .data$event_id) |>
    dplyr::summarize(
      dplyr::across(dplyr::any_of(indicator_cols), ~ mean(.x, na.rm = TRUE)),
      .groups = "drop"
    )

  base_cols <- c(
    "asset", "company", "latitude", "longitude", "municipality", "state",
    "asset_category", "asset_subtype", "size_in_m2", "share_of_economic_activity",
    "cnae", "hazard_name", "hazard_type", "hazard_indicator", "return_period",
    "gwl", "season", "ensemble", "source", "matching_method", "event_id", "event_year"
  )
  base_cols <- base_cols[base_cols %in% names(primary_rows)]

  base_table <- primary_rows |>
    dplyr::select(dplyr::all_of(base_cols)) |>
    dplyr::distinct()

  base_table <- dplyr::left_join(
    base_table,
    indicator_wide,
    by = c("asset", "event_id")
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

