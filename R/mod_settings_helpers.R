#' @noRd
safe_id <- function(value) {
  gsub("[^A-Za-z0-9_]", "_", value)
}

#' @noRd
drop_nulls <- function(values) {
  if (!is.list(values)) {
    return(values)
  }
  values[!vapply(values, is.null, logical(1))]
}

#' @noRd
get_mapping_preview <- function(base_dir, mapping_cfg) {
  if (is.null(base_dir) || !nzchar(base_dir)) {
    return(NULL)
  }
  if (is.null(mapping_cfg) || is.null(mapping_cfg$file)) {
    return(NULL)
  }

  mapping_path <- file.path(base_dir, "hazards", "mappings", mapping_cfg$file)
  if (!file.exists(mapping_path)) {
    return(NULL)
  }

  ext <- tolower(tools::file_ext(mapping_path))
  df <- tryCatch({
    if (ext == "csv") {
      readr::read_csv(mapping_path, n_max = 100, show_col_types = FALSE)
    } else if (ext %in% c("xls", "xlsx")) {
      readxl::read_excel(mapping_path, n_max = 100)
    } else {
      NULL
    }
  }, error = function(e) NULL)

  if (is.null(df) || nrow(df) == 0) {
    return(NULL)
  }

  # Identify join keys
  join_cfg <- mapping_cfg$join
  key_cols <- character(0)
  if (!is.null(join_cfg)) {
    key_cols <- unique(c(
      join_cfg$on_indicator_intensity,
      join_cfg$on_indicator_index,
      join_cfg$on_assets
    ))
  }

  if (length(key_cols) == 0) {
    return(NULL)
  }

  # Filter to keys that actually exist in the file
  key_cols <- intersect(key_cols, names(df))
  if (length(key_cols) == 0) {
    return(NULL)
  }

  # Get unique combinations of keys
  preview_df <- unique(df[key_cols])
  
  # Limit to first few combinations to keep it clean
  max_preview <- 10
  if (nrow(preview_df) > max_preview) {
    preview_df <- head(preview_df, max_preview)
    has_more <- TRUE
  } else {
    has_more <- FALSE
  }

  # Create a small tabulated view
  shiny::div(
    style = "margin-top: 8px; font-size: 0.85em; color: #4b5563;",
    shiny::div(
      style = "font-weight: 600; margin-bottom: 4px; color: #374151;",
      "Join keys preview (unique values):"
    ),
    shiny::tags$table(
      style = "width: 100%; border-collapse: collapse; background: #f8fafc; border: 1px solid #e2e8f0; border-radius: 4px; overflow: hidden;",
      shiny::tags$thead(
        shiny::tags$tr(
          style = "background: #f1f5f9; border-bottom: 1px solid #e2e8f0;",
          lapply(key_cols, function(col) {
            shiny::tags$th(style = "padding: 4px 8px; text-align: left; font-weight: 600;", col)
          })
        )
      ),
      shiny::tags$tbody(
        lapply(seq_len(nrow(preview_df)), function(i) {
          shiny::tags$tr(
            style = if (i %% 2 == 0) "background: #f8fafc;" else "background: #ffffff;",
            lapply(key_cols, function(col) {
              shiny::tags$td(style = "padding: 2px 8px; border-bottom: 1px solid #f1f5f9;", as.character(preview_df[i, col]))
            })
          )
        })
      )
    ),
    if (has_more) {
      shiny::div(
        style = "margin-top: 2px; font-style: italic; color: #94a3b8; font-size: 0.9em;",
        "... and more unique combinations"
      )
    }
  )
}

#' @noRd
name_eq <- function(x, opts) any(tolower(x) == tolower(opts))

#' @noRd
normalize_indexed_dim <- function(raw_vals, mapping) {
  if (is.null(raw_vals) || length(raw_vals) == 0) return(raw_vals)
  if ((is.integer(raw_vals) || is.numeric(raw_vals)) &&
    length(raw_vals) == length(mapping) &&
    all(as.integer(raw_vals) == seq_along(mapping))) {
    return(mapping)
  }
  raw_vals
}

#' @noRd
get_indicator_dim_values <- function(base_dir, indicator_cfg, key) {
  if (is.null(base_dir) || !nzchar(base_dir)) {
    return(character(0))
  }
  if (is.null(indicator_cfg) || is.null(indicator_cfg$file)) {
    return(character(0))
  }
  if (!identical(indicator_cfg$source, "nc")) {
    return(character(0))
  }

  indicator_path <- file.path(base_dir, "hazards", "indicators", indicator_cfg$file)
  if (!file.exists(indicator_path)) {
    # Try aggregated fallback if the base file is missing
    base_path <- sub("__agg\\d+\\.nc$", ".nc", indicator_path)
    agg_factor <- getOption("climate_risk_tool_nc_aggregate_factor", 1L)
    agg_factor <- as.integer(agg_factor)
    if (agg_factor > 1) {
      agg_path <- sub("\\.nc$", paste0("__agg", agg_factor, ".nc"), base_path)
      if (file.exists(agg_path)) {
        indicator_path <- agg_path
      }
    }
    if (!file.exists(indicator_path)) {
      pattern <- paste0(basename(sub("\\.nc$", "", base_path)), "__agg\\d+\\.nc$")
      dir_path <- dirname(base_path)
      if (dir.exists(dir_path)) {
        agg_files <- list.files(dir_path, pattern = pattern, full.names = TRUE)
        if (length(agg_files) > 0) {
          indicator_path <- agg_files[1]
        }
      }
    }
  }
  if (!file.exists(indicator_path)) {
    return(character(0))
  }

  nc <- tryCatch(ncdf4::nc_open(indicator_path), error = function(e) NULL)
  if (is.null(nc)) {
    return(character(0))
  }
  on.exit(try(ncdf4::nc_close(nc), silent = TRUE), add = TRUE)

  dim_names <- names(nc$dim)
  if (length(dim_names) == 0) {
    return(character(0))
  }

  lon_dim <- dim_names[vapply(dim_names, function(nm) name_eq(nm, c("lon", "longitude", "x")), logical(1))]
  lat_dim <- dim_names[vapply(dim_names, function(nm) name_eq(nm, c("lat", "latitude", "y")), logical(1))]
  ens_dim <- dim_names[vapply(dim_names, function(nm) name_eq(nm, c("ensemble")), logical(1))]
  gwl_dim <- dim_names[vapply(dim_names, function(nm) name_eq(nm, c("gwl", "GWL", "scenario")), logical(1))]
  season_dim <- dim_names[vapply(dim_names, function(nm) name_eq(nm, c("season")), logical(1))]
  remaining <- setdiff(dim_names, c(lon_dim[1], lat_dim[1], ens_dim, gwl_dim, season_dim))
  rp_dim <- if (length(remaining) > 0) remaining[[1]] else "return_period"

  target_dim <- switch(
    key,
    ensemble = if (length(ens_dim) > 0) ens_dim[1] else NULL,
    gwl = if (length(gwl_dim) > 0) gwl_dim[1] else NULL,
    scenario_name = if (length(gwl_dim) > 0) gwl_dim[1] else NULL,
    season = if (length(season_dim) > 0) season_dim[1] else NULL,
    return_period = if (rp_dim %in% dim_names) rp_dim else NULL,
    # Default to exact match if not one of the standard ones
    if (key %in% dim_names) key else NULL
  )
  if (is.null(target_dim) || !(target_dim %in% dim_names)) {
    return(character(0))
  }

  vals <- nc$dim[[target_dim]]$vals
  if (is.null(vals) || length(vals) == 0) {
    return(character(0))
  }

  if (identical(key, "ensemble")) {
    vals <- normalize_indexed_dim(vals, c("mean", "median", "p10", "p90", "min", "max", "std"))
  }
  if (identical(key, "scenario_name")) {
    vals <- normalize_indexed_dim(vals, c("present", "1.5", "2", "3"))
  }
  if (identical(key, "season")) {
    vals <- normalize_indexed_dim(vals, c("Summer", "Autumn", "Winter", "Spring"))
  }

  vals <- as.character(vals)
  vals[!is.na(vals) & nzchar(vals)]
}

#' @noRd
get_fixed_choices <- function(key, current_value, hazard_type, indicator_key, indicator_cfg, inventory_df, base_dir) {
  if (is.null(inventory_df) || !is.data.frame(inventory_df) || nrow(inventory_df) == 0) {
    inventory_df <- NULL
  }

  # Valid columns to look for in inventory (include all potential index names)
  valid_cols <- c("ensemble", "scenario_name", "return_period", "season", "gwl")

  choices <- character(0)
  if (!is.null(inventory_df) && (key %in% valid_cols || key %in% names(inventory_df))) {
    # Filter inventory for this hazard and indicator
    sub_inv <- inventory_df[inventory_df$hazard_type == hazard_type &
                            inventory_df$hazard_indicator == indicator_key, ]
    if (nrow(sub_inv) > 0) {
      choices <- unique(as.character(sub_inv[[key]]))
      choices <- choices[!is.na(choices) & nzchar(choices)]
    }
  }

  dim_choices <- get_indicator_dim_values(base_dir, indicator_cfg, key)
  if (length(dim_choices) > 0) {
    choices <- unique(c(choices, dim_choices))
  }

  if (!is.null(current_value) && nzchar(as.character(current_value))) {
    choices <- unique(c(as.character(current_value), choices))
  }

  choices
}

#' @noRd
settings_row <- function(label, value_ui) {
  if (is.null(value_ui) || length(value_ui) == 0) {
    value_ui <- ""
  }
  if (is.atomic(value_ui) && length(value_ui) > 1) {
    value_ui <- paste(value_ui, collapse = ", ")
  }
  if (is.atomic(value_ui)) {
    value_ui <- shiny::tags$span(
      style = "color: #111827; word-break: break-all;",
      value_ui
    )
  }
  shiny::tags$div(
    style = "display: flex; gap: 8px; align-items: baseline; margin-bottom: 4px;",
    shiny::tags$span(
      style = "min-width: 200px; font-weight: 600; color: #4b5563; flex-shrink: 0;",
      label
    ),
    shiny::tags$div(style = "flex: 1;", value_ui)
  )
}
