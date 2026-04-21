#' Extract statistics from spatial hazards (NetCDF sources)
#' @noRd
format_spatial_duration <- function(seconds) {
  total_seconds <- max(0L, as.integer(round(seconds)))
  hours <- total_seconds %/% 3600L
  minutes <- (total_seconds %% 3600L) %/% 60L
  secs <- total_seconds %% 60L

  if (hours > 0L) {
    sprintf("%02d:%02d:%02d", hours, minutes, secs)
  } else {
    sprintf("%02d:%02d", minutes, secs)
  }
}

extract_spatial_batch_values <- function(extracted, expected_rows, hazard_name, batch_idx, total_batches) {
  if (!is.null(extracted) && nrow(extracted) == expected_rows) {
    if (ncol(extracted) == 2) {
      return(as.numeric(extracted[[2]]))
    }
    if (ncol(extracted) > 2) {
      warning(
        "[extract_spatial_statistics] Extraction returned ", ncol(extracted) - 1,
        " layers for ", hazard_name, " in batch ", batch_idx, "/", total_batches,
        ", expected 1"
      )
      return(as.numeric(extracted[[2]]))
    }
    warning(
      "[extract_spatial_statistics] Extraction returned no value columns for ",
      hazard_name, " in batch ", batch_idx, "/", total_batches
    )
    return(rep(NA_real_, expected_rows))
  }

  if (!is.null(extracted)) {
    warning(
      "[extract_spatial_statistics] Extraction returned ", nrow(extracted),
      " rows, expected ", expected_rows, " for ", hazard_name,
      " in batch ", batch_idx, "/", total_batches
    )
  }
  rep(NA_real_, expected_rows)
}

estimate_spatial_extent_cells <- function(ext_obj, rast) {
  raster_res <- terra::res(rast)
  width <- max(0, ext_obj[2] - ext_obj[1])
  height <- max(0, ext_obj[4] - ext_obj[3])
  n_cols <- max(1, ceiling(width / raster_res[1]))
  n_rows <- max(1, ceiling(height / raster_res[2]))
  as.numeric(n_cols) * as.numeric(n_rows)
}

compute_spatial_batch_settings <- function(hazard_rast, extraction_mode, n_geoms) {
  n_geoms <- max(1L, as.integer(n_geoms))

  if (identical(extraction_mode, "closest")) {
    return(list(
      batch_size = max(1L, min(1000L, n_geoms)),
      max_cells = NA_real_
    ))
  }

  total_cells <- tryCatch(
    as.numeric(terra::ncell(hazard_rast)),
    error = function(e) NA_real_
  )
  if (!is.finite(total_cells) || total_cells <= 0) {
    total_cells <- 1e6
  }

  batch_size <- if (total_cells <= 5e5) {
    128L
  } else if (total_cells <= 5e6) {
    96L
  } else if (total_cells <= 5e7) {
    64L
  } else if (total_cells <= 2e8) {
    32L
  } else {
    16L
  }
  batch_size <- max(1L, min(batch_size, n_geoms))

  max_cells <- min(max(5e5, total_cells * 0.02), 5e6)

  list(
    batch_size = batch_size,
    max_cells = as.numeric(max_cells)
  )
}

build_spatial_batches <- function(indices, coords, rast, max_batch_size, max_cells) {
  split_indices <- function(local_idx) {
    if (length(local_idx) <= 1L) {
      return(list(local_idx))
    }

    local_coords <- coords[local_idx, , drop = FALSE]
    local_ext <- terra::ext(
      min(local_coords[, 1], na.rm = TRUE),
      max(local_coords[, 1], na.rm = TRUE),
      min(local_coords[, 2], na.rm = TRUE),
      max(local_coords[, 2], na.rm = TRUE)
    )
    est_cells <- estimate_spatial_extent_cells(local_ext, rast)

    if (length(local_idx) <= max_batch_size && est_cells <= max_cells) {
      return(list(local_idx))
    }

    x_spread <- diff(range(local_coords[, 1], na.rm = TRUE))
    y_spread <- diff(range(local_coords[, 2], na.rm = TRUE))
    split_axis <- if (x_spread >= y_spread) 1 else 2
    axis_order <- order(local_coords[, split_axis], na.last = TRUE)
    midpoint <- ceiling(length(local_idx) / 2)
    left_idx <- local_idx[axis_order[seq_len(midpoint)]]
    right_idx <- if (midpoint < length(local_idx)) {
      local_idx[axis_order[seq.int(midpoint + 1L, length(local_idx))]]
    } else {
      integer(0)
    }

    c(
      split_indices(left_idx),
      if (length(right_idx) > 0L) split_indices(right_idx) else list()
    )
  }

  split_indices(indices)
}

resolve_spatial_extraction_entries <- function(raster_inventory, hazards, aggregation_method, aggregation_functions) {
  entries <- vector("list", nrow(raster_inventory))

  for (i in seq_len(nrow(raster_inventory))) {
    hazard_meta <- raster_inventory[i, , drop = FALSE]
    base_hazard_name <- hazard_meta$hazard_name
    base_indicator_key <- hazard_meta$indicator_key
    hazard_source <- hazard_meta$source
    hazard_rast <- hazards[[base_indicator_key]]

    if (is.null(hazard_rast)) {
      hazard_rast <- hazards[[base_hazard_name]]
    }

    if (is.null(hazard_rast)) {
      warning(
        "Hazard '", base_hazard_name, "' (key: ", base_indicator_key, ") not found in hazards list. ",
        "Skipping extraction for this hazard."
      )
      next
    }

    hazard_indicator <- hazard_meta$hazard_indicator
    effective_aggregation_method <- if (!is.null(hazard_meta$agg) && !is.na(hazard_meta$agg)) {
      hazard_meta$agg
    } else {
      aggregation_method
    }

    if (isTRUE(hazard_meta$categorical) && !effective_aggregation_method %in% c("mode", "closest")) {
      effective_aggregation_method <- "mode"
    }

    if (is.null(effective_aggregation_method) ||
      !effective_aggregation_method %in% c(names(aggregation_functions), "closest")) {
      stop(
        "Invalid aggregation method '", effective_aggregation_method, "' for indicator ",
        hazard_indicator, ". Valid options: ", paste(c(names(aggregation_functions), "closest"), collapse = ", ")
      )
    }

    r_crs <- terra::crs(hazard_rast)
    if (is.na(r_crs) || r_crs == "") {
      stop("Raster CRS is not set")
    }

    extraction_mode <- if (effective_aggregation_method == "closest") "closest" else "polygon"

    entries[[i]] <- list(
      result_index = i,
      hazard_meta = hazard_meta,
      base_hazard_name = base_hazard_name,
      base_indicator_key = base_indicator_key,
      hazard_source = hazard_source,
      hazard_rast = hazard_rast,
      hazard_indicator = hazard_indicator,
      effective_aggregation_method = effective_aggregation_method,
      extraction_mode = extraction_mode,
      agg_func = if (effective_aggregation_method == "closest") NULL else aggregation_functions[[effective_aggregation_method]],
      r_crs = r_crs,
      crs_key = enc2utf8(as.character(r_crs))
    )
  }

  entries[!vapply(entries, is.null, logical(1))]
}

get_cached_spatial_assets <- function(assets_sf, geometry_cache, extraction_mode, crs_key, r_crs) {
  cache_key <- paste(extraction_mode, crs_key, sep = "::")
  if (exists(cache_key, envir = geometry_cache, inherits = FALSE)) {
    return(get(cache_key, envir = geometry_cache, inherits = FALSE))
  }

  transformed_assets <- if (identical(extraction_mode, "closest")) {
    sf::st_transform(sf::st_set_geometry(assets_sf, "centroid"), r_crs)
  } else {
    sf::st_transform(assets_sf, r_crs)
  }

  assign(cache_key, transformed_assets, envir = geometry_cache)
  transformed_assets
}

extract_points_batched <- function(hazard_rast, assets_centroids_sf, hazard_name, batch_size) {
  n_geoms <- nrow(assets_centroids_sf)
  hazard_vals <- rep(NA_real_, n_geoms)
  coords <- sf::st_coordinates(assets_centroids_sf)
  batch_order <- order(coords[, 1], coords[, 2], na.last = TRUE)
  batch_groups <- split(batch_order, ceiling(seq_along(batch_order) / batch_size))
  n_batches <- length(batch_groups)
  message("      Running ", n_batches, " batch(es) of up to ", batch_size, " asset(s) each")
  hazard_start_time <- Sys.time()

  for (batch_idx in seq_along(batch_groups)) {
    batch_rows <- batch_groups[[batch_idx]]
    batch_sf <- assets_centroids_sf[batch_rows, , drop = FALSE]
    batch_vect <- terra::vect(batch_sf)
    batch_n <- length(batch_rows)

    extracted <- tryCatch(
      terra::extract(hazard_rast, batch_vect),
      error = function(e) NULL
    )
    hazard_vals[batch_rows] <- extract_spatial_batch_values(
      extracted = extracted,
      expected_rows = batch_n,
      hazard_name = hazard_name,
      batch_idx = batch_idx,
      total_batches = n_batches
    )

    elapsed_seconds <- as.numeric(difftime(Sys.time(), hazard_start_time, units = "secs"))
    eta_seconds <- if (batch_idx < n_batches) {
      elapsed_seconds / batch_idx * (n_batches - batch_idx)
    } else {
      0
    }
    message(
      "      Batch ", batch_idx, "/", n_batches,
      " complete (", batch_n, " assets) | elapsed ",
      format_spatial_duration(elapsed_seconds),
      " | ETA ",
      format_spatial_duration(eta_seconds)
    )
  }

  message(
    "      Hazard complete | total elapsed ",
    format_spatial_duration(as.numeric(difftime(Sys.time(), hazard_start_time, units = "secs")))
  )

  hazard_vals
}

extract_polygons_batched <- function(hazard_rast, assets_polygons_sf, hazard_name, agg_func, batch_size, max_cells) {
  n_geoms <- nrow(assets_polygons_sf)
  hazard_vals <- rep(NA_real_, n_geoms)
  batch_centroids <- sf::st_coordinates(sf::st_centroid(sf::st_geometry(assets_polygons_sf)))
  batch_groups <- build_spatial_batches(
    indices = seq_len(n_geoms),
    coords = batch_centroids,
    rast = hazard_rast,
    max_batch_size = batch_size,
    max_cells = max_cells
  )
  n_batches <- length(batch_groups)
  message("      Running ", n_batches, " batch(es) of up to ", batch_size, " asset(s) each")
  hazard_start_time <- Sys.time()

  for (batch_idx in seq_along(batch_groups)) {
    batch_rows <- batch_groups[[batch_idx]]
    batch_sf <- assets_polygons_sf[batch_rows, , drop = FALSE]
    batch_vect <- terra::vect(batch_sf)
    hazard_rast_extract <- tryCatch(
      terra::crop(hazard_rast, terra::ext(batch_vect), snap = "out"),
      error = function(e) hazard_rast
    )
    if (inherits(hazard_rast_extract, "SpatRaster") && terra::ncell(hazard_rast_extract) == 0) {
      hazard_rast_extract <- hazard_rast
    }
    batch_cells <- if (inherits(hazard_rast_extract, "SpatRaster")) terra::ncell(hazard_rast_extract) else NA_real_

    extracted <- tryCatch(
      terra::extract(hazard_rast_extract, batch_vect, fun = agg_func, na.rm = TRUE, small = TRUE),
      error = function(e) NULL
    )
    batch_n <- length(batch_rows)
    hazard_vals[batch_rows] <- extract_spatial_batch_values(
      extracted = extracted,
      expected_rows = batch_n,
      hazard_name = hazard_name,
      batch_idx = batch_idx,
      total_batches = n_batches
    )

    elapsed_seconds <- as.numeric(difftime(Sys.time(), hazard_start_time, units = "secs"))
    eta_seconds <- if (batch_idx < n_batches) {
      elapsed_seconds / batch_idx * (n_batches - batch_idx)
    } else {
      0
    }
    message(
      "      Batch ", batch_idx, "/", n_batches,
      " complete (", batch_n, " assets) | elapsed ",
      format_spatial_duration(elapsed_seconds),
      " | ETA ",
      format_spatial_duration(eta_seconds),
      if (!is.na(batch_cells)) paste0(" | cells ", format(batch_cells, big.mark = ",")) else ""
    )
  }

  message(
    "      Hazard complete | total elapsed ",
    format_spatial_duration(as.numeric(difftime(Sys.time(), hazard_start_time, units = "secs")))
  )

  hazard_vals
}

build_spatial_result_row <- function(asset_attrs, hazard_meta, hazard_vals, base_hazard_name, base_indicator_key, hazard_source) {
  hazard_type <- hazard_meta$hazard_type
  hazard_indicator <- hazard_meta$hazard_indicator
  hazard_return_period <- hazard_meta$return_period
  hazard_scenario_name <- hazard_meta$scenario_name
  hazard_season <- if ("season" %in% names(hazard_meta)) hazard_meta$season else NA_character_
  hazard_ensemble <- if ("ensemble" %in% names(hazard_meta)) hazard_meta$ensemble else NA_character_

  inventory_index_cols <- setdiff(
    names(hazard_meta),
    c(
      "hazard_type", "hazard_indicator", "hazard_name", "hazard_key", "indicator_key", "scenario_name", "return_period",
      "season", "ensemble", "source", "agg", "categorical", "variable", "indicator_file", "indicator_variable", "indicator_file_key"
    )
  )
  extra_index_values <- hazard_meta[inventory_index_cols]

  indicator_col <- if (!is.null(hazard_meta$variable) && !is.na(hazard_meta$variable)) {
    as.character(hazard_meta$variable)
  } else {
    as.character(hazard_indicator)
  }

  df_i <- dplyr::bind_cols(
    asset_attrs,
    tibble::tibble(.indicator_value = hazard_vals)
  ) |>
    dplyr::mutate(
      hazard_name = base_hazard_name,
      hazard_key = base_hazard_name,
      indicator_key = base_indicator_key,
      hazard_type = hazard_type,
      scenario_name = hazard_scenario_name,
      hazard_indicator = hazard_indicator,
      return_period = hazard_return_period,
      season = hazard_season,
      ensemble = hazard_ensemble,
      source = hazard_source,
      hazard_intensity = .data$.indicator_value,
      matching_method = "geolocated extracted",
      !!rlang::sym(indicator_col) := .data$.indicator_value
    ) |>
    dplyr::bind_cols(extra_index_values)

  df_i |>
    dplyr::select(
      dplyr::any_of(c(
        "asset", "company", "latitude", "longitude",
        "municipality", "state", "asset_category", "asset_subtype", "size_in_m2",
        "share_of_economic_activity", "cost_factor", "cnae", "hazard_name", "hazard_key", "indicator_key", "hazard_type",
        "hazard_indicator", "return_period", "scenario_name", "season", "ensemble", "source",
        indicator_col, "matching_method"
      )),
      dplyr::everything(),
      -dplyr::any_of(c(".indicator_value", "ID", "id"))
    )
}

propagate_spatial_index_values <- function(raster_results) {
  if (!"hazard_name" %in% names(raster_results)) {
    return(raster_results)
  }

  potential_index_cols <- c("gwl", "scenario_name", "season", "return_period")
  existing_index_cols <- intersect(potential_index_cols, names(raster_results))

  for (hazard_name_val in unique(raster_results$hazard_name)) {
    hazard_rows <- raster_results$hazard_name == hazard_name_val & !is.na(raster_results$hazard_name)

    for (idx_col in existing_index_cols) {
      non_na_values <- raster_results[[idx_col]][hazard_rows & !is.na(raster_results[[idx_col]])]
      if (length(non_na_values) > 0) {
        raster_results[[idx_col]][hazard_rows & is.na(raster_results[[idx_col]])] <- non_na_values[1]
      }
    }
  }

  raster_results
}

extract_spatial_statistics <- function(assets_df, hazards, hazards_inventory, aggregation_method = "mean") {
  message("  [extract_spatial_statistics] Extracting hazard statistics...")

  old_progress <- terra::terraOptions()$progress
  on.exit(try(terra::terraOptions(progress = old_progress), silent = TRUE), add = TRUE)
  terra::terraOptions(progress = 0)

  available_hazard_keys <- names(hazards)
  raster_inventory <- hazards_inventory |>
    dplyr::filter(.data$source %in% c("nc", "tif"), .data$indicator_key %in% available_hazard_keys)

  if (nrow(raster_inventory) == 0) {
    return(tibble::tibble())
  }

  message("  [extract_spatial_statistics] Processing raster hazards (NC/TIF) with vectorized extraction...")

  aggregation_functions <- list(
    "mean" = "mean",
    "median" = "median",
    "max" = "max",
    "min" = "min",
    "p10" = function(x, ...) as.numeric(stats::quantile(x, 0.10, na.rm = TRUE, type = 7)),
    "p90" = function(x, ...) as.numeric(stats::quantile(x, 0.90, na.rm = TRUE, type = 7)),
    "mode" = "modal"
  )

  assets_sf <- create_asset_geometries(
    assets_df,
    default_buffer_size_m = 1111,
    output_crs = 4326
  )
  if (!inherits(assets_sf$geometry, "sfc")) {
    assets_sf <- sf::st_as_sf(assets_sf)
  } else {
    assets_sf <- sf::st_as_sf(assets_sf, sf_column_name = "geometry")
  }

  asset_attrs <- tibble::as_tibble(assets_df)
  n_geoms <- nrow(assets_sf)
  geometry_cache <- new.env(parent = emptyenv())

  entries <- resolve_spatial_extraction_entries(
    raster_inventory = raster_inventory,
    hazards = hazards,
    aggregation_method = aggregation_method,
    aggregation_functions = aggregation_functions
  )

  if (length(entries) == 0) {
    return(tibble::tibble())
  }

  entry_group_keys <- vapply(
    entries,
    function(entry) paste(entry$extraction_mode, entry$crs_key, sep = "::"),
    character(1)
  )
  grouped_entry_indices <- split(seq_along(entries), entry_group_keys)
  results_list <- vector("list", nrow(raster_inventory))

  for (group_indices in grouped_entry_indices) {
    group_entry <- entries[[group_indices[[1]]]]
    transformed_assets <- get_cached_spatial_assets(
      assets_sf = assets_sf,
      geometry_cache = geometry_cache,
      extraction_mode = group_entry$extraction_mode,
      crs_key = group_entry$crs_key,
      r_crs = group_entry$r_crs
    )

    for (entry_idx in group_indices) {
      entry <- entries[[entry_idx]]
      hazard_meta <- entry$hazard_meta

      message(
        "    Processing ", toupper(entry$hazard_source), " hazard ", entry$result_index, "/",
        nrow(raster_inventory), ": ", entry$base_hazard_name
      )

      batch_settings <- compute_spatial_batch_settings(
        hazard_rast = entry$hazard_rast,
        extraction_mode = entry$extraction_mode,
        n_geoms = n_geoms
      )

      hazard_vals <- if (identical(entry$extraction_mode, "closest")) {
        extract_points_batched(
          hazard_rast = entry$hazard_rast,
          assets_centroids_sf = transformed_assets,
          hazard_name = entry$base_hazard_name,
          batch_size = batch_settings$batch_size
        )
      } else {
        extract_polygons_batched(
          hazard_rast = entry$hazard_rast,
          assets_polygons_sf = transformed_assets,
          hazard_name = entry$base_hazard_name,
          agg_func = entry$agg_func,
          batch_size = batch_settings$batch_size,
          max_cells = batch_settings$max_cells
        )
      }

      if (isTRUE(hazard_meta$categorical)) {
        hazard_vals <- ifelse(is.na(hazard_vals), NA_real_, round(hazard_vals))
      }

      results_list[[entry$result_index]] <- build_spatial_result_row(
        asset_attrs = asset_attrs,
        hazard_meta = hazard_meta,
        hazard_vals = hazard_vals,
        base_hazard_name = entry$base_hazard_name,
        base_indicator_key = entry$base_indicator_key,
        hazard_source = entry$hazard_source
      )
    }
  }

  results_list <- results_list[!vapply(results_list, is.null, logical(1))]
  if (length(results_list) == 0) {
    return(tibble::tibble())
  }

  propagate_spatial_index_values(dplyr::bind_rows(results_list))
}
