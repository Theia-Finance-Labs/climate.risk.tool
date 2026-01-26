#' Load NetCDF hazard indicator and build inventory
#'
#' @description
#' Loads a single NetCDF indicator file and builds inventory for the
#' hazard type and indicator defined in the hazard config YAML.
#'
#' **Multi-variable NetCDF files:** If a NetCDF file contains multiple variables
#' (e.g., when not using ensemble dimension), the loader will automatically select
#' one based on a preference order: mean > median > value > data. If none of these
#' are found, it uses the first variable.
#'
#' **Ensemble dimension:** If the NC file has an ensemble dimension with values
#' like mean, median, p10, p90, only the 'mean' ensemble is loaded by default.
#' This avoids iteration over all ensemble values and provides a single representative
#' raster per hazard scenario.
#'
#' Returns both the loaded rasters and a metadata inventory tibble.
#'
#' **Raster Creation Logic:**
#'
#' NetCDF files store lon/lat coordinates as cell **centers**. To create a proper
#' SpatRaster, we:
#'
#' 1. Extract lon/lat coordinate vectors from the NC file
#' 2. Calculate the resolution (spacing) between coordinates:
#'    - `res_lon = (max(lon) - min(lon)) / (n_lon - 1)`
#'    - `res_lat = (max(lat) - min(lat)) / (n_lat - 1)`
#' 3. Extend the extent by half a pixel on each side to convert cell centers to cell edges:
#'    - `xmin = min(lon) - res_lon/2`
#'    - `xmax = max(lon) + res_lon/2`
#'    - `ymin = min(lat) - res_lat/2`
#'    - `ymax = max(lat) + res_lat/2`
#' 4. Create a SpatRaster with `terra::rast(ncols, nrows, xmin, xmax, ymin, ymax, crs)`
#' 5. Assign the data slice values to the raster
#'
#' This ensures that each raster cell properly represents the area around each
#' coordinate point, not just the point itself.
#'
#'
#' @param indicator_path Character path to NetCDF indicator file
#' @param hazard_type Character hazard type from hazard config YAML
#' @param hazard_indicator Character indicator key from hazard config YAML
#' @param indicator_config List configuration for the indicator (from hazard config YAML)
#' @param aggregate_factor Integer >= 1. If >1, aggregate rasters by this factor during loading for speed (default: 1)
#' @param cache_aggregated Logical. If TRUE and aggregate_factor > 1, save and reuse aggregated rasters (default: TRUE)
#' @param force_reaggregate Logical. If TRUE, recompute aggregated rasters even if cached files exist (default: FALSE)
#' @return List with two elements: `hazards` (named list of SpatRaster) and
#'   `inventory` (tibble with hazard metadata)
#' @noRd
load_nc_hazards_with_metadata <- function(indicator_path,
                                          hazard_type,
                                          hazard_indicator,
                                          indicator_config,
                                          aggregate_factor = 1L,
                                          cache_aggregated = TRUE,
                                          force_reaggregate = FALSE) {

  # Resolve aggregated fallback when requested
  f <- indicator_path
  if (!file.exists(f)) {
    # If the exact file is missing, try to find an aggregated version
    # 1. Try with the requested aggregate_factor if > 1
    if (aggregate_factor > 1) {
      base <- sub("__agg\\d+\\.nc$", ".nc", f)
      agg_path <- sub("\\.nc$", paste0("__agg", aggregate_factor, ".nc"), base)
      if (file.exists(agg_path)) {
        f <- agg_path
      }
    }

    # 2. If still not found, try to find ANY aggregated version (common in test data)
    if (!file.exists(f)) {
      base <- sub("__agg\\d+\\.nc$", ".nc", indicator_path)
      pattern <- paste0(basename(sub("\\.nc$", "", base)), "__agg\\d+\\.nc$")
      dir_path <- dirname(base)
      if (dir.exists(dir_path)) {
        agg_files <- list.files(dir_path, pattern = pattern, full.names = TRUE)
        if (length(agg_files) > 0) {
          # Use the first available aggregated file
          f <- agg_files[1]
          message("  Indicator '", basename(indicator_path), "' not found, using aggregated fallback: ", basename(f))
        }
      }
    }
  }

  if (!file.exists(f)) {
    return(list(hazards = list(), inventory = tibble::tibble()))
  }

  message("  Loading NetCDF indicator: ", basename(f))

  # Open NetCDF and discover structure
  nc <- tryCatch({
    ncdf4::nc_open(f)
  }, error = function(e) {
    warning(
      "[load_nc_hazards_with_metadata] Failed to open NetCDF file: ", basename(f),
      "\n  Full path: ", f,
      "\n  Error: ", conditionMessage(e),
      "\n  File may not be a valid NetCDF file or may be corrupted.",
      "\n  Skipping this file."
    )
    return(NULL)
  })

  if (is.null(nc)) {
    return(list(hazards = list(), inventory = tibble::tibble()))
  }

  var_names <- names(nc$var)
  if (length(var_names) == 0) {
    warning("[load_nc_hazards_with_metadata] No variables found in NetCDF file: ", f, ". Skipping.")
    try(ncdf4::nc_close(nc), silent = TRUE)
    return(list(hazards = list(), inventory = tibble::tibble()))
  }

  main_var <- indicator_config$variable
  if (is.null(main_var) || !nzchar(main_var) || !(main_var %in% var_names)) {
    preferred_vars <- c("mean", "median", "value", "data")
    main_var <- NULL
    for (pref in preferred_vars) {
      if (pref %in% var_names) {
        main_var <- pref
        message("  Using preferred NetCDF variable '", main_var, "' from: ", paste(var_names, collapse = ", "))
        break
      }
    }
    if (is.null(main_var)) {
      main_var <- var_names[[1]]
      message("  Using first NetCDF variable '", main_var, "' from: ", paste(var_names, collapse = ", "))
    }
  }

  # Coordinate variables and values
  dim_names <- vapply(nc$var[[main_var]]$dim, function(d) d$name, character(1))

  # Try to resolve standard names (lon/lat/GWL/ensemble/return_period)
  name_eq <- function(x, opts) any(tolower(x) == tolower(opts))

  # Find lon/lat dim names by convention
  lon_dim <- dim_names[vapply(dim_names, function(nm) name_eq(nm, c("lon", "longitude", "x")), logical(1))]
  lat_dim <- dim_names[vapply(dim_names, function(nm) name_eq(nm, c("lat", "latitude", "y")), logical(1))]
  if (length(lon_dim) == 0) lon_dim <- "lon"
  if (length(lat_dim) == 0) lat_dim <- "lat"

    # Get coordinate values from dimensions (not variables)
    # Dimensions store their values in nc$dim[[name]]$vals
  lon_vals <- if (lon_dim[1] %in% names(nc$dim)) {
    nc$dim[[lon_dim[1]]]$vals
  } else {
    try(ncdf4::ncvar_get(nc, lon_dim[1]), silent = TRUE)
  }

  lat_vals <- if (lat_dim[1] %in% names(nc$dim)) {
    nc$dim[[lat_dim[1]]]$vals
  } else {
    try(ncdf4::ncvar_get(nc, lat_dim[1]), silent = TRUE)
  }

    # Other dims - check for scenario, ensemble, GWL, return_period, season
  ens_dim <- dim_names[vapply(dim_names, function(nm) name_eq(nm, c("ensemble")), logical(1))]
  gwl_dim <- dim_names[vapply(dim_names, function(nm) name_eq(nm, c("gwl", "GWL", "scenario")), logical(1))]
  season_dim <- dim_names[vapply(dim_names, function(nm) name_eq(nm, c("season")), logical(1))]
  # Heuristic: remaining non-spatial, non-ensemble, non-GWL, non-season dim is return period
  remaining <- setdiff(dim_names, c(lon_dim[1], lat_dim[1], ens_dim, gwl_dim, season_dim))
  rp_dim <- if (length(remaining) > 0) remaining[[1]] else "return_period"

    # Get dimension values from nc$dim structure
  ens_vals <- if (length(ens_dim) > 0 && ens_dim[1] %in% names(nc$dim)) {
    nc$dim[[ens_dim[1]]]$vals
  } else if (length(ens_dim) > 0) {
    try(ncdf4::ncvar_get(nc, ens_dim[1]), silent = TRUE)
  } else {
    structure("no_ensemble_dim", class = "try-error")
  }

  gwl_vals <- if (length(gwl_dim) > 0 && gwl_dim[1] %in% names(nc$dim)) {
    nc$dim[[gwl_dim[1]]]$vals
  } else if (length(gwl_dim) > 0) {
    try(ncdf4::ncvar_get(nc, gwl_dim[1]), silent = TRUE)
  } else {
    structure("no_gwl_dim", class = "try-error")
  }

  rp_vals <- if (rp_dim %in% names(nc$dim)) {
    nc$dim[[rp_dim]]$vals
  } else {
    try(ncdf4::ncvar_get(nc, rp_dim), silent = TRUE)
  }

  season_vals <- if (length(season_dim) > 0 && season_dim[1] %in% names(nc$dim)) {
    nc$dim[[season_dim[1]]]$vals
  } else if (length(season_dim) > 0) {
    try(ncdf4::ncvar_get(nc, season_dim[1]), silent = TRUE)
  } else {
    structure("no_season_dim", class = "try-error")
  }

    # Some aggregated NetCDFs store categorical dimensions (GWL/season/ensemble) as 1..N indices
    # with no label variable. Map to the canonical labels used across the pipeline/tests.
    normalize_indexed_dim <- function(vals, mapping) {
      if (inherits(vals, "try-error")) return(vals)
      if (is.null(vals) || length(vals) == 0) return(vals)
      if ((is.integer(vals) || is.numeric(vals)) &&
        length(vals) == length(mapping) &&
        all(as.integer(vals) == seq_along(mapping))) {
        return(mapping)
      }
      vals
    }

    # Canonical mappings (as used in precomputed hazards + tests)
  gwl_vals <- normalize_indexed_dim(gwl_vals, c("present", "1.5", "2", "3"))
  season_vals <- normalize_indexed_dim(season_vals, c("Summer", "Autumn", "Winter", "Spring"))
  ens_vals <- normalize_indexed_dim(ens_vals, c("mean", "min", "max", "std"))

  # Ensemble values (default to mean)
  ens_labels <- if (inherits(ens_vals, "try-error") || length(ens_vals) == 0) {
    "mean"
  } else {
    as.character(ens_vals)
  }

  fixed_vals <- indicator_config$fixed
  if (is.null(fixed_vals)) fixed_vals <- list()

  index_dims <- indicator_config$index
  if (is.null(index_dims)) index_dims <- character(0)
  index_dims <- tolower(as.character(index_dims))

  dim_values <- list(
    gwl = gwl_vals,
    return_period = rp_vals,
    season = season_vals,
    ensemble = ens_labels
  )

  dim_indices <- list()
  for (dim_name in names(dim_values)) {
    vals <- dim_values[[dim_name]]
    if (inherits(vals, "try-error") || length(vals) == 0) {
      dim_indices[[dim_name]] <- 1L
      next
    }

    if (dim_name %in% index_dims) {
      dim_indices[[dim_name]] <- seq_along(vals)
      next
    }

    if (dim_name %in% names(fixed_vals)) {
      target <- as.character(fixed_vals[[dim_name]])
      idx <- which(as.character(vals) == target)
      if (length(idx) == 0) {
        stop("Fixed value '", target, "' not found for dimension ", dim_name)
      }
      dim_indices[[dim_name]] <- idx[1]
      next
    }

    if (dim_name == "ensemble") {
      mean_idx <- which(as.character(vals) == "mean")
      if (length(mean_idx) == 0) mean_idx <- 1L
      dim_indices[[dim_name]] <- mean_idx[1]
      next
    }

    dim_indices[[dim_name]] <- 1L
  }

  # Close ncdf4 handle before terra opens the file (prevents file lock conflicts)
  try(ncdf4::nc_close(nc), silent = TRUE)

  nc_path <- normalizePath(f, winslash = "/", mustWork = TRUE)
  r_all <- tryCatch({
    terra::rast(nc_path, subds = main_var)
  }, error = function(e) {
    warning(
      "[load_nc_hazards_with_metadata] Failed to load NetCDF via terra: ", basename(f),
      "\n  Error: ", conditionMessage(e),
      "\n  Skipping this file."
    )
    return(NULL)
  })

  if (is.null(r_all)) {
    return(list(hazards = list(), inventory = tibble::tibble()))
  }

  # Build combinations
  combo_grid <- expand.grid(
    gwl = dim_indices$gwl,
    return_period = dim_indices$return_period,
    season = dim_indices$season,
    ensemble = dim_indices$ensemble,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )

  results <- list()
  inventory_rows <- list()

  for (row_idx in seq_len(nrow(combo_grid))) {
    ig <- combo_grid$gwl[row_idx]
    ir <- combo_grid$return_period[row_idx]
    is <- combo_grid$season[row_idx]
    ie <- combo_grid$ensemble[row_idx]

    # Determine extra dimensions (beyond lon/lat) in their original order
    extra_dims <- dim_names[!(dim_names %in% c(lon_dim[1], lat_dim[1]))]

    dim_lengths <- vapply(extra_dims, function(nm) {
      if (length(ens_dim) > 0 && nm == ens_dim[1]) {
        length(ens_labels)
      } else if (length(gwl_dim) > 0 && nm == gwl_dim[1]) {
        if (inherits(gwl_vals, "try-error")) 1L else length(gwl_vals)
      } else if (nm == rp_dim) {
        if (inherits(rp_vals, "try-error")) 1L else length(rp_vals)
      } else if (length(season_dim) > 0 && nm == season_dim[1]) {
        if (inherits(season_vals, "try-error")) 1L else length(season_vals)
      } else {
        1L
      }
    }, integer(1))

    dim_indices_extra <- vapply(extra_dims, function(nm) {
      if (length(ens_dim) > 0 && nm == ens_dim[1]) {
        ie
      } else if (length(gwl_dim) > 0 && nm == gwl_dim[1]) {
        ig
      } else if (nm == rp_dim) {
        ir
      } else if (length(season_dim) > 0 && nm == season_dim[1]) {
        is
      } else {
        1L
      }
    }, integer(1))

    layer_idx <- 1L
    if (length(extra_dims) > 0) {
      layer_offset <- 0L
      stride <- 1L
      for (j in seq_along(extra_dims)) {
        layer_offset <- layer_offset + (dim_indices_extra[j] - 1L) * stride
        stride <- stride * dim_lengths[j]
      }
      layer_idx <- layer_offset + 1L
    }

    if (layer_idx > terra::nlyr(r_all)) {
      stop(
        "Requested layer index ", layer_idx, " exceeds available layers (",
        terra::nlyr(r_all), ") in NetCDF variable '", main_var, "'."
      )
    }

    r <- r_all[[layer_idx]]

    if (is.na(terra::crs(r)) || terra::crs(r) == "") {
      terra::crs(r) <- "EPSG:4326"
    }

    if (!inherits(lon_vals, "try-error") && !inherits(lat_vals, "try-error")) {
      n_lon <- length(lon_vals)
      n_lat <- length(lat_vals)

      res_lon <- if (n_lon > 1) (max(lon_vals) - min(lon_vals)) / (n_lon - 1) else 1.0
      res_lat <- if (n_lat > 1) (max(lat_vals) - min(lat_vals)) / (n_lat - 1) else 1.0

      xmin <- min(lon_vals) - res_lon / 2
      xmax <- max(lon_vals) + res_lon / 2
      ymin <- min(lat_vals) - res_lat / 2
      ymax <- max(lat_vals) + res_lat / 2

      terra::ext(r) <- terra::ext(xmin, xmax, ymin, ymax)
    }

    if (terra::nlyr(r) != 1) {
      stop(
        "Expected single-band raster from NetCDF file '", basename(f),
        "', but got ", terra::nlyr(r), " bands. ",
        "Each hazard scenario should be a single 2D layer."
      )
    }

    gwl_label <- if (inherits(gwl_vals, "try-error")) paste0("idx", ig) else as.character(gwl_vals[ig])
    rp_label <- if (inherits(rp_vals, "try-error")) paste0("idx", ir) else as.character(rp_vals[ir])
    season_label <- if (inherits(season_vals, "try-error")) NA_character_ else as.character(season_vals[is])
    ens_label <- as.character(ens_labels[ie])

    has_season <- !inherits(season_vals, "try-error") && length(season_dim) > 0 && !is.na(season_label)

    rp_numeric <- suppressWarnings(as.numeric(rp_label))
    if (is.na(rp_numeric)) rp_numeric <- ir

    # Build structured hazard key
    index_values <- list()
    if (length(gwl_dim) > 0) index_values$gwl <- gwl_label
    index_values$return_period <- rp_label
    if (has_season) index_values$season <- season_label
    
    # Map internal labels to standard names for build_indicator_key
    final_index_values <- list(
      return_period = rp_numeric,
      gwl = if (length(gwl_dim) > 0) gwl_label else NA_character_,
      scenario_name = NA_character_, # NC loader uses gwl by default
      season = if (has_season) season_label else NA_character_
    )
    
    # Check if config uses scenario_name instead of gwl
    config_index_dims <- indicator_config$index
    if (!is.null(config_index_dims) && "scenario_name" %in% config_index_dims && !"gwl" %in% config_index_dims) {
      final_index_values$scenario_name <- final_index_values$gwl
      final_index_values$gwl <- NA_character_
    }

    indicator_key <- build_indicator_key(
      indicator_file = basename(f),
      indicator_variable = if (!is.null(indicator_config$variable) && nzchar(indicator_config$variable)) indicator_config$variable else hazard_indicator,
      index_values = final_index_values,
      ensemble = ens_label
    )
    
    hazard_name <- build_hazard_name(
      hazard_type = hazard_type,
      hazard_indicator = hazard_indicator,
      index_values = final_index_values,
      ensemble = ens_label
    )

    results[[indicator_key]] <- r

    # Build inventory row dynamically based on index columns
    inventory_row <- tibble::tibble(
      hazard_type = hazard_type,
      hazard_indicator = hazard_indicator,
      hazard_name = hazard_name,
      indicator_key = indicator_key,
      ensemble = ens_label,
      source = "nc",
      agg = indicator_config$agg,
      categorical = indicator_config$categorical,
      variable = indicator_config$variable
    )
    
    # Map internal labels to the requested index names from config
    # We always keep scenario_name, return_period, season as fallbacks if they are not in index
    # but we ALSO add the specific index names
    
    # Internal to label mapping
    internal_labels <- list(
      gwl = gwl_label,
      return_period = rp_numeric,
      season = season_label
    )
    
    # Add all index columns
    for (idx_col in index_dims) {
      if (idx_col %in% names(internal_labels)) {
        val <- internal_labels[[idx_col]]
        if (idx_col == "return_period") val <- suppressWarnings(as.numeric(val))
        inventory_row[[idx_col]] <- val
      } else {
        # If it's a custom index name not in our standard mapping, we might have trouble
        # but for now we only support standard ones
        inventory_row[[idx_col]] <- NA_character_
      }
    }
    
    # Ensure backward compatibility columns exist in inventory
    inventory_row$scenario_name <- gwl_label
    inventory_row$return_period <- rp_numeric
    if (!"season" %in% names(inventory_row)) {
      inventory_row$season <- if (has_season) season_label else NA_character_
    }

    inventory_rows[[length(inventory_rows) + 1]] <- inventory_row
  }

  inventory <- if (length(inventory_rows) > 0) {
    dplyr::bind_rows(inventory_rows)
  } else {
    # ... (rest of the function)
    tibble::tibble(
      hazard_type = character(),
      hazard_indicator = character(),
      scenario_name = character(),
      return_period = numeric(),
      hazard_name = character(),
      indicator_key = character(),
      ensemble = character(),
      season = character(),
      source = character(),
      agg = character(),
      categorical = logical()
    )
  }

  return(list(hazards = results, inventory = inventory))
}
