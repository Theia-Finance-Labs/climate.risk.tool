#' Load NetCDF hazards and build inventory
#'
#' @description
#' Scans for .nc files in the hazards directory, extracts metadata from the folder
#' structure (hazard_type, hazard_indicator) and from NetCDF dimensions (GWL,
#' return_period, ensemble). For each combination of GWL, return_period, and
#' ensemble value, creates a separate SpatRaster.
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
#' **For TIF files:** The existing loader (`load_tif_hazards()`) uses
#' `terra::rast(file_path)` which automatically reads the georeferencing information
#' (extent, resolution, CRS) embedded in the GeoTIFF file itself. No manual extent
#' calculation is needed.
#'
#' @param hazards_dir Character. Root directory that contains `hazards/` subfolders
#' @param aggregate_factor Integer >= 1. If >1, aggregate rasters by this factor during loading for speed (default: 1)
#' @param cache_aggregated Logical. If TRUE and aggregate_factor > 1, save and reuse aggregated rasters (default: TRUE)
#' @param force_reaggregate Logical. If TRUE, recompute aggregated rasters even if cached files exist (default: FALSE)
#' @return List with two elements: `hazards` (named list of SpatRaster) and
#'   `inventory` (tibble with hazard metadata)
#' @noRd
load_nc_hazards_with_metadata <- function(hazards_dir,
                                          aggregate_factor = 1L,
                                          cache_aggregated = TRUE,
                                          force_reaggregate = FALSE) {
  all_nc_files <- list.files(hazards_dir, pattern = "\\.nc$", full.names = TRUE, recursive = TRUE)

  # Filter out aggregated files from the initial scan (they'll be loaded via aggregation logic)
  nc_files <- all_nc_files[!grepl("__agg\\d+\\.nc$", all_nc_files)]

  if (length(nc_files) == 0) {
    return(list(
      hazards = list(),
      inventory = tibble::tibble()
    ))
  }

  results <- list()
  inventory_rows <- list()

  for (original_file in nc_files) {
    f <- original_file

    # NC aggregation disabled - terra::aggregate breaks multi-dimensional structure
    # (converts string dimension labels to numeric indices, loses metadata)
    # For NC files, always use original file regardless of aggregate_factor
    if (aggregate_factor > 1) {
      message("  Loading NetCDF file: ", basename(f), " (aggregation not supported for NC files)")
    } else {
      message("  Loading NetCDF file: ", basename(f))
    }

    # Path parsing: Support both:
    # - 4-part: {hazards_dir}/{hazard_type}/{hazard_indicator}/{model_type}/{file}.nc
    # - 3-part: {hazards_dir}/{hazard_type}/{hazard_indicator}/{file}.nc (e.g., Fire)
    # Use relative path from hazards_dir for more robust parsing
    hazards_dir_norm <- normalizePath(hazards_dir, winslash = "/")
    f_norm <- normalizePath(f, winslash = "/")
    relative_path <- sub(paste0("^", hazards_dir_norm, "/"), "", f_norm)
    parts <- strsplit(relative_path, "/", fixed = TRUE)[[1]]

    if (length(parts) >= 4) {
      # hazards/{hazard_type}/{hazard_indicator}/{model_type}/{file}.nc
      file_name <- parts[length(parts)]
      model_type <- parts[length(parts) - 1]
      hazard_indicator <- parts[length(parts) - 2]
      hazard_type <- parts[length(parts) - 3]
    } else if (length(parts) == 3) {
      # hazards/{hazard_type}/{hazard_indicator}/{file}.nc
      file_name <- parts[length(parts)]
      model_type <- "ensemble" # Infer ensemble as default model type
      hazard_type <- parts[length(parts) - 2]      # First folder = hazard_type
      hazard_indicator <- parts[length(parts) - 1] # Second folder = hazard_indicator
      message("  3-part path detected for ", hazard_type, "/", hazard_indicator, " - assuming model_type='ensemble'")
    } else {
      # Fallbacks (ideal-path assumption per user instruction)
      file_name <- basename(f)
      model_type <- "ensemble"
      hazard_indicator <- "indicator"
      hazard_type <- "unknown"
      message("  Warning: Unexpected path structure, using fallback values for: ", f)
    }

    # Open NetCDF and discover structure
    nc <- ncdf4::nc_open(f)

    # Identify main data variable
    var_names <- names(nc$var)

    if (length(var_names) == 0) {
      warning("[load_nc_hazards_with_metadata] No variables found in NetCDF file: ", f, ". Skipping.")
      try(ncdf4::nc_close(nc), silent = TRUE)
      next
    }

    # If multiple variables, select one based on preference order
    if (length(var_names) > 1) {
      # Preference order: mean, median, value, data, or first available
      preferred_vars <- c("mean", "median", "value", "data")
      main_var <- NULL

      for (pref in preferred_vars) {
        if (pref %in% var_names) {
          main_var <- pref
          message(
            "  Multi-variable NetCDF detected, using '", main_var, "' from: ",
            paste(var_names, collapse = ", ")
          )
          break
        }
      }

      # If no preferred variable found, use first one
      if (is.null(main_var)) {
        main_var <- var_names[[1]]
        message(
          "  Multi-variable NetCDF detected, using first variable '", main_var, "' from: ",
          paste(var_names, collapse = ", ")
        )
      }
    } else {
      main_var <- var_names[[1]]
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
      # No ensemble dimension - return try-error to indicate missing dimension
      structure("no_ensemble_dim", class = "try-error")
    }

    gwl_vals <- if (length(gwl_dim) > 0 && gwl_dim[1] %in% names(nc$dim)) {
      nc$dim[[gwl_dim[1]]]$vals
    } else if (length(gwl_dim) > 0) {
      try(ncdf4::ncvar_get(nc, gwl_dim[1]), silent = TRUE)
    } else {
      # No GWL dimension - return try-error to indicate missing dimension
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
      # No season dimension - return try-error to indicate missing dimension
      structure("no_season_dim", class = "try-error")
    }

    # Determine ensemble values to iterate over
    # Only load 'mean' ensemble by default to avoid iteration over all ensemble values
    ensemble_values <- list(list(idx = 1L, label = "mean")) # Default: only mean ensemble

    if (!inherits(ens_vals, "try-error") && length(ens_vals) > 0) {
      # Convert to character for consistent handling
      if (is.factor(ens_vals)) {
        ens_chars <- as.character(ens_vals)
      } else if (is.character(ens_vals)) {
        ens_chars <- ens_vals
      } else {
        ens_chars <- as.character(ens_vals)
      }

      # Find the 'mean' ensemble index if it exists
      mean_idx <- which(ens_chars == "mean")
      if (length(mean_idx) > 0) {
        # Use only the mean ensemble
        ensemble_values <- list(list(idx = as.integer(mean_idx[1]), label = "mean"))
      } else {
        # If no 'mean' ensemble found, use the first one but label it as 'mean'
        ensemble_values <- list(list(idx = 1L, label = "mean"))
      }
    }

    # Indices helpers for start/count
    dim_index <- function(nm, idx_full = TRUE, pick = NULL) {
      pos <- match(nm, dim_names)
      if (is.na(pos)) {
        return(list(start = 1L, count = -1L, pos = NA_integer_))
      }
      if (isTRUE(idx_full)) {
        return(list(start = 1L, count = -1L, pos = pos))
      }
      list(start = as.integer(pick), count = 1L, pos = pos)
    }

    # Iterate over GWL, return_period, season, and ensemble values
    n_gwl <- if (inherits(gwl_vals, "try-error")) 1L else length(gwl_vals)
    n_rp <- if (inherits(rp_vals, "try-error")) 1L else length(rp_vals)
    n_season <- if (inherits(season_vals, "try-error")) 1L else length(season_vals)

    for (ig in seq_len(n_gwl)) {
      for (ir in seq_len(n_rp)) {
        for (is in seq_len(n_season)) {
          # Iterate over ensemble values (only mean by default)
          for (ie in seq_along(ensemble_values)) {
            ens_info <- ensemble_values[[ie]]
            ens_idx <- ens_info$idx
            ens_label <- ens_info$label

            # Build start/count vectors aligned to var dim order
            sc_list <- vector("list", length(dim_names))
            for (k in seq_along(dim_names)) {
              nm <- dim_names[k]
              if (nm == lon_dim[1] || nm == lat_dim[1]) {
                sc_list[[k]] <- list(start = 1L, count = -1L)
              } else if (length(ens_dim) > 0 && nm == ens_dim[1]) {
                sc_list[[k]] <- list(start = ens_idx, count = 1L)
              } else if (length(gwl_dim) > 0 && nm == gwl_dim[1]) {
                sc_list[[k]] <- list(start = ig, count = 1L)
              } else if (nm == rp_dim) {
                sc_list[[k]] <- list(start = ir, count = 1L)
              } else if (length(season_dim) > 0 && nm == season_dim[1]) {
                sc_list[[k]] <- list(start = is, count = 1L)
              } else {
                sc_list[[k]] <- list(start = 1L, count = -1L)
              }
            }
            start <- vapply(sc_list, function(z) z$start, integer(1))
            count <- vapply(sc_list, function(z) z$count, integer(1))

            # Read the 2D slice
            slice <- ncdf4::ncvar_get(nc, main_var, start = start, count = count)

            # Normalize lon/lat vectors
            if (inherits(lon_vals, "try-error")) {
              # Infer from slice ncol if needed (ideal path assumption)
              lon_vals <- seq_len(dim(slice)[1])
            }
            if (inherits(lat_vals, "try-error")) {
              lat_vals <- seq_len(dim(slice)[2])
            }

            # Calculate resolution and extent
            # Coordinates in NC files are cell centers; we need to extend by half-pixel
            n_lon <- length(lon_vals)
            n_lat <- length(lat_vals)

            # Resolution: spacing between coordinate centers
            res_lon <- if (n_lon > 1) (max(lon_vals) - min(lon_vals)) / (n_lon - 1) else 1.0
            res_lat <- if (n_lat > 1) (max(lat_vals) - min(lat_vals)) / (n_lat - 1) else 1.0

            # Extent: expand by half-pixel on each side to convert centers to edges
            xmin <- min(lon_vals) - res_lon / 2
            xmax <- max(lon_vals) + res_lon / 2
            ymin <- min(lat_vals) - res_lat / 2
            ymax <- max(lat_vals) + res_lat / 2

            # Ensure correct orientation: rows = lat (descending), cols = lon (ascending)
            if (length(dim(slice)) == 2L) {
              if (identical(dim(slice), c(length(lon_vals), length(lat_vals)))) {
                mat <- t(slice)
              } else if (identical(dim(slice), c(length(lat_vals), length(lon_vals)))) {
                mat <- slice
              } else {
                mat <- t(slice)
              }
            } else {
              mat <- as.matrix(slice)
            }
            # Flip rows so that first row is max(lat)
            mat <- mat[rev(seq_len(nrow(mat))), , drop = FALSE]

            r <- terra::rast(
              ncols = n_lon,
              nrows = n_lat,
              xmin = xmin, xmax = xmax,
              ymin = ymin, ymax = ymax,
              crs = "EPSG:4326"
            )
            terra::values(r) <- as.vector(mat)

            # Validate that raster is single-band
            if (terra::nlyr(r) != 1) {
              stop(
                "Expected single-band raster from NetCDF file '", basename(f),
                "', but got ", terra::nlyr(r), " bands. ",
                "Each hazard scenario should be a single 2D layer."
              )
            }

            # Compose hazard name including ensemble/statistic
            gwl_label <- if (inherits(gwl_vals, "try-error")) paste0("idx", ig) else as.character(gwl_vals[ig])
            rp_label <- if (inherits(rp_vals, "try-error")) paste0("idx", ir) else as.character(rp_vals[ir])
            season_label <- if (inherits(season_vals, "try-error")) NA_character_ else as.character(season_vals[is])


            # Unified hazard name WITH ensemble suffix for NC files
            # NC files always use mean ensemble during load
            # Include season in hazard_name if present (e.g., for Drought SPI3)
            hazard_name <- if (!is.na(season_label) && !inherits(season_vals, "try-error")) {
              paste0(
                hazard_type, "__", hazard_indicator,
                "__GWL=", gwl_label,
                "__RP=", rp_label,
                "__season=", season_label,
                "__ensemble=", ens_label
              )
            } else {
              paste0(
                hazard_type, "__", hazard_indicator,
                "__GWL=", gwl_label,
                "__RP=", rp_label,
                "__ensemble=", ens_label
              )
            }

            results[[hazard_name]] <- r

            # Build inventory row
            rp_numeric <- suppressWarnings(as.numeric(rp_label))
            if (is.na(rp_numeric)) rp_numeric <- ir

            inventory_rows[[length(inventory_rows) + 1]] <- tibble::tibble(
              hazard_type = hazard_type,
              hazard_indicator = hazard_indicator,
              scenario_name = gwl_label,
              hazard_return_period = rp_numeric,
              hazard_name = hazard_name,
              ensemble = ens_label,
              season = season_label,
              source = "nc"
            )
          }
        }
      }
    }

    # Close the NetCDF file after processing all slices
    try(ncdf4::nc_close(nc), silent = TRUE)
  }

  # Combine inventory
  inventory <- if (length(inventory_rows) > 0) {
    dplyr::bind_rows(inventory_rows)
  } else {
    tibble::tibble()
  }

  return(list(
    hazards = results,
    inventory = inventory
  ))
}
