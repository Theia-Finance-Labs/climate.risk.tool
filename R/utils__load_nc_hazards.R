#' Load NetCDF cube format with terra
#'
#' @description
#' Loads multi-layer NetCDF files where terra can directly parse the structure.
#' This is used for "cube" format files where each layer represents a combination
#' of scenario and return period, with layer names like:
#' "flood_depth_return_period=2_scenario=_1"
#'
#' @param file_path Character. Path to the NetCDF file
#' @param terra_rast SpatRaster. Pre-loaded terra raster object
#' @return List with `hazards` (named list of SpatRaster) and `inventory_rows` (list of tibbles)
#' @noRd
load_nc_cube_with_terra <- function(file_path, terra_rast) {
  # Parse folder structure for hazard_type and hazard_indicator
  # Expected structure: .../hazards/{hazard_type}/{hazard_indicator?}/file.nc
  # Or: .../hazards/{hazard_type}/file.nc
  parts <- strsplit(normalizePath(file_path), .Platform$file.sep, fixed = TRUE)[[1]]

  # Find the "hazards" directory index
  hazards_idx <- which(parts == "hazards")

  if (length(hazards_idx) > 0 && length(parts) > hazards_idx[length(hazards_idx)]) {
    # Get parts after "hazards" directory
    after_hazards <- parts[(hazards_idx[length(hazards_idx)] + 1):length(parts)]

    if (length(after_hazards) >= 2) {
      # hazards/{hazard_type}/file.nc or hazards/{hazard_type}/{indicator}/file.nc
      hazard_type <- after_hazards[1]

      if (length(after_hazards) == 2) {
        # File is directly in hazard_type folder
        # Extract indicator from filename
        file_name <- after_hazards[2]
        # Try to extract indicator from filename (e.g., "GIRI_flood_depth_cube.nc" -> "flood_depth")
        indicator_match <- regmatches(file_name, regexpr("_([a-z_]+)_cube", file_name))
        if (length(indicator_match) > 0) {
          hazard_indicator <- sub("^_", "", sub("_cube$", "", indicator_match))
        } else {
          hazard_indicator <- sub("\\.nc$", "", file_name)
        }
      } else {
        # File is in hazard_type/indicator folder
        hazard_indicator <- after_hazards[2]
      }
    } else {
      hazard_type <- "unknown"
      hazard_indicator <- "indicator"
    }
  } else {
    # Fallback to old logic if "hazards" not found
    if (length(parts) >= 3) {
      hazard_indicator <- parts[length(parts) - 1]
      hazard_type <- parts[length(parts) - 2]
    } else {
      hazard_indicator <- "indicator"
      hazard_type <- "unknown"
    }
  }

  # Try to read dimension values from NetCDF metadata
  scenario_mapping <- list()
  ensemble_mapping <- list()
  gwl_mapping <- list()

  nc <- try(ncdf4::nc_open(file_path), silent = TRUE)
  if (!inherits(nc, "try-error")) {
    # Check if scenario dimension exists
    if ("scenario" %in% names(nc$dim)) {
      scenario_vals <- nc$dim[["scenario"]]$vals
      for (i in seq_along(scenario_vals)) {
        scenario_mapping[[paste0("_", i)]] <- as.character(scenario_vals[i])
      }
    }

    # Check if ensemble dimension exists
    if ("ensemble" %in% names(nc$dim)) {
      ensemble_vals <- nc$dim[["ensemble"]]$vals
      for (i in seq_along(ensemble_vals)) {
        ensemble_mapping[[as.character(i)]] <- as.character(ensemble_vals[i])
      }
    }

    # Check if GWL dimension exists
    if ("GWL" %in% names(nc$dim)) {
      gwl_vals <- nc$dim[["GWL"]]$vals
      for (i in seq_along(gwl_vals)) {
        gwl_mapping[[as.character(i)]] <- as.character(gwl_vals[i])
      }
    }

    try(ncdf4::nc_close(nc), silent = TRUE)
  }

  # Get layer names
  layer_names <- names(terra_rast)

  # Parse layer names to extract metadata
  # Expected formats:
  # - "variable_return_period=X_scenario=_Y"
  # - "variable_ensemble=_GWL=_return_period=X_Y" where Y is ensemble index
  results <- list()
  inventory_rows <- list()

  for (i in seq_len(terra::nlyr(terra_rast))) {
    layer_name <- layer_names[i]
    layer_rast <- terra_rast[[i]]

    # Validate that extracted layer is single-band
    if (terra::nlyr(layer_rast) != 1) {
      stop(
        "Expected single-band layer from NetCDF file '", basename(file_path),
        "', but layer ", i, " ('", layer_name, "') has ", terra::nlyr(layer_rast), " bands. ",
        "Each layer in a multi-layer NC file should represent a single hazard scenario."
      )
    }

    # Parse layer name for return_period
    # Example: "SPI6_ensemble=_GWL=_return_period=5_1"
    rp_match <- regmatches(layer_name, regexpr("return_period=([0-9]+)", layer_name))

    if (length(rp_match) > 0) {
      rp_str <- sub("return_period=", "", rp_match)
      rp_numeric <- as.numeric(rp_str)
    } else {
      rp_str <- paste0("idx", i)
      rp_numeric <- i
    }

    # Parse for scenario
    scenario_match <- regmatches(layer_name, regexpr("scenario=(_[0-9]+)", layer_name))
    if (length(scenario_match) > 0) {
      scenario_idx <- sub("scenario=", "", scenario_match)
      # Look up actual scenario name from mapping
      if (scenario_idx %in% names(scenario_mapping)) {
        scenario_str <- scenario_mapping[[scenario_idx]]
      } else {
        scenario_str <- paste0("scenario", scenario_idx)
      }
    } else {
      scenario_str <- "unknown"
    }

    # Parse for GWL and ensemble based on file structure
    gwl_str <- "present" # Default
    ensemble_str <- "mean" # Default - always use mean ensemble

    # Check if this is a GIRI-style file (explicit scenario indices) vs ensemble-style file (combination indices)
    has_explicit_scenario <- grepl("scenario=_", layer_name)
    has_gwl_in_name <- grepl("GWL=", layer_name)

    if (has_explicit_scenario && !has_gwl_in_name) {
      # GIRI-style file: scenario index is explicit in layer name
      # Example: "flood_depth_return_period=2_scenario=_1"
      # Use the scenario_str that was already parsed above
      gwl_str <- scenario_str
      ensemble_str <- "mean" # GIRI files don't have ensemble dimension
    } else {
      # Ensemble-style file: trailing number is combination index
      # Example: "SPI6_ensemble=_GWL=_return_period=5_1"
      trailing_match <- regmatches(layer_name, regexpr("_([0-9]+)$", layer_name))
      if (length(trailing_match) > 0) {
        trailing_idx <- sub("^_", "", trailing_match)
        idx_numeric <- as.numeric(trailing_idx)

        if (!is.na(idx_numeric) && length(gwl_mapping) > 0 && length(ensemble_mapping) > 0) {
          # The trailing number is a combination index representing both GWL and ensemble
          # Calculate GWL and ensemble indices from the combination index
          n_ensemble <- length(ensemble_mapping)
          gwl_idx <- ((idx_numeric - 1) %/% n_ensemble) + 1
          ensemble_idx <- ((idx_numeric - 1) %% n_ensemble) + 1

          # Look up actual values from mappings
          if (as.character(gwl_idx) %in% names(gwl_mapping)) {
            gwl_str <- gwl_mapping[[as.character(gwl_idx)]]
          }
          # Only use ensemble if it's 'mean', otherwise skip this layer
          if (as.character(ensemble_idx) %in% names(ensemble_mapping)) {
            potential_ensemble <- ensemble_mapping[[as.character(ensemble_idx)]]
            if (potential_ensemble != "mean") {
              # Skip this layer - we only want mean ensemble
              next
            }
            ensemble_str <- potential_ensemble
          }
        } else if (!is.na(idx_numeric) && length(gwl_mapping) > 0 && length(ensemble_mapping) == 0) {
          # Only GWL dimension, no ensemble
          if (as.character(idx_numeric) %in% names(gwl_mapping)) {
            gwl_str <- gwl_mapping[[as.character(idx_numeric)]]
          }
        } else if (!is.na(idx_numeric) && length(ensemble_mapping) > 0 && length(gwl_mapping) == 0) {
          # Only ensemble dimension, no GWL
          if (as.character(idx_numeric) %in% names(ensemble_mapping)) {
            potential_ensemble <- ensemble_mapping[[as.character(idx_numeric)]]
            if (potential_ensemble != "mean") {
              # Skip this layer - we only want mean ensemble
              next
            }
            ensemble_str <- potential_ensemble
          }
        }
      }

      # Fallback: try to parse GWL from layer name if combination parsing failed
      if (gwl_str == "present") {
        gwl_match <- regmatches(layer_name, regexpr("GWL=([^_]*)", layer_name))
        if (length(gwl_match) > 0) {
          gwl_str <- sub("GWL=", "", gwl_match)
          if (gwl_str == "" || gwl_str == "_") {
            gwl_str <- "present"
          }
        }
      }
    }


    # Unified hazard name WITHOUT ensemble suffix for inventory
    hazard_name <- paste0(
      hazard_type, "__", hazard_indicator,
      "__GWL=", gwl_str,
      "__RP=", rp_str,
      "__ensemble=", ensemble_str
    )

    results[[hazard_name]] <- layer_rast

    # Build inventory row
    inventory_rows[[length(inventory_rows) + 1]] <- tibble::tibble(
      hazard_type = hazard_type,
      hazard_indicator = hazard_indicator,
      scenario_name = gwl_str,
      hazard_return_period = rp_numeric,
      scenario_code = gwl_str,
      hazard_name = hazard_name,
      ensemble = ensemble_str,
      source = "nc"
    )
  }

  return(list(
    hazards = results,
    inventory_rows = inventory_rows
  ))
}

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
#' like ["mean", "median", "p10", "p90"], only the 'mean' ensemble is loaded by default.
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
#' @return List with two elements: `hazards` (named list of SpatRaster) and
#'   `inventory` (tibble with hazard metadata)
#' @noRd
load_nc_hazards_with_metadata <- function(hazards_dir) {
  nc_files <- list.files(hazards_dir, pattern = "\\.nc$", full.names = TRUE, recursive = TRUE)
  if (length(nc_files) == 0) {
    return(list(
      hazards = list(),
      inventory = tibble::tibble()
    ))
  }

  results <- list()
  inventory_rows <- list()

  for (f in nc_files) {
    # Try to load with terra first to check if it's a cube format
    terra_rast <- try(terra::rast(f), silent = TRUE)

    # Check if this is a "cube" format NetCDF by examining layer names
    # Cube format has metadata in layer names like: "variable_return_period=X_scenario=_Y"
    # Dimension format has generic names and uses NetCDF dimensions for metadata
    is_cube_format <- FALSE
    if (!inherits(terra_rast, "try-error") && terra::nlyr(terra_rast) > 1) {
      layer_names <- names(terra_rast)
      # Check if layer names contain metadata patterns
      has_rp_in_name <- any(grepl("return_period=", layer_names))
      has_scenario_in_name <- any(grepl("scenario=", layer_names))
      is_cube_format <- has_rp_in_name || has_scenario_in_name
    }

    if (is_cube_format) {
      # This is a cube format - use terra to load it efficiently
      message("  Loading multi-layer NetCDF with terra: ", basename(f))

      result <- load_nc_cube_with_terra(f, terra_rast)
      results <- c(results, result$hazards)
      inventory_rows <- c(inventory_rows, result$inventory_rows)
      next
    }
    # Path parsing: .../hazards/{hazard_type}/{hazard_indicator}/{model_type}/{file}.nc
    parts <- strsplit(normalizePath(f), .Platform$file.sep, fixed = TRUE)[[1]]
    # Find indices for segments
    if (length(parts) >= 4) {
      # Use last 4 meaningful segments: {hazard_type}/{hazard_indicator}/{model_type}/{file}
      file_name <- parts[length(parts)]
      model_type <- parts[length(parts) - 1]
      hazard_indicator <- parts[length(parts) - 2]
      hazard_type <- parts[length(parts) - 3]
    } else {
      # Fallbacks (ideal-path assumption per user instruction)
      file_name <- basename(f)
      model_type <- "ensemble"
      hazard_indicator <- "indicator"
      hazard_type <- "unknown"
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

    # Other dims - check for scenario, ensemble, GWL, return_period
    ens_dim <- dim_names[vapply(dim_names, function(nm) name_eq(nm, c("ensemble")), logical(1))]
    gwl_dim <- dim_names[vapply(dim_names, function(nm) name_eq(nm, c("gwl", "GWL", "scenario")), logical(1))]
    # Heuristic: remaining non-spatial, non-ensemble, non-GWL dim is return period
    remaining <- setdiff(dim_names, c(lon_dim[1], lat_dim[1], ens_dim, gwl_dim))
    rp_dim <- if (length(remaining) > 0) remaining[[1]] else "return_period"

    # Get dimension values from nc$dim structure
    ens_vals <- if (length(ens_dim) > 0 && ens_dim[1] %in% names(nc$dim)) {
      nc$dim[[ens_dim[1]]]$vals
    } else {
      try(ncdf4::ncvar_get(nc, ens_dim[1]), silent = TRUE)
    }

    gwl_vals <- if (length(gwl_dim) > 0 && gwl_dim[1] %in% names(nc$dim)) {
      nc$dim[[gwl_dim[1]]]$vals
    } else {
      try(ncdf4::ncvar_get(nc, gwl_dim[1]), silent = TRUE)
    }

    rp_vals <- if (rp_dim %in% names(nc$dim)) {
      nc$dim[[rp_dim]]$vals
    } else {
      try(ncdf4::ncvar_get(nc, rp_dim), silent = TRUE)
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

    # Iterate over GWL, return_period, and ensemble values
    n_gwl <- if (inherits(gwl_vals, "try-error")) 1L else length(gwl_vals)
    n_rp <- if (inherits(rp_vals, "try-error")) 1L else length(rp_vals)

    for (ig in seq_len(n_gwl)) {
      for (ir in seq_len(n_rp)) {
        # Iterate over ALL ensemble values (mean, median, p10, p90, etc.)
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
              "', but got ", terra::nlyr(r), " bands for GWL=", GWL_val, " RP=", rp_val, ", ensemble=", ens_val, ". ",
              "Each hazard scenario should be a single 2D layer."
            )
          }

          # Compose hazard name including ensemble/statistic
          gwl_label <- if (inherits(gwl_vals, "try-error")) paste0("idx", ig) else as.character(gwl_vals[ig])
          rp_label <- if (inherits(rp_vals, "try-error")) paste0("idx", ir) else as.character(rp_vals[ir])


          # Unified hazard name WITH ensemble suffix for NC files
          # NC files always use mean ensemble during load
          hazard_name <- paste0(
            hazard_type, "__", hazard_indicator,
            "__GWL=", gwl_label,
            "__RP=", rp_label,
            "__ensemble=", ens_label
          )

          results[[hazard_name]] <- r

          # Build inventory row
          rp_numeric <- suppressWarnings(as.numeric(rp_label))
          if (is.na(rp_numeric)) rp_numeric <- ir

          inventory_rows[[length(inventory_rows) + 1]] <- tibble::tibble(
            hazard_type = hazard_type,
            hazard_indicator = hazard_indicator,
            scenario_name = gwl_label,
            hazard_return_period = rp_numeric,
            scenario_code = gwl_label,
            hazard_name = hazard_name,
            ensemble = ens_label,
            source = "nc"
          )
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
