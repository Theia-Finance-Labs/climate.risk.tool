#' Load hazards (TIF + NC) and build complete inventory
#'
#' @title Load all hazard data and generate inventory
#' @description Self-contained loader that:
#' 1. Auto-discovers `hazards_metadata.csv` for TIF files (if present)
#' 2. Loads TIF rasters from mapping
#' 3. Scans directory tree for NetCDF files and loads them
#' 4. Generates a unified inventory combining TIF and NC metadata
#' 5. Returns both hazards and inventory
#'
#' @param hazards_dir Character. Root directory that contains hazard files and subdirectories
#' @param aggregate_factor Integer >= 1. Aggregation factor for TIF rasters (default: 1)
#' @return A list with two elements:
#'   - `hazards`: Nested list with `tif` and `nc` keys, each containing named SpatRaster objects
#'   - `inventory`: Tibble with columns: hazard_type, scenario_name, hazard_return_period, 
#'     scenario_code, hazard_name, source (either "tif" or "nc")
#' @examples
#' \dontrun{
#' result <- load_hazards_and_inventory(
#'   hazards_dir = file.path(base_dir, "hazards"),
#'   aggregate_factor = 1L
#' )
#' 
#' # Access hazards (flattened for compute)
#' all_hazards <- c(result$hazards$tif, result$hazards$nc)
#' 
#' # Access inventory (for UI dropdowns)
#' inventory <- result$inventory
#' }
#' @export
load_hazards_and_inventory <- function(hazards_dir, aggregate_factor = 1L) {
  message("[load_hazards_and_inventory] Starting hazard loading and inventory...")
  
  # Try to find and read TIF mapping file
  parent_dir <- dirname(hazards_dir)
  mapping_path <- file.path(parent_dir, "hazards_metadata.csv")
  
  tif_list <- list()
  tif_inventory <- tibble::tibble()
  
  if (file.exists(mapping_path)) {
    message("  Found TIF mapping at: ", mapping_path)
    mapping_df <- read_hazards_mapping(mapping_path)
    
    tif_list <- load_hazards_from_mapping_internal(
      mapping_df = mapping_df,
      hazards_dir = hazards_dir,
      aggregate_factor = as.integer(aggregate_factor)
    )
    
    # Build TIF inventory
    tif_inventory <- mapping_df |>
      dplyr::mutate(
        hazard_name = paste0(
          .data$hazard_type, "__",
          .data$scenario_code, "_h",
          .data$hazard_return_period, "glob"
        ),
        source = "tif"
      ) |>
      dplyr::select(
        .data$hazard_type,
        .data$hazard_indicator,
        .data$scenario_name,
        .data$hazard_return_period,
        .data$scenario_code,
        .data$hazard_name,
        .data$source
      )
  } else {
    message("  No TIF mapping file found (searched: ", mapping_path, ")")
  }
  
  # Load NC files and build inventory
  nc_result <- load_nc_hazards_with_metadata(hazards_dir = hazards_dir)
  nc_list <- nc_result$hazards
  nc_inventory <- nc_result$inventory
  
  # Combine inventories
  inventory <- dplyr::bind_rows(tif_inventory, nc_inventory)
  
  message("[load_hazards_and_inventory] Complete: ", 
          length(tif_list), " TIF + ", length(nc_list), " NC hazards loaded")
  
  return(list(
    hazards = list(tif = tif_list, nc = nc_list),
    inventory = inventory
  ))
}

#' Internal TIF loader (previously load_hazards_from_mapping)
#' @noRd
load_hazards_from_mapping_internal <- function(mapping_df, hazards_dir, aggregate_factor = 1L) {
  return(load_hazards_from_mapping(
    mapping_df = mapping_df,
    hazards_dir = hazards_dir,
    aggregate_factor = aggregate_factor
  ))
}

#' Load NetCDF hazards and build inventory
#'
#' @description
#' Scans for .nc files in the hazards directory, extracts metadata from the folder
#' structure (hazard_type, hazard_indicator) and from NetCDF dimensions (GWL,
#' return_period, ensemble). For each combination of GWL and return_period where
#' ensemble='mean', creates a separate SpatRaster.
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
#' **For TIF files:** The existing loader (`load_hazards_from_mapping()`) uses
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

    # Identify main data variable (exclude typical coordinate dims)
    var_names <- names(nc$var)
    coord_like <- tolower(names(nc$dim))
    # prefer variable with "_max" suffix if present (FWI_max, HI_max ...)
    main_var <- NULL
    if (length(var_names) > 0) {
      mv <- var_names[grepl("_max$", var_names)]
      main_var <- if (length(mv) > 0) mv[[1]] else var_names[[1]]
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

    lon_vals <- try(ncdf4::ncvar_get(nc, lon_dim[1]), silent = TRUE)
    lat_vals <- try(ncdf4::ncvar_get(nc, lat_dim[1]), silent = TRUE)

    # Other dims
    ens_dim <- dim_names[vapply(dim_names, function(nm) name_eq(nm, c("ensemble")), logical(1))]
    gwl_dim <- dim_names[vapply(dim_names, function(nm) name_eq(nm, c("gwl", "GWL")), logical(1))]
    # Heuristic: remaining non-spatial, non-ensemble, non-GWL dim is return period
    remaining <- setdiff(dim_names, c(lon_dim[1], lat_dim[1], ens_dim, gwl_dim))
    rp_dim <- if (length(remaining) > 0) remaining[[1]] else "return_period"

    ens_vals <- try(ncdf4::ncvar_get(nc, ens_dim[1]), silent = TRUE)
    gwl_vals <- try(ncdf4::ncvar_get(nc, gwl_dim[1]), silent = TRUE)
    rp_vals <- try(ncdf4::ncvar_get(nc, rp_dim), silent = TRUE)

    # Pick ensemble == 'mean'
    ens_idx <- 1L
    if (!inherits(ens_vals, "try-error")) {
      if (is.character(ens_vals)) {
        mi <- which(ens_vals == "mean")
        if (length(mi) == 1) ens_idx <- as.integer(mi)
      } else if (is.factor(ens_vals)) {
        mi <- which(as.character(ens_vals) == "mean")
        if (length(mi) == 1) ens_idx <- as.integer(mi)
      } else {
        ens_idx <- 1L
      }
    }

    # Indices helpers for start/count
    dim_index <- function(nm, idx_full = TRUE, pick = NULL) {
      pos <- match(nm, dim_names)
      if (is.na(pos)) return(list(start = 1L, count = -1L, pos = NA_integer_))
      if (isTRUE(idx_full)) return(list(start = 1L, count = -1L, pos = pos))
      list(start = as.integer(pick), count = 1L, pos = pos)
    }

    # Iterate over GWL and return_period values
    n_gwl <- if (inherits(gwl_vals, "try-error")) 1L else length(gwl_vals)
    n_rp <- if (inherits(rp_vals, "try-error")) 1L else length(rp_vals)

    for (ig in seq_len(n_gwl)) {
      for (ir in seq_len(n_rp)) {
        # Build start/count vectors aligned to var dim order
        sc_list <- vector("list", length(dim_names))
        for (k in seq_along(dim_names)) {
          nm <- dim_names[k]
          if (nm == lon_dim[1] || nm == lat_dim[1]) {
            sc_list[[k]] <- list(start = 1L, count = -1L)
          } else if (nm == ens_dim) {
            sc_list[[k]] <- list(start = ens_idx, count = 1L)
          } else if (nm == gwl_dim) {
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

        # Compose hazard name as agreed
        gwl_label <- if (inherits(gwl_vals, "try-error")) paste0("idx", ig) else as.character(gwl_vals[ig])
        rp_label <- if (inherits(rp_vals, "try-error")) paste0("idx", ir) else as.character(rp_vals[ir])

        hz_name <- paste0(
          hazard_type, "__", hazard_indicator,
          "__GWL=", gwl_label,
          "__RP=", rp_label,
          "__ensemble=mean"
        )

        results[[hz_name]] <- r
        
        # Build inventory row
        rp_numeric <- suppressWarnings(as.numeric(rp_label))
        if (is.na(rp_numeric)) rp_numeric <- ir
        
        inventory_rows[[length(inventory_rows) + 1]] <- tibble::tibble(
          hazard_type = hazard_type,
          hazard_indicator = hazard_indicator,
          scenario_name = gwl_label,
          hazard_return_period = rp_numeric,
          scenario_code = gwl_label,
          hazard_name = hz_name,
          source = "nc"
        )
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


