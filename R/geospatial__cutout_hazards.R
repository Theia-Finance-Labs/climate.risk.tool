#' Cut out hazard values for asset geometries
#'
#' @title Extract hazard raster values for asset polygons
#' @description For each hazard raster, extracts values within each asset's geometry polygon
#'   and adds them as new columns to the assets dataframe. Column names are derived from hazard names.
#'   Optimized to process unique geometries only once per geolocation method.
#' @param assets_with_geometry Data frame with asset information including geometry and geolocation_method columns (from geolocate_assets)
#' @param hazards Named list of hazard rasters (from load_hazards)
#' @param use_exactextractr Logical. If TRUE and package available, use exactextractr for polygon means (faster and accurate)
#' @param parallel Logical. If TRUE and future.apply available, parallelize across hazards
#' @param crop_margin Numeric. Margin added around geometry extent when cropping raster before extraction
#' @return Data frame with original columns plus one numeric column per hazard containing extracted values
#' @examples
#' \dontrun{
#' base_dir <- system.file("tests_data", package = "climate.risk.tool")
#' assets <- read_inputs(base_dir)$assets
#' hazards <- load_hazards(file.path(base_dir, "hazards"))
#' assets_geo <- geolocate_assets(assets, hazards, base_dir)
#' assets_hazards <- cutout_hazards(assets_geo, hazards)
#' }
#' @export
cutout_hazards <- function(assets_with_geometry,
                           hazards,
                           use_exactextractr = TRUE, # Use faster extraction
                           parallel = FALSE,
                           crop_margin = 0.001, # Larger margin for safety
                           batch_groups = TRUE) {
  message("✂️ [cutout_hazards] Starting hazard extraction for ", nrow(assets_with_geometry), " assets...")

  if (!"geometry" %in% names(assets_with_geometry)) {
    stop("Input dataframe must have a 'geometry' column. Use geolocate_assets() first.")
  }

  if (!"geolocation_method" %in% names(assets_with_geometry)) {
    stop("Input dataframe must have a 'geolocation_method' column. Use updated geolocate_assets() first.")
  }

  if (length(hazards) == 0) {
    stop("Hazards list is empty")
  }

  # Configure terra to prefer on-disk processing for large rasters
  old_terra_opts <- terra::terraOptions()
  on.exit(try(terra::terraOptions(old_terra_opts), silent = TRUE), add = TRUE)
  terra::terraOptions(todisk = TRUE, memfrac = 0.3, progress = 0)

  # Aggregation is now done at load time in load_hazards(), so hazards are already optimized

  # Start with the input dataframe
  result <- assets_with_geometry

  # Convert to sf object for spatial operations
  assets_sf <- sf::st_as_sf(assets_with_geometry)

  # Pre-compute indices for each geolocation method (avoid recomputing per hazard)
  municipality_assets <- which(assets_sf$geolocation_method == "municipality")
  province_assets <- which(assets_sf$geolocation_method == "province")
  individual_assets <- which(assets_sf$geolocation_method %in% c("coordinates", "default"))

  # Build unique geometry groups once
  group_indices <- list()
  group_geoms <- list()

  if (length(municipality_assets) > 0) {
    unique_municipalities <- unique(assets_sf$municipality[municipality_assets])
    unique_municipalities <- unique_municipalities[!is.na(unique_municipalities)]
    for (muni in unique_municipalities) {
      idxs <- municipality_assets[assets_sf$municipality[municipality_assets] == muni]
      if (length(idxs) > 0) {
        group_indices[[length(group_indices) + 1L]] <- idxs
        group_geoms[[length(group_geoms) + 1L]] <- assets_sf$geometry[[idxs[1]]]
      }
    }
  }

  if (length(province_assets) > 0) {
    unique_provinces <- unique(assets_sf$province[province_assets])
    unique_provinces <- unique_provinces[!is.na(unique_provinces)]
    for (prov in unique_provinces) {
      idxs <- province_assets[assets_sf$province[province_assets] == prov]
      if (length(idxs) > 0) {
        group_indices[[length(group_indices) + 1L]] <- idxs
        group_geoms[[length(group_geoms) + 1L]] <- assets_sf$geometry[[idxs[1]]]
      }
    }
  }

  if (length(individual_assets) > 0) {
    for (idx in individual_assets) {
      group_indices[[length(group_indices) + 1L]] <- idx
      group_geoms[[length(group_geoms) + 1L]] <- assets_sf$geometry[[idx]]
    }
  }

  # If nothing to extract, early return
  if (length(group_geoms) == 0) {
    for (hazard_name in names(hazards)) {
      result[[hazard_name]] <- vector("list", nrow(assets_sf))
    }
    message("✅ [cutout_hazards] No geometries to extract; returning input with empty list hazard columns")
    return(result)
  }

  # Prepare geometry collection
  geoms_sf <- sf::st_sfc(group_geoms, crs = sf::st_crs(assets_sf))
  # Always extract all values within polygon (no method selection needed)

  # Function to process one hazard
  process_one_hazard <- function(hazard_name, hazard_source) {
    # Initialize column with empty lists
    hazard_values <- vector("list", nrow(assets_sf))

    # Open hazard raster lazily if a path was provided
    hazard_raster <- if (is.character(hazard_source)) terra::rast(hazard_source) else hazard_source
    on.exit(
      {
        try(terra::close(hazard_raster), silent = TRUE)
      },
      add = TRUE
    )

    # Process per-group to bound memory usage
    if (isTRUE(batch_groups)) {
      for (k in seq_along(group_indices)) {
        idxs <- group_indices[[k]]
        geom_k <- geoms_sf[k]

        # Project geometry to hazard CRS
        geom_k_y <- sf::st_transform(geom_k, terra::crs(hazard_raster))

        # Convert to SpatVector
        geoms_k_vect <- terra::vect(geom_k_y)

        # Crop raster to group's extent (+ margin)
        extk <- terra::ext(geoms_k_vect)
        extk <- terra::ext(
          terra::xmin(extk) - crop_margin,
          terra::xmax(extk) + crop_margin,
          terra::ymin(extk) - crop_margin,
          terra::ymax(extk) + crop_margin
        )
        hrk <- try(terra::crop(hazard_raster, extk), silent = TRUE)
        if (!inherits(hrk, "SpatRaster")) {
          hrk <- hazard_raster
        }

        # Aggregation already done at the start, no need to repeat

        # Extract all values within polygon (return raw pixel values)
        if (isTRUE(use_exactextractr) && requireNamespace("exactextractr", quietly = TRUE)) {
          # Extract all values, not aggregated
          val <- try(exactextractr::exact_extract(hrk, sf::st_as_sf(geoms_k_vect)), silent = TRUE)
          if (inherits(val, "try-error") || length(val) == 0) {
            # Fallback to terra extract without aggregation
            extracted <- try(terra::extract(hrk, geoms_k_vect), silent = TRUE)
            pixel_values <- if (inherits(extracted, "try-error")) numeric(0) else extracted[, 2][!is.na(extracted[, 2])]
          } else {
            # exactextractr returns a list of data.frames, get all the values
            pixel_values <- val[[1]]$value[!is.na(val[[1]]$value)]
          }
        } else {
          # Use terra extract without aggregation function, return all values
          extracted <- try(terra::extract(hrk, geoms_k_vect), silent = TRUE)
          pixel_values <- if (inherits(extracted, "try-error")) numeric(0) else extracted[, 2][!is.na(extracted[, 2])]
        }

        # Assign the list of pixel values to all assets in this group
        for (idx in idxs) {
          hazard_values[[idx]] <- pixel_values
        }
      }
    } else {
      # Original batch method across all groups (may use more memory)
      # Determine if all hazards share same CRS to avoid reprojecting every iteration
      target_crs <- terra::crs(hazard_raster)

      geoms_y <- terra::vect(sf::st_transform(geoms_sf, target_crs))

      # Crop raster to union extent (+ margin)
      ext <- terra::ext(geoms_y)
      ext <- terra::ext(
        terra::xmin(ext) - crop_margin,
        terra::xmax(ext) + crop_margin,
        terra::ymin(ext) - crop_margin,
        terra::ymax(ext) + crop_margin
      )
      hr <- try(terra::crop(hazard_raster, ext), silent = TRUE)
      if (!inherits(hr, "SpatRaster")) {
        hr <- hazard_raster
      }

      # Extract all values for each geometry (no aggregation)
      if (isTRUE(use_exactextractr) && requireNamespace("exactextractr", quietly = TRUE)) {
        vals_list <- try(exactextractr::exact_extract(hr, sf::st_as_sf(geoms_y)), silent = TRUE)
        if (inherits(vals_list, "try-error")) {
          # Fallback to terra extract
          vals_list <- vector("list", length(group_indices))
          for (i in seq_along(group_indices)) {
            extracted <- try(terra::extract(hr, geoms_y[i]), silent = TRUE)
            vals_list[[i]] <- if (inherits(extracted, "try-error")) numeric(0) else extracted[, 2][!is.na(extracted[, 2])]
          }
        } else {
          # Process exactextractr results
          vals_list <- lapply(vals_list, function(x) x$value[!is.na(x$value)])
        }
      } else {
        # Use terra extract for each geometry
        vals_list <- vector("list", length(group_indices))
        for (i in seq_along(group_indices)) {
          extracted <- try(terra::extract(hr, geoms_y[i]), silent = TRUE)
          vals_list[[i]] <- if (inherits(extracted, "try-error")) numeric(0) else extracted[, 2][!is.na(extracted[, 2])]
        }
      }

      # Assign pixel values to assets
      for (k in seq_along(group_indices)) {
        idxs <- group_indices[[k]]
        pixel_values <- vals_list[[k]]
        for (idx in idxs) {
          hazard_values[[idx]] <- pixel_values
        }
      }
    }

    hazard_values
  }

  # Process hazards (optionally in parallel)
  hazard_names <- names(hazards)

  if (isTRUE(parallel) && requireNamespace("future.apply", quietly = TRUE)) {
    vals_list <- future.apply::future_lapply(
      X = hazard_names,
      FUN = function(hn) process_one_hazard(hn, hazards[[hn]]),
      future.seed = TRUE
    )
  } else if (isTRUE(parallel) && requireNamespace("parallel", quietly = TRUE)) {
    # Fallback simple parallelism
    vals_list <- parallel::mclapply(hazard_names, function(hn) process_one_hazard(hn, hazards[[hn]]))
  } else {
    vals_list <- lapply(hazard_names, function(hn) process_one_hazard(hn, hazards[[hn]]))
  }

  # Attach results
  for (i in seq_along(hazard_names)) {
    result[[hazard_names[[i]]]] <- vals_list[[i]]
  }

  message("✅ [cutout_hazards] Hazard extraction completed for ", nrow(assets_with_geometry), " assets")
  return(result)
}

# Helper function to extract all hazard values for a single geometry
#' @noRd
extract_hazard_values <- function(asset_geom, hazard_raster) {
  # Convert sf geometry to terra SpatVector for extraction
  asset_vect <- terra::vect(asset_geom)

  # Ensure CRS compatibility
  if (!identical(terra::crs(asset_vect), terra::crs(hazard_raster))) {
    asset_vect <- terra::project(asset_vect, terra::crs(hazard_raster))
  }

  # Extract all values within the polygon (no aggregation)
  extracted <- terra::extract(hazard_raster, asset_vect)

  # Return all non-NA values
  if (nrow(extracted) > 0) {
    values <- extracted[, 2][!is.na(extracted[, 2])]
    return(values)
  } else {
    return(numeric(0))
  }
}
