#' Load hazard rasters from mapping metadata (INTERNAL)
#'
#' @description Internal function used by load_hazards_and_inventory().
#'   Loads hazard rasters based on a mapping dataframe that defines
#'   hazard_file, hazard_type, scenario_name, and return_period.
#'   Validates that all files exist and that there are no duplicates on the filtering
#'   columns (hazard_type, scenario_name, return_period).
#' @param mapping_df Data frame with columns: hazard_file, hazard_type,
#'   scenario_name, return_period
#' @param hazards_dir Character path to the directory containing hazard files (will search subdirectories)
#' @param aggregate_factor Integer >= 1. If >1, aggregate rasters by this factor during loading for speed (default: 1)
#' @param cache_aggregated Logical. If TRUE and aggregate_factor > 1, save and reuse aggregated rasters (default: TRUE)
#' @param force_reaggregate Logical. If TRUE, recompute aggregated rasters even if cached files exist (default: FALSE)
#' @param memfrac Numeric in (0,1]. Memory fraction hint passed to terra options during load (default: 0.3)
#' @return Named list of SpatRaster objects
#' @importFrom utils head
#' @noRd
load_tif_hazards <- function(mapping_df,
                             hazards_dir,
                             aggregate_factor = 1L,
                             cache_aggregated = TRUE,
                             force_reaggregate = FALSE,
                             memfrac = 0.3,
                             indicator_index_dims = NULL,
                             indicator_ensemble = NULL) {
  message("[load_tif_hazards] Loading hazards...")


  mapping <- tibble::as_tibble(mapping_df)
  message("  Found ", nrow(mapping), " hazard entries in mapping")

  # Check for duplicates on filter columns
  filter_cols <- c("hazard_type", "scenario_name", "return_period")
  duplicates <- mapping |>
    dplyr::group_by(dplyr::across(dplyr::all_of(filter_cols))) |>
    dplyr::filter(dplyr::n() > 1) |>
    dplyr::ungroup()

  if (nrow(duplicates) > 0) {
    dup_info <- duplicates |>
      dplyr::select(dplyr::all_of(filter_cols)) |>
      dplyr::distinct()

    stop(
      "Found duplicate entries for filter columns (hazard_type, scenario_name, return_period):\n",
      paste(utils::capture.output(print(dup_info)), collapse = "\n")
    )
  }

  # Find full paths for all hazard files
  message("  Validating hazard files exist...")
  all_tif_files <- list.files(hazards_dir,
    pattern = "\\.tif$",
    full.names = TRUE, recursive = TRUE
  )


  if (length(all_tif_files) == 0) {
    warning(
      "No .tif files found in hazards directory: ", hazards_dir,
      ". Returning empty list (NC files may still be available)."
    )
    return(list())
  }

  mapping <- mapping |>
    dplyr::mutate(full_path = NA_character_)

  files_not_found <- character(0)

  for (i in seq_len(nrow(mapping))) {
    hazard_file <- mapping$hazard_file[i]

    # Find matching file (basename match)
    matching_files <- all_tif_files[basename(all_tif_files) == hazard_file]

    if (length(matching_files) == 0) {
      # Fallback: Try to find any aggregated version if the original is missing.
      # This is crucial for tests that only ship aggregated files.
      base_name <- tools::file_path_sans_ext(hazard_file)
      
      # 1. Try specifically with the requested aggregate_factor if > 1
      if (is.numeric(aggregate_factor) && aggregate_factor > 1L) {
        agg_pattern <- paste0("^", base_name, "__agg", aggregate_factor, ".tif$")
        agg_files <- all_tif_files[grepl(agg_pattern, basename(all_tif_files))]
        if (length(agg_files) > 0) {
          mapping$full_path[i] <- agg_files[1]
          next
        }
      }
      
      # 2. General fallback: find any aggregated version (__agg{N}.tif)
      general_agg_pattern <- paste0("^", base_name, "__agg\\d+.tif$")
      general_agg_files <- all_tif_files[grepl(general_agg_pattern, basename(all_tif_files))]
      
      if (length(general_agg_files) > 0) {
        # Prefer the first one found if multiple exist
        mapping$full_path[i] <- general_agg_files[1]
        message("  Fallback: using aggregated version for ", hazard_file, ": ", basename(general_agg_files[1]))
        next
      }

      files_not_found <- c(files_not_found, hazard_file)
      next
    }

    if (length(matching_files) > 1) {
      warning("Multiple files found for ", hazard_file, ", using first: ", matching_files[1])
    }

    mapping$full_path[i] <- matching_files[1]
  }

  # Filter out rows where files weren't found
  mapping <- mapping |>
    dplyr::filter(!is.na(.data$full_path))

  if (length(files_not_found) > 0) {
    warning(
      "Some TIF files from mapping not found (", length(files_not_found), "): ",
      paste(head(files_not_found, 3), collapse = ", "),
      if (length(files_not_found) > 3) "..." else ""
    )
  }

  if (nrow(mapping) == 0) {
    warning(
      "No TIF files from mapping found in hazards directory: ", hazards_dir,
      ". Returning empty list (NC files may still be available)."
    )
    return(list())
  }

  message("  ", nrow(mapping), " hazard files validated")

  # Load rasters
  message("[load_tif_hazards] Loading ", nrow(mapping), " rasters...")

  # Configure terra for efficient loading
  old_opts <- terra::terraOptions()
  restore_opts <- old_opts[setdiff(names(old_opts), "metadata")]
  on.exit(try(do.call(terra::terraOptions, restore_opts), silent = TRUE), add = TRUE)
  terra::terraOptions(todisk = TRUE, memfrac = memfrac, progress = 0)

  # Create named list for rasters
  # Use unified naming format (WITH ensemble=mean for consistency)
  # Structured naming based on indicator file and variable
  rasters <- list()
  for (i in seq_len(nrow(mapping))) {
    row <- mapping[i, ]
    
    # Build index values list for this row.
    # When indicator_index_dims is provided (from the hazard config's index field),
    # only include dims declared in that list so raster keys stay aligned with
    # the inventory keys and precomputed CSV keys.
    index_dims <- if (!is.null(indicator_index_dims)) indicator_index_dims else
      c("return_period", "gwl", "scenario_name", "season")
    ens <- if (!is.null(indicator_ensemble) && nzchar(indicator_ensemble)) indicator_ensemble else "mean"

    index_values <- list(
      return_period = if ("return_period" %in% index_dims && "return_period" %in% names(row)) row$return_period else NA_real_,
      gwl           = if ("gwl"           %in% index_dims && "gwl"           %in% names(row)) row$gwl           else NA_character_,
      scenario_name = if ("scenario_name" %in% index_dims && "scenario_name" %in% names(row)) row$scenario_name else NA_character_,
      season        = if ("season"        %in% index_dims && "season"        %in% names(row)) row$season        else NA_character_
    )

    # Construct structured name
    raster_name <- build_indicator_key(
      indicator_file = basename(dirname(row$full_path)),
      indicator_variable = if ("variable" %in% names(row) && !is.na(row$variable) && nzchar(as.character(row$variable))) {
        as.character(row$variable)
      } else {
        as.character(row$hazard_indicator)
      },
      index_values = index_values,
      ensemble = ens
    )
    
    tif_file <- row$full_path
    message("  Loading [", i, "/", nrow(mapping), "]: ", basename(tif_file))

    # Load raster
    r <- terra::rast(tif_file)

    # Validate that raster is single-band
    if (terra::nlyr(r) != 1) {
      stop(
        "Expected single-band TIF file '", basename(tif_file),
        "', but got ", terra::nlyr(r), " bands. ",
        "Each TIF file should contain only one hazard scenario layer."
      )
    }

    # Handle aggregation if requested
    if (is.numeric(aggregate_factor) && aggregate_factor > 1L) {
      # Check if already aggregated at target factor
      base_filename <- basename(tif_file)
      target_agg_pattern <- paste0("__agg", aggregate_factor, "\\.tif$")
      is_already_target_aggregated <- grepl(target_agg_pattern, base_filename)

      if (!is_already_target_aggregated) {
        orig_dims <- dim(r)
        message("    Original dimensions: ", paste(orig_dims, collapse = "x"))
        message("    Aggregating by factor ", aggregate_factor, "...")

        # Determine original filename for cache naming
        if (grepl("__agg\\d+\\.tif$", base_filename)) {
          original_name <- sub("__agg\\d+\\.tif$", ".tif", base_filename)
        } else {
          original_name <- base_filename
        }

        cache_filename <- paste0(
          tools::file_path_sans_ext(original_name),
          "__agg", aggregate_factor, ".tif"
        )
        cache_file <- file.path(dirname(tif_file), cache_filename)

        if (isTRUE(cache_aggregated) && file.exists(cache_file) && !isTRUE(force_reaggregate)) {
          message("    Using cached aggregated raster: ", cache_filename)
          r <- terra::rast(cache_file)
        } else {
          # Perform aggregation
          r_agg <- try(
            terra::aggregate(
              r,
              fact = aggregate_factor,
              fun = mean,
              na.rm = TRUE,
              filename = if (isTRUE(cache_aggregated)) cache_file else "",
              overwrite = TRUE,
              wopt = list(
                datatype = "FLT4S",
                gdal = c("BIGTIFF=YES", "COMPRESS=LZW", "TILED=YES")
              )
            ),
            silent = TRUE
          )

          if (!inherits(r_agg, "try-error")) {
            new_dims <- dim(r_agg)
            reduction <- prod(orig_dims) / prod(new_dims)
            message(
              "    New dimensions: ", paste(new_dims, collapse = "x"), " (",
              format(reduction, big.mark = ","), "x smaller)"
            )
            r <- if (isTRUE(cache_aggregated)) terra::rast(cache_file) else r_agg
          } else {
            message("    Aggregation failed, using original raster")
          }
        }
      }
    }

    rasters[[raster_name]] <- r
  }

  message("[load_tif_hazards] Successfully loaded ", length(rasters), " hazard rasters")

  return(rasters)
}
