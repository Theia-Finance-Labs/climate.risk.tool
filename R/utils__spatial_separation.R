# Spatial separation helpers (internal)

spatial_status_not_exposed <- function() {
  "not exposed to selected hazard event"
}

spatial_status_insufficient <- function() {
  "insufficient location data available. Less granular spatial separation necessary"
}

parse_spatial_values <- function(x) {
  if (is.null(x) || length(x) == 0) {
    return(character(0))
  }
  x_chr <- as.character(x[[1]])
  if (is.na(x_chr) || !nzchar(trimws(x_chr))) {
    return(character(0))
  }
  vals <- unlist(strsplit(x_chr, "[|;,]"))
  vals <- trimws(vals)
  vals[nzchar(vals)]
}

normalize_spatial_text <- function(x) {
  x_chr <- as.character(x)
  x_chr <- trimws(x_chr)
  x_chr[x_chr == "" | x_chr == "NA"] <- NA_character_
  if (!any(!is.na(x_chr))) {
    return(x_chr)
  }
  tolower(stringi::stri_trans_general(x_chr, "Latin-ASCII"))
}

is_non_empty_chr <- function(x) {
  !is.na(x) & nzchar(trimws(as.character(x)))
}

first_non_missing <- function(x) {
  if (is.null(x)) return(NA)
  if (is.character(x)) {
    idx <- which(!is.na(x) & nzchar(trimws(x)))
  } else {
    idx <- which(!is.na(x))
  }
  if (length(idx) == 0) {
    return(x[NA_integer_][1])
  }
  x[[idx[1]]]
}

find_first_existing <- function(paths) {
  if (length(paths) == 0) return(NULL)
  for (path in paths) {
    if (!is.null(path) && file.exists(path)) {
      return(path)
    }
  }
  NULL
}

repair_spatial_layer_geometries <- function(sf_obj, layer_name) {
  if (is.null(sf_obj) || nrow(sf_obj) == 0) {
    return(list(data = sf_obj, warnings = character()))
  }

  validity <- tryCatch(
    suppressWarnings(sf::st_is_valid(sf_obj)),
    error = function(e) rep(NA, nrow(sf_obj))
  )
  invalid_idx <- which(is.na(validity) | !validity)

  if (length(invalid_idx) == 0) {
    return(list(data = sf_obj, warnings = character()))
  }

  repaired <- sf_obj
  old_s2 <- suppressMessages(sf::sf_use_s2(FALSE))
  on.exit(suppressMessages(sf::sf_use_s2(old_s2)), add = TRUE)
  repaired[invalid_idx, ] <- sf::st_make_valid(repaired[invalid_idx, , drop = FALSE])

  repaired_validity <- tryCatch(
    suppressWarnings(sf::st_is_valid(repaired)),
    error = function(e) rep(NA, nrow(repaired))
  )
  remaining_invalid <- sum(is.na(repaired_validity) | !repaired_validity)
  repaired_count <- max(0L, length(invalid_idx) - remaining_invalid)

  warnings_out <- character()

  repair_note <- if (repaired_count > 0L) {
    paste0(
      "[spatial_separation] Repaired ",
      repaired_count,
      " invalid geomet", if (repaired_count == 1L) "ry" else "ries",
      " in ", layer_name, "."
    )
  } else {
    paste0(
      "[spatial_separation] Attempted to repair ",
      length(invalid_idx),
      " invalid geomet", if (length(invalid_idx) == 1L) "ry" else "ries",
      " in ", layer_name, "."
    )
  }
  warning(repair_note, call. = FALSE)
  warnings_out <- c(warnings_out, repair_note)

  if (remaining_invalid > 0) {
    remaining_note <- paste0(
      "[spatial_separation] ",
      remaining_invalid,
      " geomet", if (remaining_invalid == 1L) "ry remains" else "ries remain",
      " invalid in ", layer_name,
      " after repair; spatial joins may still fail."
    )
    warning(remaining_note, call. = FALSE)
    warnings_out <- c(warnings_out, remaining_note)
  }

  list(data = repaired, warnings = warnings_out)
}

extract_shape_id_col <- function(sf_obj) {
  candidates <- c("shapeID", "shape_id", "shapeid", "ShapeID")
  present <- candidates[candidates %in% names(sf_obj)]
  if (length(present) == 0) {
    return(NULL)
  }
  present[[1]]
}

prepare_adm_layer <- function(sf_obj, adm_codes, adm_level) {
  if (is.null(sf_obj) || nrow(sf_obj) == 0) {
    return(sf_obj)
  }

  shape_id_col <- extract_shape_id_col(sf_obj)
  label_col <- if ("shapeName" %in% names(sf_obj)) "shapeName" else names(sf_obj)[1]

  out <- sf_obj |>
    dplyr::mutate(
      region_shape_id = if (!is.null(shape_id_col)) as.character(.data[[shape_id_col]]) else NA_character_,
      region_label_raw = as.character(.data[[label_col]])
    )

  if (!is.null(adm_codes) && nrow(adm_codes) > 0) {
    lookup <- adm_codes |>
      dplyr::filter(tolower(.data$adm_level) == tolower(adm_level)) |>
      dplyr::transmute(
        lookup_shape_id = as.character(.data$shapeID),
        region_code = as.character(.data$code),
        region_label = as.character(.data$name)
      ) |>
      dplyr::distinct()

    out <- out |>
      dplyr::left_join(lookup, by = c("region_shape_id" = "lookup_shape_id"))
  } else {
    out$region_code <- NA_character_
    out$region_label <- NA_character_
  }

  out <- out |>
    dplyr::mutate(
      region_label = dplyr::coalesce(.data$region_label, .data$region_label_raw),
      region_code = dplyr::na_if(as.character(.data$region_code), ""),
      normalized_label = normalize_spatial_text(.data$region_label)
    )

  if (tolower(adm_level) == "adm2") {
    out <- out |>
      dplyr::mutate(state_code = ifelse(is_non_empty_chr(.data$region_code), substr(.data$region_code, 1, 2), NA_character_))
  }

  out
}

prepare_hydro_layer <- function(sf_obj, level) {
  if (is.null(sf_obj) || nrow(sf_obj) == 0) {
    return(sf_obj)
  }

  code_candidates <- switch(level,
    macro = c("cd_macroRH", "cd_macrorh", "macro_code", "region_code", "code"),
    meso = c("cd_mesoRH", "cd_mesorh", "meso_code", "region_code", "code"),
    micro = c("cd_microRH", "cd_microrh", "micro_code", "region_code", "code"),
    c("region_code", "code")
  )
  label_candidates <- switch(level,
    macro = c("nm_macroRH", "nm_macrorh", "macro_name", "region_label", "name"),
    meso = c("nm_mesoRH", "nm_mesorh", "meso_name", "region_label", "name"),
    micro = c("nm_microRH", "nm_microrh", "micro_name", "region_label", "name"),
    c("region_label", "name")
  )

  code_col <- code_candidates[code_candidates %in% names(sf_obj)]
  label_col <- label_candidates[label_candidates %in% names(sf_obj)]
  code_col <- if (length(code_col) > 0) code_col[[1]] else NULL
  label_col <- if (length(label_col) > 0) label_col[[1]] else names(sf_obj)[1]

  out <- sf_obj |>
    dplyr::mutate(
      region_code = if (!is.null(code_col)) as.character(.data[[code_col]]) else NA_character_,
      region_label = as.character(.data[[label_col]]),
      normalized_label = normalize_spatial_text(.data$region_label)
    )

  # Keep hierarchical parent codes when available.
  if ("cd_macroRH" %in% names(out)) {
    out$macro_code <- as.character(out$cd_macroRH)
  }
  if ("cd_mesoRH" %in% names(out)) {
    out$meso_code <- as.character(out$cd_mesoRH)
  }

  out
}

read_overlap_table <- function(overlaps_dir, source_level, target_level) {
  empty <- tibble::tibble(
    source_code = character(),
    target_code = character(),
    fraction = numeric()
  )

  if (is.null(overlaps_dir) || !dir.exists(overlaps_dir)) {
    return(empty)
  }

  csv_files <- list.files(overlaps_dir, pattern = "\\.csv$", full.names = TRUE)
  if (length(csv_files) == 0) {
    return(empty)
  }

  base_names <- tolower(basename(csv_files))
  idx <- grepl(source_level, base_names) & grepl(target_level, base_names)
  if (!any(idx)) {
    return(empty)
  }

  overlap_path <- csv_files[which(idx)[1]]
  overlap_df <- try(readr::read_csv(overlap_path, show_col_types = FALSE), silent = TRUE)
  if (inherits(overlap_df, "try-error") || is.null(overlap_df) || nrow(overlap_df) == 0) {
    return(empty)
  }

  overlap_df <- tibble::as_tibble(overlap_df)

  source_candidates <- switch(source_level,
    municipality = c("municipality_code", "adm2_code", "source_code", "adm_code", "code"),
    state = c("state_code", "adm1_code", "source_code", "adm_code", "code"),
    c("source_code", "adm_code", "code")
  )
  target_candidates <- switch(target_level,
    macro = c("macro_code", "cd_macroRH", "target_code", "region_code", "code"),
    meso = c("meso_code", "cd_mesoRH", "target_code", "region_code", "code"),
    micro = c("micro_code", "cd_microRH", "target_code", "region_code", "code"),
    c("target_code", "region_code", "code")
  )
  fraction_candidates <- c("fraction", "area_fraction", "overlap_fraction", "share", "pct_area", "percentage")

  source_col <- source_candidates[source_candidates %in% names(overlap_df)]
  target_col <- target_candidates[target_candidates %in% names(overlap_df)]
  fraction_col <- fraction_candidates[fraction_candidates %in% names(overlap_df)]

  if (length(source_col) == 0 || length(target_col) == 0 || length(fraction_col) == 0) {
    return(empty)
  }

  out <- overlap_df |>
    dplyr::transmute(
      source_code = as.character(.data[[source_col[[1]]]]),
      target_code = as.character(.data[[target_col[[1]]]]),
      fraction = as.numeric(.data[[fraction_col[[1]]]])
    ) |>
    dplyr::filter(is_non_empty_chr(.data$source_code), is_non_empty_chr(.data$target_code), !is.na(.data$fraction))

  if (nrow(out) == 0) {
    return(empty)
  }

  # Allow percentage-style tables as input (0-100).
  if (max(out$fraction, na.rm = TRUE) > 1 && max(out$fraction, na.rm = TRUE) <= 100) {
    out$fraction <- out$fraction / 100
  }

  out |>
    dplyr::mutate(
      fraction = pmax(0, pmin(1, .data$fraction))
    )
}

get_spatial_level_choices <- function(scheme) {
  if (identical(scheme, "hydro_regions")) {
    return(c(
      "Brazil (whole)" = "brazil",
      "Macro hydrological regions" = "macro",
      "Meso hydrological regions" = "meso",
      "Micro hydrological regions" = "micro"
    ))
  }

  c(
    "Brazil (whole)" = "brazil",
    "States" = "state",
    "Municipalities" = "municipality"
  )
}

get_spatial_region_choices <- function(spatial_data, scheme, level) {
  if (is.null(spatial_data) || is.null(level) || level == "brazil") {
    return(tibble::tibble(region_code = character(), region_label = character()))
  }

  layer <- NULL
  if (scheme == "hydro_regions") {
    layer <- spatial_data$hydro[[level]]
  } else {
    layer <- spatial_data$adm[[level]]
  }

  if (is.null(layer) || nrow(layer) == 0) {
    return(tibble::tibble(region_code = character(), region_label = character()))
  }

  layer |>
    sf::st_drop_geometry() |>
    dplyr::transmute(
      region_code = as.character(.data$region_code),
      region_label = as.character(.data$region_label)
    ) |>
    dplyr::filter(is_non_empty_chr(.data$region_code), is_non_empty_chr(.data$region_label)) |>
    dplyr::distinct() |>
    dplyr::arrange(.data$region_label)
}

load_spatial_separation_data <- function(base_dir, adm1_boundaries = NULL, adm2_boundaries = NULL, adm_codes = NULL) {
  if (is.null(base_dir) || !dir.exists(base_dir)) {
    return(NULL)
  }

  spatial_warnings <- character()

  if (is.null(adm_codes)) {
    adm_codes <- try(load_adm_codes(base_dir), silent = TRUE)
    if (inherits(adm_codes, "try-error")) {
      adm_codes <- NULL
    }
  }

  spatial_root <- file.path(base_dir, "spatial_separation")

  # ADM layers
  adm_state_sf <- NULL
  adm_muni_sf <- NULL

  state_path <- find_first_existing(c(
    file.path(spatial_root, "adm_regions", "state", "geoBoundaries-BRA-ADM1.shp"),
    file.path(spatial_root, "adm_regions", "state", "geoBoundaries-BRA-ADM1_simplified.geojson"),
    file.path(base_dir, "areas", "state", "geoBoundaries-BRA-ADM1_simplified.geojson")
  ))
  muni_path <- find_first_existing(c(
    file.path(spatial_root, "adm_regions", "municipality", "geoBoundaries-BRA-ADM2.shp"),
    file.path(spatial_root, "adm_regions", "municipality", "geoBoundaries-BRA-ADM2_simplified.geojson"),
    file.path(base_dir, "areas", "municipality", "geoBoundaries-BRA-ADM2_simplified.geojson")
  ))

  if (!is.null(adm1_boundaries) && inherits(adm1_boundaries, "sf")) {
    adm_state_sf <- adm1_boundaries
  } else if (!is.null(state_path)) {
    adm_state_sf <- sf::st_read(state_path, quiet = TRUE)
  }

  if (!is.null(adm2_boundaries) && inherits(adm2_boundaries, "sf")) {
    adm_muni_sf <- adm2_boundaries
  } else if (!is.null(muni_path)) {
    adm_muni_sf <- sf::st_read(muni_path, quiet = TRUE)
  }

  if (!is.null(adm_state_sf) && !is.na(sf::st_crs(adm_state_sf)) && sf::st_crs(adm_state_sf)$epsg != 4326) {
    adm_state_sf <- sf::st_transform(adm_state_sf, 4326)
  }
  if (!is.null(adm_muni_sf) && !is.na(sf::st_crs(adm_muni_sf)) && sf::st_crs(adm_muni_sf)$epsg != 4326) {
    adm_muni_sf <- sf::st_transform(adm_muni_sf, 4326)
  }

  adm_state_repair <- repair_spatial_layer_geometries(adm_state_sf, "ADM1 spatial separation layer")
  adm_state_sf <- adm_state_repair$data
  spatial_warnings <- c(spatial_warnings, adm_state_repair$warnings)

  adm_muni_repair <- repair_spatial_layer_geometries(adm_muni_sf, "ADM2 spatial separation layer")
  adm_muni_sf <- adm_muni_repair$data
  spatial_warnings <- c(spatial_warnings, adm_muni_repair$warnings)

  adm_state <- if (!is.null(adm_state_sf)) prepare_adm_layer(adm_state_sf, adm_codes, "adm1") else NULL
  adm_muni <- if (!is.null(adm_muni_sf)) prepare_adm_layer(adm_muni_sf, adm_codes, "adm2") else NULL

  # Hydro layers
  hydro_root <- file.path(spatial_root, "hydro_regions")
  read_hydro <- function(level, folder) {
    shp_path <- find_first_existing(c(
      file.path(hydro_root, folder, paste0(folder, ".shp")),
      file.path(hydro_root, folder, paste0(folder, ".geojson"))
    ))
    if (is.null(shp_path)) return(NULL)
    sf_obj <- sf::st_read(shp_path, quiet = TRUE)
    if (!is.na(sf::st_crs(sf_obj)) && sf::st_crs(sf_obj)$epsg != 4326) {
      sf_obj <- sf::st_transform(sf_obj, 4326)
    }
    repair_result <- repair_spatial_layer_geometries(
      sf_obj,
      paste0(toupper(substr(level, 1, 1)), substr(level, 2, nchar(level)), " hydro spatial separation layer")
    )
    spatial_warnings <<- c(spatial_warnings, repair_result$warnings)
    prepare_hydro_layer(repair_result$data, level)
  }

  hydro_macro <- read_hydro("macro", "macro_RH")
  hydro_meso <- read_hydro("meso", "meso_RH")
  hydro_micro <- read_hydro("micro", "micro_RH")

  overlaps_dir <- file.path(hydro_root, "overlaps")
  overlaps <- list(
    municipality_macro = read_overlap_table(overlaps_dir, "municipality", "macro"),
    municipality_meso = read_overlap_table(overlaps_dir, "municipality", "meso"),
    municipality_micro = read_overlap_table(overlaps_dir, "municipality", "micro"),
    state_macro = read_overlap_table(overlaps_dir, "state", "macro"),
    state_meso = read_overlap_table(overlaps_dir, "state", "meso")
  )

  state_name_to_code <- c()
  municipality_name_to_code <- c()
  if (!is.null(adm_state) && nrow(adm_state) > 0) {
    state_df <- adm_state |>
      sf::st_drop_geometry() |>
      dplyr::filter(is_non_empty_chr(.data$region_code), is_non_empty_chr(.data$normalized_label)) |>
      dplyr::distinct(.data$normalized_label, .keep_all = TRUE)
    state_name_to_code <- state_df$region_code
    names(state_name_to_code) <- state_df$normalized_label
  }
  if (!is.null(adm_muni) && nrow(adm_muni) > 0) {
    muni_df <- adm_muni |>
      sf::st_drop_geometry() |>
      dplyr::filter(is_non_empty_chr(.data$region_code), is_non_empty_chr(.data$normalized_label)) |>
      dplyr::distinct(.data$normalized_label, .keep_all = TRUE)
    municipality_name_to_code <- muni_df$region_code
    names(municipality_name_to_code) <- muni_df$normalized_label
  }

  list(
    adm = list(
      state = adm_state,
      municipality = adm_muni
    ),
    hydro = list(
      macro = hydro_macro,
      meso = hydro_meso,
      micro = hydro_micro
    ),
    overlaps = overlaps,
    lookup = list(
      state_name_to_code = state_name_to_code,
      municipality_name_to_code = municipality_name_to_code
    ),
    warnings = unique(spatial_warnings)
  )
}

resolve_selected_region_codes <- function(spatial_data, scheme, level, selected_codes, selected_labels) {
  codes <- unique(as.character(selected_codes))
  codes <- codes[is_non_empty_chr(codes)]

  labels_norm <- normalize_spatial_text(selected_labels)
  labels_norm <- labels_norm[is_non_empty_chr(labels_norm)]

  layer_choices <- get_spatial_region_choices(spatial_data, scheme, level)
  if (nrow(layer_choices) > 0 && length(labels_norm) > 0) {
    choices_norm <- normalize_spatial_text(layer_choices$region_label)
    code_from_labels <- layer_choices$region_code[choices_norm %in% labels_norm]
    codes <- unique(c(codes, code_from_labels))
  }

  unique(codes)
}

get_hydro_overlap_table <- function(spatial_data, source_level, target_level) {
  key <- paste0(source_level, "_", target_level)
  tbl <- spatial_data$overlaps[[key]]
  if (is.null(tbl)) {
    return(tibble::tibble(source_code = character(), target_code = character(), fraction = numeric()))
  }
  tbl
}

evaluate_event_spatial_selection <- function(asset_rows, scheme, level, selected_codes, selected_labels, spatial_data) {
  if (is.null(asset_rows) || nrow(asset_rows) == 0) {
    return(tibble::tibble(
      asset = character(),
      spatial_included = logical(),
      spatial_multiplier = numeric(),
      spatial_exposure_status = character()
    ))
  }

  res <- asset_rows |>
    dplyr::transmute(
      asset = as.character(.data$asset),
      spatial_included = FALSE,
      spatial_multiplier = 0,
      spatial_exposure_status = spatial_status_not_exposed()
    )

  if (level == "brazil" || (length(selected_codes) == 0 && length(selected_labels) == 0)) {
    res$spatial_included <- TRUE
    res$spatial_multiplier <- 1
    res$spatial_exposure_status <- NA_character_
    return(res)
  }

  # Coordinates path (highest-fidelity location).
  has_coords <- !is.na(asset_rows$latitude) & !is.na(asset_rows$longitude)

  if (any(has_coords) && !is.null(spatial_data)) {
    layer <- if (scheme == "hydro_regions") spatial_data$hydro[[level]] else spatial_data$adm[[level]]
    if (!is.null(layer) && nrow(layer) > 0) {
      selected_layer <- layer |>
        dplyr::filter(.data$region_code %in% selected_codes)

      if (nrow(selected_layer) > 0) {
        pts <- asset_rows[has_coords, , drop = FALSE]
        pts_sf <- sf::st_as_sf(pts, coords = c("longitude", "latitude"), crs = 4326)
        hits <- sf::st_join(
          pts_sf,
          selected_layer |>
            dplyr::select("region_code"),
          join = sf::st_within,
          left = TRUE
        )
        matched_assets <- as.character(hits$asset[!is.na(hits$region_code)])
        if (length(matched_assets) > 0) {
          idx <- res$asset %in% matched_assets
          res$spatial_included[idx] <- TRUE
          res$spatial_multiplier[idx] <- 1
          res$spatial_exposure_status[idx] <- NA_character_
        }
      }
    }
  }

  # Non-coordinate path.
  no_coords_assets <- asset_rows$asset[!has_coords]
  if (length(no_coords_assets) == 0) {
    return(res)
  }

  no_coord_rows <- asset_rows |>
    dplyr::filter(.data$asset %in% no_coords_assets)

  municipality_name_vals <- if ("municipality_name" %in% names(no_coord_rows)) no_coord_rows$municipality_name else rep(NA_character_, nrow(no_coord_rows))
  municipality_code_vals <- if ("municipality_code" %in% names(no_coord_rows)) no_coord_rows$municipality_code else rep(NA_character_, nrow(no_coord_rows))
  state_name_vals <- if ("state_name" %in% names(no_coord_rows)) no_coord_rows$state_name else rep(NA_character_, nrow(no_coord_rows))
  state_code_vals <- if ("state_code" %in% names(no_coord_rows)) no_coord_rows$state_code else rep(NA_character_, nrow(no_coord_rows))
  state_vals <- if ("state" %in% names(no_coord_rows)) no_coord_rows$state else rep(NA_character_, nrow(no_coord_rows))
  municipality_vals <- if ("municipality" %in% names(no_coord_rows)) no_coord_rows$municipality else rep(NA_character_, nrow(no_coord_rows))

  if (scheme == "adm_regions") {
    labels_norm <- normalize_spatial_text(selected_labels)

    if (level == "state") {
      state_norm <- normalize_spatial_text(dplyr::coalesce(state_name_vals, state_vals))
      state_code <- as.character(state_code_vals)
      matched <- (is_non_empty_chr(state_code) & state_code %in% selected_codes) |
        (is_non_empty_chr(state_norm) & state_norm %in% labels_norm)

      if (any(matched)) {
        matched_assets <- as.character(no_coord_rows$asset[matched])
        idx <- res$asset %in% matched_assets
        res$spatial_included[idx] <- TRUE
        res$spatial_multiplier[idx] <- 1
        res$spatial_exposure_status[idx] <- NA_character_
      }
      return(res)
    }

    if (level == "municipality") {
      muni_norm <- normalize_spatial_text(dplyr::coalesce(municipality_name_vals, municipality_vals))
      muni_code <- as.character(municipality_code_vals)
      state_present <- is_non_empty_chr(state_code_vals) |
        is_non_empty_chr(state_vals) |
        is_non_empty_chr(state_name_vals)
      muni_present <- is_non_empty_chr(muni_code) | is_non_empty_chr(muni_norm)

      matched <- (is_non_empty_chr(muni_code) & muni_code %in% selected_codes) |
        (is_non_empty_chr(muni_norm) & muni_norm %in% labels_norm)

      if (any(matched)) {
        matched_assets <- as.character(no_coord_rows$asset[matched])
        idx <- res$asset %in% matched_assets
        res$spatial_included[idx] <- TRUE
        res$spatial_multiplier[idx] <- 1
        res$spatial_exposure_status[idx] <- NA_character_
      }

      insufficient <- !matched & !muni_present & state_present
      if (any(insufficient)) {
        bad_assets <- as.character(no_coord_rows$asset[insufficient])
        idx <- res$asset %in% bad_assets
        res$spatial_exposure_status[idx] <- spatial_status_insufficient()
      }
      return(res)
    }

    return(res)
  }

  # Hydro scheme
  if (scheme != "hydro_regions") {
    return(res)
  }

  muni_codes <- as.character(municipality_code_vals)
  state_codes <- as.character(state_code_vals)

  muni_norm <- normalize_spatial_text(dplyr::coalesce(municipality_name_vals, municipality_vals))
  state_norm <- normalize_spatial_text(dplyr::coalesce(state_name_vals, state_vals))

  if (!is.null(spatial_data$lookup$municipality_name_to_code) && length(spatial_data$lookup$municipality_name_to_code) > 0) {
    needs_muni <- !is_non_empty_chr(muni_codes) & is_non_empty_chr(muni_norm)
    muni_codes[needs_muni] <- spatial_data$lookup$municipality_name_to_code[muni_norm[needs_muni]]
  }
  if (!is.null(spatial_data$lookup$state_name_to_code) && length(spatial_data$lookup$state_name_to_code) > 0) {
    needs_state <- !is_non_empty_chr(state_codes) & is_non_empty_chr(state_norm)
    state_codes[needs_state] <- spatial_data$lookup$state_name_to_code[state_norm[needs_state]]
  }

  for (i in seq_len(nrow(no_coord_rows))) {
    asset_id <- as.character(no_coord_rows$asset[[i]])

    has_muni <- is_non_empty_chr(muni_codes[[i]])
    has_state <- is_non_empty_chr(state_codes[[i]]) || is_non_empty_chr(state_vals[[i]]) || is_non_empty_chr(state_name_vals[[i]])

    if (has_muni) {
      overlap_tbl <- get_hydro_overlap_table(spatial_data, "municipality", level)
      if (nrow(overlap_tbl) == 0) {
        res$spatial_exposure_status[res$asset == asset_id] <- spatial_status_insufficient()
        next
      }

      frac <- overlap_tbl |>
        dplyr::filter(.data$source_code == muni_codes[[i]], .data$target_code %in% selected_codes) |>
        dplyr::summarise(total = sum(.data$fraction, na.rm = TRUE)) |>
        dplyr::pull(.data$total)

      frac <- ifelse(length(frac) == 0 || is.na(frac), 0, frac)
      frac <- pmax(0, pmin(1, as.numeric(frac)))

      if (frac > 0) {
        idx <- res$asset == asset_id
        res$spatial_included[idx] <- TRUE
        res$spatial_multiplier[idx] <- frac
        res$spatial_exposure_status[idx] <- NA_character_
      }
      next
    }

    if (has_state) {
      if (level == "micro") {
        res$spatial_exposure_status[res$asset == asset_id] <- spatial_status_insufficient()
        next
      }

      overlap_tbl <- get_hydro_overlap_table(spatial_data, "state", level)
      if (nrow(overlap_tbl) == 0 || !is_non_empty_chr(state_codes[[i]])) {
        res$spatial_exposure_status[res$asset == asset_id] <- spatial_status_insufficient()
        next
      }

      frac <- overlap_tbl |>
        dplyr::filter(.data$source_code == state_codes[[i]], .data$target_code %in% selected_codes) |>
        dplyr::summarise(total = sum(.data$fraction, na.rm = TRUE)) |>
        dplyr::pull(.data$total)

      frac <- ifelse(length(frac) == 0 || is.na(frac), 0, frac)
      frac <- pmax(0, pmin(1, as.numeric(frac)))

      if (frac > 0) {
        idx <- res$asset == asset_id
        res$spatial_included[idx] <- TRUE
        res$spatial_multiplier[idx] <- frac
        res$spatial_exposure_status[idx] <- NA_character_
      }
      next
    }

    res$spatial_exposure_status[res$asset == asset_id] <- spatial_status_insufficient()
  }

  res
}

evaluate_spatial_separation <- function(
  assets_with_events,
  events,
  hazard_configs,
  spatial_separation_data = NULL,
  base_dir = NULL,
  adm1_boundaries = NULL,
  adm2_boundaries = NULL
) {
  if (is.null(assets_with_events) || nrow(assets_with_events) == 0) {
    return(tibble::tibble(
      asset = character(),
      event_id = character(),
      spatial_included = logical(),
      spatial_multiplier = numeric(),
      spatial_exposure_status = character()
    ))
  }

  if (is.null(events) || nrow(events) == 0 || !"event_id" %in% names(events)) {
    return(assets_with_events |>
      dplyr::distinct(.data$asset, .data$event_id) |>
      dplyr::mutate(
        spatial_included = TRUE,
        spatial_multiplier = 1,
        spatial_exposure_status = NA_character_
      ))
  }

  events_spatial <- events |>
    dplyr::mutate(
      spatial_scheme = if ("spatial_scheme" %in% names(events)) as.character(.data$spatial_scheme) else NA_character_,
      spatial_level = if ("spatial_level" %in% names(events)) as.character(.data$spatial_level) else NA_character_,
      spatial_region_codes = if ("spatial_region_codes" %in% names(events)) as.character(.data$spatial_region_codes) else NA_character_,
      spatial_region_labels = if ("spatial_region_labels" %in% names(events)) as.character(.data$spatial_region_labels) else NA_character_
    )

  if (nrow(events_spatial) > 0) {
    events_spatial$spatial_scheme <- vapply(
      seq_len(nrow(events_spatial)),
      function(i) {
        raw_scheme <- events_spatial$spatial_scheme[[i]]
        if (is_non_empty_chr(raw_scheme)) {
          scheme <- tolower(as.character(raw_scheme))
          if (scheme %in% c("adm_regions", "hydro_regions")) {
            return(scheme)
          }
        }
        get_hazard_spatial_scheme(hazard_configs, as.character(events_spatial$hazard_type[[i]]))
      },
      character(1)
    )

    events_spatial$spatial_level <- ifelse(
      is_non_empty_chr(events_spatial$spatial_level),
      tolower(as.character(events_spatial$spatial_level)),
      "brazil"
    )
  }

  non_brazil <- events_spatial$spatial_level != "brazil"
  if (any(non_brazil) && is.null(spatial_separation_data)) {
    spatial_separation_data <- load_spatial_separation_data(
      base_dir = base_dir,
      adm1_boundaries = adm1_boundaries,
      adm2_boundaries = adm2_boundaries
    )
  }

  event_ids <- unique(as.character(assets_with_events$event_id))
  eval_rows <- vector("list", length(event_ids))

  for (i in seq_along(event_ids)) {
    ev_id <- event_ids[[i]]
    event_row <- events_spatial |>
      dplyr::filter(.data$event_id == ev_id) |>
      dplyr::slice(1)

    event_assets <- assets_with_events |>
      dplyr::filter(.data$event_id == ev_id)

    if (nrow(event_assets) == 0) {
      next
    }

    asset_rows <- event_assets |>
      dplyr::select(
        "asset",
        dplyr::any_of(c(
          "company", "latitude", "longitude",
          "state", "state_code", "state_name",
          "municipality", "municipality_code", "municipality_name"
        ))
      ) |>
      dplyr::distinct(.data$asset, .keep_all = TRUE)

    if (nrow(event_row) == 0) {
      eval_rows[[i]] <- asset_rows |>
        dplyr::transmute(
          asset = as.character(.data$asset),
          event_id = ev_id,
          spatial_included = TRUE,
          spatial_multiplier = 1,
          spatial_exposure_status = NA_character_
        )
      next
    }

    scheme <- as.character(event_row$spatial_scheme[[1]])
    level <- as.character(event_row$spatial_level[[1]])
    selected_codes <- parse_spatial_values(event_row$spatial_region_codes)
    selected_labels <- parse_spatial_values(event_row$spatial_region_labels)

    selected_codes <- resolve_selected_region_codes(
      spatial_data = spatial_separation_data,
      scheme = scheme,
      level = level,
      selected_codes = selected_codes,
      selected_labels = selected_labels
    )

    event_eval <- evaluate_event_spatial_selection(
      asset_rows = asset_rows,
      scheme = scheme,
      level = level,
      selected_codes = selected_codes,
      selected_labels = selected_labels,
      spatial_data = spatial_separation_data
    ) |>
      dplyr::mutate(event_id = ev_id)

    eval_rows[[i]] <- event_eval
  }

  dplyr::bind_rows(eval_rows)
}

build_spatial_exclusion_rows <- function(excluded_assets_events) {
  if (is.null(excluded_assets_events) || nrow(excluded_assets_events) == 0) {
    return(tibble::tibble())
  }

  grouping_cols <- c("asset", "event_id", "hazard_type", "hazard_name")
  grouping_cols <- grouping_cols[grouping_cols %in% names(excluded_assets_events)]

  metadata_cols <- c(
    "company", "latitude", "longitude", "municipality", "state",
    "asset_category", "asset_subtype", "size_in_m2", "share_of_economic_activity", "cost_factor",
    "cnae", "state_code", "municipality_code", "state_name", "municipality_name",
    "scenario_name", "return_period", "event_year", "matching_method"
  )
  metadata_cols <- metadata_cols[metadata_cols %in% names(excluded_assets_events)]

  summarized <- excluded_assets_events |>
    dplyr::group_by(dplyr::across(dplyr::all_of(grouping_cols))) |>
    dplyr::summarise(
      dplyr::across(dplyr::any_of(metadata_cols), first_non_missing),
      spatial_multiplier = max(dplyr::coalesce(.data$spatial_multiplier, 0), na.rm = TRUE),
      spatial_exposure_status = first_non_missing(.data$spatial_exposure_status),
      .groups = "drop"
    )

  if ("return_period" %in% names(summarized)) {
    summarized$hazard_return_period <- summarized$return_period
  } else {
    summarized$hazard_return_period <- NA_real_
  }

  if ("matching_method" %in% names(summarized)) {
    summarized$matching_method <- dplyr::coalesce(as.character(summarized$matching_method), "spatial separation")
  } else {
    summarized$matching_method <- "spatial separation"
  }

  summarized
}
