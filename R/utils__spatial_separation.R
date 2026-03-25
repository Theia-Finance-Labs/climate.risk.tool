#' Spatial separation utilities (internal)
#' @noRd
normalize_spatial_separation <- function(spatial_separation) {
  default_cfg <- list(
    enabled = FALSE,
    level = "brazil",
    selected_codes = character(0),
    hazard_types = c("Heat", "Drought", "Fire")
  )

  if (is.null(spatial_separation)) {
    return(default_cfg)
  }

  if (!is.list(spatial_separation)) {
    stop("spatial_separation must be NULL or a list")
  }

  cfg <- utils::modifyList(default_cfg, spatial_separation)

  cfg$enabled <- isTRUE(cfg$enabled)
  cfg$level <- tolower(as.character(cfg$level[[1]]))
  cfg$selected_codes <- unique(as.character(stats::na.omit(cfg$selected_codes)))
  cfg$hazard_types <- unique(as.character(stats::na.omit(cfg$hazard_types)))

  valid_levels <- c("brazil", "state", "municipality")
  if (!cfg$level %in% valid_levels) {
    stop("spatial_separation$level must be one of: brazil, state, municipality")
  }

  if (!cfg$enabled || cfg$level == "brazil") {
    cfg$enabled <- FALSE
    cfg$level <- "brazil"
    cfg$selected_codes <- character(0)
  }

  if (cfg$enabled && length(cfg$selected_codes) == 0) {
    stop("spatial_separation$selected_codes cannot be empty when enabled = TRUE")
  }

  cfg
}

#' Resolve state/municipality codes for assets (prefer explicit code columns, fallback to names)
#' @noRd
resolve_asset_region_codes <- function(assets_df, adm_codes) {
  result <- assets_df

  if (!"state_code" %in% names(result)) result$state_code <- NA_character_
  if (!"municipality_code" %in% names(result)) result$municipality_code <- NA_character_

  result$state_code_resolved <- coerce_geo_code(result$state_code, width = 2)
  result$municipality_code_resolved <- coerce_geo_code(result$municipality_code, width = 7)

  if (is.null(adm_codes) || !is.data.frame(adm_codes) || nrow(adm_codes) == 0) {
    return(result)
  }

  adm1_lookup <- adm_codes |>
    dplyr::filter(.data$adm == "adm1") |>
    dplyr::transmute(
      state_name_norm = normalize_geo_name(.data$name),
      state_code_lookup = coerce_geo_code(.data$code, width = 2)
    ) |>
    dplyr::distinct()

  adm2_lookup <- adm_codes |>
    dplyr::filter(.data$adm == "adm2") |>
    dplyr::transmute(
      municipality_name_norm = normalize_geo_name(.data$name),
      municipality_code_lookup = coerce_geo_code(.data$code, width = 7)
    ) |>
    dplyr::distinct()

  n <- nrow(result)
  state_name_source <- if ("state_name" %in% names(result)) {
    result$state_name
  } else if ("state" %in% names(result)) {
    result$state
  } else {
    rep(NA_character_, n)
  }

  municipality_name_source <- if ("municipality_name" %in% names(result)) {
    result$municipality_name
  } else if ("municipality" %in% names(result)) {
    result$municipality
  } else {
    rep(NA_character_, n)
  }

  result <- result |>
    dplyr::mutate(
      state_name_norm = normalize_geo_name(state_name_source),
      municipality_name_norm = normalize_geo_name(municipality_name_source)
    ) |>
    dplyr::left_join(adm1_lookup, by = "state_name_norm") |>
    dplyr::left_join(adm2_lookup, by = "municipality_name_norm") |>
    dplyr::mutate(
      state_code_resolved = dplyr::coalesce(.data$state_code_resolved, .data$state_code_lookup),
      municipality_code_resolved = dplyr::coalesce(.data$municipality_code_resolved, .data$municipality_code_lookup)
    ) |>
    dplyr::select(-dplyr::any_of(c("state_name_norm", "municipality_name_norm", "state_code_lookup", "municipality_code_lookup")))

  result
}

#' Build selected region polygons by ADM code
#' @noRd
build_selected_region_sf <- function(level, selected_codes, adm1_boundaries, adm2_boundaries, adm_codes) {
  if (is.null(adm_codes) || !is.data.frame(adm_codes) || nrow(adm_codes) == 0) {
    stop("adm_codes is required for spatial separation when using state/municipality filters")
  }

  if (identical(level, "state")) {
    if (is.null(adm1_boundaries)) {
      stop("adm1_boundaries is required for state-level spatial separation")
    }

    lookup <- adm_codes |>
      dplyr::filter(.data$adm == "adm1") |>
      dplyr::transmute(shapeID = as.character(.data$shapeID), region_code = coerce_geo_code(.data$code, width = 2)) |>
      dplyr::distinct()

    selected_sf <- adm1_boundaries |>
      dplyr::mutate(shapeID = as.character(.data$shapeID)) |>
      dplyr::left_join(lookup, by = "shapeID") |>
      dplyr::filter(!is.na(.data$region_code), .data$region_code %in% selected_codes)

    return(selected_sf)
  }

  if (is.null(adm2_boundaries)) {
    stop("adm2_boundaries is required for municipality-level spatial separation")
  }

  lookup <- adm_codes |>
    dplyr::filter(.data$adm == "adm2") |>
    dplyr::transmute(shapeID = as.character(.data$shapeID), region_code = coerce_geo_code(.data$code, width = 7)) |>
    dplyr::distinct()

  selected_sf <- adm2_boundaries |>
    dplyr::mutate(shapeID = as.character(.data$shapeID)) |>
    dplyr::left_join(lookup, by = "shapeID") |>
    dplyr::filter(!is.na(.data$region_code), .data$region_code %in% selected_codes)

  selected_sf
}

#' Apply spatial separation to asset-event rows
#' @noRd
apply_spatial_separation <- function(assets_with_events,
                                     spatial_separation = NULL,
                                     adm1_boundaries = NULL,
                                     adm2_boundaries = NULL,
                                     adm_codes = NULL) {
  cfg <- normalize_spatial_separation(spatial_separation)

  if (!cfg$enabled || nrow(assets_with_events) == 0) {
    return(list(
      exposed = assets_with_events,
      status = tibble::tibble()
    ))
  }

  targeted <- assets_with_events$hazard_type %in% cfg$hazard_types
  if (!any(targeted, na.rm = TRUE)) {
    return(list(
      exposed = assets_with_events,
      status = tibble::tibble()
    ))
  }

  selected_sf <- build_selected_region_sf(
    level = cfg$level,
    selected_codes = cfg$selected_codes,
    adm1_boundaries = adm1_boundaries,
    adm2_boundaries = adm2_boundaries,
    adm_codes = adm_codes
  )

  if (nrow(selected_sf) == 0) {
    stop("Selected region codes are not present in ADM boundaries")
  }

  df <- resolve_asset_region_codes(assets_with_events, adm_codes)
  df$spatial_status <- "exposed"

  targeted_idx <- which(targeted)
  targeted_df <- df[targeted_idx, , drop = FALSE]

  has_coords <- !is.na(targeted_df$latitude) & !is.na(targeted_df$longitude)

  # Coordinate-based matching against selected polygons
  if (any(has_coords)) {
    pts <- sf::st_as_sf(
      targeted_df[has_coords, , drop = FALSE],
      coords = c("longitude", "latitude"),
      crs = 4326,
      remove = FALSE
    )

    if (sf::st_crs(selected_sf) != sf::st_crs(pts)) {
      selected_sf <- sf::st_transform(selected_sf, sf::st_crs(pts))
    }

    inside <- lengths(sf::st_intersects(pts, selected_sf)) > 0
    targeted_df$spatial_status[has_coords] <- ifelse(
      inside,
      "exposed",
      "Not exposed to selected hazard event"
    )
  }

  # Administrative matching for assets without coordinates
  if (any(!has_coords)) {
    no_coord_idx <- which(!has_coords)

    if (identical(cfg$level, "state")) {
      matched_state <- targeted_df$state_code_resolved[no_coord_idx] %in% cfg$selected_codes
      targeted_df$spatial_status[no_coord_idx] <- ifelse(
        matched_state,
        "exposed",
        "Not exposed to selected hazard event"
      )
    }

    if (identical(cfg$level, "municipality")) {
      muni_code <- targeted_df$municipality_code_resolved[no_coord_idx]
      state_code <- targeted_df$state_code_resolved[no_coord_idx]

      has_muni <- !is.na(muni_code) & nzchar(muni_code)
      matched_muni <- has_muni & muni_code %in% cfg$selected_codes

      # Explicit requirement: state-only location with municipality selection
      has_state_only <- !has_muni & !is.na(state_code) & nzchar(state_code)

      targeted_df$spatial_status[no_coord_idx] <- dplyr::case_when(
        matched_muni ~ "exposed",
        has_state_only ~ "Insufficient location data available. Less granular spatial separation necessary",
        TRUE ~ "Not exposed to selected hazard event"
      )
    }
  }

  df$spatial_status[targeted_idx] <- targeted_df$spatial_status

  status_df <- df |>
    dplyr::filter(.data$spatial_status != "exposed") |>
    dplyr::select(-dplyr::any_of(c("state_code_resolved", "municipality_code_resolved", "geometry", "centroid")))

  exposed_df <- df |>
    dplyr::filter(.data$spatial_status == "exposed") |>
    dplyr::select(-dplyr::any_of(c("spatial_status", "state_code_resolved", "municipality_code_resolved")))

  list(
    exposed = exposed_df,
    status = status_df
  )
}
