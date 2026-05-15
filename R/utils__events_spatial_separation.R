#' Set per-event spatial separation filters
#'
#' @description
#' Programmatic helper to define spatial separation for selected events.
#' If `spatial_filters` is `NULL`, the input is returned unchanged so
#' `compute_risk()` can apply its default behavior (no spatial separation
#' when spatial columns are absent).
#'
#' @param events Data frame of events.
#' @param spatial_filters Named list keyed by `event_id`.
#'   Each entry can use:
#'   - shorthand: `list(state = c("11", "33"))`, `list(micro = c("R1"))`
#'   - explicit: `list(level = "state", codes = c("11", "33"), labels = c("Rondonia", "Rio de Janeiro"), scheme = "adm_regions")`
#' @param hazard_configs Optional named hazard config list (used to derive default scheme).
#' @param strict Logical. If `TRUE`, unknown `event_id` in filters throws an error.
#'
#' @return Events data frame with `spatial_level`, `spatial_region_codes`,
#'   `spatial_region_labels`, and `spatial_scheme`.
#' @export
set_events_spatial_separation <- function(events,
                                          spatial_filters = NULL,
                                          hazard_configs = NULL,
                                          strict = TRUE) {
  if (!is.data.frame(events)) {
    stop("events must be a data.frame")
  }

  if (is.null(spatial_filters)) {
    return(events)
  }

  out <- tibble::as_tibble(events)
  if (!"event_id" %in% names(out)) {
    out$event_id <- paste0("event_", seq_len(nrow(out)))
  }
  hazard_type_vec <- if ("hazard_type" %in% names(out)) as.character(out$hazard_type) else rep(NA_character_, nrow(out))

  default_scheme_for <- function(hazard_type) {
    if (!is.null(hazard_configs) && !is.null(hazard_type) && nzchar(as.character(hazard_type))) {
      return(get_hazard_spatial_scheme(hazard_configs, as.character(hazard_type)))
    }
    if (tolower(as.character(hazard_type)) == "flood") {
      return("hydro_regions")
    }
    "adm_regions"
  }

  if (!"spatial_scheme" %in% names(out)) {
    out$spatial_scheme <- unname(vapply(hazard_type_vec, default_scheme_for, character(1)))
  } else {
    out$spatial_scheme <- dplyr::coalesce(
      tolower(as.character(out$spatial_scheme)),
      unname(vapply(hazard_type_vec, default_scheme_for, character(1)))
    )
  }

  if (!"spatial_level" %in% names(out)) out$spatial_level <- "brazil"
  out$spatial_level <- dplyr::coalesce(tolower(as.character(out$spatial_level)), "brazil")

  if (!"spatial_region_codes" %in% names(out)) out$spatial_region_codes <- NA_character_
  if (!"spatial_region_labels" %in% names(out)) out$spatial_region_labels <- NA_character_

  parse_filter <- function(filter, fallback_scheme) {
    if (!is.list(filter)) {
      stop("each spatial filter must be a list")
    }

    level <- NULL
    codes <- character(0)
    labels <- character(0)
    scheme <- NULL

    explicit_level <- filter$level
    if (!is.null(explicit_level) && nzchar(as.character(explicit_level))) {
      level <- tolower(as.character(explicit_level[[1]]))
      code_vals <- NULL
      if (!is.null(filter$codes)) {
        code_vals <- filter$codes
      } else if (!is.null(filter$region_codes)) {
        code_vals <- filter$region_codes
      } else if (!is.null(filter$regions)) {
        code_vals <- filter$regions
      }
      if (!is.null(code_vals)) {
        codes <- as.character(unlist(code_vals))
      }
    } else {
      known_levels <- c("brazil", "state", "municipality", "macro", "meso", "micro")
      level_keys <- intersect(names(filter), known_levels)
      if (length(level_keys) == 0) {
        stop("filter must contain 'level' or one of: ", paste(known_levels, collapse = ", "))
      }
      if (length(level_keys) > 1) {
        stop("filter must define exactly one level; got: ", paste(level_keys, collapse = ", "))
      }
      level <- level_keys[[1]]
      vals <- filter[[level]]
      if (!is.null(vals)) {
        codes <- as.character(unlist(vals))
      }
    }

    if (!is.null(filter$labels)) {
      labels <- as.character(unlist(filter$labels))
    } else if (!is.null(filter$region_labels)) {
      labels <- as.character(unlist(filter$region_labels))
    }

    if (!is.null(filter$scheme) && nzchar(as.character(filter$scheme))) {
      scheme <- tolower(as.character(filter$scheme[[1]]))
    }

    codes <- unique(trimws(codes))
    codes <- codes[nzchar(codes)]
    labels <- unique(trimws(labels))
    labels <- labels[nzchar(labels)]

    if (level == "brazil") {
      codes <- character(0)
      labels <- character(0)
    }

    if (is.null(scheme) || !scheme %in% c("adm_regions", "hydro_regions")) {
      if (level %in% c("macro", "meso", "micro")) {
        scheme <- "hydro_regions"
      } else if (level %in% c("state", "municipality")) {
        scheme <- "adm_regions"
      } else {
        scheme <- fallback_scheme
      }
    }

    list(level = level, codes = codes, labels = labels, scheme = scheme)
  }

  if (!is.list(spatial_filters) || is.null(names(spatial_filters))) {
    stop("spatial_filters must be a named list keyed by event_id")
  }

  for (event_id in names(spatial_filters)) {
    idx <- which(as.character(out$event_id) == as.character(event_id))
    if (length(idx) == 0) {
      if (isTRUE(strict)) {
        stop("spatial_filters contains unknown event_id: ", event_id)
      }
      next
    }

    parsed <- parse_filter(
      filter = spatial_filters[[event_id]],
      fallback_scheme = default_scheme_for(hazard_type_vec[idx[[1]]])
    )

    out$spatial_level[idx] <- parsed$level
    out$spatial_scheme[idx] <- unname(parsed$scheme)
    out$spatial_region_codes[idx] <- if (length(parsed$codes) > 0) {
      paste(parsed$codes, collapse = "|")
    } else {
      NA_character_
    }
    out$spatial_region_labels[idx] <- if (length(parsed$labels) > 0) {
      paste(parsed$labels, collapse = "|")
    } else {
      NA_character_
    }
  }

  out
}
