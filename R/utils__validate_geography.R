#' Load ADM1 (state) normalized names from boundaries file
#'
#' @param base_dir Base directory containing areas subdirectory
#' @return Character vector of normalized state names
#' @noRd
load_adm1_state_names <- function(base_dir) {
  state_path <- file.path(base_dir, "areas", "state", "geoBoundaries-BRA-ADM1_simplified.geojson")

  if (!file.exists(state_path)) {
    warning("[load_adm1_state_names] State boundaries not found at: ", state_path)
    return(character(0))
  }

  states_sf <- sf::st_read(state_path, quiet = TRUE)

  # Normalize names same way as in assign_state_to_assets
  state_names <- states_sf |>
    dplyr::pull(.data$shapeName) |>
    as.character() |>
    stringi::stri_trans_general("Latin-ASCII")

  return(unique(state_names))
}


#' Load ADM2 (municipality) normalized names from boundaries file
#'
#' @param base_dir Base directory containing areas subdirectory
#' @return Character vector of normalized municipality names
#' @noRd
load_adm2_municipality_names <- function(base_dir) {
  municipality_path <- file.path(base_dir, "areas", "municipality", "geoBoundaries-BRA-ADM2_simplified.geojson")

  if (!file.exists(municipality_path)) {
    warning("[load_adm2_municipality_names] Municipality boundaries not found at: ", municipality_path)
    return(character(0))
  }

  municipalities_sf <- sf::st_read(municipality_path, quiet = TRUE)

  # Normalize names same way as in assign_province_to_assets
  municipality_names <- municipalities_sf |>
    dplyr::pull(.data$shapeName) |>
    as.character() |>
    stringi::stri_trans_general("Latin-ASCII")

  return(unique(municipality_names))
}


#' Validate damage factors province names against ADM1 boundaries
#'
#' @param damage_factors_df Damage factors data frame
#' @param adm1_names Character vector of valid ADM1 province names
#' @param validation_results List with errors and warnings vectors
#' @return Updated validation_results list
#' @noRd
validate_damage_factors_states <- function(damage_factors_df, adm1_names, validation_results) {
  if (length(adm1_names) == 0) {
    validation_results$warnings <- c(
      validation_results$warnings,
      "Cannot validate damage factors states: ADM1 boundaries not loaded"
    )
    return(validation_results)
  }

  # Get unique state names from damage factors (excluding "-" placeholder)
  df_states <- damage_factors_df |>
    dplyr::filter(!is.na(.data$state), .data$state != "-") |>
    dplyr::pull(.data$state) |>
    unique()

  # Normalize damage factor states for comparison
  df_states_normalized <- stringi::stri_trans_general(df_states, "Latin-ASCII")

  # Find mismatches
  invalid_states <- df_states_normalized[!df_states_normalized %in% adm1_names]

  if (length(invalid_states) > 0) {
    validation_results$errors <- c(
      validation_results$errors,
      paste0(
        "Damage factors contain state names not in ADM1 boundaries (after normalization): ",
        paste(invalid_states, collapse = ", ")
      )
    )
  }

  return(validation_results)
}


#' Validate assets geography (province and municipality) against ADM1/ADM2 boundaries
#'
#' @param assets_df Assets data frame
#' @param adm1_names Character vector of valid ADM1 province names
#' @param adm2_names Character vector of valid ADM2 municipality names
#' @param validation_results List with errors and warnings vectors
#' @return Updated validation_results list
#' @noRd
validate_assets_geography <- function(assets_df, adm1_names, adm2_names, validation_results) {
  # Flag rows with no geographic information at all
  if (all(c("latitude", "longitude", "municipality", "state") %in% names(assets_df))) {
    no_geo_idx <- which(
      (is.na(assets_df$latitude) | is.null(assets_df$latitude)) &
        (is.na(assets_df$longitude) | is.null(assets_df$longitude)) &
        (is.na(assets_df$municipality) | is.null(assets_df$municipality) | !nzchar(trimws(as.character(assets_df$municipality)))) &
        (is.na(assets_df$state) | is.null(assets_df$state) | !nzchar(trimws(as.character(assets_df$state))))
    )
    if (length(no_geo_idx) > 0) {
      validation_results$errors <- c(
        validation_results$errors,
        paste0("Assets have no geographic information (lat/lon/municipality/state) for rows: ", paste(no_geo_idx, collapse = ", "))
      )
    }

    # Check that if latitude OR longitude is filled, both must be filled
    lat_lon_mismatch_idx <- which(
      (!is.na(assets_df$latitude) & !is.null(assets_df$latitude) & (is.na(assets_df$longitude) | is.null(assets_df$longitude))) |
      (!is.na(assets_df$longitude) & !is.null(assets_df$longitude) & (is.na(assets_df$latitude) | is.null(assets_df$latitude)))
    )
    if (length(lat_lon_mismatch_idx) > 0) {
      validation_results$errors <- c(
        validation_results$errors,
        paste0("Assets have latitude or longitude filled but not both for rows: ", paste(lat_lon_mismatch_idx, collapse = ", "), ". Both latitude and longitude must be provided if at least one is filled.")
      )
    }
  }

  # Normalize asset text fields to ASCII for comparison
  if ("municipality" %in% names(assets_df)) {
    assets_df <- assets_df |>
      dplyr::mutate(
        municipality = dplyr::if_else(
          !is.na(.data$municipality) & nzchar(trimws(as.character(.data$municipality))),
          stringi::stri_trans_general(as.character(trimws(.data$municipality)), "Latin-ASCII"),
          as.character(.data$municipality)
        )
      )
  }
  if ("state" %in% names(assets_df)) {
    assets_df <- assets_df |>
      dplyr::mutate(
        state = dplyr::if_else(
          !is.na(.data$state) & nzchar(trimws(as.character(.data$state))),
          stringi::stri_trans_general(as.character(trimws(.data$state)), "Latin-ASCII"),
          as.character(.data$state)
        )
      )
  }
  # Validate states
  if (length(adm1_names) > 0) {
    asset_states <- assets_df |>
      dplyr::filter(!is.na(.data$state)) |>
      dplyr::pull(.data$state) |>
      unique()

    invalid_states <- asset_states[!asset_states %in% adm1_names]

    if (length(invalid_states) > 0) {
      validation_results$errors <- c(
        validation_results$errors,
        paste0(
          "Assets contain state names not in ADM1 boundaries: ",
          paste(invalid_states, collapse = ", ")
        )
      )
    }
  } else {
    validation_results$warnings <- c(
      validation_results$warnings,
      "Cannot validate asset states: ADM1 boundaries not loaded"
    )
  }

  # Validate municipalities
  if (length(adm2_names) > 0) {
    asset_municipalities <- assets_df |>
      dplyr::filter(!is.na(.data$municipality)) |>
      dplyr::pull(.data$municipality) |>
      unique()

    invalid_municipalities <- asset_municipalities[!asset_municipalities %in% adm2_names]

    if (length(invalid_municipalities) > 0) {
      validation_results$errors <- c(
        validation_results$errors,
        paste0(
          "Assets contain municipality names not in ADM2 boundaries: ",
          paste(invalid_municipalities, collapse = ", ")
        )
      )
    }
  } else {
    validation_results$warnings <- c(
      validation_results$warnings,
      "Cannot validate asset municipalities: ADM2 boundaries not loaded"
    )
  }

  return(validation_results)
}


#' Validate precomputed hazards geography against ADM1/ADM2 boundaries
#'
#' @param precomputed_hazards_df Precomputed hazards data frame
#' @param adm1_names Character vector of valid ADM1 province names
#' @param adm2_names Character vector of valid ADM2 municipality names
#' @param validation_results List with errors and warnings vectors
#' @param assets_df Optional assets data frame to check hazard coverage for specific regions
#' @param events_df Optional events data frame to know which hazards are required
#' @param hazard_configs Optional hazard configs list for variable name resolution
#' @return Updated validation_results list
#' @noRd
validate_precomputed_hazards_geography <- function(
  precomputed_hazards_df,
  adm1_names,
  adm2_names,
  validation_results,
  assets_df = NULL,
  events_df = NULL,
  hazard_configs = NULL
) {
  # Validate provinces (if column exists)
  if ("region" %in% names(precomputed_hazards_df) && length(adm1_names) > 0) {
    # Check adm_level to see if these are provinces
    province_rows <- precomputed_hazards_df |>
      dplyr::filter(.data$adm_level == "ADM1", !is.na(.data$region))

    if (nrow(province_rows) > 0) {
      hazard_provinces <- province_rows |>
        dplyr::pull(.data$region) |>
        unique()

      invalid_provinces <- hazard_provinces[!hazard_provinces %in% adm1_names]

      if (length(invalid_provinces) > 0) {
        validation_results$errors <- c(
          validation_results$errors,
          paste0(
            "Precomputed hazards contain province names not in ADM1 boundaries: ",
            paste(invalid_provinces, collapse = ", ")
          )
        )
      }
    }
  }

  # Validate municipalities (if column exists)
  if ("region" %in% names(precomputed_hazards_df) && length(adm2_names) > 0) {
    municipality_rows <- precomputed_hazards_df |>
      dplyr::filter(.data$adm_level == "ADM2", !is.na(.data$region))

    if (nrow(municipality_rows) > 0) {
      hazard_municipalities <- municipality_rows |>
        dplyr::pull(.data$region) |>
        unique()

      invalid_municipalities <- hazard_municipalities[!hazard_municipalities %in% adm2_names]

      if (length(invalid_municipalities) > 0) {
        validation_results$errors <- c(
          validation_results$errors,
          paste0(
            "Precomputed hazards contain municipality names not in ADM2 boundaries: ",
            paste(invalid_municipalities, collapse = ", ")
          )
        )
      }
    }
  }

  # Validate that state and municipality names in assets exist in reference lists
  if (!is.null(assets_df) && nrow(assets_df) > 0) {
    # Check states
    if ("state" %in% names(assets_df) && length(adm1_names) > 0) {
      asset_states <- assets_df |>
        dplyr::filter(!is.na(.data$state), nzchar(as.character(.data$state))) |>
        dplyr::pull(.data$state) |>
        unique()

      invalid_states <- asset_states[!asset_states %in% adm1_names]

      if (length(invalid_states) > 0) {
        validation_results$errors <- c(
          validation_results$errors,
          paste0(
            "Assets contain state names not in ADM1 boundaries: ",
            paste(invalid_states, collapse = ", ")
          )
        )
      }
    }

    # Check municipalities
    if ("municipality" %in% names(assets_df) && length(adm2_names) > 0) {
      asset_municipalities <- assets_df |>
        dplyr::filter(!is.na(.data$municipality), nzchar(as.character(.data$municipality))) |>
        dplyr::pull(.data$municipality) |>
        unique()

      invalid_municipalities <- asset_municipalities[!asset_municipalities %in% adm2_names]

      if (length(invalid_municipalities) > 0) {
        validation_results$errors <- c(
          validation_results$errors,
          paste0(
            "Assets contain municipality names not in ADM2 boundaries: ",
            paste(invalid_municipalities, collapse = ", ")
          )
        )
      }
    }
  }

  # NEW: Validate hazard-specific coverage for asset regions
  if (!is.null(assets_df) && !is.null(events_df) && nrow(assets_df) > 0 && nrow(events_df) > 0) {
    # Get required hazards from events
    required_hazards <- events_df |>
      dplyr::select("hazard_type", "hazard_indicator") |>
      dplyr::distinct()

    # Get unique municipalities from assets WITHOUT coordinates (these require precomputed lookup)
    asset_municipalities <- assets_df |>
      dplyr::filter(is.na(.data$latitude) | is.na(.data$longitude)) |>
      dplyr::filter(!is.na(.data$municipality), nzchar(as.character(.data$municipality))) |>
      dplyr::pull(.data$municipality) |>
      unique()

    # Get unique states from assets WITHOUT coordinates (these require precomputed lookup)
    asset_states <- assets_df |>
      dplyr::filter(is.na(.data$latitude) | is.na(.data$longitude)) |>
      dplyr::filter(!is.na(.data$state), nzchar(as.character(.data$state))) |>
      dplyr::pull(.data$state) |>
      unique()

    # Check hazard coverage for each municipality
    for (municipality in asset_municipalities) {
      municipality_hazards <- precomputed_hazards_df |>
        dplyr::filter(
          .data$region == !!municipality,
          .data$adm_level == "ADM2"
        ) |>
        dplyr::select("hazard_type", "hazard_indicator") |>
        dplyr::distinct()

      # Check if all required hazards are present
      for (i in seq_len(nrow(required_hazards))) {
        hazard_type <- required_hazards$hazard_type[i]
        hazard_indicator <- required_hazards$hazard_indicator[i]

        # Determine the display name (variable name if possible)
        # We look up the variable name from hazard_configs
        hazard_var <- if (!is.null(hazard_configs) &&
          !is.null(hazard_configs[[hazard_type]]) &&
          !is.null(hazard_configs[[hazard_type]]$indicators[[hazard_indicator]]) &&
          !is.null(hazard_configs[[hazard_type]]$indicators[[hazard_indicator]]$variable)) {
          hazard_configs[[hazard_type]]$indicators[[hazard_indicator]]$variable
        } else {
          hazard_indicator
        }

        has_hazard <- municipality_hazards |>
          dplyr::filter(
            .data$hazard_type == !!hazard_type,
            # Try matching by indicator key OR variable name
            (.data$hazard_indicator == !!hazard_indicator | .data$hazard_indicator == !!hazard_var)
          ) |>
          nrow() > 0

        if (!has_hazard) {
          # Check if state has this hazard (fallback)
          state_for_municipality <- assets_df |>
            dplyr::filter(.data$municipality == !!municipality) |>
            dplyr::pull(.data$state) |>
            unique() |>
            head(1)

          if (length(state_for_municipality) > 0 && !is.na(state_for_municipality)) {
            state_has_hazard <- precomputed_hazards_df |>
              dplyr::filter(
                .data$region == !!state_for_municipality,
                .data$adm_level == "ADM1",
                .data$hazard_type == !!hazard_type,
                # Try matching by indicator key OR variable name
                (.data$hazard_indicator == !!hazard_indicator | .data$hazard_indicator == !!hazard_var)
              ) |>
              nrow() > 0

            if (!state_has_hazard) {
              validation_results$errors <- c(
                validation_results$errors,
                paste0(
                  "Municipality '", municipality, "' is missing precomputed hazard data for ",
                  hazard_type, "__", hazard_var, ". ",
                  "State '", state_for_municipality, "' also lacks this hazard data."
                )
              )
            }
          } else {
            validation_results$errors <- c(
              validation_results$errors,
              paste0(
                "Municipality '", municipality, "' is missing precomputed hazard data for ",
                hazard_type, "__", hazard_var, "."
              )
            )
          }
        }
      }
    }

    # Check hazard coverage for states (for all assets with states)
    for (state in asset_states) {
      state_hazards <- precomputed_hazards_df |>
        dplyr::filter(
          .data$region == !!state,
          .data$adm_level == "ADM1"
        ) |>
        dplyr::select("hazard_type", "hazard_indicator") |>
        dplyr::distinct()

      # Check if all required hazards are present
      for (i in seq_len(nrow(required_hazards))) {
        hazard_type <- required_hazards$hazard_type[i]
        hazard_indicator <- required_hazards$hazard_indicator[i]

        # Determine the display name (variable name if possible)
        hazard_var <- if (!is.null(hazard_configs) &&
          !is.null(hazard_configs[[hazard_type]]) &&
          !is.null(hazard_configs[[hazard_type]]$indicators[[hazard_indicator]]) &&
          !is.null(hazard_configs[[hazard_type]]$indicators[[hazard_indicator]]$variable)) {
          hazard_configs[[hazard_type]]$indicators[[hazard_indicator]]$variable
        } else {
          hazard_indicator
        }

        has_hazard <- state_hazards |>
          dplyr::filter(
            .data$hazard_type == !!hazard_type,
            # Try matching by indicator key OR variable name
            (.data$hazard_indicator == !!hazard_indicator | .data$hazard_indicator == !!hazard_var)
          ) |>
          nrow() > 0

        if (!has_hazard) {
          validation_results$errors <- c(
            validation_results$errors,
            paste0(
              "State '", state, "' is missing precomputed hazard data for ",
              hazard_type, "__", hazard_var, "."
            )
          )
        }
      }
    }
  }

  return(validation_results)
}
