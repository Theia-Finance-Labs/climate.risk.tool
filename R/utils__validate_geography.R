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
  adm_codes_path <- file.path(base_dir, "areas", "brazil_adm_codes.csv")
  adm_codes <- if (file.exists(adm_codes_path)) {
    load_adm_codes_from_path(adm_codes_path)
  } else {
    NULL
  }
  states_sf <- repair_adm_boundary_names(states_sf, adm_codes, "adm1")

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
  adm_codes_path <- file.path(base_dir, "areas", "brazil_adm_codes.csv")
  adm_codes <- if (file.exists(adm_codes_path)) {
    load_adm_codes_from_path(adm_codes_path)
  } else {
    NULL
  }
  municipalities_sf <- repair_adm_boundary_names(municipalities_sf, adm_codes, "adm2")

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
    # Accept IBGE-like numeric codes in `state`; only validate textual names against ADM names.
    asset_states <- asset_states[!grepl("^\\d+$", as.character(asset_states))]

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
    # Accept IBGE-like numeric codes in `municipality`; only validate textual names against ADM names.
    asset_municipalities <- asset_municipalities[!grepl("^\\d+$", as.character(asset_municipalities))]

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
  adm1_codes = NULL,
  adm2_codes = NULL,
  adm1_shape_ids = NULL,
  adm2_shape_ids = NULL,
  assets_df = NULL,
  events_df = NULL,
  hazard_configs = NULL
) {
  non_empty_unique <- function(x) {
    x <- as.character(x)
    unique(x[!is.na(x) & nzchar(trimws(x))])
  }

  validate_precomputed_level <- function(rows, level_label, region_label, valid_names, valid_codes, valid_shape_ids, validation_results) {
    if (nrow(rows) == 0) {
      return(validation_results)
    }

    checked_identifier <- FALSE

    if ("shape_id" %in% names(rows) && length(valid_shape_ids) > 0) {
      shape_ids <- non_empty_unique(rows$shape_id)
      if (length(shape_ids) > 0) {
        checked_identifier <- TRUE
        invalid_shape_ids <- shape_ids[!shape_ids %in% valid_shape_ids]
        if (length(invalid_shape_ids) > 0) {
          validation_results$errors <- c(
            validation_results$errors,
            paste0(
              "Precomputed hazards contain ", level_label, " shape IDs not in boundaries: ",
              paste(invalid_shape_ids, collapse = ", ")
            )
          )
        }
      }
    }

    if ("adm_code" %in% names(rows) && length(valid_codes) > 0) {
      adm_codes <- non_empty_unique(rows$adm_code)
      if (length(adm_codes) > 0) {
        checked_identifier <- TRUE
        invalid_codes <- adm_codes[!adm_codes %in% valid_codes]
        if (length(invalid_codes) > 0) {
          validation_results$errors <- c(
            validation_results$errors,
            paste0(
              "Precomputed hazards contain ", level_label, " ADM codes not in boundaries: ",
              paste(invalid_codes, collapse = ", ")
            )
          )
        }
      }
    }

    if (!checked_identifier && "adm_name" %in% names(rows) && length(valid_names) > 0) {
      adm_names <- non_empty_unique(rows$adm_name)
      invalid_names <- adm_names[!adm_names %in% valid_names]
      if (length(invalid_names) > 0) {
        validation_results$errors <- c(
          validation_results$errors,
          paste0(
            "Precomputed hazards contain ", region_label, " names not in ", level_label, " boundaries: ",
            paste(invalid_names, collapse = ", ")
          )
        )
      }
    }

    validation_results
  }

  province_rows <- precomputed_hazards_df |>
    dplyr::filter(.data$adm_level == "ADM1")
  validation_results <- validate_precomputed_level(
    rows = province_rows,
    level_label = "ADM1",
    region_label = "province",
    valid_names = adm1_names,
    valid_codes = adm1_codes,
    valid_shape_ids = adm1_shape_ids,
    validation_results = validation_results
  )

  municipality_rows <- precomputed_hazards_df |>
    dplyr::filter(.data$adm_level == "ADM2")
  validation_results <- validate_precomputed_level(
    rows = municipality_rows,
    level_label = "ADM2",
    region_label = "municipality",
    valid_names = adm2_names,
    valid_codes = adm2_codes,
    valid_shape_ids = adm2_shape_ids,
    validation_results = validation_results
  )

  filter_precomputed_region <- function(level, name = NULL, code = NULL) {
    rows <- precomputed_hazards_df |>
      dplyr::filter(.data$adm_level == !!level)
    if (!is.null(code) && !is.na(code) && nzchar(as.character(code)) && "adm_code" %in% names(rows)) {
      return(rows |> dplyr::filter(.data$adm_code == !!as.character(code)))
    }
    if (!is.null(name) && !is.na(name) && nzchar(as.character(name)) && "adm_name" %in% names(rows)) {
      return(rows |> dplyr::filter(.data$adm_name == !!as.character(name)))
    }
    rows[0, , drop = FALSE]
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
      municipality_code <- if ("municipality_code" %in% names(assets_df)) {
        assets_df |>
          dplyr::filter(.data$municipality == !!municipality) |>
          dplyr::pull(.data$municipality_code) |>
          non_empty_unique() |>
          head(1)
      } else {
        character(0)
      }
      municipality_code <- if (length(municipality_code) > 0) municipality_code[[1]] else NULL

      municipality_hazards <- filter_precomputed_region("ADM2", name = municipality, code = municipality_code) |>
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
            state_code_for_municipality <- if ("state_code" %in% names(assets_df)) {
              assets_df |>
                dplyr::filter(.data$municipality == !!municipality) |>
                dplyr::pull(.data$state_code) |>
                non_empty_unique() |>
                head(1)
            } else {
              character(0)
            }
            state_code_for_municipality <- if (length(state_code_for_municipality) > 0) {
              state_code_for_municipality[[1]]
            } else {
              NULL
            }

            state_has_hazard <- filter_precomputed_region(
              "ADM1",
              name = state_for_municipality,
              code = state_code_for_municipality
            ) |>
              dplyr::filter(
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
      state_code <- if ("state_code" %in% names(assets_df)) {
        assets_df |>
          dplyr::filter(.data$state == !!state) |>
          dplyr::pull(.data$state_code) |>
          non_empty_unique() |>
          head(1)
      } else {
        character(0)
      }
      state_code <- if (length(state_code) > 0) state_code[[1]] else NULL

      state_hazards <- filter_precomputed_region("ADM1", name = state, code = state_code) |>
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
