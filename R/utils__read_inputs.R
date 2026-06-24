# Helper function to convert column names to snake_case
to_snake_case <- function(names) {
  names |>
    gsub("([a-z])([A-Z])", "\\1_\\2", x = _) |> # camelCase to snake_case
    gsub("\\s+", "_", x = _) |> # spaces to underscores
    gsub("\\.", "_", x = _) |> # dots to underscores
    gsub("_+", "_", x = _) |> # multiple underscores to single
    gsub("^_|_$", "", x = _) |> # remove leading/trailing underscores
    tolower()
}

normalize_geo_name <- function(name) {
  name_chr <- as.character(name)
  name_chr <- trimws(name_chr)
  name_chr[name_chr == "" | name_chr == "NA"] <- NA_character_
  if (!any(!is.na(name_chr))) {
    return(name_chr)
  }
  stringi::stri_trans_general(name_chr, "Latin-ASCII")
}

coerce_geo_code <- function(code, width = NULL) {
  code_chr <- trimws(as.character(code))
  code_chr[code_chr == "" | code_chr == "NA"] <- NA_character_
  code_chr <- gsub("\\.0+$", "", code_chr)
  is_digits <- !is.na(code_chr) & grepl("^\\d+$", code_chr)
  if (!is.null(width)) {
    code_chr <- ifelse(
      is_digits,
      sprintf(paste0("%0", width, "d"), as.integer(code_chr)),
      NA_character_
    )
  }
  code_chr
}

#' Detect CSV separator (comma or semicolon)
#'
#' @title Detect CSV separator
#' @description Detects whether a CSV file uses comma or semicolon as separator
#'   by analyzing the first non-empty line of the file.
#' @param file_path Path to CSV file
#' @return Character string: "comma" or "semicolon" (defaults to "comma" if unclear)
#' @noRd
detect_csv_separator <- function(file_path) {
  # Read first 100 lines
  lines <- readLines(file_path, n = 100, warn = FALSE)
  # Find first non-empty line
  non_empty_lines <- lines[nzchar(trimws(lines))]
  if (length(non_empty_lines) == 0) {
    return("comma")  # default
  }
  first_line <- non_empty_lines[1]
  
  comma_count <- stringr::str_count(first_line, ",")
  semicolon_count <- stringr::str_count(first_line, ";")
  
  if (semicolon_count > comma_count) {
    return("semicolon")
  } else {
    return("comma")
  }
}

#' Read asset data from Excel or CSV file
#'
#' @title Read asset information from Excel or CSV file
#' @description Reads asset information from Excel (.xlsx) or CSV (.csv) file in the specified folder.
#'   The folder must directly contain asset_information.xlsx or asset_information.csv (but not both).
#'   For CSV files, automatically detects separator (comma or semicolon).
#'   Converts column names to snake_case and parses numeric columns correctly.
#'   Municipality and state columns accept names. Use state_code and municipality_code
#'   for IBGE codes; legacy numeric codes in state or municipality are still accepted.
#' @param folder_path Character string specifying the folder containing asset_information.xlsx or asset_information.csv
#' @return tibble with asset information (includes state_code and municipality_code when available)
#' @examples
#' \dontrun{
#' # Folder path containing asset_information.xlsx or asset_information.csv
#' assets <- read_assets("path/to/folder")
#' }
#' @export
read_assets <- function(folder_path) {
  message("[read_assets] Reading asset data from: ", folder_path)

  # Check for both Excel and CSV files
  assets_xlsx <- file.path(folder_path, "asset_information.xlsx")
  assets_csv <- file.path(folder_path, "asset_information.csv")
  has_xlsx <- file.exists(assets_xlsx)
  has_csv <- file.exists(assets_csv)

  # Validate that only one format exists
  if (has_xlsx && has_csv) {
    stop("Both asset_information.xlsx and asset_information.csv found. Please use only one format.")
  }
  if (!has_xlsx && !has_csv) {
    stop("Neither asset_information.xlsx nor asset_information.csv found in: ", folder_path)
  }

  # Read assets data based on file format
  if (has_xlsx) {
    assets_raw <- readxl::read_excel(assets_xlsx) |>
      tibble::as_tibble() |>
      dplyr::rename_with(to_snake_case)
  } else {
    # CSV file - detect separator
    separator <- detect_csv_separator(assets_csv)
    if (separator == "semicolon") {
      assets_raw <- readr::read_csv2(
        assets_csv,
        col_types = readr::cols(.default = "c"),
        show_col_types = FALSE,
        locale = readr::locale(encoding = "UTF-8")
      ) |>
        tibble::as_tibble() |>
        dplyr::rename_with(to_snake_case)
    } else {
      assets_raw <- readr::read_csv(
        assets_csv,
        col_types = readr::cols(.default = "c"),
        show_col_types = FALSE,
        locale = readr::locale(encoding = "UTF-8")
      ) |>
        tibble::as_tibble() |>
        dplyr::rename_with(to_snake_case)
    }
  }

  # Drop blank trailing rows that Excel adds when a range is formatted beyond data
  if ("asset" %in% names(assets_raw) && "company" %in% names(assets_raw)) {
    n_before <- nrow(assets_raw)
    assets_raw <- assets_raw |>
      dplyr::filter(
        !(is.na(.data$asset) | trimws(as.character(.data$asset)) == "") |
        !(is.na(.data$company) | trimws(as.character(.data$company)) == "")
      )
    n_dropped <- n_before - nrow(assets_raw)
    if (n_dropped > 0) {
      message("[read_assets] Dropped ", n_dropped, " blank rows (trailing empty rows in Excel/CSV)")
    }
  }

  # Convert numeric columns for assets
  numeric_asset_cols <- c(
    "share_of_economic_activity", "latitude", "longitude",
    "size_in_m2", "size_in_hectare", "cost_factor", "growth_rate"
  )

  assets_raw <- assets_raw |>
    dplyr::mutate(
      # For latitude, longitude - suppress coercion warnings (can have empty values)
      dplyr::across(
        dplyr::any_of(c("latitude", "longitude")),
        ~ suppressWarnings(as.numeric(dplyr::if_else(. == "", NA_character_, as.character(.))))
      ),
      # Clean size_in_m2 column by extracting numeric part
      dplyr::across(
        dplyr::any_of("size_in_m2"),
        ~ suppressWarnings(as.numeric(gsub("^([0-9.]+).*", "\\1", as.character(.))))
      ),
      # Convert other numeric columns (replace empty strings with NA before conversion)
      dplyr::across(
        dplyr::any_of(setdiff(numeric_asset_cols, c("latitude", "longitude", "size_in_m2"))),
        ~ as.numeric(dplyr::if_else(as.character(.) == "", NA_character_, as.character(.)))
      )
    )

  # Handle character columns that can have empty values (municipality, state, asset_subtype)
  char_cols_with_empty <- c("municipality", "state", "asset_subtype")

  assets_raw <- assets_raw |>
    dplyr::mutate(
      dplyr::across(
        dplyr::any_of(char_cols_with_empty),
        ~ {
          # Convert to character first to handle all types (logical NA, numeric, etc.)
          col_data <- as.character(.)
          # Replace empty strings with NA
          col_data[col_data == "" | col_data == "NA"] <- NA_character_
          # Set encoding only if we have non-NA values
          if (any(!is.na(col_data))) {
            Encoding(col_data) <- "UTF-8"
          }
          col_data
        }
      )
    )

  # Ensure municipality and state columns exist (add as NA if missing)
  if (!"municipality" %in% names(assets_raw)) {
    assets_raw$municipality <- NA_character_
  }
  if (!"state" %in% names(assets_raw)) {
    assets_raw$state <- NA_character_
  }
  
  # Normalize municipality and state names (remove accents, convert to ASCII)
  # Also ensure that whitespace-only strings are converted to NA
  assets_raw <- assets_raw |>
    dplyr::mutate(
      municipality = dplyr::if_else(
        !is.na(.data$municipality) & nzchar(trimws(as.character(.data$municipality))),
        stringi::stri_trans_general(as.character(trimws(.data$municipality)), "Latin-ASCII"),
        NA_character_
      ),
      state = dplyr::if_else(
        !is.na(.data$state) & nzchar(trimws(as.character(.data$state))),
        stringi::stri_trans_general(as.character(trimws(.data$state)), "Latin-ASCII"),
        NA_character_
      )
    )

  if (!"state_code" %in% names(assets_raw)) {
    assets_raw$state_code <- NA_character_
  }
  if (!"municipality_code" %in% names(assets_raw)) {
    assets_raw$municipality_code <- NA_character_
  }

  # Load ADM codes file - required for code matching
  # The file is in the base_dir/areas/ folder (one level up from input folder)
  # e.g., if folder_path is tests/tests_data/user_input, look in tests/tests_data/areas/
  base_dir <- dirname(folder_path)
  adm_codes_path <- file.path(base_dir, "areas", "brazil_adm_codes.csv")
  
  if (!file.exists(adm_codes_path)) {
    stop("brazil_adm_codes.csv not found. Expected at: ", adm_codes_path)
  }
  
  message("[read_assets] Loading brazil_adm_codes.csv and matching codes to names...")
  adm_codes <- load_adm_codes_from_path(adm_codes_path)
  assets_raw <- match_adm_codes_to_names(assets_raw, adm_codes)

  message("[read_assets] Loaded ", nrow(assets_raw), " assets")
  assets_raw
}

#' Assign ADM regions to assets using already-loaded boundaries
#'
#' @title Assign ADM1/ADM2 regions to assets using spatial matching with loaded boundaries
#' @description For geolocated assets (latitude/longitude), assigns municipality and state
#'   through point-in-polygon matching against ADM2/ADM1 boundaries. Coordinates are the
#'   source of truth: existing ADM fields are overwritten for geolocated rows. For non-geolocated
#'   assets, state can still be inferred from municipality when possible.
#' @param assets_df Data frame with asset information
#' @param adm1_boundaries sf object with ADM1 (state) boundaries
#' @param adm2_boundaries Optional sf object with ADM2 (municipality) boundaries for municipality-based lookup
#' @param adm_codes Optional data frame with ADM codes (from load_adm_codes()). Required for code assignment.
#' @return Data frame with ADM columns assigned where possible
#' @examples
#' \dontrun{
#' adm1 <- sf::st_read("path/to/ADM1.geojson")
#' adm_codes <- load_adm_codes("path/to/base_dir")
#' assets_with_states <- assign_state_to_assets_with_boundaries(assets, adm1, adm_codes = adm_codes)
#' }
#' @export
assign_state_to_assets_with_boundaries <- function(assets_df, adm1_boundaries, adm2_boundaries = NULL, adm_codes = NULL) {
  result <- tibble::as_tibble(assets_df) |>
    dplyr::mutate(.asset_row_id = dplyr::row_number())

  required_cols <- c(
    "state", "state_code", "state_name",
    "municipality", "municipality_code", "municipality_name"
  )
  for (col in required_cols) {
    if (!col %in% names(result)) {
      result[[col]] <- NA_character_
    } else {
      result[[col]] <- as.character(result[[col]])
    }
  }

  adm1_lookup <- NULL
  adm2_lookup <- NULL
  if (!is.null(adm_codes) && nrow(adm_codes) > 0) {
    adm1_lookup <- adm_codes |>
      dplyr::filter(.data$adm_level == "adm1") |>
      dplyr::transmute(
        shapeID = as.character(.data$shapeID),
        state_code_lookup = as.character(.data$code),
        state_name_lookup_raw = as.character(.data$name)
      ) |>
      dplyr::distinct()

    adm2_lookup <- adm_codes |>
      dplyr::filter(.data$adm_level == "adm2") |>
      dplyr::transmute(
        shapeID = as.character(.data$shapeID),
        municipality_code_lookup = as.character(.data$code),
        municipality_name_lookup_raw = as.character(.data$name)
      ) |>
      dplyr::distinct()
  }

  states_sf <- adm1_boundaries |>
    dplyr::mutate(
      shapeID = as.character(.data$shapeID),
      state_name_raw = as.character(.data$shapeName),
      state_name = normalize_geo_name(.data$state_name_raw)
    )
  if (!is.null(adm1_lookup)) {
    states_sf <- states_sf |>
      dplyr::left_join(adm1_lookup, by = "shapeID")
  } else {
    states_sf$state_code_lookup <- NA_character_
    states_sf$state_name_lookup_raw <- NA_character_
  }
  states_sf <- states_sf |>
    dplyr::mutate(
      state_name_raw = dplyr::coalesce(.data$state_name_lookup_raw, .data$state_name_raw),
      state_name = normalize_geo_name(.data$state_name_raw)
    )

  if (!sf::st_is_longlat(states_sf)) {
    states_sf <- sf::st_transform(states_sf, 4326)
  }

  municipalities_sf <- NULL
  if (!is.null(adm2_boundaries)) {
    municipalities_sf <- adm2_boundaries |>
      dplyr::mutate(
        shapeID = as.character(.data$shapeID),
        municipality_name_raw = as.character(.data$shapeName),
        municipality_name = normalize_geo_name(.data$municipality_name_raw)
      )

    if (!is.null(adm2_lookup)) {
      municipalities_sf <- municipalities_sf |>
        dplyr::left_join(adm2_lookup, by = "shapeID")
    } else {
      municipalities_sf$municipality_code_lookup <- NA_character_
      municipalities_sf$municipality_name_lookup_raw <- NA_character_
    }

    municipalities_sf <- municipalities_sf |>
      dplyr::mutate(
        municipality_name_raw = dplyr::coalesce(.data$municipality_name_lookup_raw, .data$municipality_name_raw),
        municipality_name = normalize_geo_name(.data$municipality_name_raw),
        municipality_code_lookup = as.character(.data$municipality_code_lookup),
        state_code_from_muni = dplyr::if_else(
          !is.na(.data$municipality_code_lookup) & nzchar(.data$municipality_code_lookup),
          substr(.data$municipality_code_lookup, 1, 2),
          NA_character_
        )
      )

    if (!is.null(adm1_lookup)) {
      state_name_lookup_from_code <- adm1_lookup |>
        dplyr::select("state_code_lookup", "state_name_lookup_raw") |>
        dplyr::distinct()
      municipalities_sf <- municipalities_sf |>
        dplyr::left_join(
          state_name_lookup_from_code,
          by = c("state_code_from_muni" = "state_code_lookup")
        ) |>
        dplyr::rename(state_name_from_muni_raw = "state_name_lookup_raw")
    } else {
      municipalities_sf$state_name_from_muni_raw <- NA_character_
    }

    if (!sf::st_is_longlat(municipalities_sf)) {
      municipalities_sf <- sf::st_transform(municipalities_sf, 4326)
    }
  }

  has_coords <- !is.na(result$latitude) & !is.na(result$longitude)
  if (any(has_coords)) {
    assets_with_coords <- result[has_coords, , drop = FALSE]
    pts_sf <- sf::st_as_sf(
      assets_with_coords,
      coords = c("longitude", "latitude"),
      crs = 4326
    )

    first_hit <- function(lst) {
      vapply(lst, function(x) if (length(x) == 0) NA_integer_ else x[[1]], integer(1))
    }

    muni_idx <- rep(NA_integer_, nrow(assets_with_coords))
    if (!is.null(municipalities_sf) && nrow(municipalities_sf) > 0) {
      muni_idx <- first_hit(sf::st_intersects(pts_sf, municipalities_sf, sparse = TRUE))
    }
    state_idx <- first_hit(sf::st_intersects(pts_sf, states_sf, sparse = TRUE))

    municipality_code_new <- if (!is.null(municipalities_sf)) municipalities_sf$municipality_code_lookup[muni_idx] else rep(NA_character_, nrow(assets_with_coords))
    municipality_name_raw_new <- if (!is.null(municipalities_sf)) municipalities_sf$municipality_name_raw[muni_idx] else rep(NA_character_, nrow(assets_with_coords))
    state_code_from_muni <- if (!is.null(municipalities_sf)) municipalities_sf$state_code_from_muni[muni_idx] else rep(NA_character_, nrow(assets_with_coords))
    state_name_from_muni_raw <- if (!is.null(municipalities_sf)) municipalities_sf$state_name_from_muni_raw[muni_idx] else rep(NA_character_, nrow(assets_with_coords))

    state_code_from_state <- states_sf$state_code_lookup[state_idx]
    state_name_from_state_raw <- states_sf$state_name_raw[state_idx]

    state_code_new <- dplyr::coalesce(state_code_from_muni, state_code_from_state)
    state_name_raw_new <- dplyr::coalesce(state_name_from_muni_raw, state_name_from_state_raw)

    idx <- which(has_coords)
    result$municipality_code[idx] <- as.character(municipality_code_new)
    result$municipality_name[idx] <- as.character(municipality_name_raw_new)
    result$municipality[idx] <- normalize_geo_name(municipality_name_raw_new)
    result$state_code[idx] <- as.character(state_code_new)
    result$state_name[idx] <- as.character(state_name_raw_new)
    result$state[idx] <- normalize_geo_name(state_name_raw_new)

    outside_all <- is.na(muni_idx) & is.na(state_idx)
    if (any(outside_all)) {
      unresolved_rows <- idx[outside_all]
      unresolved_assets <- if ("asset" %in% names(result)) {
        as.character(result$asset[unresolved_rows])
      } else {
        as.character(unresolved_rows)
      }
      warning(
        paste0(
          "[assign_state_to_assets] Could not map geolocated assets to ADM polygons: ",
          paste(unresolved_assets, collapse = ", ")
        ),
        call. = FALSE
      )
    }
  }

  non_geo_missing_state <- (!has_coords) & is.na(result$state)
  if (any(non_geo_missing_state)) {
    message("[assign_state_to_assets] Assigning state for non-geolocated assets via municipality lookup")

    if (!is.null(adm_codes) && "municipality" %in% names(result)) {
      muni_to_state_lookup <- adm_codes |>
        dplyr::filter(.data$adm_level == "adm2") |>
        dplyr::transmute(
          municipality_lookup = tolower(trimws(normalize_geo_name(.data$name))),
          municipality_code_lookup = as.character(.data$code),
          state_code_lookup = substr(as.character(.data$code), 1, 2)
        ) |>
        dplyr::left_join(
          adm_codes |>
            dplyr::filter(.data$adm_level == "adm1") |>
            dplyr::transmute(
              state_code_lookup = as.character(.data$code),
              state_name_lookup_raw = as.character(.data$name)
            ) |>
            dplyr::distinct(),
          by = "state_code_lookup"
        ) |>
        dplyr::distinct()

      non_geo_rows <- result |>
        dplyr::filter(.data$.asset_row_id %in% result$.asset_row_id[non_geo_missing_state]) |>
        dplyr::mutate(municipality_lookup = tolower(trimws(as.character(.data$municipality)))) |>
        dplyr::left_join(muni_to_state_lookup, by = "municipality_lookup") |>
        dplyr::mutate(
          municipality_code = dplyr::coalesce(.data$municipality_code, .data$municipality_code_lookup),
          state_code = dplyr::coalesce(.data$state_code, .data$state_code_lookup),
          state_name = dplyr::coalesce(.data$state_name, .data$state_name_lookup_raw),
          state = dplyr::coalesce(.data$state, normalize_geo_name(.data$state_name_lookup_raw))
        ) |>
        dplyr::select(-dplyr::any_of(c(
          "municipality_lookup",
          "municipality_code_lookup",
          "state_code_lookup",
          "state_name_lookup_raw"
        )))

      result <- result |>
        dplyr::filter(!(.data$.asset_row_id %in% non_geo_rows$.asset_row_id)) |>
        dplyr::bind_rows(non_geo_rows) |>
        dplyr::arrange(.data$.asset_row_id)
    }
  }

  result |>
    dplyr::select(-".asset_row_id")
}


#' Assign state to assets based on coordinates or municipality (from base_dir)
#'
#' @title Assign states to assets using spatial matching (loads boundaries from base_dir)
#' @description For assets without state assigned, uses spatial matching with ADM1 boundaries
#'   based on coordinates (if available) or municipality lookup (if no coordinates).
#'   State names are ASCII-normalized for consistency.
#'   This is a convenience wrapper that loads boundaries from base_dir.
#' @param assets_df Data frame with asset information
#' @param base_dir Base directory containing areas subdirectory with geoBoundaries files
#' @return Data frame with state assigned to all assets
#' @examples
#' \dontrun{
#' assets <- read_assets("tests/tests_data")
#' assets_with_states <- assign_state_to_assets(assets, "tests/tests_data")
#' }
#' @export
assign_state_to_assets <- function(assets_df, base_dir) {
  # Load state boundaries
  state_path <- file.path(base_dir, "areas", "state", "geoBoundaries-BRA-ADM1_simplified.geojson")
  municipality_path <- file.path(base_dir, "areas", "municipality", "geoBoundaries-BRA-ADM2_simplified.geojson")
  adm_codes_path <- file.path(base_dir, "areas", "brazil_adm_codes.csv")

  if (!file.exists(state_path)) {
    message("[assign_state_to_assets] State boundaries not found, skipping state assignment")
    return(assets_df)
  }

  # Load boundaries
  adm1_boundaries <- sf::st_read(state_path, quiet = TRUE)
  adm2_boundaries <- if (file.exists(municipality_path)) {
    sf::st_read(municipality_path, quiet = TRUE)
  } else {
    NULL
  }
  
  # Load ADM codes
  adm_codes <- if (file.exists(adm_codes_path)) {
    load_adm_codes_from_path(adm_codes_path)
  } else {
    NULL
  }

  adm1_boundaries <- repair_adm_boundary_names(adm1_boundaries, adm_codes, "adm1")
  adm2_boundaries <- repair_adm_boundary_names(adm2_boundaries, adm_codes, "adm2")

  # Call the main function with loaded boundaries
  assign_state_to_assets_with_boundaries(assets_df, adm1_boundaries, adm2_boundaries, adm_codes)
}

#' Load ADM codes from brazil_adm_codes.csv file
#'
#' @title Load ADM codes mapping file
#' @description Loads the brazil_adm_codes.csv file which maps ADM codes to names.
#'   The file should have columns: adm_code, adm_name, adm_level (adm1 or adm2), shape_id
#' @param base_dir Base directory containing areas/brazil_adm_codes.csv
#' @return Data frame with columns: code, name, adm_level, shapeID (normalized from file columns)
#' @examples
#' \dontrun{
#' adm_codes <- load_adm_codes("tests/tests_data")
#' }
#' @export
load_adm_codes <- function(base_dir) {
  adm_codes_path <- file.path(base_dir, "areas", "brazil_adm_codes.csv")
  if (!file.exists(adm_codes_path)) {
    stop("brazil_adm_codes.csv not found at: ", adm_codes_path)
  }
  load_adm_codes_from_path(adm_codes_path)
}

#' Load ADM codes from a specific file path
#'
#' @title Load ADM codes from file path
#' @description Internal function to load ADM codes from a specific file path
#' @param file_path Path to brazil_adm_codes.csv file
#' @return Data frame with columns: code, name, adm_level, shapeID
#' @noRd
load_adm_codes_from_path <- function(file_path) {
  readr::read_csv(
    file_path,
    col_types = readr::cols(
      adm_code = "c",
      adm_name = "c",
      adm_level = "c",
      shape_id = "c"
    ),
    show_col_types = FALSE,
    locale = readr::locale(encoding = "UTF-8")
  ) |>
    dplyr::rename(
      code = "adm_code",
      name = "adm_name",
      shapeID = "shape_id"
    ) |>
    tibble::as_tibble()
}

#' Repair ADM boundary names from canonical ADM codes
#'
#' @param boundaries_sf sf object containing shapeID and shapeName columns
#' @param adm_codes Data frame from load_adm_codes()
#' @param adm_level ADM level to use from adm_codes ("adm1" or "adm2")
#' @return sf object with shapeName repaired when shapeID matches adm_codes
#' @noRd
repair_adm_boundary_names <- function(boundaries_sf, adm_codes, adm_level) {
  if (is.null(boundaries_sf) || is.null(adm_codes)) {
    return(boundaries_sf)
  }
  if (!all(c("shapeID", "shapeName") %in% names(boundaries_sf))) {
    return(boundaries_sf)
  }
  if (!all(c("shapeID", "name", "adm_level") %in% names(adm_codes))) {
    return(boundaries_sf)
  }

  adm_lookup <- adm_codes |>
    dplyr::filter(.data$adm_level == !!adm_level) |>
    dplyr::transmute(
      shapeID = as.character(.data$shapeID),
      shapeName_repaired = as.character(.data$name)
    ) |>
    dplyr::filter(!is.na(.data$shapeID), !is.na(.data$shapeName_repaired), nzchar(.data$shapeName_repaired)) |>
    dplyr::distinct(.data$shapeID, .keep_all = TRUE)

  if (nrow(adm_lookup) == 0) {
    return(boundaries_sf)
  }

  repaired <- boundaries_sf |>
    dplyr::mutate(
      shapeID = as.character(.data$shapeID),
      shapeName_before_repair = as.character(.data$shapeName)
    ) |>
    dplyr::left_join(adm_lookup, by = "shapeID")

  changed <- !is.na(repaired$shapeName_repaired) &
    repaired$shapeName_repaired != repaired$shapeName_before_repair

  repaired <- repaired |>
    dplyr::mutate(
      shapeName = dplyr::coalesce(.data$shapeName_repaired, .data$shapeName_before_repair)
    )

  if (any(changed)) {
    existing_original <- if ("shapeName_original" %in% names(repaired)) {
      as.character(repaired$shapeName_original)
    } else {
      rep(NA_character_, nrow(repaired))
    }
    repaired <- repaired |>
      dplyr::mutate(
        shapeName_original = dplyr::if_else(
          changed,
          .data$shapeName_before_repair,
          existing_original
        )
      )
  }

  repaired |>
    dplyr::select(-"shapeName_repaired", -"shapeName_before_repair")
}

#' Match ADM codes to names in assets data frame
#'
#' @title Match ADM codes to names
#' @description Matches ADM codes to their names. state_code and municipality_code
#'   are the canonical input columns for IBGE codes. Legacy numeric codes in state
#'   or municipality are also accepted and normalized into the canonical code columns.
#' @param assets_df Data frame with asset information containing state and/or municipality columns
#' @param adm_codes Data frame with ADM codes (from load_adm_codes())
#' @return Data frame with state_code, state_name, municipality_code, municipality_name columns added
#' @examples
#' \dontrun{
#' adm_codes <- load_adm_codes("tests/tests_data")
#' assets <- match_adm_codes_to_names(assets, adm_codes)
#' }
#' @export
match_adm_codes_to_names <- function(assets_df, adm_codes) {
  clean_chr <- function(x) {
    x <- trimws(as.character(x))
    x[x == "" | x == "NA"] <- NA_character_
    x
  }

  # Ensure code columns exist
  if (!"state_code" %in% names(assets_df)) {
    assets_df$state_code <- NA_character_
  }
  if (!"municipality_code" %in% names(assets_df)) {
    assets_df$municipality_code <- NA_character_
  }
  if (!"state_name" %in% names(assets_df)) {
    assets_df$state_name <- NA_character_
  }
  if (!"municipality_name" %in% names(assets_df)) {
    assets_df$municipality_name <- NA_character_
  }
  
  # Create lookup tables for adm1 and adm2
  adm1_lookup <- adm_codes |>
    dplyr::filter(.data$adm_level == "adm1") |>
    dplyr::transmute(
      code = as.character(.data$code),
      name = as.character(.data$name),
      name_normalized = normalize_geo_name(.data$name)
    ) |>
    dplyr::distinct()
  
  adm2_lookup <- adm_codes |>
    dplyr::filter(.data$adm_level == "adm2") |>
    dplyr::transmute(
      code = as.character(.data$code),
      name = as.character(.data$name),
      name_normalized = normalize_geo_name(.data$name),
      state_code = substr(as.character(.data$code), 1, 2)
    ) |>
    dplyr::left_join(
      adm1_lookup |>
        dplyr::select(state_code = "code", state_name = "name", state_name_normalized = "name_normalized"),
      by = "state_code"
    ) |>
    dplyr::distinct()

  state_code_input <- coerce_geo_code(assets_df$state_code, width = 2)
  state_legacy_input <- clean_chr(assets_df$state)
  state_code_from_legacy <- ifelse(state_legacy_input %in% adm1_lookup$code, state_legacy_input, NA_character_)
  state_code_resolved <- dplyr::coalesce(state_code_input, state_code_from_legacy)
  state_idx <- match(state_code_resolved, adm1_lookup$code)
  state_matched <- !is.na(state_idx)

  assets_df$state_code <- dplyr::coalesce(state_code_resolved, clean_chr(assets_df$state_code))
  assets_df$state_name <- ifelse(state_matched, adm1_lookup$name[state_idx], clean_chr(assets_df$state_name))
  assets_df$state <- ifelse(state_matched, adm1_lookup$name_normalized[state_idx], clean_chr(assets_df$state))

  municipality_code_input <- coerce_geo_code(assets_df$municipality_code, width = 7)
  municipality_legacy_input <- clean_chr(assets_df$municipality)
  municipality_code_from_legacy <- ifelse(
    municipality_legacy_input %in% adm2_lookup$code,
    municipality_legacy_input,
    NA_character_
  )
  municipality_code_resolved <- dplyr::coalesce(municipality_code_input, municipality_code_from_legacy)
  municipality_idx <- match(municipality_code_resolved, adm2_lookup$code)
  municipality_matched <- !is.na(municipality_idx)

  assets_df$municipality_code <- dplyr::coalesce(municipality_code_resolved, clean_chr(assets_df$municipality_code))
  assets_df$municipality_name <- ifelse(
    municipality_matched,
    adm2_lookup$name[municipality_idx],
    clean_chr(assets_df$municipality_name)
  )
  assets_df$municipality <- ifelse(
    municipality_matched,
    adm2_lookup$name_normalized[municipality_idx],
    clean_chr(assets_df$municipality)
  )

  state_missing <- is.na(clean_chr(assets_df$state_code)) &
    is.na(clean_chr(assets_df$state_name)) &
    is.na(clean_chr(assets_df$state))
  fill_state_from_municipality <- municipality_matched & state_missing
  if (any(fill_state_from_municipality)) {
    assets_df$state_code[fill_state_from_municipality] <- adm2_lookup$state_code[municipality_idx[fill_state_from_municipality]]
    assets_df$state_name[fill_state_from_municipality] <- adm2_lookup$state_name[municipality_idx[fill_state_from_municipality]]
    assets_df$state[fill_state_from_municipality] <- adm2_lookup$state_name_normalized[municipality_idx[fill_state_from_municipality]]
  }
  
  assets_df
}

#' Read company data from Excel or CSV file
#'
#' @title Read company information from Excel or CSV file
#' @description Reads company information from an Excel (.xlsx) or CSV (.csv) file,
#'   converting column names to snake_case and parsing numeric columns correctly.
#'   Can accept either a direct file path or a folder path containing company_information.xlsx or company_information.csv.
#'   For CSV files, automatically detects separator (comma or semicolon).
#'   If both Excel and CSV files exist in the folder, an error is raised.
#' @param file_path Character string specifying either the path to the company file directly,
#'   or a folder path containing company_information.xlsx or company_information.csv
#' @return tibble with company information
#' @examples
#' \dontrun{
#' # Direct file path
#' companies <- read_companies("path/to/company_information.xlsx")
#' # Or folder path (will look for .xlsx or .csv)
#' companies <- read_companies("path/to/folder")
#' }
#' @export
read_companies <- function(file_path) {
  message("[read_companies] Reading company data from: ", file_path)

  # If file_path is a directory, look for company_information files
  if (dir.exists(file_path)) {
    company_xlsx <- file.path(file_path, "company_information.xlsx")
    company_csv <- file.path(file_path, "company_information.csv")
    has_xlsx <- file.exists(company_xlsx)
    has_csv <- file.exists(company_csv)

    # Validate that only one format exists
    if (has_xlsx && has_csv) {
      stop("Both company_information.xlsx and company_information.csv found. Please use only one format.")
    }
    if (!has_xlsx && !has_csv) {
      stop("Neither company_information.xlsx nor company_information.csv found in: ", file_path)
    }

    # Use the file that exists
    file_path <- if (has_xlsx) company_xlsx else company_csv
  }

  # Check if file exists
  if (!file.exists(file_path)) {
    stop("Company file not found at: ", file_path)
  }

  # Determine file format and read accordingly
  is_csv <- grepl("\\.csv$", file_path, ignore.case = TRUE)

  if (is_csv) {
    # CSV file - detect separator
    separator <- detect_csv_separator(file_path)
    if (separator == "semicolon") {
      companies_raw <- readr::read_csv2(
        file_path,
        col_types = readr::cols(.default = "c"),
        show_col_types = FALSE,
        locale = readr::locale(encoding = "UTF-8")
      ) |>
        tibble::as_tibble() |>
        dplyr::rename_with(to_snake_case)
    } else {
      companies_raw <- readr::read_csv(
        file_path,
        col_types = readr::cols(.default = "c"),
        show_col_types = FALSE,
        locale = readr::locale(encoding = "UTF-8")
      ) |>
        tibble::as_tibble() |>
        dplyr::rename_with(to_snake_case)
    }
  } else {
    # Excel file
    companies_raw <- readxl::read_excel(file_path) |>
      tibble::as_tibble() |>
      dplyr::rename_with(to_snake_case)
  }

  # Remove unnamed columns that Excel adds (e.g. ...9, ...10 → snake_case becomes 9, 10)
  companies_raw <- companies_raw |>
    dplyr::select(-dplyr::matches("^(\\.\\.\\.)?\\d+$"))

  # Drop blank trailing rows
  if ("company" %in% names(companies_raw)) {
    n_before <- nrow(companies_raw)
    companies_raw <- companies_raw |>
      dplyr::filter(!is.na(.data$company) & trimws(as.character(.data$company)) != "")
    n_dropped <- n_before - nrow(companies_raw)
    if (n_dropped > 0) {
      message("[read_companies] Dropped ", n_dropped, " blank rows (trailing empty rows in Excel/CSV)")
    }
  }

  # Convert numeric columns for companies
  numeric_company_cols <- c("revenues", "debt", "volatility", "net_profit_margin", "loan_size", "lgd", "term")

  companies_raw <- companies_raw |>
    dplyr::mutate(
      dplyr::across(
        dplyr::any_of(numeric_company_cols),
        ~ as.numeric(dplyr::if_else(as.character(.) == "", NA_character_, as.character(.)))
      )
    )

  message("[read_companies] Loaded ", nrow(companies_raw), " companies")
  companies_raw
}
