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
#'   Municipality and state columns accept names.
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

  # Convert numeric columns for assets
  numeric_asset_cols <- c(
    "share_of_economic_activity", "latitude", "longitude",
    "size_in_m2", "size_in_hectare", "cost_factor"
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

#' Assign state to assets using already-loaded boundaries
#'
#' @title Assign states to assets using spatial matching with loaded boundaries
#' @description For assets without state assigned, uses spatial matching with ADM1 boundaries
#'   based on coordinates (if available) or municipality lookup (if no coordinates).
#'   State names are ASCII-normalized for consistency.
#' @param assets_df Data frame with asset information
#' @param adm1_boundaries sf object with ADM1 (state) boundaries
#' @param adm2_boundaries Optional sf object with ADM2 (municipality) boundaries for municipality-based lookup
#' @param adm_codes Optional data frame with ADM codes (from load_adm_codes()). Required for code assignment.
#' @return Data frame with state assigned to all assets
#' @examples
#' \dontrun{
#' adm1 <- sf::st_read("path/to/ADM1.geojson")
#' adm_codes <- load_adm_codes("path/to/base_dir")
#' assets_with_states <- assign_state_to_assets_with_boundaries(assets, adm1, adm_codes = adm_codes)
#' }
#' @export
assign_state_to_assets_with_boundaries <- function(assets_df, adm1_boundaries, adm2_boundaries = NULL, adm_codes = NULL) {
  # Ensure state column is character (not logical if all NA)
  if ("state" %in% names(assets_df)) {
    assets_df$state <- as.character(assets_df$state)
  }
  if (!"state_code" %in% names(assets_df)) {
    assets_df$state_code <- NA_character_
  }

  # Prepare ADM1 lookup if codes available
  adm1_lookup <- NULL
  if (!is.null(adm_codes)) {
    adm1_lookup <- adm_codes |>
      dplyr::filter(.data$adm == "adm1") |>
      dplyr::select("code", "name", "shapeID") |>
      dplyr::distinct()
  }

  # Normalize state names in boundaries and add codes if available
  states_sf <- adm1_boundaries |>
    dplyr::mutate(
      state_name = stringi::stri_trans_general(as.character(.data$shapeName), "Latin-ASCII"),
      shapeID = as.character(.data$shapeID)
    )
  
  if (!is.null(adm1_lookup)) {
    states_sf <- states_sf |>
      dplyr::left_join(
        adm1_lookup |> dplyr::select("shapeID", "code") |> dplyr::rename(state_code_lookup = "code"),
        by = "shapeID"
      )
  } else {
    states_sf$state_code_lookup <- NA_character_
  }

  # Identify assets without state
  assets_without_state <- assets_df |>
    dplyr::filter(is.na(.data$state))

  assets_with_state_already <- assets_df |>
    dplyr::filter(!is.na(.data$state))

  if (nrow(assets_without_state) == 0) {
    message("[assign_state_to_assets] All assets already have state assigned")
    return(assets_df)
  }

  message("[assign_state_to_assets] Assigning state to ", nrow(assets_without_state), " assets")

  # Strategy 1: Assets with lat/lon - spatial join to state
  assets_with_coords <- assets_without_state |>
    dplyr::filter(!is.na(.data$latitude), !is.na(.data$longitude))

  if (nrow(assets_with_coords) > 0) {
    message("  Assigning state via coordinates for ", nrow(assets_with_coords), " assets")

    # Convert to sf object
    assets_coords_sf <- sf::st_as_sf(
      assets_with_coords,
      coords = c("longitude", "latitude"),
      crs = 4326
    )

    # Spatial join with states
    assets_coords_joined <- sf::st_join(assets_coords_sf, states_sf, join = sf::st_within)

    # Extract coordinates back and assign state
    # Handle missing columns safely
    state_names <- if ("state_name" %in% names(assets_coords_joined)) assets_coords_joined$state_name else NA_character_
    state_codes <- if ("state_code_lookup" %in% names(assets_coords_joined)) assets_coords_joined$state_code_lookup else NA_character_
    
    assets_with_coords <- assets_with_coords |>
      dplyr::mutate(
        # Assign name as fallback or primary if code missing
        state = state_names,
        state_name = state_names,
        state_code = state_codes
      )
      
    # If we have codes, prefer using code in state column (based on new requirement)
    # But for now, user asked to "keep the name in for good measures" and "use codes instead of names"
    # The requirement "use codes instead of names for the adm regions" implies 'state' column should be code?
    # Or should we just ensure we have 'state_code' column?
    # The prompt says: "I'll have the shape id from the regions shapefiles, and the adm id from the user input"
    # And "fix all issues that can happen with this change of not using the names anymore"
    # It seems we should ensure 'state_code' is populated. 
    # Let's keep 'state' as name for now to avoid breaking too much, but ensure 'state_code' is set.
    # Actually, if the user input uses codes, 'state' might be a code.
    # match_adm_codes_to_names logic: if state is code, state_code = state, state_name = looked up name.
    # Here we are deriving from lat/lon. So we should set both if possible.
  }

  # Strategy 2: Assets with municipality but no coordinates - join via municipality
  assets_with_municipality <- assets_without_state |>
    dplyr::filter(is.na(.data$latitude) | is.na(.data$longitude)) |>
    dplyr::filter(!is.na(.data$municipality))

  if (nrow(assets_with_municipality) > 0 && !is.null(adm2_boundaries)) {
    message("  Assigning state via municipality for ", nrow(assets_with_municipality), " assets")

    # Prefer deterministic lookup via ADM codes when available.
    if (!is.null(adm_codes)) {
      adm2_lookup <- adm_codes |>
        dplyr::filter(.data$adm == "adm2") |>
        dplyr::transmute(
          municipality_lookup = tolower(trimws(stringi::stri_trans_general(as.character(.data$name), "Latin-ASCII"))),
          municipality_code_lookup = as.character(.data$code)
        ) |>
        dplyr::distinct()

      adm1_lookup_codes <- adm_codes |>
        dplyr::filter(.data$adm == "adm1") |>
        dplyr::transmute(
          state_code_lookup = as.character(.data$code),
          state_name_lookup = stringi::stri_trans_general(as.character(.data$name), "Latin-ASCII")
        ) |>
        dplyr::distinct()

      muni_to_state_lookup <- adm2_lookup |>
        dplyr::mutate(state_code_lookup = substr(.data$municipality_code_lookup, 1, 2)) |>
        dplyr::left_join(adm1_lookup_codes, by = "state_code_lookup") |>
        dplyr::select("municipality_lookup", "state_code_lookup", "state_name_lookup") |>
        dplyr::distinct()

      assets_with_municipality <- assets_with_municipality |>
        dplyr::mutate(municipality_lookup = tolower(trimws(as.character(.data$municipality)))) |>
        dplyr::left_join(muni_to_state_lookup, by = "municipality_lookup") |>
        dplyr::mutate(
          state = dplyr::coalesce(.data$state_name_lookup, .data$state),
          state_code = dplyr::coalesce(.data$state_code_lookup, .data$state_code)
        ) |>
        dplyr::select(-dplyr::any_of(c("municipality_lookup", "state_name_lookup", "state_code_lookup")))
    }

    remaining_for_spatial <- assets_with_municipality |>
      dplyr::filter(is.na(.data$state))

    # Normalize municipality names in boundaries
    municipalities_sf <- adm2_boundaries |>
      dplyr::mutate(
        municipality_name = stringi::stri_trans_general(as.character(.data$shapeName), "Latin-ASCII"),
        shapeID = as.character(.data$shapeID)
      )

    # Ensure both layers share CRS
    if (!sf::st_is_longlat(municipalities_sf)) {
      municipalities_sf <- sf::st_transform(municipalities_sf, 4326)
    }
    if (!sf::st_is_longlat(states_sf)) {
      states_sf <- sf::st_transform(states_sf, 4326)
    }

    muni_state_join <- sf::st_join(municipalities_sf, states_sf, join = sf::st_intersects)

    municipality_lookup <- muni_state_join |>
      sf::st_drop_geometry() |>
      dplyr::select(dplyr::any_of(c("municipality_name", "state_name", "state_code_lookup"))) |>
      dplyr::arrange(!is.na(.data$state_name), !is.na(.data$state_code_lookup)) |>
      dplyr::distinct(.data$municipality_name, .keep_all = TRUE)

    # Prepare join columns
    # We join by municipality name. If municipality column is code, we should have used municipality_name?
    # assets_with_municipality likely has 'municipality' column. 
    # If read_assets was used, 'municipality' is normalized name if it was a code.
    
    join_by <- "municipality_name"
    # If municipality_lookup doesn't have municipality_name (should have it from above mutate), checks needed?
    
    spatial_completed <- remaining_for_spatial |>
      dplyr::mutate(municipality_join = tolower(trimws(as.character(.data$municipality)))) |>
      dplyr::left_join(
        municipality_lookup |>
          dplyr::mutate(municipality_name_join = tolower(trimws(as.character(.data$municipality_name)))),
        by = c("municipality_join" = "municipality_name_join")
      ) |>
      dplyr::mutate(
        state = dplyr::coalesce(if ("state_name" %in% names(.data)) .data$state_name else NULL, .data$state),
        state_code = {
          has_lookup <- "state_code_lookup" %in% names(.data)
          has_code <- "state_code" %in% names(.data)
          if (has_lookup && has_code) {
            dplyr::coalesce(.data$state_code_lookup, .data$state_code)
          } else if (has_lookup) {
            .data$state_code_lookup
          } else if (has_code) {
            .data$state_code
          } else {
            NA_character_
          }
        }
      ) |>
      dplyr::select(-dplyr::any_of(c("state_name", "state_code_lookup", "adm1_name", "municipality_join", "municipality_name_join")))
    
    assets_with_municipality <- dplyr::bind_rows(
      assets_with_municipality |>
        dplyr::filter(!is.na(.data$state)),
      spatial_completed
    )
  }

  # Combine all assets back together
  result <- dplyr::bind_rows(
    assets_with_state_already,
    if (exists("assets_with_coords") && nrow(assets_with_coords) > 0) assets_with_coords else NULL,
    if (exists("assets_with_municipality") && nrow(assets_with_municipality) > 0) assets_with_municipality else NULL,
    # Assets that still don't have state (no coords, no municipality)
    assets_without_state |>
      dplyr::filter(
        (is.na(.data$latitude) | is.na(.data$longitude)) &
          is.na(.data$municipality)
      )
  )

  n_assigned <- sum(!is.na(result$state)) - sum(!is.na(assets_df$state))
  message("[assign_state_to_assets] Assigned state to ", n_assigned, " additional assets")

  return(result)
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

  # Call the main function with loaded boundaries
  assign_state_to_assets_with_boundaries(assets_df, adm1_boundaries, adm2_boundaries, adm_codes)
}

#' Load ADM codes from brazil_adm_codes.csv file
#'
#' @title Load ADM codes mapping file
#' @description Loads the brazil_adm_codes.csv file which maps ADM codes to names.
#'   The file should have columns: code, name, adm (adm1 or adm2), shapeID
#' @param base_dir Base directory containing areas/brazil_adm_codes.csv
#' @return Data frame with columns: code, name, adm, shapeID
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
#' @return Data frame with columns: code, name, adm, shapeID
#' @noRd
load_adm_codes_from_path <- function(file_path) {
  readr::read_csv(
    file_path,
    col_types = readr::cols(
      code = "c",
      name = "c",
      adm = "c",
      shapeID = "c"
    ),
    show_col_types = FALSE,
    locale = readr::locale(encoding = "UTF-8")
  ) |>
    tibble::as_tibble()
}

#' Match ADM codes to names in assets data frame
#'
#' @title Match ADM codes to names
#' @description Matches ADM codes in State and Municipality columns to their names.
#'   If State/Municipality columns contain numeric codes matching ADM codes, they are
#'   matched to names and both code and name columns are populated.
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
    dplyr::filter(.data$adm == "adm1") |>
    dplyr::select("code", "name") |>
    dplyr::distinct()
  
  adm2_lookup <- adm_codes |>
    dplyr::filter(.data$adm == "adm2") |>
    dplyr::select("code", "name") |>
    dplyr::distinct()
  
  # Match State codes (adm1)
  if ("state" %in% names(assets_df)) {
    # Check if state column contains codes (numeric strings matching adm1 codes)
    state_values <- assets_df$state
    is_code <- !is.na(state_values) & 
               nzchar(trimws(as.character(state_values))) &
               grepl("^\\d+$", trimws(as.character(state_values))) &
               trimws(as.character(state_values)) %in% adm1_lookup$code
    
    # Match codes to names
    assets_df <- assets_df |>
      dplyr::mutate(
        state_code = dplyr::if_else(
          is_code,
          trimws(as.character(.data$state)),
          .data$state_code
        ),
        state_name = dplyr::if_else(
          is_code,
          adm1_lookup$name[match(trimws(as.character(.data$state)), adm1_lookup$code)],
          .data$state_name
        ),
        # Update state column with normalized name if code was matched
        state = dplyr::if_else(
          is_code,
          stringi::stri_trans_general(
            adm1_lookup$name[match(trimws(as.character(.data$state)), adm1_lookup$code)],
            "Latin-ASCII"
          ),
          .data$state
        )
      )
  }
  
  # Match Municipality codes (adm2)
  if ("municipality" %in% names(assets_df)) {
    # Check if municipality column contains codes (numeric strings matching adm2 codes)
    municipality_values <- assets_df$municipality
    is_code <- !is.na(municipality_values) & 
               nzchar(trimws(as.character(municipality_values))) &
               grepl("^\\d+$", trimws(as.character(municipality_values))) &
               trimws(as.character(municipality_values)) %in% adm2_lookup$code
    
    # Match codes to names
    assets_df <- assets_df |>
      dplyr::mutate(
        municipality_code = dplyr::if_else(
          is_code,
          trimws(as.character(.data$municipality)),
          .data$municipality_code
        ),
        municipality_name = dplyr::if_else(
          is_code,
          adm2_lookup$name[match(trimws(as.character(.data$municipality)), adm2_lookup$code)],
          .data$municipality_name
        ),
        # Update municipality column with normalized name if code was matched
        municipality = dplyr::if_else(
          is_code,
          stringi::stri_trans_general(
            adm2_lookup$name[match(trimws(as.character(.data$municipality)), adm2_lookup$code)],
            "Latin-ASCII"
          ),
          .data$municipality
        )
      )
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
