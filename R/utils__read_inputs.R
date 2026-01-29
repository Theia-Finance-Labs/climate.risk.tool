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

resolve_geo_mapping_base_dir <- function(folder_path) {
  if (is.null(folder_path) || !nzchar(as.character(folder_path))) {
    return(NULL)
  }
  candidates <- unique(c(folder_path, dirname(folder_path)))
  for (candidate in candidates) {
    mapping_path <- file.path(candidate, "areas", "geo_code_mapping.csv")
    if (file.exists(mapping_path)) {
      return(candidate)
    }
  }
  return(NULL)
}

#' Load IBGE code mapping for states and municipalities
#'
#' @title Load IBGE code mapping
#' @description Loads a mapping table linking IBGE codes to normalized names for ADM1/ADM2.
#'   The mapping file must be located at {base_dir}/areas/geo_code_mapping.csv.
#' @param base_dir Base directory containing areas/geo_code_mapping.csv
#' @return tibble with columns:
#'   - adm_level: ADM1 or ADM2
#'   - code: IBGE code as string (2 digits for ADM1, 7 digits for ADM2)
#'   - state_code: ADM1 code (2 digits) for both ADM1 and ADM2 rows
#'   - name: original name from mapping file
#'   - name_normalized: ASCII-normalized name used internally
#'   - state_name: ADM1 name resolved from state_code
#'   - state_name_normalized: ASCII-normalized ADM1 name
#' @examples
#' \dontrun{
#' base_dir <- system.file("tests_data", package = "climate.risk.tool")
#' mapping <- load_geo_code_mapping(base_dir)
#' }
#' @export
load_geo_code_mapping <- function(base_dir) {
  mapping_path <- file.path(base_dir, "areas", "geo_code_mapping.csv")
  if (!file.exists(mapping_path)) {
    message("[load_geo_code_mapping] Mapping not found at: ", mapping_path)
    return(tibble::tibble(
      adm_level = character(),
      code = character(),
      state_code = character(),
      name = character(),
      name_normalized = character(),
      state_name = character(),
      state_name_normalized = character()
    ))
  }

  mapping <- readr::read_csv(
    mapping_path,
    col_types = readr::cols(.default = "c"),
    show_col_types = FALSE
  ) |>
    tibble::as_tibble() |>
    dplyr::rename_with(to_snake_case)

  mapping <- mapping |>
    dplyr::mutate(
      adm_level = toupper(.data$adm_level),
      code = dplyr::case_when(
        .data$adm_level == "ADM1" ~ coerce_geo_code(.data$code, width = 2),
        .data$adm_level == "ADM2" ~ coerce_geo_code(.data$code, width = 7),
        TRUE ~ coerce_geo_code(.data$code)
      ),
      state_code = coerce_geo_code(.data$state_code, width = 2),
      name = as.character(.data$name),
      name_normalized = normalize_geo_name(.data$name)
    )

  adm1_lookup <- mapping |>
    dplyr::filter(.data$adm_level == "ADM1") |>
    dplyr::select(
      state_code = .data$code,
      state_name = .data$name,
      state_name_normalized = .data$name_normalized
    )

  mapping <- mapping |>
    dplyr::left_join(adm1_lookup, by = "state_code")

  mapping
}

attach_geo_codes <- function(df, geo_mapping, state_col = "state", municipality_col = "municipality") {
  if (is.null(geo_mapping) || nrow(geo_mapping) == 0) {
    return(df)
  }

  adm1 <- geo_mapping |>
    dplyr::filter(.data$adm_level == "ADM1")
  adm2 <- geo_mapping |>
    dplyr::filter(.data$adm_level == "ADM2")

  state_code_lookup <- setNames(adm1$code, adm1$name_normalized)
  state_name_lookup <- setNames(adm1$name_normalized, adm1$code)
  municipality_code_lookup <- setNames(adm2$code, adm2$name_normalized)
  municipality_name_lookup <- setNames(adm2$name_normalized, adm2$code)
  municipality_state_lookup <- setNames(adm2$state_name_normalized, adm2$code)

  if (state_col %in% names(df)) {
    state_values <- as.character(df[[state_col]])
    state_code_candidate <- coerce_geo_code(state_values, width = 2)
    state_is_code <- !is.na(state_code_candidate) & state_code_candidate %in% names(state_name_lookup)

    df[[state_col]] <- dplyr::if_else(
      state_is_code,
      state_name_lookup[state_code_candidate],
      state_values
    )

    state_name_norm <- normalize_geo_name(df[[state_col]])
    df$state_code <- dplyr::coalesce(
      dplyr::if_else(state_is_code, state_code_candidate, NA_character_),
      state_code_lookup[state_name_norm]
    )
  }

  if (municipality_col %in% names(df)) {
    municipality_values <- as.character(df[[municipality_col]])
    municipality_code_candidate <- coerce_geo_code(municipality_values, width = 7)
    municipality_is_code <- !is.na(municipality_code_candidate) & municipality_code_candidate %in% names(municipality_name_lookup)

    df[[municipality_col]] <- dplyr::if_else(
      municipality_is_code,
      municipality_name_lookup[municipality_code_candidate],
      municipality_values
    )

    municipality_name_norm <- normalize_geo_name(df[[municipality_col]])
    df$municipality_code <- dplyr::coalesce(
      dplyr::if_else(municipality_is_code, municipality_code_candidate, NA_character_),
      municipality_code_lookup[municipality_name_norm]
    )

    if (state_col %in% names(df)) {
      state_from_muni <- municipality_state_lookup[municipality_code_candidate]
      df[[state_col]] <- dplyr::coalesce(df[[state_col]], state_from_muni)
    }
  }

  df
}

#' Read asset data from Excel file
#'
#' @title Read asset information from Excel file
#' @description Reads asset information from Excel file in the specified folder.
#'   The folder must directly contain asset_information.xlsx.
#'   Converts column names to snake_case and parses numeric columns correctly.
#'   Municipality and state columns accept either names or IBGE codes.
#' @param folder_path Character string specifying the folder containing asset_information.xlsx
#' @return tibble with asset information (includes state_code and municipality_code when available)
#' @examples
#' \dontrun{
#' # Folder path containing asset_information.xlsx
#' assets <- read_assets("path/to/folder")
#' }
#' @export
read_assets <- function(folder_path) {
  message("[read_assets] Reading asset data from: ", folder_path)

  # File must be directly in the specified folder
  assets_path <- file.path(folder_path, "asset_information.xlsx")

  # Check if file exists
  if (!file.exists(assets_path)) {
    stop("Asset information file not found at: ", assets_path)
  }

  # Read assets data
  assets_raw <- readxl::read_excel(assets_path) |>
    tibble::as_tibble() |>
    dplyr::rename_with(to_snake_case)

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

  # Map IBGE codes to names when provided in inputs
  mapping_base_dir <- resolve_geo_mapping_base_dir(folder_path)
  if (!is.null(mapping_base_dir)) {
    geo_mapping <- load_geo_code_mapping(mapping_base_dir)
    assets_raw <- attach_geo_codes(assets_raw, geo_mapping)
  }

  if (!"state_code" %in% names(assets_raw)) {
    assets_raw$state_code <- NA_character_
  }
  if (!"municipality_code" %in% names(assets_raw)) {
    assets_raw$municipality_code <- NA_character_
  }

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
#' @return Data frame with state assigned to all assets
#' @examples
#' \dontrun{
#' adm1 <- sf::st_read("path/to/ADM1.geojson")
#' assets_with_states <- assign_state_to_assets_with_boundaries(assets, adm1)
#' }
#' @export
assign_state_to_assets_with_boundaries <- function(assets_df, adm1_boundaries, adm2_boundaries = NULL) {
  # Ensure state column is character (not logical if all NA)
  if ("state" %in% names(assets_df)) {
    assets_df$state <- as.character(assets_df$state)
  }

  # Normalize state names in boundaries
  states_sf <- adm1_boundaries |>
    dplyr::mutate(
      state_name = stringi::stri_trans_general(as.character(.data$shapeName), "Latin-ASCII")
    )

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
    coords_matrix <- sf::st_coordinates(assets_coords_joined)
    assets_with_coords <- assets_with_coords |>
      dplyr::mutate(
        state = assets_coords_joined$state_name
      )
  }

  # Strategy 2: Assets with municipality but no coordinates - join via municipality
  assets_with_municipality <- assets_without_state |>
    dplyr::filter(is.na(.data$latitude) | is.na(.data$longitude)) |>
    dplyr::filter(!is.na(.data$municipality))

  if (nrow(assets_with_municipality) > 0 && !is.null(adm2_boundaries)) {
    message("  Assigning state via municipality for ", nrow(assets_with_municipality), " assets")

    # Normalize municipality names in boundaries
    municipalities_sf <- adm2_boundaries |>
      dplyr::mutate(
        municipality_name = stringi::stri_trans_general(as.character(.data$shapeName), "Latin-ASCII")
      )

    # Ensure both layers share CRS
    if (!sf::st_is_longlat(municipalities_sf)) {
      municipalities_sf <- sf::st_transform(municipalities_sf, 4326)
    }
    if (!sf::st_is_longlat(states_sf)) {
      states_sf <- sf::st_transform(states_sf, 4326)
    }

    muni_points <- sf::st_point_on_surface(municipalities_sf)
    muni_points_joined <- sf::st_join(muni_points, states_sf, join = sf::st_within)

    municipality_lookup <- muni_points_joined |>
      sf::st_drop_geometry() |>
      dplyr::select("municipality_name", "state_name")

    assets_with_municipality <- assets_with_municipality |>
      dplyr::left_join(
        municipality_lookup,
        by = c("municipality" = "municipality_name")
      ) |>
      dplyr::mutate(
        state = dplyr::coalesce(.data$state_name, .data$state)
      ) |>
      dplyr::select(-dplyr::any_of(c("state_name", "adm1_name")))
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

  # Call the main function with loaded boundaries
  assign_state_to_assets_with_boundaries(assets_df, adm1_boundaries, adm2_boundaries)
}

#' Read company data from Excel file
#'
#' @title Read company information from Excel file
#' @description Reads company information from an Excel file,
#'   converting column names to snake_case and parsing numeric columns correctly.
#'   Can accept either a direct file path or a folder path containing company_information.xlsx.
#' @param file_path Character string specifying either the path to the company Excel file directly,
#'   or a folder path containing company_information.xlsx
#' @return tibble with company information
#' @examples
#' \dontrun{
#' # Direct file path
#' companies <- read_companies("path/to/company_information.xlsx")
#' # Or folder path
#' companies <- read_companies("path/to/folder")
#' }
#' @export
read_companies <- function(file_path) {
  message("[read_companies] Reading company data from: ", file_path)

  # If file_path is a directory, look for company_information.xlsx in it
  if (dir.exists(file_path)) {
    file_path <- file.path(file_path, "company_information.xlsx")
  }

  # Check if file exists
  if (!file.exists(file_path)) {
    stop("Company file not found at: ", file_path)
  }

  # Read companies data
  companies_raw <- readxl::read_excel(file_path) |>
    tibble::as_tibble() |>
    dplyr::rename_with(to_snake_case)

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
