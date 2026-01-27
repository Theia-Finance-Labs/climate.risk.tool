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

#' Read asset data from Excel file
#'
#' @title Read asset information from Excel file
#' @description Reads asset information from Excel file in the specified folder.
#'   The folder must directly contain asset_information.xlsx.
#'   Converts column names to snake_case and parses numeric columns correctly.
#' @param folder_path Character string specifying the folder containing asset_information.xlsx
#' @return tibble with asset information
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
  numeric_asset_cols <- c("share_of_economic_activity", "latitude", "longitude", "size_in_m2", "size_in_hectare")

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

#' Read damage and cost factors from CSV file
#'
#' @title Read damage and cost factors lookup table
#' @description Reads damage and cost factors from CSV file, parsing numeric columns
#'   and handling comma decimal separators correctly.
#' @param base_dir Character string specifying the base directory containing hazards/mappings/damage_and_cost_factors.csv
#' @return tibble with damage and cost factors
#' @examples
#' \dontrun{
#' base_dir <- system.file("tests_data", package = "climate.risk.tool")
#' factors <- read_damage_cost_factors(base_dir)
#' }
#' @export
read_damage_cost_factors <- function(base_dir) {
  message("[read_damage_cost_factors] Reading damage and cost factors from: ", base_dir)

  # Define file path - now inside hazards/mappings
  factors_path <- file.path(base_dir, "hazards", "mappings", "damage_and_cost_factors.csv")

  # Check if file exists
  if (!file.exists(factors_path)) {
    stop("Damage and cost factors file not found at: ", factors_path)
  }

  # Read the damage and cost factors CSV
  # The CSV uses comma as decimal separator and quotes around numbers
  # GWL column can contain both numeric values (e.g., "1.5") and text values (e.g., "present"),
  # so we must read it as character to preserve both
  factors_df <- readr::read_csv(
    factors_path,
    col_types = readr::cols(GWL = readr::col_character()),
    show_col_types = FALSE
  ) |>
    tibble::as_tibble() |>
    # Convert column names to snake_case for consistency
    dplyr::rename_with(to_snake_case)

  df_names <- names(factors_df)

  factors_df <- factors_df |>
    dplyr::mutate(
      # Clean up the numeric columns that have comma decimal separators and quotes (if they are characters)
      damage_factor = if (is.character(.data$damage_factor)) {
        as.numeric(gsub(",", ".", gsub('"', "", .data$damage_factor)))
      } else {
        as.numeric(.data$damage_factor)
      },
      cost_factor = if ("cost_factor" %in% df_names) {
        if (is.character(.data$cost_factor)) {
          suppressWarnings(as.numeric(gsub(",", ".", gsub('"', "", .data$cost_factor))))
        } else {
          as.numeric(.data$cost_factor)
        }
      } else {
        NA_real_
      }
    ) |>
    # Normalize state names (remove accents, convert to ASCII)
    dplyr::mutate(
      state = if ("state" %in% df_names) {
        dplyr::if_else(
          !is.na(.data$state) & .data$state != "-" & nzchar(as.character(.data$state)),
          stringi::stri_trans_general(as.character(.data$state), "Latin-ASCII"),
          .data$state
        )
      } else {
        NA_character_
      }
    )

  message("[read_damage_cost_factors] Loaded ", nrow(factors_df), " factor records")
  factors_df
}

#' Load mapping table from hazard config
#'
#' @title Load mapping table from config
#' @description Loads a mapping table specified in a hazard config file. This is a generalized
#'   function that replaces hardcoded mapping readers like read_cnae_labor_productivity_exposure
#'   and read_land_cover_legend. Mapping tables are defined in hazard YAML config files
#'   under the mappings section.
#' @param base_dir Character string specifying the base directory
#' @param hazard_configs Named list of hazard configs from load_hazard_configs()
#' @param hazard_type Character name of the hazard type (e.g., "Heat", "Fire")
#' @param mapping_key Character key of the mapping in the config (e.g., "cnae_exposure", "land_cover_legend")
#' @return tibble with mapping data
#' @examples
#' \dontrun{
#' base_dir <- system.file("tests_data", package = "climate.risk.tool")
#' hazards_dir <- file.path(base_dir, "hazards", "config")
#' hazard_configs <- load_hazard_configs(hazards_dir)
#' cnae_exposure <- load_mapping_from_config(base_dir, hazard_configs, "Heat", "cnae_exposure")
#' }
#' @export
load_mapping_from_config <- function(base_dir, hazard_configs, hazard_type, mapping_key) {
  if (is.null(hazard_configs) || length(hazard_configs) == 0) {
    stop("hazard_configs is required")
  }
  if (!hazard_type %in% names(hazard_configs)) {
    stop("Hazard type '", hazard_type, "' not found in hazard_configs")
  }
  
  hazard_config <- hazard_configs[[hazard_type]]
  if (is.null(hazard_config$mappings) || !mapping_key %in% names(hazard_config$mappings)) {
    stop("Mapping '", mapping_key, "' not found in hazard config for '", hazard_type, "'")
  }
  
  mapping <- hazard_config$mappings[[mapping_key]]
  mappings_dir <- file.path(base_dir, "hazards", "mappings")
  table_path <- file.path(mappings_dir, mapping$file)
  
  if (!file.exists(table_path)) {
    stop("Mapping table not found: ", table_path)
  }
  
  # Read the mapping table based on file extension
  ext <- tolower(tools::file_ext(table_path))
  if (ext == "csv") {
    mapping_df <- readr::read_csv(table_path, show_col_types = FALSE) |> tibble::as_tibble()
  } else if (ext %in% c("xlsx", "xls")) {
    mapping_df <- readxl::read_excel(table_path) |> tibble::as_tibble()
  } else {
    stop("Unsupported mapping table extension: ", ext)
  }
  
  # Apply snake_case conversion for consistency
  mapping_df <- mapping_df |>
    dplyr::rename_with(to_snake_case)
  
  message("[load_mapping_from_config] Loaded mapping '", mapping_key, "' for hazard '", hazard_type, "': ", nrow(mapping_df), " records")
  mapping_df
}

#' Read precomputed administrative hazard statistics from CSV file
#'
#' @title Read precomputed hazard statistics for provinces and municipalities
#' @description Reads precomputed hazard statistics from CSV file containing hazard data
#'   aggregated at ADM1 (province) and ADM2 (municipality) levels. Used to look up hazard
#'   values for assets without coordinates but with province or municipality information.
#'   The file uses indicator metadata (`indicator_file`, `indicator_variable`) which are mapped
#'   to `hazard_type` and `hazard_indicator` using the hazard config YAML files.
#' @param base_dir Character string specifying the base directory. The function looks for precomputed_adm_hazards.csv in base_dir/hazards/
#' @return tibble with precomputed hazard statistics including columns: region, adm_level,
#'   scenario_name, return_period, hazard_type, hazard_indicator, hazard_name,
#'   aggregation_method, hazard_value. adm_level is "ADM1" for provinces or "ADM2" for municipalities.
#' @examples
#' \dontrun{
#' base_dir <- system.file("tests_data", package = "climate.risk.tool")
#' precomputed <- read_precomputed_hazards(base_dir)
#' # Look up Amazonas province flood hazard
#' amazonas_flood <- precomputed |>
#'   dplyr::filter(region == "Amazonas", adm_level == "ADM1", hazard_type == "flood")
#' }
#' @export
read_precomputed_hazards <- function(base_dir) {
  message("[read_precomputed_hazards] Reading precomputed hazard statistics from: ", base_dir)

  # Define file path
  precomputed_path <- file.path(base_dir, "hazards", "precomputed_adm_hazards.csv")

  # Check if file exists
  if (!file.exists(precomputed_path)) {
    stop("Precomputed hazards file not found at: ", precomputed_path)
  }

  # Read the precomputed hazards CSV with optimized options
  # Providing col_types explicitly speeds up reading large files
  precomputed_df <- readr::read_csv(
    precomputed_path,
    col_types = readr::cols(
      region = "c",
      adm_level = "c",
      gwl = "c",
      return_period = "d",
      indicator_file = "c",
      indicator_variable = "c",
      ensemble = "c",
      season = "c",
      scenario_name = "c",
      .default = "d"
    ),
    show_col_types = FALSE,
    lazy = FALSE,
    progress = FALSE
  ) |>
    tibble::as_tibble() |>
    dplyr::rename_with(to_snake_case)

  message("  File read complete (", nrow(precomputed_df), " rows). Mapping indicators from config...")

  # Load hazard configs to map indicator metadata -> hazard_type + hazard_indicator
  hazards_dir <- file.path(base_dir, "hazards", "config")
  hazard_configs <- load_hazard_configs(hazards_dir)

  normalize_indicator_file <- function(x) {
    x <- gsub("/+$", "", as.character(x))
    tools::file_path_sans_ext(x)
  }

  indicator_registry <- lapply(names(hazard_configs), function(hazard_type) {
    hazard_config <- hazard_configs[[hazard_type]]
    lapply(names(hazard_config$indicators), function(indicator_key) {
      indicator <- hazard_config$indicators[[indicator_key]]
      tibble::tibble(
        hazard_type = hazard_type,
        hazard_indicator = indicator_key,
        indicator_file = indicator$file,
        indicator_variable = indicator$variable,
        variable = indicator$variable,
        agg = indicator$agg,
        categorical = indicator$categorical
      )
    }) |>
      dplyr::bind_rows()
  }) |>
    dplyr::bind_rows() |>
    dplyr::mutate(indicator_file_key = normalize_indicator_file(.data$indicator_file))

  precomputed_df <- precomputed_df |>
    dplyr::mutate(
      indicator_file_key = normalize_indicator_file(.data$indicator_file),
      scenario_name = dplyr::na_if(as.character(.data$scenario_name), "NA"),
      scenario_name = dplyr::na_if(.data$scenario_name, ""),
      scenario_name = dplyr::coalesce(.data$scenario_name, as.character(.data$gwl)),
      season = dplyr::na_if(as.character(.data$season), "NA"),
      ensemble = dplyr::na_if(as.character(.data$ensemble), "NA"),
      ensemble = dplyr::na_if(.data$ensemble, ""),
      ensemble = dplyr::coalesce(.data$ensemble, "mean")
    ) |>
    dplyr::left_join(
      indicator_registry |>
        dplyr::select(
          "indicator_file_key",
          "hazard_type",
          "hazard_indicator",
          "variable",
          "agg",
          "categorical"
        ),
      by = "indicator_file_key"
    )

  # Fallback mapping by indicator_variable if file matching failed
  is_unmapped <- is.na(precomputed_df$hazard_type)
  if (any(is_unmapped)) {
    fallback_registry <- indicator_registry |>
      dplyr::distinct(.data$indicator_variable, .data$hazard_type, .data$hazard_indicator, .data$variable, .data$agg, .data$categorical)

    fallback_df <- precomputed_df[is_unmapped, ] |>
      dplyr::left_join(
        fallback_registry,
        by = c("indicator_variable" = "indicator_variable"),
        suffix = c("", "_fallback")
      )

    precomputed_df$hazard_type[is_unmapped] <- fallback_df$hazard_type_fallback
    precomputed_df$hazard_indicator[is_unmapped] <- fallback_df$hazard_indicator_fallback
    precomputed_df$variable[is_unmapped] <- fallback_df$variable_fallback
    precomputed_df$agg[is_unmapped] <- fallback_df$agg_fallback
    precomputed_df$categorical[is_unmapped] <- fallback_df$categorical_fallback
  }

  if (any(is.na(precomputed_df$hazard_indicator))) {
    missing_rows <- precomputed_df |>
      dplyr::filter(is.na(.data$hazard_indicator)) |>
      dplyr::distinct(.data$indicator_file, .data$indicator_variable)
    stop(
      "Could not resolve hazard_indicator for: ",
      paste(
        paste0(missing_rows$indicator_file, " (", missing_rows$indicator_variable, ")"),
        collapse = ", "
      )
    )
  }

  if (any(is.na(precomputed_df$hazard_type))) {
    missing_rows <- precomputed_df |>
      dplyr::filter(is.na(.data$hazard_type)) |>
      dplyr::distinct(.data$indicator_file, .data$indicator_variable)
    stop(
      "Could not map indicator metadata to hazard config. Missing mappings for: ",
      paste(
        paste0(missing_rows$indicator_file, " (", missing_rows$indicator_variable, ")"),
        collapse = ", "
      )
    )
  }

  if (any(is.na(precomputed_df$scenario_name))) {
    stop("scenario_name is missing for some rows; ensure scenario_name or gwl is provided")
  }

  message("  Deduplicating...")

  # Deduplicate variant per hazard scenario
  group_cols <- c("region", "adm_level", "scenario_name", "return_period", "hazard_type", "hazard_indicator")
  has_season <- "season" %in% names(precomputed_df)
  if (has_season) group_cols <- c(group_cols, "season")

  # Use unique-mapping for ensemble priority
  unique_ensembles <- unique(precomputed_df$ensemble)
  ensemble_priority_map <- dplyr::case_when(
    tolower(unique_ensembles) == "mean" ~ 1L,
    tolower(unique_ensembles) == "median" ~ 2L,
    is.na(unique_ensembles) ~ 3L,
    TRUE ~ 4L
  )
  names(ensemble_priority_map) <- unique_ensembles
  
  precomputed_df <- precomputed_df |>
    dplyr::mutate(ensemble_priority = ensemble_priority_map[.data$ensemble]) |>
    dplyr::arrange(.data$ensemble_priority) |>
    dplyr::distinct(dplyr::across(dplyr::any_of(group_cols)), .keep_all = TRUE) |>
    dplyr::select(-"ensemble_priority")

  # 4. Optimized hazard_name construction using unique combinations
  message("  Building hazard names...")
  
  # Ensure variable is set (prefer config variable, fallback to indicator_variable or hazard_indicator)
  if (!"variable" %in% names(precomputed_df)) {
    precomputed_df$variable <- NA_character_
  }
  precomputed_df <- precomputed_df |>
    dplyr::mutate(
      variable = dplyr::coalesce(.data$variable, .data$indicator_variable, .data$hazard_indicator)
    )

  distinct_cols <- c(
    "hazard_type",
    "hazard_indicator",
    "variable",
    "scenario_name",
    "gwl",
    "return_period",
    "indicator_file",
    "indicator_variable",
    "ensemble"
  )
  if (has_season) distinct_cols <- c(distinct_cols, "season")
  
  combos <- precomputed_df |>
    dplyr::distinct(dplyr::across(dplyr::any_of(distinct_cols)))
  
  get_index_dims <- function(hazard_type, hazard_indicator) {
    if (is.null(hazard_configs) || !hazard_type %in% names(hazard_configs)) {
      return(character())
    }
    indicator_cfg <- hazard_configs[[hazard_type]]$indicators[[hazard_indicator]]
    if (is.null(indicator_cfg) || is.null(indicator_cfg$index)) {
      return(character())
    }
    index_dims <- as.character(indicator_cfg$index)
    index_dims <- index_dims[!is.na(index_dims)]
    index_dims
  }

  build_index_values <- function(row) {
    index_dims <- get_index_dims(row$hazard_type, row$hazard_indicator)
    has_index <- function(name) length(index_dims) > 0 && name %in% index_dims
    list(
      return_period = if (has_index("return_period") && !is.na(row$return_period)) row$return_period else NA_real_,
      gwl = if (has_index("gwl") && !is.na(row$gwl)) row$gwl else NA_character_,
      scenario_name = if (has_index("scenario_name") && !is.na(row$scenario_name)) row$scenario_name else NA_character_,
      season = if (has_season && has_index("season") && !is.na(row$season)) row$season else NA_character_
    )
  }

  # Construct structured hazard_name for each combination
  # Use fixed ensemble from config if specified, otherwise use actual ensemble from data
  combos$indicator_key <- purrr::pmap_chr(combos, function(...) {
    row <- list(...)
    
    index_values <- build_index_values(row)
    
    # Get fixed ensemble from config if it exists
    effective_ensemble <- row$ensemble
    if (!is.null(hazard_configs) && row$hazard_type %in% names(hazard_configs)) {
      indicator_cfg <- hazard_configs[[row$hazard_type]]$indicators[[row$hazard_indicator]]
      if (!is.null(indicator_cfg$fixed) && !is.null(indicator_cfg$fixed$ensemble)) {
        effective_ensemble <- indicator_cfg$fixed$ensemble
      }
    }
    
    build_indicator_key(
      indicator_file = row$indicator_file,
      indicator_variable = row$indicator_variable,
      index_values = index_values,
      ensemble = effective_ensemble
    )
  })

  combos$hazard_name <- purrr::pmap_chr(combos, function(...) {
    row <- list(...)
    
    index_values <- build_index_values(row)
    
    # Get fixed ensemble from config if it exists
    effective_ensemble <- row$ensemble
    if (!is.null(hazard_configs) && row$hazard_type %in% names(hazard_configs)) {
      indicator_cfg <- hazard_configs[[row$hazard_type]]$indicators[[row$hazard_indicator]]
      if (!is.null(indicator_cfg$fixed) && !is.null(indicator_cfg$fixed$ensemble)) {
        effective_ensemble <- indicator_cfg$fixed$ensemble
      }
    }
    
    build_hazard_name(
      hazard_type = row$hazard_type,
      hazard_indicator = row$hazard_indicator,
      index_values = index_values,
      ensemble = effective_ensemble
    )
  })
  
  # Join names back to full data frame
  join_cols <- intersect(names(combos), names(precomputed_df))
  precomputed_df <- precomputed_df |>
    dplyr::left_join(combos |> dplyr::select(dplyr::all_of(c(join_cols, "hazard_name", "indicator_key"))), by = join_cols) |>
    # hazard_key is the same as indicator_key (implementation detail)
    dplyr::mutate(hazard_key = .data$indicator_key)
  
  precomputed_df <- precomputed_df |>
    dplyr::select(-"indicator_file_key")

  # Define ensemble columns to pivot
  summary_cols <- intersect(
    c("mean", "median", "p10", "p90", "min", "max", "mode"),
    names(precomputed_df)
  )
  
  if (length(summary_cols) == 0) {
    stop("No valid aggregation columns found in precomputed hazards file")
  }

  message("  Pivoting...")

  precomputed_final <- precomputed_df |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(summary_cols),
      names_to = "aggregation_method",
      values_to = "hazard_value",
      values_drop_na = TRUE
    ) |>
    dplyr::mutate(
      ensemble = as.character(.data$ensemble),
      scenario_name = as.character(.data$scenario_name),
      # Add default metadata columns to match inventory structure
      agg = NA_character_,
      categorical = FALSE
    )

  if (has_season) {
    precomputed_final$season <- as.character(precomputed_final$season)
  }

  message("[read_precomputed_hazards] Loaded ", nrow(precomputed_final), " final records")
  precomputed_final
}

#' Read TIF hazard mapping file
#'
#' @title Read TIF hazard mapping file
#' @description Reads a metadata CSV file that maps TIF filenames
#'   to hazard metadata (type, indicator, scenario_name, return period).
#' @param mapping_path Character path to a metadata CSV file
#' @return Tibble with mapping information
#' @noRd
read_hazards_mapping <- function(mapping_path) {
  if (!file.exists(mapping_path)) {
    stop("Mapping file not found: ", mapping_path)
  }

  mapping <- utils::read.csv(mapping_path, stringsAsFactors = FALSE, strip.white = TRUE)
  mapping <- tibble::as_tibble(mapping)

  # Validate required columns
  required_cols <- c(
    "hazard_file", "hazard_indicator",
    "scenario_name", "return_period"
  )
  missing_cols <- setdiff(required_cols, names(mapping))
  if (length(missing_cols) > 0) {
    stop("Mapping file missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  # Normalize hazard_type if present
  if ("hazard_type" %in% names(mapping)) {
    mapping <- mapping |>
      dplyr::mutate(hazard_type = as.character(.data$hazard_type))
  }

  return(mapping)
}


#' Load region name mapping dictionary
#'
#' @title Load mapping from normalized names to original names
#' @description Creates a dictionary mapping normalized (ASCII) region names to their original
#'   names with special characters. This is used to display original names in the frontend
#'   while keeping normalized names for internal processing.
#' @param base_dir Base directory containing areas subdirectory
#' @param adm1_sf Optional sf object for ADM1 boundaries (if already loaded)
#' @param adm2_sf Optional sf object for ADM2 boundaries (if already loaded)
#' @return Named list with two elements:
#'   - province: Named character vector mapping normalized province names to original names
#'   - municipality: Named character vector mapping normalized municipality names to original names
#' @examples
#' \dontrun{
#' base_dir <- system.file("tests_data", package = "climate.risk.tool")
#' name_mapping <- load_region_name_mapping(base_dir)
#' # Access original name: name_mapping$province["Sao Paulo"] returns "São Paulo"
#' }
#' @export
load_region_name_mapping <- function(base_dir, adm1_sf = NULL, adm2_sf = NULL) {
  # Initialize result list
  mapping <- list(province = character(0), municipality = character(0))

  # Load province (ADM1) names
  if (!is.null(adm1_sf)) {
    provinces_sf <- adm1_sf
  } else {
    province_path <- file.path(base_dir, "areas", "province", "geoBoundaries-BRA-ADM1_simplified.geojson")
    provinces_sf <- if (file.exists(province_path)) sf::st_read(province_path, quiet = TRUE) else NULL
  }

  if (!is.null(provinces_sf) && "shapeName" %in% names(provinces_sf)) {
    # Get original names
    original_names <- as.character(provinces_sf$shapeName)

    # Get normalized names (same way as used throughout the codebase)
    normalized_names <- stringi::stri_trans_general(original_names, "Latin-ASCII")

    # Create mapping: normalized -> original
    mapping$province <- original_names
    names(mapping$province) <- normalized_names
  }

  # Load municipality (ADM2) names
  if (!is.null(adm2_sf)) {
    municipalities_sf <- adm2_sf
  } else {
    municipality_path <- file.path(base_dir, "areas", "municipality", "geoBoundaries-BRA-ADM2_simplified.geojson")
    municipalities_sf <- if (file.exists(municipality_path)) sf::st_read(municipality_path, quiet = TRUE) else NULL
  }

  if (!is.null(municipalities_sf) && "shapeName" %in% names(municipalities_sf)) {
    # Get original names
    original_names <- as.character(municipalities_sf$shapeName)

    # Get normalized names (same way as used throughout the codebase)
    normalized_names <- stringi::stri_trans_general(original_names, "Latin-ASCII")

    # Create mapping: normalized -> original
    mapping$municipality <- original_names
    names(mapping$municipality) <- normalized_names
  }

  message(
    "[load_region_name_mapping] Loaded ", length(mapping$province), " province names and ",
    length(mapping$municipality), " municipality names"
  )

  return(mapping)
}
