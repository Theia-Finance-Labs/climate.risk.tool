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
#' @param base_dir Character string specifying the base directory containing damage_and_cost_factors.csv
#' @return tibble with damage and cost factors
#' @examples
#' \dontrun{
#' base_dir <- system.file("tests_data", package = "climate.risk.tool")
#' factors <- read_damage_cost_factors(base_dir)
#' }
#' @export
read_damage_cost_factors <- function(base_dir) {
  message("[read_damage_cost_factors] Reading damage and cost factors from: ", base_dir)

  # Define file path
  factors_path <- file.path(base_dir, "damage_and_cost_factors.csv")

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
    dplyr::mutate(
      # Clean up the numeric columns that have comma decimal separators and quotes
      damage_factor = as.numeric(gsub(",", ".", gsub('"', "", .data$damage_factor))),
      cost_factor = suppressWarnings(as.numeric(gsub(",", ".", gsub('"', "", .data$cost_factor)))),
      # Ensure hazard_intensity is numeric
      hazard_intensity = as.numeric(.data$hazard_intensity)
    ) |>
    # Convert column names to snake_case for consistency
    dplyr::rename_with(to_snake_case) |>
    # Normalize state names (remove accents, convert to ASCII)
    dplyr::mutate(
      state = dplyr::if_else(
        !is.na(.data$state) & .data$state != "-" & nzchar(as.character(.data$state)),
        stringi::stri_trans_general(as.character(.data$state), "Latin-ASCII"),
        .data$state
      )
    )

  message("[read_damage_cost_factors] Loaded ", nrow(factors_df), " factor records")
  factors_df
}

#' Read CNAE Labor Productivity Exposure data from Excel file
#'
#' @title Read CNAE Labor Productivity Exposure lookup table
#' @description Reads CNAE sector codes and their labor productivity exposure classification
#'   from Excel file. Used to determine metric (high/median/low) for Heat hazard damage factors.
#' @param base_dir Character string specifying the base directory containing cnae_labor_productivity_exposure.xlsx
#' @return tibble with columns: cnae (numeric), description, lp_exposure (character: "high", "median", "low")
#' @examples
#' \dontrun{
#' base_dir <- system.file("tests_data", package = "climate.risk.tool")
#' cnae_exposure <- read_cnae_labor_productivity_exposure(base_dir)
#' }
#' @export
read_cnae_labor_productivity_exposure <- function(base_dir) {
  message("[read_cnae_labor_productivity_exposure] Reading CNAE exposure data from: ", base_dir)

  # Define file path
  cnae_path <- file.path(base_dir, "cnae_labor_productivity_exposure.xlsx")

  # Check if file exists
  if (!file.exists(cnae_path)) {
    stop("CNAE labor productivity exposure file not found at: ", cnae_path)
  }

  # Read CNAE data
  cnae_raw <- readxl::read_excel(cnae_path) |>
    tibble::as_tibble() |>
    dplyr::rename_with(to_snake_case)

  # Normalize lp_exposure values to lowercase
  cnae_df <- cnae_raw |>
    dplyr::mutate(
      cnae = as.numeric(.data$cnae)
    ) |>
    dplyr::select("cnae", "description", "lp_exposure")

  message("[read_cnae_labor_productivity_exposure] Loaded ", nrow(cnae_df), " CNAE records")
  cnae_df
}

#' Read precomputed administrative hazard statistics from CSV file
#'
#' @title Read precomputed hazard statistics for provinces and municipalities
#' @description Reads precomputed hazard statistics from CSV file containing hazard data
#'   aggregated at ADM1 (province) and ADM2 (municipality) levels. Used to look up hazard
#'   values for assets without coordinates but with province or municipality information.
#' @param base_dir Character string specifying the base directory containing precomputed_adm_hazards.csv
#' @return tibble with precomputed hazard statistics including columns: region, adm_level,
#'   scenario_name, hazard_return_period, hazard_type, min, max, mean, median,
#'   p2_5, p5, p95, p97_5. adm_level is "ADM1" for provinces or "ADM2" for municipalities.
#'   Note: scenario_code may be present in the CSV but is not used (scenario_name is used instead).
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
  precomputed_path <- file.path(base_dir, "precomputed_adm_hazards.csv")

  # Check if file exists
  if (!file.exists(precomputed_path)) {
    stop("Precomputed hazards file not found at: ", precomputed_path)
  }

  # Read the precomputed hazards CSV
  precomputed_df <- readr::read_csv(precomputed_path, show_col_types = FALSE) |>
    tibble::as_tibble()

  # # Ensure numeric columns are numeric
  # numeric_cols <- c(
  #   "hazard_return_period", "min", "max", "mean", "median",
  #   "p2_5", "p5", "p95", "p97_5", "n_obs", "max_x", "max_y"
  # )

  # precomputed_df <- precomputed_df |>
  #   dplyr::mutate(
  #     dplyr::across(
  #       dplyr::any_of(numeric_cols),
  #       ~ as.numeric(.)
  #     )
  #   )

  # Validate adm_level values
  valid_adm_levels <- c("ADM1", "ADM2")
  invalid_levels <- setdiff(unique(precomputed_df$adm_level), valid_adm_levels)
  if (length(invalid_levels) > 0) {
    warning("Found unexpected adm_level values: ", paste(invalid_levels, collapse = ", "))
  }

  message("[read_precomputed_hazards] Loaded ", nrow(precomputed_df), " precomputed hazard records")
  message("  ADM1 (province) records: ", sum(precomputed_df$adm_level == "ADM1"))
  message("  ADM2 (municipality) records: ", sum(precomputed_df$adm_level == "ADM2"))

  # Transform data: construct proper hazard_name and create ensemble-specific rows
  # Unified naming WITHOUT ensemble suffix (base event format)

  # Define ensemble columns to melt
  summary_cols <- c("mean", "median", "p2_5", "p5", "p95", "p97_5")

  transformed_list <- list()

  for (summ_col in summary_cols) {
    # Create rows for this ensemble
    # Check if season column exists for drought hazards
    has_season <- "season" %in% names(precomputed_df)

    if (has_season) {
      ensemble_data <- precomputed_df |>
        dplyr::mutate(
          # Unified hazard_name - include season for drought (like NC files do)
          hazard_name = dplyr::if_else(
            .data$hazard_type == "Drought" & !is.na(.data$season),
            paste0(
              .data$hazard_type, "__", .data$hazard_indicator,
              "__GWL=", .data$scenario_name,
              "__RP=", .data$hazard_return_period,
              "__season=", .data$season,
              ifelse(is.na(.data$ensemble), "", paste0("__ensemble=", .data$ensemble))
            ),
            paste0(
              .data$hazard_type, "__", .data$hazard_indicator,
              "__GWL=", .data$scenario_name,
              "__RP=", .data$hazard_return_period,
              ifelse(is.na(.data$ensemble), "", paste0("__ensemble=", .data$ensemble))
            )
          ),
          aggregation_method = summ_col,
          hazard_value = .data[[summ_col]]
        )
    } else {
      ensemble_data <- precomputed_df |>
        dplyr::mutate(
          # Unified hazard_name WITHOUT season
          hazard_name = paste0(
            .data$hazard_type, "__", .data$hazard_indicator,
            "__GWL=", .data$scenario_name,
            "__RP=", .data$hazard_return_period,
            ifelse(is.na(.data$ensemble), "", paste0("__ensemble=", .data$ensemble))
          ),
          aggregation_method = summ_col,
          hazard_value = .data[[summ_col]]
        )
    }

    transformed_list[[summ_col]] <- ensemble_data
  }

  # Combine all ensemble variants
  precomputed_final <- dplyr::bind_rows(transformed_list)

  # Preserve season column if it exists (for drought hazards)
  if ("season" %in% names(precomputed_final)) {
    precomputed_final <- precomputed_final |>
      dplyr::mutate(
        # region = stringi::stri_trans_general(as.character(.data$region), "Latin-ASCII"),
        scenario_name = as.character(.data$scenario_name),
        season = as.character(.data$season)
      )
    season_count <- sum(!is.na(precomputed_final$season))
    message("  Season column preserved with ", season_count, " non-NA values")
  } else {
    precomputed_final <- precomputed_final |>
      dplyr::mutate(
        # region = stringi::stri_trans_general(as.character(.data$region), "Latin-ASCII"),
        scenario_name = as.character(.data$scenario_name)
      )
  }

  message("  Transformed to ", nrow(precomputed_final), " records with hazard_name and ensemble columns")

  precomputed_final
}

#' Read hazards name mapping from CSV (INTERNAL)
#'
#' @description Internal function used by load_hazards_and_inventory().
#'   Reads the hazards name mapping CSV file and validates that it
#'   contains all required columns including hazard_indicator.
#' @param mapping_path Character path to the CSV mapping file
#' @return Tibble with columns: hazard_file, hazard_type, hazard_indicator,
#'   scenario_name, hazard_return_period (scenario_code may be present but is not used)
#' @noRd
read_hazards_mapping <- function(mapping_path) {
  if (!file.exists(mapping_path)) {
    stop("Mapping file not found: ", mapping_path)
  }

  mapping <- utils::read.csv(mapping_path, stringsAsFactors = FALSE, strip.white = TRUE)
  mapping <- tibble::as_tibble(mapping)

  # Validate required columns
  required_cols <- c(
    "hazard_file", "hazard_type", "hazard_indicator",
    "scenario_name", "hazard_return_period"
  )
  missing_cols <- setdiff(required_cols, names(mapping))
  if (length(missing_cols) > 0) {
    stop("Mapping file missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  return(mapping)
}

#' Read Land Cover Legend and Risk Metrics
#'
#' @title Read Land Cover Legend
#' @description Reads the land cover legend Excel file that maps land cover codes
#'   to risk metrics. Used for Fire hazard to translate land cover extraction
#'   results into fire risk percentages.
#' @param base_dir Character. Base directory path containing land_cover_legend_and_index.xlsx
#' @return Tibble with columns: land_cover_code (numeric), land_cover_class (character),
#'   land_cover_category (numeric), land_cover_risk (numeric between 0 and 1)
#' @examples
#' \dontrun{
#' legend <- read_land_cover_legend("workspace/demo_inputs")
#' # Returns tibble with columns: land_cover_code, land_cover_class,
#' # land_cover_category, land_cover_risk
#' }
#' @export
read_land_cover_legend <- function(base_dir) {
  file_path <- file.path(base_dir, "land_cover_legend_and_index.xlsx")

  if (!file.exists(file_path)) {
    stop("Land cover legend file not found: ", file_path)
  }

  message("[read_land_cover_legend] Reading land cover legend from: ", file_path)

  # Read Excel file
  legend_df <- readxl::read_excel(file_path)

  # Validate required columns
  required_cols <- c("Code", "Class", "Category", "Risk")
  missing_cols <- setdiff(required_cols, names(legend_df))
  if (length(missing_cols) > 0) {
    stop("Land cover legend file missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  # Rename and clean
  legend_clean <- legend_df |>
    dplyr::select(
      land_cover_code = "Code",
      land_cover_class = "Class",
      land_cover_category = "Category",
      land_cover_risk = "Risk"
    ) |>
    dplyr::mutate(
      land_cover_code = as.numeric(.data$land_cover_code),
      land_cover_risk = as.numeric(.data$land_cover_risk)
    )

  message("[read_land_cover_legend] Loaded ", nrow(legend_clean), " land cover categories")

  return(legend_clean)
}

#' Load region name mapping dictionary
#'
#' @title Load mapping from normalized names to original names
#' @description Creates a dictionary mapping normalized (ASCII) region names to their original
#'   names with special characters. This is used to display original names in the frontend
#'   while keeping normalized names for internal processing.
#' @param base_dir Base directory containing areas subdirectory
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
load_region_name_mapping <- function(base_dir) {
  # Initialize result list
  mapping <- list(province = character(0), municipality = character(0))

  # Load province (ADM1) names
  province_path <- file.path(base_dir, "areas", "province", "geoBoundaries-BRA-ADM1_simplified.geojson")
  if (file.exists(province_path)) {
    provinces_sf <- sf::st_read(province_path, quiet = TRUE)

    if ("shapeName" %in% names(provinces_sf)) {
      # Get original names
      original_names <- as.character(provinces_sf$shapeName)

      # Get normalized names (same way as used throughout the codebase)
      normalized_names <- stringi::stri_trans_general(original_names, "Latin-ASCII")

      # Create mapping: normalized -> original
      mapping$province <- original_names
      names(mapping$province) <- normalized_names
    }
  }

  # Load municipality (ADM2) names
  municipality_path <- file.path(base_dir, "areas", "municipality", "geoBoundaries-BRA-ADM2_simplified.geojson")
  if (file.exists(municipality_path)) {
    municipalities_sf <- sf::st_read(municipality_path, quiet = TRUE)

    if ("shapeName" %in% names(municipalities_sf)) {
      # Get original names
      original_names <- as.character(municipalities_sf$shapeName)

      # Get normalized names (same way as used throughout the codebase)
      normalized_names <- stringi::stri_trans_general(original_names, "Latin-ASCII")

      # Create mapping: normalized -> original
      mapping$municipality <- original_names
      names(mapping$municipality) <- normalized_names
    }
  }

  message(
    "[load_region_name_mapping] Loaded ", length(mapping$province), " province names and ",
    length(mapping$municipality), " municipality names"
  )

  return(mapping)
}
