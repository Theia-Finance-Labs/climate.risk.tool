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

#' Read asset data from CSV file
#'
#' @title Read asset information from CSV file
#' @description Reads asset information from CSV file in the user_input directory,
#'   converting column names to snake_case and parsing numeric columns correctly.
#' @param base_dir Character string specifying the base directory containing user_input subdirectory
#' @return tibble with asset information
#' @examples
#' \dontrun{
#' base_dir <- system.file("tests_data", package = "climate.risk.tool")
#' assets <- read_assets(base_dir)
#' }
#' @export
read_assets <- function(base_dir) {

  # Define file path
  assets_path <- file.path(base_dir, "user_input", "asset_information.csv")

  # Check if file exists
  if (!file.exists(assets_path)) {
    stop("Asset information file not found at: ", assets_path)
  }

  # Read assets data
  assets_raw <- readr::read_csv(assets_path, show_col_types = FALSE) |>
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

  # Handle character columns that can have empty values (municipality, province)
  char_cols_with_empty <- c("municipality", "province")

  assets_raw <- assets_raw |>
    dplyr::mutate(
      dplyr::across(
        dplyr::any_of(char_cols_with_empty),
        ~ {
          col_data <- .
          col_data[col_data == ""] <- NA
          Encoding(col_data) <- "UTF-8"
          col_data
        }
      )
    )

  # Normalize municipality and province names (remove accents, convert to ASCII)
  assets_raw <- assets_raw |>
    dplyr::mutate(
      municipality = dplyr::if_else(
        !is.na(.data$municipality) & nzchar(as.character(.data$municipality)),
        stringi::stri_trans_general(as.character(.data$municipality), "Latin-ASCII"),
        NA_character_
      ),
      province = dplyr::if_else(
        !is.na(.data$province) & nzchar(as.character(.data$province)),
        stringi::stri_trans_general(as.character(.data$province), "Latin-ASCII"),
        NA_character_
      )
    )

  # Assign province to assets that don't have one
  assets_with_province <- assign_province_to_assets(assets_raw, base_dir)

  assets_with_province
}

#' Assign province to assets based on coordinates or municipality (internal function)
#'
#' @param assets_df Data frame with asset information
#' @param base_dir Base directory containing areas subdirectory
#' @return Data frame with province assigned to all assets
#' @noRd
assign_province_to_assets <- function(assets_df, base_dir) {
  # Ensure province column is character (not logical if all NA)
  if ("province" %in% names(assets_df)) {
    assets_df$province <- as.character(assets_df$province)
  }
  
  # Load province boundaries
  province_path <- file.path(base_dir, "areas", "province", "geoBoundaries-BRA-ADM1_simplified.geojson")
  municipality_path <- file.path(base_dir, "areas", "municipality", "geoBoundaries-BRA-ADM2_simplified.geojson")

  if (!file.exists(province_path)) {
    return(assets_df)
  }

  # Read province boundaries and normalize names
  provinces_sf <- sf::st_read(province_path, quiet = TRUE) |>
    dplyr::mutate(
      province_name = stringi::stri_trans_general(as.character(.data$shapeName), "Latin-ASCII")
    )

  # Identify assets without province
  assets_without_province <- assets_df |>
    dplyr::filter(is.na(.data$province))

  assets_with_province_already <- assets_df |>
    dplyr::filter(!is.na(.data$province))

  if (nrow(assets_without_province) == 0) {
    return(assets_df)
  }

  # Strategy 1: Assets with lat/lon - spatial join to province
  assets_with_coords <- assets_without_province |>
    dplyr::filter(!is.na(.data$latitude), !is.na(.data$longitude))

  if (nrow(assets_with_coords) > 0) {
    # Convert to sf object
    assets_coords_sf <- sf::st_as_sf(
      assets_with_coords,
      coords = c("longitude", "latitude"),
      crs = 4326
    )

    # Spatial join with provinces
    assets_coords_joined <- sf::st_join(assets_coords_sf, provinces_sf, join = sf::st_within)

    # Extract coordinates back and assign province
    coords_matrix <- sf::st_coordinates(assets_coords_joined)
    assets_with_coords <- assets_with_coords |>
      dplyr::mutate(
        province = assets_coords_joined$province_name
      )
  }

  # Strategy 2: Assets with municipality but no coordinates - join via municipality
  assets_with_municipality <- assets_without_province |>
    dplyr::filter(is.na(.data$latitude) | is.na(.data$longitude)) |>
    dplyr::filter(!is.na(.data$municipality))

  if (nrow(assets_with_municipality) > 0 && file.exists(municipality_path)) {
    # Read municipality boundaries and normalize names
    municipalities_sf <- sf::st_read(municipality_path, quiet = TRUE) |>
      dplyr::mutate(
        municipality_name = stringi::stri_trans_general(as.character(.data$shapeName), "Latin-ASCII")
      )

    # Build municipality -> province lookup using municipality barycenter (point-on-surface) within province
    # This avoids polygon-polygon topological quirks and ensures the point lies within the intended province
    # Ensure both layers share CRS
    if (!sf::st_is_longlat(municipalities_sf)) {
      municipalities_sf <- sf::st_transform(municipalities_sf, 4326)
    }
    if (!sf::st_is_longlat(provinces_sf)) {
      provinces_sf <- sf::st_transform(provinces_sf, 4326)
    }

    muni_points <- sf::st_point_on_surface(municipalities_sf)
    muni_points_joined <- sf::st_join(muni_points, provinces_sf, join = sf::st_within)

    municipality_lookup <- muni_points_joined |>
      sf::st_drop_geometry() |>
      dplyr::select("municipality_name", "province_name")

    assets_with_municipality <- assets_with_municipality |>
      dplyr::left_join(
        municipality_lookup,
        by = c("municipality" = "municipality_name")
      ) |>
      dplyr::mutate(
        province = dplyr::coalesce(.data$province_name, .data$province)
      ) |>
      dplyr::select(-dplyr::any_of(c("province_name", "adm1_name")))
  }

  # Combine all assets back together
  result <- dplyr::bind_rows(
    assets_with_province_already,
    if (exists("assets_with_coords") && nrow(assets_with_coords) > 0) assets_with_coords else NULL,
    if (exists("assets_with_municipality") && nrow(assets_with_municipality) > 0) assets_with_municipality else NULL,
    # Assets that still don't have province (no coords, no municipality)
    assets_without_province |>
      dplyr::filter(
        (is.na(.data$latitude) | is.na(.data$longitude)) &
        is.na(.data$municipality)
      )
  )

  n_assigned <- sum(!is.na(result$province)) - sum(!is.na(assets_df$province))

  return(result)
}

#' Read company data from CSV file
#'
#' @title Read company information from CSV file
#' @description Reads company information from a CSV file,
#'   converting column names to snake_case and parsing numeric columns correctly.
#' @param file_path Character string specifying the path to the company CSV file
#' @return tibble with company information
#' @examples
#' \dontrun{
#' companies <- read_companies("path/to/company.csv")
#' }
#' @export
read_companies <- function(file_path) {

  # Check if file exists
  if (!file.exists(file_path)) {
    stop("Company file not found at: ", file_path)
  }

  # Read companies data
  companies_raw <- readr::read_csv(file_path, show_col_types = FALSE) |>
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

  # Define file path
  factors_path <- file.path(base_dir, "damage_and_cost_factors.csv")

  # Check if file exists
  if (!file.exists(factors_path)) {
    stop("Damage and cost factors file not found at: ", factors_path)
  }

  # Read the damage and cost factors CSV
  # The CSV uses comma as decimal separator and quotes around numbers
  factors_df <- readr::read_csv(factors_path, show_col_types = FALSE) |>
    tibble::as_tibble() |>
    dplyr::mutate(
      # Clean up the numeric columns that have comma decimal separators and quotes
      damage_factor = as.numeric(gsub(",", ".", gsub('"', "", .data$damage_factor))),
      cost_factor = as.numeric(gsub(",", ".", gsub('"', "", .data$cost_factor))),
      # Ensure hazard_intensity is numeric
      hazard_intensity = as.numeric(.data$hazard_intensity)
    ) |>
    # Convert column names to snake_case for consistency
    dplyr::rename_with(to_snake_case) |>
    # Normalize province names (remove accents, convert to ASCII)
    dplyr::mutate(
      province = dplyr::if_else(
        !is.na(.data$province) & .data$province != "-" & nzchar(as.character(.data$province)),
        stringi::stri_trans_general(as.character(.data$province), "Latin-ASCII"),
        .data$province
      )
    )

  factors_df
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

  # Define file path
  precomputed_path <- file.path(base_dir, "precomputed_adm_hazards.csv")

  # Check if file exists
  if (!file.exists(precomputed_path)) {
    stop("Precomputed hazards file not found at: ", precomputed_path)
  }

  # Read the precomputed hazards CSV
  precomputed_df <- readr::read_csv(precomputed_path, show_col_types = FALSE) |>
    tibble::as_tibble()

  # Ensure numeric columns are numeric
  numeric_cols <- c(
    "hazard_return_period", "min", "max", "mean", "median",
    "p2_5", "p5", "p95", "p97_5", "n_obs", "max_x", "max_y"
  )

  precomputed_df <- precomputed_df |>
    dplyr::mutate(
      dplyr::across(
        dplyr::any_of(numeric_cols),
        ~ as.numeric(.)
      )
    )

  # Validate adm_level values
  valid_adm_levels <- c("ADM1", "ADM2")
  invalid_levels <- setdiff(unique(precomputed_df$adm_level), valid_adm_levels)
  if (length(invalid_levels) > 0) {
    warning("Found unexpected adm_level values: ", paste(invalid_levels, collapse = ", "))
  }

  # Transform data: construct proper hazard_name and create ensemble-specific rows
  # Unified naming WITHOUT ensemble suffix (base event format)

  # Define ensemble columns to melt
  summary_cols <- c("mean", "median", "p2_5", "p5", "p95", "p97_5")

  transformed_list <- list()

  for (summ_col in summary_cols) {
    # Create rows for this ensemble
    ensemble_data <- precomputed_df |>
      dplyr::mutate(
        # Unified hazard_name WITHOUT ensemble suffix
        hazard_name = paste0(
          .data$hazard_type, "__", .data$hazard_indicator,
          "__GWL=", .data$scenario_name,
          "__RP=", .data$hazard_return_period,
          ifelse(is.na(.data$ensemble), "", paste0("__ensemble=", .data$ensemble))
        ),
        aggregation_method = summ_col,
        hazard_value = .data[[summ_col]]
      )

    transformed_list[[summ_col]] <- ensemble_data
  }

  # Combine all ensemble variants
  precomputed_final <- dplyr::bind_rows(transformed_list)

  precomputed_final <- precomputed_final |>
  dplyr::mutate(
    region = stringi::stri_trans_general(as.character(.data$region), "Latin-ASCII")
  )

  precomputed_final
}

#' Read hazards name mapping from CSV (INTERNAL)
#'
#' @description Internal function used by load_hazards_and_inventory().
#'   Reads the hazards name mapping CSV file and validates that it
#'   contains all required columns including hazard_indicator.
#' @param mapping_path Character path to the CSV mapping file
#' @return Tibble with columns: hazard_file, hazard_type, hazard_indicator,
#'   scenario_name, hazard_return_period
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
