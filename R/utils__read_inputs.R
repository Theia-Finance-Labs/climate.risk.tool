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
  message("[read_assets] Reading asset data from: ", base_dir)

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

  message("[read_assets] Loaded ", nrow(assets_raw), " assets")
  assets_raw
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
  message("[read_companies] Reading company data from: ", file_path)

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
    dplyr::rename_with(to_snake_case)

  message("[read_damage_cost_factors] Loaded ", nrow(factors_df), " factor records")
  factors_df
}
