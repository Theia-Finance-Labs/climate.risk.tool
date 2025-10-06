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
#' @return data.frame with asset information
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
  assets_raw <- readr::read_csv(assets_path, show_col_types = FALSE)
  names(assets_raw) <- to_snake_case(names(assets_raw))

  # Convert numeric columns for assets
  numeric_asset_cols <- c("share_of_economic_activity", "latitude", "longitude", "size_in_m2", "size_in_hectare")
  for (col in numeric_asset_cols) {
    if (col %in% names(assets_raw)) {
      # Replace empty strings with NA before converting to numeric
      assets_raw[[col]][assets_raw[[col]] == ""] <- NA

      # For latitude, longitude - suppress coercion warnings as they can have empty values
      if (col %in% c("latitude", "longitude")) {
        assets_raw[[col]] <- suppressWarnings(as.numeric(assets_raw[[col]]))
      } else if (col == "size_in_m2") {
        # Clean size_in_m2 column by extracting numeric part (e.g., "4693m2" -> 4693)
        assets_raw[[col]] <- suppressWarnings(as.numeric(gsub("^([0-9.]+).*", "\\1", assets_raw[[col]])))
      } else {
        assets_raw[[col]] <- as.numeric(assets_raw[[col]])
      }
    }
  }

  # Handle character columns that can have empty values (municipality, province)
  char_cols_with_empty <- c("municipality", "province")
  for (col in char_cols_with_empty) {
    if (col %in% names(assets_raw)) {
      # Replace empty strings with NA for character columns
      assets_raw[[col]][assets_raw[[col]] == ""] <- NA
      # Set UTF-8 encoding for proper string comparison (handles accented characters like e, a, n, etc.)
      Encoding(assets_raw[[col]]) <- "UTF-8"
    }
  }

  message("[read_assets] Loaded ", nrow(assets_raw), " assets")
  assets_raw
}

#' Read company data from CSV file
#'
#' @title Read company information from CSV file
#' @description Reads company information from a CSV file,
#'   converting column names to snake_case and parsing numeric columns correctly.
#' @param file_path Character string specifying the path to the company CSV file
#' @return data.frame with company information
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
  companies_raw <- readr::read_csv(file_path, show_col_types = FALSE)
  names(companies_raw) <- to_snake_case(names(companies_raw))

  # Convert numeric columns for companies
  numeric_company_cols <- c("revenues", "debt", "volatility", "net_profit_margin", "loan_size", "lgd", "term")
  for (col in numeric_company_cols) {
    if (col %in% names(companies_raw)) {
      # Replace empty strings with NA before converting to numeric
      companies_raw[[col]][companies_raw[[col]] == ""] <- NA
      companies_raw[[col]] <- as.numeric(companies_raw[[col]])
    }
  }

  message("[read_companies] Loaded ", nrow(companies_raw), " companies")
  companies_raw
}

#' Read damage and cost factors from CSV file
#'
#' @title Read damage and cost factors lookup table
#' @description Reads damage and cost factors from CSV file, parsing numeric columns
#'   and handling comma decimal separators correctly.
#' @param base_dir Character string specifying the base directory containing damage_and_cost_factors.csv
#' @return data.frame with damage and cost factors
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
  factors_df <- readr::read_csv(factors_path, show_col_types = FALSE)

  # Clean up the numeric columns that have comma decimal separators and quotes
  factors_df$damage_factor <- as.numeric(gsub(",", ".", gsub('"', "", factors_df$damage_factor)))
  factors_df$cost_factor <- as.numeric(gsub(",", ".", gsub('"', "", factors_df$cost_factor)))

  # Ensure hazard_intensity is numeric
  factors_df$hazard_intensity <- as.numeric(factors_df$hazard_intensity)

  # Convert column names to snake_case for consistency
  names(factors_df) <- to_snake_case(names(factors_df))

  message("[read_damage_cost_factors] Loaded ", nrow(factors_df), " factor records")
  factors_df
}
