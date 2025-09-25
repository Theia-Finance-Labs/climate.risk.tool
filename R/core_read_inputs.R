# Helper function to convert column names to snake_case
to_snake_case <- function(names) {
  names |>
    gsub("([a-z])([A-Z])", "\\1_\\2", x = _) |>  # camelCase to snake_case
    gsub("\\s+", "_", x = _) |>                   # spaces to underscores
    gsub("\\.", "_", x = _) |>                    # dots to underscores
    gsub("_+", "_", x = _) |>                     # multiple underscores to single
    gsub("^_|_$", "", x = _) |>                   # remove leading/trailing underscores
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
  message("📁 [read_assets] Reading asset data from: ", base_dir)

  # Define file path
  assets_path <- file.path(base_dir, "user_input", "asset_information.csv")

  # Check if file exists
  if (!file.exists(assets_path)) {
    stop("Asset information file not found at: ", assets_path)
  }

  # Read assets data
  assets_raw <- utils::read.csv(assets_path, stringsAsFactors = FALSE)
  names(assets_raw) <- to_snake_case(names(assets_raw))

  # Convert numeric columns for assets
  numeric_asset_cols <- c("share_of_economic_activity", "latitude", "longitude", "size_in_m2", "size_in_hectare")
  for (col in numeric_asset_cols) {
    if (col %in% names(assets_raw)) {
      # Replace empty strings with NA before converting to numeric
      assets_raw[[col]][assets_raw[[col]] == ""] <- NA
      assets_raw[[col]] <- as.numeric(assets_raw[[col]])
    }
  }

  message("✅ [read_assets] Loaded ", nrow(assets_raw), " assets")
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
  message("📁 [read_companies] Reading company data from: ", file_path)

  # Check if file exists
  if (!file.exists(file_path)) {
    stop("Company file not found at: ", file_path)
  }

  # Read companies data
  companies_raw <- utils::read.csv(file_path, stringsAsFactors = FALSE)
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

  message("✅ [read_companies] Loaded ", nrow(companies_raw), " companies")
  companies_raw
}

