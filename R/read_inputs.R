#' Read input data files
#'
#' @title Read asset and company data from CSV files
#' @description Reads asset information and company data from CSV files in the user_input directory,
#'   converting column names to snake_case and parsing numeric columns correctly.
#' @param base_dir Character string specifying the base directory containing user_input subdirectory
#' @return List with two elements: assets (data.frame) and companies (data.frame)
#' @examples
#' \dontrun{
#' base_dir <- system.file("tests_data", package = "climate.risk.tool")
#' data <- read_inputs(base_dir)
#' }
#' @export
read_inputs <- function(base_dir) {
  message("📁 [read_inputs] Starting to read input files from: ", base_dir)
  
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
  
  # Define file paths
  assets_path <- file.path(base_dir, "user_input", "asset_information.csv")
  companies_path <- file.path(base_dir, "user_input", "company.csv")
  
  message("📄 [read_inputs] Looking for files:")
  message("  - Assets: ", assets_path)
  message("  - Companies: ", companies_path)
  
  # Check if files exist
  if (!file.exists(assets_path)) {
    stop("Asset information file not found at: ", assets_path)
  }
  if (!file.exists(companies_path)) {
    stop("Company file not found at: ", companies_path)
  }
  
  message("✅ [read_inputs] Files found, reading data...")
  
  # Read assets data
  message("📊 [read_inputs] Reading asset information...")
  assets_raw <- utils::read.csv(assets_path, stringsAsFactors = FALSE)
  names(assets_raw) <- to_snake_case(names(assets_raw))
  message("  - Loaded ", nrow(assets_raw), " assets with ", ncol(assets_raw), " columns")
  
  # Read companies data
  message("🏢 [read_inputs] Reading company information...")
  companies_raw <- utils::read.csv(companies_path, stringsAsFactors = FALSE)
  names(companies_raw) <- to_snake_case(names(companies_raw))
  message("  - Loaded ", nrow(companies_raw), " companies with ", ncol(companies_raw), " columns")
  
  # Convert numeric columns for assets
  numeric_asset_cols <- c("share_of_economic_activity", "latitude", "longitude", "size_in_m2", "size_in_hectare")
  for (col in numeric_asset_cols) {
    if (col %in% names(assets_raw)) {
      # Replace empty strings with NA before converting to numeric
      assets_raw[[col]][assets_raw[[col]] == ""] <- NA
      assets_raw[[col]] <- as.numeric(assets_raw[[col]])
    }
  }
  
  # Convert numeric columns for companies
  numeric_company_cols <- c("revenues", "debt", "volatility", "net_profit_margin", "loan_size", "lgd", "term")
  for (col in numeric_company_cols) {
    if (col %in% names(companies_raw)) {
      # Replace empty strings with NA before converting to numeric
      companies_raw[[col]][companies_raw[[col]] == ""] <- NA
      companies_raw[[col]] <- as.numeric(companies_raw[[col]])
    }
  }
  
  # Return as list
  message("✅ [read_inputs] Successfully loaded input data")
  message("  - Assets: ", nrow(assets_raw), " rows")
  message("  - Companies: ", nrow(companies_raw), " rows")
  
  list(
    assets = assets_raw,
    companies = companies_raw
  )
}
