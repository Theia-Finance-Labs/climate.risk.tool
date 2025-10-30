# Script to process asset_information.xlsx
# - Removes Subsector column
# - Splits Sector column into sector (CNAE code) and cnae (description)

library(readxl)
library(dplyr)
library(writexl)
library(stringr)

# Input and output paths
input_file <- "tests/tests_data/user_input/asset_information.xlsx"
output_file <- "tests/tests_data/user_input/asset_information.xlsx"

# Read the Excel file
message("Reading file: ", input_file)
df <- readxl::read_excel(input_file) |>
  tibble::as_tibble()

message("Original columns: ", paste(names(df), collapse = ", "))
message("Number of rows: ", nrow(df))

# Remove Subsector column if it exists
if ("Subsector" %in% names(df)) {
  df <- df |>
    dplyr::select(-Subsector)
  message("Removed 'Subsector' column")
} else {
  message("Warning: 'Subsector' column not found")
}

# Split Sector column into sector (CNAE code) and cnae (description)
# Format: "06 (Oil and Gas Extraction)" -> sector = "06", cnae = "Oil and Gas Extraction"
if ("Sector" %in% names(df)) {
  df <- df |>
    dplyr::mutate(
      # Extract CNAE code (digits at the start)
      sector = stringr::str_extract(Sector, "^\\d+"),
      # Extract description (text within parentheses)
      cnae = stringr::str_extract(Sector, "(?<=\\()[^)]+(?=\\))"),
      .before = Sector
    ) |>
    # Remove original Sector column
    dplyr::select(-Sector)
  message("Split 'Sector' column into 'sector' and 'cnae' columns")
} else {
  message("Warning: 'Sector' column not found")
}

message("New columns: ", paste(names(df), collapse = ", "))
message("Number of rows: ", nrow(df))

# Write back to the same file
message("Writing to: ", output_file)
writexl::write_xlsx(df, output_file)

message("Processing complete!")

