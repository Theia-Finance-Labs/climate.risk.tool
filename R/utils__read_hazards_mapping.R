#' Read hazards name mapping from CSV
#'
#' @title Read hazards metadata from CSV file
#' @description Reads the hazards name mapping CSV file and validates that it
#'   contains all required columns including hazard_indicator.
#' @param mapping_path Character path to the CSV mapping file
#' @return Tibble with columns: hazard_file, hazard_type, hazard_indicator,
#'   scenario_code, scenario_name, hazard_return_period
#' @examples
#' \dontrun{
#' mapping <- read_hazards_mapping("hazards_name_mapping.csv")
#' }
#' @export
read_hazards_mapping <- function(mapping_path) {
  if (!file.exists(mapping_path)) {
    stop("Mapping file not found: ", mapping_path)
  }
  
  mapping <- utils::read.csv(mapping_path, stringsAsFactors = FALSE, strip.white = TRUE)
  mapping <- tibble::as_tibble(mapping)
  
  # Validate required columns
  required_cols <- c("hazard_file", "hazard_type", "hazard_indicator", "scenario_code", 
                     "scenario_name", "hazard_return_period")
  missing_cols <- setdiff(required_cols, names(mapping))
  if (length(missing_cols) > 0) {
    stop("Mapping file missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  return(mapping)
}

