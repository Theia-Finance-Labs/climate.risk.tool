#' List hazard inventory from metadata
#'
#' @title Create hazard inventory from metadata tibble
#' @description Creates a tibble inventory from hazard metadata, providing
#'   columns needed for UI selection and filtering: hazard_type, scenario_name,
#'   hazard_return_period, and hazard_name.
#' @param metadata Data frame with columns: hazard_file, hazard_type, scenario_code,
#'   scenario_name, hazard_return_period
#' @return Tibble with columns: hazard_type, scenario_name, hazard_return_period,
#'   scenario_code, hazard_name
#' @examples
#' \dontrun{
#' result <- load_hazards_from_mapping("mapping.csv", "hazards/")
#' inventory <- list_hazard_inventory_from_metadata(result$metadata)
#' }
#' @export
list_hazard_inventory_from_metadata <- function(metadata) {
  if (!tibble::is_tibble(metadata) && !is.data.frame(metadata)) {
    stop("metadata must be a tibble or data frame")
  }
  
  required_cols <- c("hazard_file", "hazard_type", "scenario_code", 
                     "scenario_name", "hazard_return_period")
  missing_cols <- setdiff(required_cols, names(metadata))
  if (length(missing_cols) > 0) {
    stop("metadata missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  # Create hazard_name following the naming convention: hazard_type__scenario_code_hXXXglob
  inventory <- metadata |>
    dplyr::mutate(
      hazard_name = paste0(
        .data$hazard_type, "__",
        .data$scenario_code, "_h",
        .data$hazard_return_period, "glob"
      )
    ) |>
    dplyr::select(
      "hazard_type",
      "scenario_name",
      "hazard_return_period",
      "scenario_code",
      "hazard_name"
    )
  
  return(inventory)
}

