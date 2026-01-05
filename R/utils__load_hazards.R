#' Load NetCDF hazards and build complete inventory
#'
#' @title Load all hazard data and generate inventory
#' @description Self-contained loader that:
#' 1. Scans directory tree for NetCDF files and loads them
#' 2. Generates a unified inventory with hazard metadata
#' 3. Returns both hazards and inventory
#'
#' @param hazards_dir Character path to hazards directory containing subdirectories with NetCDF files
#' @param aggregate_factor Integer >= 1. Aggregation factor for NetCDF rasters (default: 1, aggregation not currently supported)
#' @return A list with two elements:
#'   - `hazards`: Named list of SpatRaster objects
#'   - `inventory`: Tibble with columns: hazard_type, hazard_indicator, scenario_name,
#'     hazard_return_period, hazard_name (unified format),
#'     ensemble (ensemble variant), source ("nc")
#' @examples
#' \dontrun{
#' result <- load_hazards_and_inventory(
#'   hazards_dir = file.path(base_dir, "hazards"),
#'   aggregate_factor = 1L
#' )
#'
#' # Access hazards (for compute pipeline)
#' all_hazards <- result$hazards
#'
#' # Access inventory (for UI dropdowns)
#' inventory <- result$inventory
#' }
#' @export
load_hazards_and_inventory <- function(hazards_dir, aggregate_factor = 1L) {
  message("[load_hazards_and_inventory] Starting hazard loading and inventory...")

  # Load NC files and build inventory
  nc_result <- load_nc_hazards_with_metadata(
    hazards_dir = hazards_dir,
    aggregate_factor = as.integer(aggregate_factor)
  )
  nc_list <- nc_result$hazards
  nc_inventory <- nc_result$inventory

  message(
    "[load_hazards_and_inventory] Complete: ",
    length(nc_list), " NetCDF hazards loaded"
  )

  return(list(
    hazards = nc_list,
    inventory = nc_inventory
  ))
}
