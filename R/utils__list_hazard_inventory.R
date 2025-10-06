#' List hazard inventory from loaded hazards
#'
#' @param hazards list of SpatRaster as returned by load_hazards()
#' @return tibble with columns: key, hazard_type, scenario
#' @export
list_hazard_inventory <- function(hazards) {
  if (!is.list(hazards) || length(hazards) == 0) {
    stop("hazards must be a non-empty list")
  }
  keys <- names(hazards)
  if (is.null(keys)) keys <- as.character(seq_along(hazards))
  parts <- strsplit(keys, "__", fixed = TRUE)
  hazard_type <- vapply(parts, function(p) if (length(p) >= 1) p[[1]] else "unknown", character(1))
  scenario <- vapply(parts, function(p) if (length(p) >= 2) p[[2]] else "default", character(1))
  hazard_name <- keys
  # Also provide a nested mapping for convenience in the app
  df <- tibble::tibble(
    key = keys,
    hazard_type = hazard_type,
    hazard_name = hazard_name,
    scenario = scenario
  )
  attr(df, "by_hazard") <- split(df$hazard_name, df$hazard_type)
  df
}
