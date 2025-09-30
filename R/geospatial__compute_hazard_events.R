#' Prepare assets with geospatial hazard data and damage cost factors
#'
#' @description Performs geospatial operations to prepare assets for event processing:
#' geolocates assets, cuts out hazards, summarizes hazards in long format, and joins
#' damage cost factors. This isolates the geospatial processing from event-specific logic.
#'
#' @param assets data.frame of assets (as read by read_assets())
#' @param hazards named list of SpatRaster from load_hazards()
#' @param areas list with municipalities and provinces used by geolocate_assets()
#' @param damage_factors data.frame with damage and cost factors lookup table
#' @return data.frame with assets enriched with geospatial hazard data and damage/cost factors
#' @export
compute_hazard_events <- function(assets, hazards, areas, damage_factors) {
  if (!is.data.frame(assets) || nrow(assets) == 0) stop("assets must be data.frame")
  if (!is.list(hazards) || length(hazards) == 0) stop("hazards must be list")
  if (!is.list(areas) || !all(c("municipalities", "provinces") %in% names(areas))) stop("areas invalid")
  if (!is.data.frame(damage_factors) || nrow(damage_factors) == 0) stop("damage_factors must be data.frame")

  assets_geo <- geolocate_assets(assets, hazards, areas$municipalities, areas$provinces)
  assets_cut <- cutout_hazards(assets_geo, hazards)
  assets_long <- summarize_hazards(assets_cut)
  assets_with_factors <- join_damage_cost_factors(assets_long, damage_factors)

  assets_with_factors
}
