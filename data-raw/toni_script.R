library(sf)
library(terra)
library(dplyr)
library(climate.risk.tool)

assets <- read_assets("tests/tests_data")
hazards <- load_hazards("tests/tests_data", aggregate_factor = 1)

##uses assets and hazards inputs

df <- assets%>%filter(company %in% c("Eneva SA", "Alupar"))
flood_maps <- hazards

lat_col <- if ("Latitude" %in% names(df)) "Latitude" else "latitude"
lon_col <- if ("Longitude" %in% names(df)) "Longitude" else "longitude"

# ======= Create sf points, then ~1111 m buffer =======
# Filter out rows with missing coordinates before creating spatial points
df_clean <- df %>%
  dplyr::filter(!is.na(.data[[lon_col]]) & !is.na(.data[[lat_col]]))

pts <- st_as_sf(df_clean, coords = c(lon_col, lat_col), crs = 4326, remove = FALSE)
buffers <- pts %>% st_transform(3857) %>% st_buffer(1111) %>% st_transform(4326)

# Also filter the original df to match the cleaned data for the loop
df_loop <- df_clean


# ======= Robust extractor (handles missing CRS & empty crops) =======
.na_stats <- setNames(rep(NA_real_, 7),
                      c("mean","median","max","p2_5","p5","p95","p97_5"))

extract_flood_stats <- function(r_in, polygon_sf) {
  tryCatch({
    r <- if (inherits(r_in, "SpatRaster")) r_in else terra::rast(r_in)
    if (terra::nlyr(r) > 1) r <- r[[1]]

    # Raster CRS may be empty ("") -> assume WGS84
    r_crs <- terra::crs(r)
    if (is.na(r_crs) || r_crs == "") r_crs <- "EPSG:4326"

    # Transform polygon to raster CRS
    poly_r <- sf::st_transform(polygon_sf, r_crs)
    poly_v <- terra::vect(poly_r)

    # Crop; if no overlap, return NA stats
    r_crop <- try(suppressWarnings(terra::crop(r, poly_v)), silent = TRUE)
    if (inherits(r_crop, "try-error") || is.null(r_crop) || terra::ncell(r_crop) == 0) {
      return(.na_stats)
    }

    r_mask <- try(suppressWarnings(terra::mask(r_crop, poly_v)), silent = TRUE)
    if (inherits(r_mask, "try-error") || is.null(r_mask) || terra::ncell(r_mask) == 0) {
      return(.na_stats)
    }

    vals <- as.numeric(terra::values(r_mask, na.rm = TRUE))
    if (length(vals) == 0) return(.na_stats)

    setNames(c(
      mean(vals, na.rm = TRUE),
      stats::median(vals, na.rm = TRUE),
      max(vals, na.rm = TRUE),
      as.numeric(stats::quantile(vals, probs = 0.025, na.rm = TRUE, names = FALSE)),
      as.numeric(stats::quantile(vals, probs = 0.05,  na.rm = TRUE, names = FALSE)),
      as.numeric(stats::quantile(vals, probs = 0.95,  na.rm = TRUE, names = FALSE)),
      as.numeric(stats::quantile(vals, probs = 0.975, na.rm = TRUE, names = FALSE))
    ), names(.na_stats))
  }, error = function(e) {
    message(sprintf("Error processing raster: %s", e$message))
    .na_stats
  })
}

# ======= Loop over assets and rasters =======
results <- purrr::map_dfr(seq_len(nrow(buffers)), function(i) {
  polygon <- buffers[i, "geometry"]
  company <- df_loop$company[i]
  asset   <- df_loop$asset[i]
  lat     <- df_loop[[lat_col]][i]
  lon     <- df_loop[[lon_col]][i]

  purrr::map_dfr(names(flood_maps), function(nm) {
    stats_vec <- extract_flood_stats(flood_maps[[nm]], polygon)
    data.frame(
      company   = company,
      asset     = asset,
      latitude  = lat,
      longitude = lon,
      scenario  = nm,      # <— name of flood map used
      t(stats_vec),
      check.names = FALSE
    )
  })
})
