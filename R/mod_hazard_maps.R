#' hazard_maps UI Function
#'
#' @description Module to display interactive hazard maps with asset overlays
#' @param id Internal parameter for shiny
#' @export
mod_hazard_maps_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::div(
      class = "maps-container",
      shiny::h3("Hazard Maps with Asset Locations", class = "results-title"),
      shiny::p(
        "Interactive maps showing hazard intensity and affected assets. ",
        "Hover over assets to see details. Zoom in/out to see municipality boundaries.",
        class = "text-muted",
        style = "margin-bottom: 2rem;"
      ),
      shiny::uiOutput(ns("maps_ui"))
    )
  )
}

#' hazard_maps Server Functions
#'
#' @param id Internal parameter for shiny
#' @param results_reactive reactive containing analysis results
#' @param events_reactive reactive containing configured events
#' @param hazards_reactive reactive containing hazard data
#' @param base_dir_reactive reactive containing base directory path
#' @export
mod_hazard_maps_server <- function(id, results_reactive, events_reactive, 
                                   hazards_reactive, base_dir_reactive) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Load boundaries once
    boundaries <- shiny::reactive({
      base_dir <- base_dir_reactive()
      if (is.null(base_dir) || base_dir == "") {
        return(NULL)
      }
      
      province_path <- file.path(base_dir, "areas", "province", "geoBoundaries-BRA-ADM1_simplified.geojson")
      municipality_path <- file.path(base_dir, "areas", "municipality", "geoBoundaries-BRA-ADM2_simplified.geojson")
      
      adm1 <- NULL
      adm2 <- NULL
      
      if (file.exists(province_path)) {
        adm1 <- try(sf::st_read(province_path, quiet = TRUE), silent = TRUE)
        if (inherits(adm1, "try-error")) adm1 <- NULL
      }
      
      if (file.exists(municipality_path)) {
        adm2 <- try(sf::st_read(municipality_path, quiet = TRUE), silent = TRUE)
        if (inherits(adm2, "try-error")) adm2 <- NULL
      }
      
      list(adm1 = adm1, adm2 = adm2)
    })
    
    # Render maps dynamically based on events
    output$maps_ui <- shiny::renderUI({
      results <- results_reactive()
      events <- try(events_reactive(), silent = TRUE)
      
      if (is.null(results) || inherits(events, "try-error") || 
          is.null(events) || nrow(events) == 0) {
        return(shiny::div(
          class = "alert alert-info",
          style = "margin: 2rem;",
          shiny::icon("info-circle"),
          " No hazard maps to display. Run an analysis first."
        ))
      }
      
      # Get unique hazard names from events
      unique_hazards <- unique(events$hazard_name)
      
      if (length(unique_hazards) == 0) {
        return(shiny::div(
          class = "alert alert-warning",
          style = "margin: 2rem;",
          shiny::icon("exclamation-triangle"),
          " No hazards configured in events."
        ))
      }
      
      # Create a map for each unique hazard
      map_outputs <- lapply(seq_along(unique_hazards), function(i) {
        hazard_name <- unique_hazards[i]
        map_id <- paste0("map_", i)
        
        # Render the map for this hazard
        output[[map_id]] <- leaflet::renderLeaflet({
          create_hazard_map(
            hazard_name = hazard_name,
            results = results,
            hazards = hazards_reactive(),
            boundaries = boundaries()
          )
        })
        
        # Return UI element
        shiny::div(
          class = "map-section",
          style = "margin-bottom: 3rem;",
          shiny::h4(
            paste("Hazard:", hazard_name),
            class = "map-title",
            style = "margin-bottom: 1rem; color: #2563eb;"
          ),
          shiny::div(
            class = "map-container",
            style = "height: 600px; border: 1px solid #e2e8f0; border-radius: 8px; overflow: hidden;",
            leaflet::leafletOutput(ns(map_id), height = "100%")
          )
        )
      })
      
      shiny::tagList(map_outputs)
    })
  })
}

#' Create a hazard map with asset overlays
#'
#' @param hazard_name Character. Name of hazard to map
#' @param results List with analysis results
#' @param hazards Named list of hazard data
#' @param boundaries List with adm1 and adm2 boundaries
#' @return leaflet map object
#' @noRd
create_hazard_map <- function(hazard_name, results, hazards, boundaries) {
  # Initialize base map
  map <- leaflet::leaflet() |>
    leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) |>
    leaflet::setView(lng = -47.9292, lat = -15.7801, zoom = 4)  # Center on Brazil
  
  # Extract hazard data
  hazard_data <- extract_hazard_data(hazard_name, hazards)
  
  if (!is.null(hazard_data)) {
    hazard_type <- get_hazard_type(hazard_data)
    
    if (hazard_type == "raster") {
      # Add raster layer
      map <- tryCatch({
        # Create color palette for raster
        pal <- leaflet::colorNumeric(
          palette = "YlOrRd",
          domain = terra::values(hazard_data),
          na.color = "transparent"
        )
        
        map |>
          leaflet::addRasterImage(
            hazard_data,
            colors = pal,
            opacity = 0.6,
            group = "Hazard Layer"
          ) |>
          leaflet::addLegend(
            pal = pal,
            values = terra::values(hazard_data),
            title = "Hazard<br/>Intensity",
            position = "bottomright"
          )
      }, error = function(e) {
        message("Could not add raster layer: ", e$message)
        map
      })
    } else if (hazard_type == "points") {
      # Add point markers for CSV data
      if ("lat" %in% names(hazard_data) && "lon" %in% names(hazard_data) && 
          "hazard_intensity" %in% names(hazard_data)) {
        
        valid_points <- hazard_data |>
          dplyr::filter(!is.na(.data$lat), !is.na(.data$lon), !is.na(.data$hazard_intensity))
        
        if (nrow(valid_points) > 0) {
          pal <- leaflet::colorNumeric(
            palette = "YlOrRd",
            domain = valid_points$hazard_intensity
          )
          
          map <- map |>
            leaflet::addCircleMarkers(
              data = valid_points,
              lng = ~lon,
              lat = ~lat,
              radius = 3,
              color = ~pal(hazard_intensity),
              fillOpacity = 0.7,
              stroke = FALSE,
              group = "Hazard Layer"
            ) |>
            leaflet::addLegend(
              pal = pal,
              values = valid_points$hazard_intensity,
              title = "Hazard<br/>Intensity",
              position = "bottomright"
            )
        }
      }
    }
  }
  
  # Prepare asset overlays
  if (!is.null(results$assets_factors)) {
    asset_overlays <- prepare_asset_overlay(
      results$assets_factors,
      hazard_name,
      boundaries$adm1,
      boundaries$adm2
    )
    
    # Add geolocated assets as markers
    if (!is.null(asset_overlays$geolocated) && nrow(asset_overlays$geolocated) > 0) {
      map <- map |>
        leaflet::addCircleMarkers(
          data = asset_overlays$geolocated,
          radius = 6,
          color = "#3498db",
          fillColor = "#3498db",
          fillOpacity = 0.8,
          stroke = TRUE,
          weight = 2,
          popup = ~format_hover_text(
            asset,
            hazard_intensity,
            list(
              "Asset Category" = asset_category,
              "Matching Method" = matching_method
            )
          ),
          group = "Geolocated Assets"
        )
    }
    
    # Add municipality polygons
    if (!is.null(asset_overlays$municipalities) && nrow(asset_overlays$municipalities) > 0) {
      pal_muni <- leaflet::colorNumeric(
        palette = "Blues",
        domain = asset_overlays$municipalities$n_assets
      )
      
      map <- map |>
        leaflet::addPolygons(
          data = asset_overlays$municipalities,
          fillColor = ~pal_muni(n_assets),
          fillOpacity = 0.4,
          color = "#2c3e50",
          weight = 1,
          popup = ~paste0(
            "<strong>Municipality: ", shapeName, "</strong><br/>",
            "Number of Assets: ", n_assets, "<br/>",
            "Assets: ", asset_list, "<br/>",
            "Avg Hazard Intensity: ", round(avg_hazard_intensity, 3)
          ),
          group = "Municipality Assets"
        )
    }
    
    # Add province polygons
    if (!is.null(asset_overlays$provinces) && nrow(asset_overlays$provinces) > 0) {
      map <- map |>
        leaflet::addPolygons(
          data = asset_overlays$provinces,
          fillColor = "#e74c3c",
          fillOpacity = 0.3,
          color = "#c0392b",
          weight = 2,
          popup = ~paste0(
            "<strong>Province: ", shapeName, "</strong><br/>",
            "Number of Assets: ", n_assets, "<br/>",
            "Assets: ", asset_list, "<br/>",
            "Avg Hazard Intensity: ", round(avg_hazard_intensity, 3)
          ),
          group = "Province Assets"
        )
    }
    
    # Add layer controls
    map <- map |>
      leaflet::addLayersControl(
        overlayGroups = c("Hazard Layer", "Geolocated Assets", "Municipality Assets", "Province Assets"),
        options = leaflet::layersControlOptions(collapsed = FALSE)
      )
  }
  
  map
}

