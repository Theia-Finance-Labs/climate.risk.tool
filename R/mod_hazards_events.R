#' hazards_events UI Function
#'
#' @description Shiny module to build a list of hazard events to apply. Allows adding
#' multiple events with type, hazard name, and event year.
#' @param id Internal parameter for shiny
#' @param title Character title displayed above the controls
#' @export
mod_hazards_events_ui <- function(id, title = "Hazard events") {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h4(title),
    shiny::p("Select at least one hazard event to run the analysis:", class = "text-muted", style = "font-size: 0.9em; margin-bottom: 10px;"),
    shiny::uiOutput(ns("events_ui")),
    shiny::div(
      style = "display: flex; gap: 8px; align-items: center; justify-content: center; flex-wrap: wrap; margin-top: 15px; margin-bottom: 10px;",
      shiny::tags$label(
        `for` = ns("upload_hazard_config"),
        class = "btn btn-outline-secondary btn-sm",
        style = "margin: 0; cursor: pointer;",
        shiny::icon("upload"),
        shiny::tags$span(class = "d-none d-sm-inline", " Load Events")
      ),
      shiny::tags$a(
        id = ns("download_config"),
        class = "btn btn-outline-secondary btn-sm shiny-download-link",
        href = "",
        target = "_blank",
        download = NA,
        shiny::icon("download"),
        shiny::tags$span(class = "d-none d-sm-inline", " Save Events")
      )
    ),
    shiny::tags$input(
      id = ns("upload_hazard_config"),
      type = "file",
      accept = ".xlsx,.xls",
      style = "display: none;"
    )
  )
}

#' hazards_events Server Functions
#'
#' @param id Internal parameter for shiny
#' @param hazards_inventory reactive data.frame with columns: hazard_type, hazard_indicator, scenario_name, return_period, hazard_name
#' @param hazard_configs reactive list from load_hazards_and_inventory()$configs
#' @param base_dir_reactive optional reactive returning base_dir for loading spatial separation options
#' @return reactive data.frame of configured events with columns: event_id, hazard_type, hazard_indicator, hazard_name, scenario_name, return_period, event_year, season
#' @export
mod_hazards_events_server <- function(id, hazards_inventory, hazard_configs, base_dir_reactive = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
      events_rv <- shiny::reactiveVal(tibble::tibble(
      event_id = character(),
      hazard_type = character(),
      hazard_indicator = character(),
      hazard_name = character(),
      scenario_name = character(),
      return_period = numeric(),
      event_year = integer(),
      season = character(),
      spatial_level = character(),
      spatial_region_codes = character(),
      spatial_region_labels = character(),
      spatial_scheme = character()
    ))

    # Counter for dynamic UIs
    counter <- shiny::reactiveVal(1L)

    spatial_data <- shiny::reactive({
      if (is.null(base_dir_reactive)) {
        return(NULL)
      }
      base_dir <- base_dir_reactive()
      if (is.null(base_dir) || !nzchar(base_dir) || !dir.exists(base_dir)) {
        return(NULL)
      }
      loaded <- try(load_spatial_separation_data(base_dir), silent = TRUE)
      if (inherits(loaded, "try-error")) {
        return(NULL)
      }
      loaded
    })

    # Create UI-friendly inventory (only primary indicators)
    ui_inventory <- shiny::reactive({
      inv <- try(hazards_inventory(), silent = TRUE)
      if (inherits(inv, "try-error") || is.null(inv) || nrow(inv) == 0) {
        return(tibble::tibble())
      }
      cfg <- try(hazard_configs(), silent = TRUE)
      if (inherits(cfg, "try-error") || is.null(cfg)) {
        return(tibble::tibble())
      }
      filter_inventory_for_ui(inv, cfg)
    })

    # Add event button
    shiny::observeEvent(input$add_event, {
      k <- counter()
      haz_type <- input[[paste0("hazard_type_", k)]]
      if (is.null(haz_type)) return()

      # Get index configuration for this hazard type
      cfg <- try(hazard_configs(), silent = TRUE)
      index_ind <- get_index_indicator(cfg, haz_type)
      index_cols <- cfg[[haz_type]]$indicators[[index_ind]]$index
      
      # Collect all index values from dynamic inputs
      index_values <- list()
      for (idx_col in index_cols) {
        val <- input[[paste0("filter_", k, "_", idx_col)]]
        if (is.null(val)) {
          # If a required filter is missing, increment counter and return
          counter(k + 1L)
          return()
        }
        index_values[[idx_col]] <- val
      }

      full_inv <- try(hazards_inventory(), silent = TRUE)
      if (inherits(full_inv, "try-error")) {
        full_inv <- NULL
      }

      lookup <- lookup_hazard_entry(
        hazard_type_val = haz_type,
        index_values = index_values,
        hazard_configs = cfg,
        hazards_inventory = full_inv
      )

      hazard_indicator_val <- lookup$hazard_indicator
      hazard_name_val <- lookup$hazard_name

      if (is.na(hazard_indicator_val) || is.na(hazard_name_val) || hazard_name_val == "") {
        message(
          "[mod_hazards_events] Could not determine hazard metadata for: ",
          haz_type, " with indices ", paste(names(index_values), index_values, sep="=", collapse=", ")
        )
        counter(k + 1L)
        return()
      }

      # Prepare new row - start with basic columns
      # Generate unique event_id based on existing events to avoid duplicates
      cur_events <- events_rv()
      next_id <- if (nrow(cur_events) > 0 && "event_id" %in% names(cur_events)) {
        existing_ids <- cur_events$event_id
        # Extract numeric part from existing IDs (e.g., "ev1" -> 1)
        numeric_ids <- as.integer(gsub("^ev", "", existing_ids))
        numeric_ids <- numeric_ids[!is.na(numeric_ids)]
        if (length(numeric_ids) > 0) {
          max(numeric_ids) + 1L
        } else {
          1L
        }
      } else {
        1L
      }
      
      new_row <- tibble::tibble(
        event_id = paste0("ev", next_id),
        hazard_type = haz_type,
        hazard_indicator = hazard_indicator_val,
        hazard_name = hazard_name_val,
        event_year = as.integer(input[[paste0("year_", k)]])
      )
      
      # Add all index columns to the new row
      for (idx_col in index_cols) {
        val <- index_values[[idx_col]]
        if (idx_col == "return_period") val <- as.numeric(val)
        new_row[[idx_col]] <- val
      }
      
      # Ensure backward compatibility columns exist in events table for downstream pipeline
      # 1. Map 'gwl' to 'scenario_name' if needed
      if (!"scenario_name" %in% names(new_row)) {
        if ("gwl" %in% names(new_row)) {
          new_row$scenario_name <- new_row$gwl
        } else {
          new_row$scenario_name <- NA_character_
        }
      }
      
      # 2. Map 'return_period' (if it was called something else in index, though unlikely)
      if (!"return_period" %in% names(new_row)) {
        new_row$return_period <- NA_real_
      }
      
      # 3. Ensure season exists
      if (!"season" %in% names(new_row)) new_row$season <- NA_character_

      spatial_scheme_val <- get_hazard_spatial_scheme(cfg, haz_type)
      spatial_level_val <- input[[paste0("spatial_level_", k)]]
      if (is.null(spatial_level_val) || !nzchar(as.character(spatial_level_val))) {
        spatial_level_val <- "brazil"
      }
      spatial_level_val <- tolower(as.character(spatial_level_val))

      selected_region_codes <- input[[paste0("spatial_regions_", k)]]
      if (is.null(selected_region_codes)) selected_region_codes <- character(0)
      selected_region_codes <- as.character(selected_region_codes)
      selected_region_codes <- selected_region_codes[nzchar(selected_region_codes)]
      if (spatial_level_val == "brazil") {
        selected_region_codes <- character(0)
      }

      selected_region_labels <- character(0)
      if (length(selected_region_codes) > 0) {
        choices_df <- get_spatial_region_choices(spatial_data(), spatial_scheme_val, spatial_level_val)
        if (nrow(choices_df) > 0) {
          selected_region_labels <- choices_df$region_label[match(selected_region_codes, choices_df$region_code)]
          selected_region_labels <- selected_region_labels[!is.na(selected_region_labels) & nzchar(selected_region_labels)]
        }
      }

      new_row$spatial_level <- spatial_level_val
      new_row$spatial_region_codes <- if (length(selected_region_codes) > 0) paste(unique(selected_region_codes), collapse = "|") else NA_character_
      new_row$spatial_region_labels <- if (length(selected_region_labels) > 0) paste(unique(selected_region_labels), collapse = "|") else NA_character_
      new_row$spatial_scheme <- spatial_scheme_val

      # bind_rows handles different column sets gracefully
      events_rv(dplyr::bind_rows(cur_events, new_row))
      counter(k + 1L)
    })

    # Render dynamic UI for current event only
    output$events_ui <- shiny::renderUI({
      k <- counter()

      ui_inv <- try(ui_inventory(), silent = TRUE)
      hazard_type_choices <- character(0)
      scenario_choices <- character(0)
      return_period_choices <- numeric(0)

      if (!inherits(ui_inv, "try-error") && (tibble::is_tibble(ui_inv) || is.data.frame(ui_inv)) && nrow(ui_inv) > 0) {
        hazard_type_choices <- sort(unique(ui_inv$hazard_type))
        if (length(hazard_type_choices) > 0) {
          # Get scenario values for first hazard type (primary indicator only)
          first_hazard <- hazard_type_choices[[1]]
          scenario_choices <- unique(ui_inv$scenario_name[ui_inv$hazard_type == first_hazard])

          if (length(scenario_choices) > 0) {
            # Get return periods for first hazard type and scenario (primary indicator only)
            first_scenario <- scenario_choices[[1]]
            return_period_choices <- unique(ui_inv$return_period[
              ui_inv$hazard_type == first_hazard &
                ui_inv$scenario_name == first_scenario
            ])
          }
        }
      }

      # Only show form for current event (index k) - NO hazard_indicator dropdown
      shiny::wellPanel(
        style = "padding-bottom: 0;",
        shiny::selectInput(ns(paste0("hazard_type_", k)), "Hazard Type",
          choices = hazard_type_choices,
          selected = if (length(hazard_type_choices) > 0) hazard_type_choices[[1]] else NULL
        ),
        shiny::uiOutput(ns(paste0("dynamic_filters_ui_", k))),
        shiny::uiOutput(ns(paste0("spatial_ui_", k))),
        shiny::sliderInput(
          ns(paste0("year_", k)),
          label = "Shock Year:",
          value = 2030,
          min = 2025,
          max = 2049,
          step = 1,
          sep = "",
          ticks = TRUE
        ),
        shiny::div(
          style = "margin: 20px -15px -10px -15px;",
          shiny::actionButton(
            ns("add_event"),
            label = "Add hazard",
            class = "btn-secondary btn-block",
            icon = shiny::icon("plus")
          )
        )
      )
    })

    # Create cascading dropdowns based on index configuration
    shiny::observe({
      k <- counter()
      if (k == 0) return()

      output[[paste0("dynamic_filters_ui_", k)]] <- shiny::renderUI({
        ui_inv <- try(ui_inventory(), silent = TRUE)
        hazard_type_val <- input[[paste0("hazard_type_", k)]]
        
        if (inherits(ui_inv, "try-error") || is.null(ui_inv) || nrow(ui_inv) == 0 || is.null(hazard_type_val)) {
          return(shiny::span(""))
        }

        # Get index configuration for this hazard type
        cfg <- try(hazard_configs(), silent = TRUE)
        index_ind <- get_index_indicator(cfg, hazard_type_val)
        index_cols <- cfg[[hazard_type_val]]$indicators[[index_ind]]$index
        
        if (length(index_cols) == 0) return(shiny::helpText("No index defined for this hazard"))

        # Create a list of selectInputs, one for each index column
        # They cascade: each one filters the available choices for the next
        filter_uis <- list()
        temp_inv <- ui_inv |> dplyr::filter(.data$hazard_type == hazard_type_val)
        
        for (i in seq_along(index_cols)) {
          idx_col <- index_cols[i]
          
          # Label is the index column name itself (dynamic!)
          label <- idx_col
          # Prettify common names if desired, but keep it dynamic
          if (label == "scenario_name") label <- "Scenario"
          if (label == "return_period") label <- "Return Period (years)"
          if (label == "gwl") label <- "GWL"
          if (label == "season") label <- "Season"

          choices <- sort(unique(temp_inv[[idx_col]]))
          
          # Check if we already have a selection for this input
          input_id <- paste0("filter_", k, "_", idx_col)
          current_selection <- input[[input_id]]
          
          if (is.null(current_selection) || !(current_selection %in% choices)) {
            current_selection <- if (length(choices) > 0) choices[1] else NULL
          }

          filter_uis[[i]] <- shiny::selectInput(ns(input_id), label,
            choices = choices,
            selected = current_selection
          )
          
          # Filter the inventory for the next iteration based on this selection
          if (!is.null(current_selection)) {
             # Match types for filtering
             val_to_filter <- current_selection
             if (idx_col == "return_period") val_to_filter <- as.numeric(val_to_filter)
             temp_inv <- temp_inv |> dplyr::filter(.data[[idx_col]] == !!val_to_filter)
          }
        }
        
        shiny::tagList(filter_uis)
      })

      output[[paste0("spatial_ui_", k)]] <- shiny::renderUI({
        hazard_type_val <- input[[paste0("hazard_type_", k)]]
        if (is.null(hazard_type_val) || !nzchar(as.character(hazard_type_val))) {
          return(shiny::span(""))
        }

        cfg <- try(hazard_configs(), silent = TRUE)
        if (inherits(cfg, "try-error") || is.null(cfg) || !hazard_type_val %in% names(cfg)) {
          return(shiny::span(""))
        }

        scheme <- get_hazard_spatial_scheme(cfg, hazard_type_val)
        level_choices <- get_spatial_level_choices(scheme)
        selected_level <- input[[paste0("spatial_level_", k)]]
        if (is.null(selected_level) || !selected_level %in% unname(level_choices)) {
          selected_level <- "brazil"
        }

        region_input <- NULL
        if (selected_level != "brazil") {
          choices_df <- get_spatial_region_choices(spatial_data(), scheme, selected_level)
          if (nrow(choices_df) > 0) {
            region_choices <- as.character(choices_df$region_code)
            names(region_choices) <- as.character(choices_df$region_label)

            current_sel <- input[[paste0("spatial_regions_", k)]]
            if (is.null(current_sel)) current_sel <- character(0)
            current_sel <- intersect(as.character(current_sel), unname(region_choices))

            region_input <- shiny::selectizeInput(
              ns(paste0("spatial_regions_", k)),
              label = "Selected Regions",
              choices = region_choices,
              selected = current_sel,
              multiple = TRUE,
              options = list(placeholder = "Choose one or more regions")
            )
          } else {
            region_input <- shiny::helpText("No spatial regions available for this level.")
          }
        }

        shiny::wellPanel(
          style = "margin-top: 8px; margin-bottom: 8px; padding: 10px;",
          shiny::h5("Spatial Separation", style = "margin-top: 0;"),
          shiny::selectInput(
            ns(paste0("spatial_level_", k)),
            label = "Separation Level",
            choices = level_choices,
            selected = selected_level
          ),
          region_input
        )
      })
    })


    output$download_config <- shiny::downloadHandler(
      filename = function() {
        paste0("hazard_configuration_", Sys.Date(), ".xlsx")
      },
      content = function(file) {
        current_events <- events_rv()

        if (is.null(current_events) || nrow(current_events) == 0) {
          writexl::write_xlsx(
            tibble::tibble(message = "No hazard configuration available"),
            path = file
          )
          return()
        }

        # Persist only index-defining columns; hazard metadata is derived on load.
        export_cols <- c(
          "event_id",
          "hazard_type",
          "scenario_name",
          "return_period",
          "event_year",
          "season",
          "spatial_level",
          "spatial_region_codes",
          "spatial_region_labels",
          "spatial_scheme"
        )

        export_df <- current_events |>
          dplyr::select(dplyr::any_of(export_cols))

        writexl::write_xlsx(as.data.frame(export_df), path = file)
      }
    )

    # Delete event function
    delete_event <- function(event_id) {
      updated <- delete_event_by_id(events_rv(), event_id)
      events_rv(updated)
    }

    # Handle file upload from the hidden file input
    shiny::observeEvent(input$upload_hazard_config, {
      upload <- input$upload_hazard_config
      if (!is.null(upload) && !is.null(upload$datapath) && file.exists(upload$datapath)) {
        load_config(upload$datapath)
      }
    })

    # Load config from external file path
    load_config <- function(file_path) {
      cfg <- try(hazard_configs(), silent = TRUE)
      if (inherits(cfg, "try-error") || is.null(cfg)) {
        return()
      }

      inv <- try(hazards_inventory(), silent = TRUE)
      if (inherits(inv, "try-error")) {
        inv <- NULL
      }

      reconstructed <- load_hazards_events_config(
        file_path = file_path,
        hazard_configs = cfg,
        hazards_inventory = inv
      )

      if (is.null(reconstructed) || nrow(reconstructed) == 0) {
        return()
      }

      events_rv(reconstructed)
      counter(nrow(reconstructed) + 1L)
    }

    # Return
    return(list(
      events = events_rv,
      delete_event = delete_event,
      load_config = load_config
    ))
  })
}
