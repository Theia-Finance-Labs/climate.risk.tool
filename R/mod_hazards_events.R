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
#' @return reactive data.frame of configured events with columns: event_id, hazard_type, hazard_indicator, hazard_name, scenario_name, return_period, event_year, season
#' @export
mod_hazards_events_server <- function(id, hazards_inventory, hazard_configs) {
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
      season = character()
    ))

    # Counter for dynamic UIs
    counter <- shiny::reactiveVal(1L)

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

    lookup_hazard_entry <- function(hazard_type_val, index_values) {
      cfg <- try(hazard_configs(), silent = TRUE)
      hazard_indicator_val <- get_index_indicator(cfg, hazard_type_val)

      if (is.na(hazard_indicator_val)) {
        return(list(
          hazard_indicator = NA_character_,
          hazard_name = NA_character_
        ))
      }

      full_inv <- try(hazards_inventory(), silent = TRUE)

      if (inherits(full_inv, "try-error") || is.null(full_inv) || nrow(full_inv) == 0) {
        return(list(
          hazard_indicator = hazard_indicator_val,
          hazard_name = NA_character_
        ))
      }

      # Start with primary indicator rows
      filtered <- full_inv |>
        dplyr::filter(
          .data$hazard_type == hazard_type_val,
          .data$hazard_indicator == hazard_indicator_val
        )

      # Dynamically filter by all provided index values
      for (idx_col in names(index_values)) {
        if (idx_col %in% names(filtered)) {
          val <- index_values[[idx_col]]
          # Handle numeric return_period
          if (idx_col == "return_period") val <- as.numeric(val)
          
          filtered <- filtered |>
            dplyr::filter(.data[[idx_col]] == !!val)
        }
      }

      hazard_name_val <- if (nrow(filtered) > 0) filtered$hazard_name[[1]] else NA_character_

      list(
        hazard_indicator = hazard_indicator_val,
        hazard_name = hazard_name_val
      )
    }

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

      lookup <- lookup_hazard_entry(
        hazard_type_val = haz_type,
        index_values = index_values
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

        export_cols <- c(
          "event_id",
          "hazard_type",
          "hazard_indicator",
          "hazard_name",
          "scenario_name",
          "return_period",
          "event_year",
          "season"
        )

        export_df <- current_events |>
          dplyr::select(dplyr::any_of(export_cols))

        writexl::write_xlsx(as.data.frame(export_df), path = file)
      }
    )

    # Delete event function
    delete_event <- function(event_id) {
      cur <- events_rv()
      if (is.null(cur) || nrow(cur) == 0) {
        return()
      }
      # Filter out the event with matching event_id
      updated <- cur |>
        dplyr::filter(.data$event_id != !!event_id)
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
      if (is.null(file_path) || !file.exists(file_path)) {
        return()
      }

      uploaded <- try(readxl::read_excel(file_path), silent = TRUE)

      if (inherits(uploaded, "try-error") || is.null(uploaded) || nrow(uploaded) == 0) {
        message("[mod_hazards_events] Failed to read hazard configuration from external upload.")
        return()
      }

      # Check for required columns based on hazard types being loaded
      # We'll be flexible: if it has hazard_type and event_year, we try to resolve indices
      if (!all(c("hazard_type", "event_year") %in% names(uploaded))) {
        message("[mod_hazards_events] External configuration missing required columns: hazard_type, event_year")
        return()
      }

      # Rename 'gwl' to 'scenario_name' if present for backward compatibility
      if ("gwl" %in% names(uploaded) && !"scenario_name" %in% names(uploaded)) {
        uploaded <- uploaded |> dplyr::rename(scenario_name = "gwl")
      }

      processed <- tibble::as_tibble(uploaded) |>
        dplyr::mutate(
          hazard_type = as.character(.data$hazard_type),
          event_year = as.integer(.data$event_year)
        )

      # Convert all other potential index columns to character for now
      potential_indices <- setdiff(names(processed), c("hazard_type", "event_year", "event_id", "hazard_indicator", "hazard_name"))
      for (col in potential_indices) {
        if (col == "return_period") {
          processed[[col]] <- as.numeric(processed[[col]])
        } else {
          processed[[col]] <- as.character(processed[[col]])
        }
      }

      rows <- split(processed, seq_len(nrow(processed)))

      reconstructed <- purrr::imap_dfr(rows, function(row_df, idx) {
        hazard_type_val <- row_df$hazard_type[[1]]
        event_year_val <- row_df$event_year[[1]]
        
        # Get index configuration for this hazard type
        cfg <- try(hazard_configs(), silent = TRUE)
        index_ind <- get_index_indicator(cfg, hazard_type_val)
        index_cols <- if (!is.na(index_ind)) cfg[[hazard_type_val]]$indicators[[index_ind]]$index else character(0)
        
        # Collect index values from the row
        index_values <- list()
        for (idx_col in index_cols) {
          # Handle gwl/scenario_name alias
          alt_col <- if (idx_col == "gwl") "scenario_name" else if (idx_col == "scenario_name") "gwl" else NULL
          
          if (idx_col %in% names(row_df)) {
            index_values[[idx_col]] <- row_df[[idx_col]][[1]]
          } else if (!is.null(alt_col) && alt_col %in% names(row_df)) {
            index_values[[idx_col]] <- row_df[[alt_col]][[1]]
          }
        }

        lookup <- lookup_hazard_entry(
          hazard_type_val = hazard_type_val,
          index_values = index_values
        )

        hazard_indicator_val <- if ("hazard_indicator" %in% names(row_df) &&
          !is.na(row_df$hazard_indicator[[1]]) &&
          nzchar(row_df$hazard_indicator[[1]])) {
          as.character(row_df$hazard_indicator[[1]])
        } else {
          lookup$hazard_indicator
        }

        hazard_name_val <- if ("hazard_name" %in% names(row_df) &&
          !is.na(row_df$hazard_name[[1]]) &&
          nzchar(row_df$hazard_name[[1]])) {
          as.character(row_df$hazard_name[[1]])
        } else {
          lookup$hazard_name
        }

        if (is.na(hazard_indicator_val) || is.na(hazard_name_val) || hazard_name_val == "") {
          message(
            "[mod_hazards_events] Skipping external upload row; unable to resolve hazard metadata for: ",
            hazard_type_val, " with indices ", paste(names(index_values), index_values, sep="=", collapse=", ")
          )
          return(tibble::tibble())
        }

        event_id_val <- if ("event_id" %in% names(row_df) &&
          !is.na(row_df$event_id[[1]]) &&
          nzchar(row_df$event_id[[1]])) {
          as.character(row_df$event_id[[1]])
        } else {
          paste0("ev", idx)
        }

        res_row <- tibble::tibble(
          event_id = event_id_val,
          hazard_type = hazard_type_val,
          hazard_indicator = hazard_indicator_val,
          hazard_name = hazard_name_val,
          event_year = as.integer(event_year_val)
        )
        
        # Add all index columns
        for (idx_col in index_cols) {
          val <- index_values[[idx_col]]
          if (idx_col == "return_period") val <- as.numeric(val)
          res_row[[idx_col]] <- val
        }
        
        # Backward compatibility columns
        if (!"season" %in% names(res_row)) {
          res_row$season <- if ("season" %in% names(row_df)) as.character(row_df$season[[1]]) else NA_character_
        }
        
        if (!"scenario_name" %in% names(res_row)) {
          if ("scenario_name" %in% names(row_df)) {
            res_row$scenario_name <- as.character(row_df$scenario_name[[1]])
          } else if ("gwl" %in% names(res_row)) {
            res_row$scenario_name <- as.character(res_row$gwl)
          } else {
            res_row$scenario_name <- NA_character_
          }
        }

        return(res_row)
      })

      if (nrow(reconstructed) == 0) {
        message("[mod_hazards_events] External configuration did not contain any valid hazard rows.")
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
