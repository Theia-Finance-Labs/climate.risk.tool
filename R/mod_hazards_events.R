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
#' @param hazards_inventory reactive data.frame with columns: hazard_type, hazard_indicator, scenario_name, hazard_return_period, hazard_name
#' @return reactive data.frame of configured events with columns: event_id, hazard_type, hazard_indicator, hazard_name, scenario_name, hazard_return_period, event_year, season
#' @export
mod_hazards_events_server <- function(id, hazards_inventory) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    events_rv <- shiny::reactiveVal(tibble::tibble(
      event_id = character(),
      hazard_type = character(),
      hazard_indicator = character(),
      hazard_name = character(),
      scenario_name = character(),
      hazard_return_period = numeric(),
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
      filter_inventory_for_ui(inv)
    })

    lookup_hazard_entry <- function(hazard_type_val, scenario_val, return_period_val, season_val = NA_character_) {
      hazard_indicator_val <- get_primary_indicator(hazard_type_val)

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

      filtered <- full_inv |>
        dplyr::filter(
          .data$hazard_type == hazard_type_val,
          .data$hazard_indicator == hazard_indicator_val,
          .data$scenario_name == scenario_val,
          .data$hazard_return_period == !!as.numeric(return_period_val)
        )

      if (hazard_type_val == "Drought" && !is.na(season_val) && season_val != "" && "season" %in% names(full_inv)) {
        filtered <- full_inv |>
          dplyr::filter(
            .data$hazard_type == hazard_type_val,
            .data$hazard_indicator == hazard_indicator_val,
            .data$scenario_name == scenario_val,
            .data$hazard_return_period == !!as.numeric(return_period_val),
            .data$season == season_val
          )
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
      # Get three filter selections: hazard_type, scenario, return_period
      haz_type <- input[[paste0("hazard_type_", k)]]
      scenario <- input[[paste0("scenario_name_", k)]]
      return_period <- input[[paste0("return_period_", k)]]

      if (is.null(haz_type) || is.null(scenario) || is.null(return_period)) {
        # If the current form is incomplete, just increment counter to create a new empty form
        counter(k + 1L)
        return()
      }

      # Capture season for Drought events
      event_season <- if (haz_type == "Drought") {
        season_val <- input[[paste0("season_", k)]]
        if (is.null(season_val) || season_val == "") NA_character_ else season_val
      } else {
        NA_character_
      }

      lookup <- lookup_hazard_entry(
        hazard_type_val = haz_type,
        scenario_val = scenario,
        return_period_val = return_period,
        season_val = event_season
      )

      hazard_indicator_val <- lookup$hazard_indicator
      hazard_name_val <- lookup$hazard_name

      if (is.na(hazard_indicator_val) || is.na(hazard_name_val) || hazard_name_val == "") {
        message(
          "[mod_hazards_events] Could not determine hazard metadata for: ",
          haz_type, ", ", scenario, ", ", return_period
        )
        counter(k + 1L)
        return()
      }

      new_row <- tibble::tibble(
        event_id = paste0("ev", nrow(events_rv()) + 1L),
        hazard_type = haz_type,
        hazard_indicator = hazard_indicator_val, # Primary indicator
        hazard_name = hazard_name_val, # Primary indicator's hazard_name (with season for drought)
        scenario_name = scenario,
        hazard_return_period = as.numeric(return_period),
        event_year = as.integer(input[[paste0("year_", k)]]),
        season = event_season
      )
      cur <- events_rv()
      events_rv(rbind(cur, new_row))
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
          # Get scenarios for first hazard type (primary indicator only)
          first_hazard <- hazard_type_choices[[1]]
          scenario_choices <- unique(ui_inv$scenario_name[ui_inv$hazard_type == first_hazard])

          if (length(scenario_choices) > 0) {
            # Get return periods for first hazard type and scenario (primary indicator only)
            first_scenario <- scenario_choices[[1]]
            return_period_choices <- unique(ui_inv$hazard_return_period[
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
        shiny::uiOutput(ns(paste0("scenario_name_ui_", k))),
        shiny::uiOutput(ns(paste0("return_period_ui_", k))),
        shiny::uiOutput(ns(paste0("season_ui_", k))),
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

    # Create cascading dropdowns: hazard_type -> scenario_name -> return_period
    # hazard_indicator is hidden from user but handled internally
    shiny::observe({
      k <- counter()
      if (k == 0) {
        return()
      }

      # Scenario name UI reacts to hazard type selection only
      output[[paste0("scenario_name_ui_", k)]] <- shiny::renderUI({
        ui_inv <- try(ui_inventory(), silent = TRUE)
        hazard_type_val <- input[[paste0("hazard_type_", k)]]
        scenario_vec <- character(0)
        if (!inherits(ui_inv, "try-error") && (tibble::is_tibble(ui_inv) || is.data.frame(ui_inv)) &&
          !is.null(hazard_type_val)) {
          # Get all scenarios for this hazard type (primary indicator only)
          scenario_vec <- unique(ui_inv$scenario_name[ui_inv$hazard_type == hazard_type_val])
        }
        shiny::selectInput(ns(paste0("scenario_name_", k)), "Scenario",
          choices = scenario_vec,
          selected = if (length(scenario_vec) > 0) scenario_vec[[1]] else NULL
        )
      })

      # Return period UI reacts to hazard type and scenario selection
      output[[paste0("return_period_ui_", k)]] <- shiny::renderUI({
        ui_inv <- try(ui_inventory(), silent = TRUE)
        hazard_type_val <- input[[paste0("hazard_type_", k)]]
        scenario_val <- input[[paste0("scenario_name_", k)]]
        return_period_vec <- numeric(0)
        if (!inherits(ui_inv, "try-error") && (tibble::is_tibble(ui_inv) || is.data.frame(ui_inv)) &&
          !is.null(hazard_type_val) && !is.null(scenario_val)) {
          # Get all return periods for this hazard type and scenario (primary indicator only)
          return_period_vec <- unique(ui_inv$hazard_return_period[
            ui_inv$hazard_type == hazard_type_val &
              ui_inv$scenario_name == scenario_val
          ])
        }
        shiny::selectInput(ns(paste0("return_period_", k)), "Return Period (years)",
          choices = return_period_vec,
          selected = if (length(return_period_vec) > 0) return_period_vec[[1]] else NULL
        )
      })

      # Season UI reacts to hazard type selection (only show for Drought)
      output[[paste0("season_ui_", k)]] <- shiny::renderUI({
        hazard_type_val <- input[[paste0("hazard_type_", k)]]
        if (!is.null(hazard_type_val) && hazard_type_val == "Drought") {
          shiny::selectInput(ns(paste0("season_", k)), "Drought Season",
            choices = c("Summer", "Autumn", "Winter", "Spring"),
            selected = "Summer"
          )
        } else {
          shiny::span("")
        }
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
          "hazard_return_period",
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

      required_cols <- c("hazard_type", "scenario_name", "hazard_return_period", "event_year")
      if (!all(required_cols %in% names(uploaded))) {
        message("[mod_hazards_events] External configuration missing required columns: ",
          paste(setdiff(required_cols, names(uploaded)), collapse = ", "))
        return()
      }

      processed <- tibble::as_tibble(uploaded) |>
        dplyr::mutate(
          hazard_type = as.character(.data$hazard_type),
          scenario_name = as.character(.data$scenario_name),
          hazard_return_period = as.numeric(.data$hazard_return_period),
          event_year = as.integer(.data$event_year)
        )

      if ("season" %in% names(processed)) {
        processed <- processed |>
          dplyr::mutate(season = dplyr::if_else(
            is.na(.data$season) | .data$season == "",
            NA_character_,
            as.character(.data$season)
          ))
      } else {
        processed <- processed |>
          dplyr::mutate(season = NA_character_)
      }

      rows <- split(processed, seq_len(nrow(processed)))

      reconstructed <- purrr::imap_dfr(rows, function(row_df, idx) {
        hazard_type_val <- row_df$hazard_type[[1]]
        scenario_val <- row_df$scenario_name[[1]]
        return_period_val <- row_df$hazard_return_period[[1]]
        event_year_val <- row_df$event_year[[1]]
        season_val <- row_df$season[[1]]

        lookup <- lookup_hazard_entry(
          hazard_type_val = hazard_type_val,
          scenario_val = scenario_val,
          return_period_val = return_period_val,
          season_val = season_val
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
            hazard_type_val, ", ", scenario_val, ", ", return_period_val
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

        tibble::tibble(
          event_id = event_id_val,
          hazard_type = hazard_type_val,
          hazard_indicator = hazard_indicator_val,
          hazard_name = hazard_name_val,
          scenario_name = scenario_val,
          hazard_return_period = as.numeric(return_period_val),
          event_year = as.integer(event_year_val),
          season = if (is.null(season_val) || is.na(season_val) || season_val == "") NA_character_ else season_val
        )
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
