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
      shiny::actionButton(ns("add_event"), label = "Add hazard", class = "btn-secondary"),
      style = "margin-bottom:10px;"
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

      # Get primary indicator for this hazard type (from config)
      hazard_indicator_val <- get_primary_indicator(haz_type)
      
      if (is.na(hazard_indicator_val)) {
        message("[mod_hazards_events] Unknown hazard type: ", haz_type)
        counter(k + 1L)
        return()
      }

      # Find hazard_name from UI inventory
      ui_inv <- try(ui_inventory(), silent = TRUE)
      hazard_name_val <- NA_character_
      
      if (!inherits(ui_inv, "try-error") && (tibble::is_tibble(ui_inv) || is.data.frame(ui_inv)) && nrow(ui_inv) > 0) {
        matched <- ui_inv |>
          dplyr::filter(
            .data$hazard_type == haz_type,
            .data$scenario_name == scenario,
            .data$hazard_return_period == return_period
          )
        
        if (nrow(matched) > 0) {
          # Get hazard_name from full inventory for the primary indicator
          full_inv <- try(hazards_inventory(), silent = TRUE)
          if (!inherits(full_inv, "try-error") && nrow(full_inv) > 0) {
            full_matched <- full_inv |>
              dplyr::filter(
                .data$hazard_type == haz_type,
                .data$hazard_indicator == hazard_indicator_val,
                .data$scenario_name == scenario,
                .data$hazard_return_period == return_period
              )
            
            if (nrow(full_matched) > 0) {
              hazard_name_val <- full_matched$hazard_name[1]
            }
          }
        }
      }

      if (is.na(hazard_name_val)) {
        message(
          "[mod_hazards_events] Could not determine hazard_name for: ",
          haz_type, ", ", scenario, ", ", return_period
        )
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

      new_row <- tibble::tibble(
        event_id = paste0("ev", nrow(events_rv()) + 1L),
        hazard_type = haz_type,
        hazard_indicator = hazard_indicator_val,  # Primary indicator
        hazard_name = hazard_name_val,            # Primary indicator's hazard_name
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
        hazard_type_choices <- unique(ui_inv$hazard_type)
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
        shiny::selectInput(ns(paste0("hazard_type_", k)), "Hazard Type",
          choices = hazard_type_choices,
          selected = if (length(hazard_type_choices) > 0) hazard_type_choices[[1]] else NULL
        ),
        shiny::uiOutput(ns(paste0("scenario_name_ui_", k))),
        shiny::uiOutput(ns(paste0("return_period_ui_", k))),
        shiny::uiOutput(ns(paste0("season_ui_", k))),
        shiny::numericInput(ns(paste0("year_", k)), label = "Shock year", value = 2030, min = 2025, max = 2100, step = 1)
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

    # Return
    return(list(
      events = events_rv,
      delete_event = delete_event
    ))
  })
}
