#' hazards_events UI Function
#'
#' @description Shiny module to build a list of hazard events to apply. Allows adding
#' multiple events with type, hazard name, event year, and chronic toggle.
#' @param id,input,output,session Internal parameters for {shiny}
#' @param title Character title displayed above the controls
#' @export
mod_hazards_events_ui <- function(id, title = "Hazard events") {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h4(title),
    shiny::uiOutput(ns("events_ui")),
    shiny::div(
      shiny::actionButton(ns("add_event"), label = "Add hazard", class = "btn-secondary"),
      style = "margin-bottom:10px;"
    )
  )
}

#' hazards_events Server Functions
#'
#' @param hazards_inventory reactive data.frame with columns: key, hazard_type, hazard_name
#' @return reactive data.frame of configured events with columns: event_id, hazard_type, hazard_name, scenario_name, hazard_return_period, event_year, chronic
#' @export
mod_hazards_events_server <- function(id, hazards_inventory) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    events_rv <- shiny::reactiveVal(tibble::tibble(
      event_id = character(),
      hazard_type = character(),
      hazard_name = character(),
      scenario_name = character(),
      hazard_return_period = numeric(),
      event_year = integer(),
      chronic = logical()
    ))

    # Counter for dynamic UIs
    counter <- shiny::reactiveVal(1L)

    # Add event button
    shiny::observeEvent(input$add_event, {
      k <- counter()
      # Get all three filter selections
      haz_type <- input[[paste0("hazard_type_", k)]]
      scenario <- input[[paste0("scenario_name_", k)]]
      return_period <- input[[paste0("return_period_", k)]]
      
      if (is.null(haz_type) || is.null(scenario) || is.null(return_period)) {
        # If the current form is incomplete, just increment counter to create a new empty form
        counter(k + 1L)
        return()
      }
      
      # Derive hazard_name from the three filters
      inv <- try(hazards_inventory(), silent = TRUE)
      hazard_name_val <- NA_character_
      if (!inherits(inv, "try-error") && (tibble::is_tibble(inv) || is.data.frame(inv)) && nrow(inv) > 0) {
        matched <- inv |>
          dplyr::filter(
            .data$hazard_type == haz_type,
            .data$scenario_name == scenario,
            .data$hazard_return_period == return_period
          )
        if (nrow(matched) > 0) {
          hazard_name_val <- matched$hazard_name[1]
        }
      }
      
      if (is.na(hazard_name_val)) {
        message("Could not determine hazard_name for: ", haz_type, ", ", scenario, ", ", return_period)
        counter(k + 1L)
        return()
      }
      
      new_row <- tibble::tibble(
        event_id = paste0("ev", nrow(events_rv()) + 1L),
        hazard_type = haz_type,
        hazard_name = hazard_name_val,
        scenario_name = scenario,
        hazard_return_period = return_period,
        event_year = if (isTRUE(input[[paste0("chronic_", k)]])) NA_integer_ else as.integer(input[[paste0("year_", k)]]),
        chronic = isTRUE(input[[paste0("chronic_", k)]])
      )
      cur <- events_rv()
      events_rv(rbind(cur, new_row))
      counter(k + 1L)
    })

    # Render dynamic UI for current event only
    output$events_ui <- shiny::renderUI({
      k <- counter()

      inv <- try(hazards_inventory(), silent = TRUE)
      hazard_type_choices <- character(0)
      scenario_choices <- character(0)
      return_period_choices <- numeric(0)
      
      if (!inherits(inv, "try-error") && (tibble::is_tibble(inv) || is.data.frame(inv)) && nrow(inv) > 0) {
        hazard_type_choices <- unique(inv$hazard_type)
        if (length(hazard_type_choices) > 0) {
          # Get scenarios for first hazard type
          first_hazard <- hazard_type_choices[[1]]
          scenario_choices <- unique(inv$scenario_name[inv$hazard_type == first_hazard])
          if (length(scenario_choices) > 0) {
            # Get return periods for first hazard type and scenario
            first_scenario <- scenario_choices[[1]]
            return_period_choices <- unique(inv$hazard_return_period[
              inv$hazard_type == first_hazard & inv$scenario_name == first_scenario
            ])
          }
        }
      }

      # Only show form for current event (index k)
      shiny::wellPanel(
        shiny::selectInput(ns(paste0("hazard_type_", k)), "Hazard Type", 
                           choices = hazard_type_choices, 
                           selected = if (length(hazard_type_choices) > 0) hazard_type_choices[[1]] else NULL),
        shiny::uiOutput(ns(paste0("scenario_name_ui_", k))),
        shiny::uiOutput(ns(paste0("return_period_ui_", k))),
        shiny::checkboxInput(ns(paste0("chronic_", k)), label = "Chronic hazard (every year)", value = FALSE),
        shiny::uiOutput(ns(paste0("year_ui_", k)))
      )
    })

    # Create cascading dropdowns: hazard_type -> scenario_name -> return_period
    shiny::observe({
      k <- counter()
      if (k == 0) {
        return()
      }

      # Scenario name UI reacts to hazard type selection
      output[[paste0("scenario_name_ui_", k)]] <- shiny::renderUI({
        inv <- try(hazards_inventory(), silent = TRUE)
        hazard_type_val <- input[[paste0("hazard_type_", k)]]
        scenario_vec <- character(0)
        if (!inherits(inv, "try-error") && (tibble::is_tibble(inv) || is.data.frame(inv)) && !is.null(hazard_type_val)) {
          scenario_vec <- unique(inv$scenario_name[inv$hazard_type == hazard_type_val])
        }
        shiny::selectInput(ns(paste0("scenario_name_", k)), "Scenario", 
                           choices = scenario_vec, 
                           selected = if (length(scenario_vec) > 0) scenario_vec[[1]] else NULL)
      })
      
      # Return period UI reacts to both hazard type and scenario selection
      output[[paste0("return_period_ui_", k)]] <- shiny::renderUI({
        inv <- try(hazards_inventory(), silent = TRUE)
        hazard_type_val <- input[[paste0("hazard_type_", k)]]
        scenario_val <- input[[paste0("scenario_name_", k)]]
        return_period_vec <- numeric(0)
        if (!inherits(inv, "try-error") && (tibble::is_tibble(inv) || is.data.frame(inv)) && 
            !is.null(hazard_type_val) && !is.null(scenario_val)) {
          return_period_vec <- unique(inv$hazard_return_period[
            inv$hazard_type == hazard_type_val & inv$scenario_name == scenario_val
          ])
        }
        shiny::selectInput(ns(paste0("return_period_", k)), "Return Period (years)", 
                           choices = return_period_vec, 
                           selected = if (length(return_period_vec) > 0) return_period_vec[[1]] else NULL)
      })

      output[[paste0("year_ui_", k)]] <- shiny::renderUI({
        if (isTRUE(shiny::isTruthy(input[[paste0("chronic_", k)]]))) {
          shiny::span("")
        } else {
          shiny::numericInput(ns(paste0("year_", k)), label = "Shock year", value = 2030, min = 2025, max = 2100, step = 1)
        }
      })
    })

    # Return
    return(list(events = events_rv))
  })
}
