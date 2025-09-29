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
#' @return reactive data.frame of configured events with columns: event_id, hazard_type, hazard_name, event_year, chronic
#' @export
mod_hazards_events_server <- function(id, hazards_inventory) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    events_rv <- shiny::reactiveVal(data.frame(
      event_id = character(),
      hazard_type = character(),
      hazard_name = character(),
      event_year = integer(),
      chronic = logical(),
      stringsAsFactors = FALSE
    ))

    # Counter for dynamic UIs
    counter <- shiny::reactiveVal(1L)

    # Add event button
    shiny::observeEvent(input$add_event, {
      k <- counter()
      # Only append if both hazard and hazard name are chosen
      haz <- input[[paste0("hazard_", k)]]
      hn <- input[[paste0("hazard_name_", k)]]
      if (is.null(haz) || is.null(hn) || length(hn) == 0) {
        # If the current form is empty, just increment counter to create a new empty form
        counter(k + 1L)
        return()
      }
      new_row <- data.frame(
        event_id = paste0("ev", nrow(events_rv()) + 1L),
        hazard_type = haz,
        hazard_name = hn,
        event_year = if (isTRUE(input[[paste0("chronic_", k)]])) NA_integer_ else as.integer(input[[paste0("year_", k)]]),
        chronic = isTRUE(input[[paste0("chronic_", k)]]),
        stringsAsFactors = FALSE
      )
      cur <- events_rv()
      events_rv(rbind(cur, new_row))
      counter(k + 1L)
    })

    # Render dynamic UI for current event only
    output$events_ui <- shiny::renderUI({
      k <- counter()
      
      inv <- try(hazards_inventory(), silent = TRUE)
      hazard_choices <- character(0)
      hazard_name_choices <- character(0)
      if (!inherits(inv, "try-error") && is.data.frame(inv) && nrow(inv) > 0) {
        hazard_choices <- unique(inv$hazard_type)
        if (length(hazard_choices) > 0) {
          hazard_name_choices <- unique(inv$hazard_name[inv$hazard_type == hazard_choices[[1]]])
        }
      }
      
      # Only show form for current event (index k)
      shiny::wellPanel(
        shiny::selectInput(ns(paste0("hazard_", k)), "Hazard", choices = hazard_choices, selected = if (length(hazard_choices) > 0) hazard_choices[[1]] else NULL),
        shiny::uiOutput(ns(paste0("hazard_name_ui_", k))),
        shiny::checkboxInput(ns(paste0("chronic_", k)), label = "Chronic hazard (every year)", value = FALSE),
        shiny::uiOutput(ns(paste0("year_ui_", k)))
      )
    })

    # Hazard name UI reacts to hazard selection for current event
    shiny::observe({
      k <- counter()
      if (k == 0) return()
      
      output[[paste0("hazard_name_ui_", k)]] <- shiny::renderUI({
        inv <- try(hazards_inventory(), silent = TRUE)
        hazard_val <- input[[paste0("hazard_", k)]]
        names_vec <- character(0)
        if (!inherits(inv, "try-error") && is.data.frame(inv) && !is.null(hazard_val)) {
          names_vec <- unique(inv$hazard_name[inv$hazard_type == hazard_val])
        }
        shiny::selectInput(ns(paste0("hazard_name_", k)), "Hazard Name", choices = names_vec, selected = if (length(names_vec) > 0) names_vec[[1]] else NULL)
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


