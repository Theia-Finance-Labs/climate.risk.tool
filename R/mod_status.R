#' status UI Function
#'
#' @description Module to display analysis status and configured events
#' @param id,input,output,session Internal parameters for {shiny}
#' @export
mod_status_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::div(
      class = "status-container",
      
      # Status section
      shiny::div(
        class = "status-section",
        shiny::h3("Analysis Status", class = "status-title"),
        shiny::div(
          class = "status-indicator",
          shiny::div(
            class = "status-badge",
            shiny::textOutput(ns("status_badge"))
          ),
          shiny::div(
            class = "status-message",
            shiny::textOutput(ns("status_message"))
          )
        )
      ),
      
      # Configured events section
      shiny::div(
        class = "events-section",
        shiny::h3("Configured Hazard Events", class = "section-header"),
        shiny::div(
          class = "events-table-container",
          shiny::tableOutput(ns("events_table"))
        )
      )
    )
  )
}

#' status Server Functions
#'
#' @param status_reactive reactive containing current status message
#' @param events_reactive reactive containing configured events
#' @export
mod_status_server <- function(id, status_reactive, events_reactive) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Status badge
    output$status_badge <- shiny::renderText({
      status <- status_reactive()
      if (grepl("Error", status, ignore.case = TRUE)) {
        "ERROR"
      } else if (grepl("complete|ready", status, ignore.case = TRUE)) {
        "READY"
      } else if (grepl("running|loading", status, ignore.case = TRUE)) {
        "RUNNING"
      } else {
        "WAITING"
      }
    })
    
    # Status message
    output$status_message <- shiny::renderText({
      status_reactive()
    })
    
    # Events table
    output$events_table <- shiny::renderTable({
      events <- try(events_reactive(), silent = TRUE)
      if (inherits(events, "try-error") || is.null(events) || nrow(events) == 0) {
        data.frame(
          Message = "No events configured - will use default event",
          stringsAsFactors = FALSE
        )
      } else {
        events
      }
    }, bordered = TRUE, hover = TRUE)
  })
}
