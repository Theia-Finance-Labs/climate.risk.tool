#' status UI Function
#'
#' @description Module to display analysis status and configured events
#' @param id Internal parameter for shiny
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
#' @param id Internal parameter for shiny
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
    output$events_table <- shiny::renderTable(
      {
        events <- try(events_reactive(), silent = TRUE)
        if (inherits(events, "try-error") || is.null(events) || nrow(events) == 0) {
          # Log error information to console
          if (inherits(events, "try-error")) {
            log_module_error(
              error = attr(events, "condition"),
              module_name = "mod_status_server",
              function_name = "events_table renderTable"
            )
          }

          tibble::tibble(
            Message = "No events configured - will use default event"
          )
        } else {
          # Include event_id as first column and rename for better display
          events |>
            dplyr::select("event_id", dplyr::everything()) |>
            dplyr::rename(
              "Event ID" = .data$event_id,
              "Hazard Type" = .data$hazard_type,
              "Hazard Name" = .data$hazard_name,
              "Scenario" = .data$scenario_name,
              "Return Period (years)" = .data$hazard_return_period,
              "Shock Year" = .data$event_year
            )
        }
      },
      bordered = TRUE,
      hover = TRUE
    )
  })
}
