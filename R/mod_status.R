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
        shiny::p(
          "Click the delete button to remove individual events.",
          class = "text-muted",
          style = "margin-bottom: 1rem; font-size: 0.9em;"
        ),
        shiny::div(
          class = "events-table-container",
          DT::dataTableOutput(ns("events_table"))
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
#' @param delete_event_callback function to delete an event by event_id
#' @export
mod_status_server <- function(id, status_reactive, events_reactive, delete_event_callback = NULL) {
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

    # Events table with delete buttons
    output$events_table <- DT::renderDataTable({
      events <- try(events_reactive(), silent = TRUE)
      if (inherits(events, "try-error") || is.null(events) || nrow(events) == 0) {
        # Log error information to console
        if (inherits(events, "try-error")) {
          log_module_error(
            error = attr(events, "condition"),
            module_name = "mod_status_server",
            function_name = "events_table renderDataTable"
          )
        }

        # Return empty table with message
        return(
          DT::datatable(
            tibble::tibble(Message = "No events configured - will use default event"),
            options = list(dom = "t"),
            rownames = FALSE
          )
        )
      }

      # Prepare display data (exclude season column - it's now embedded in hazard_name)
      display_data <- events |>
        dplyr::select("event_id", "hazard_type", "hazard_name", "scenario_name", "hazard_return_period", "event_year") |>
        dplyr::rename(
          "Event ID" = "event_id",
          "Hazard Type" = "hazard_type",
          "Hazard Name" = "hazard_name",
          "Scenario" = "scenario_name",
          "Return Period (years)" = "hazard_return_period",
          "Shock Year" = "event_year"
        )

      # Add delete buttons column
      if (nrow(display_data) > 0 && !is.null(delete_event_callback)) {
        # Create delete buttons for each row
        delete_buttons <- purrr::map_chr(seq_len(nrow(events)), function(i) {
          event_id <- events$event_id[i]
          # Create button HTML with onclick handler
          paste0(
            '<button class="btn btn-danger btn-sm" onclick="Shiny.setInputValue(\'',
            ns('delete_event'),
            '\', \'',
            event_id,
            '\', {priority: \'event\'});" style="padding: 2px 8px; margin: 0;">',
            '<i class="fa fa-trash"></i>',
            '</button>'
          )
        })
        display_data$Actions <- delete_buttons
      }

      # Find "Event ID" column index in display_data (after renaming)
      event_id_col_idx <- which(names(display_data) == "Event ID") - 1

      DT::datatable(
        display_data,
        escape = FALSE,
        selection = "none",
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          dom = "ftp",
          columnDefs = list(
            list(targets = event_id_col_idx, visible = FALSE)  # Hide Event ID column
          )
        ),
        rownames = FALSE
      )
    })

    # Handle delete event action
    shiny::observeEvent(input$delete_event, {
      event_id <- input$delete_event
      if (!is.null(delete_event_callback) && !is.null(event_id) && event_id != "") {
        delete_event_callback(event_id)
      }
    })
  })
}
