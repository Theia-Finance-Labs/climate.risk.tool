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
          class = "events-controls",
          style = "margin-bottom: 1rem;",
          shiny::actionButton(
            ns("clear_all_events"),
            "Clear All Events",
            class = "btn-secondary",
            icon = shiny::icon("trash")
          )
        ),
        shiny::div(
          class = "events-table-container",
          DT::dataTableOutput(ns("events_table"))
        )
      ),
      
      # Results tables section
      shiny::div(
        class = "results-section",
        style = "margin-top: 3rem;",
        shiny::h3("Analysis Results", class = "section-header"),
        
        # Assets results
        shiny::div(
          style = "margin-bottom: 2rem;",
          shiny::h4("Asset Exposure Results"),
          DT::dataTableOutput(ns("results_assets_table"))
        ),
        
        # Companies results
        shiny::div(
          style = "margin-bottom: 2rem;",
          shiny::h4("Company Financial Results"),
          DT::dataTableOutput(ns("results_companies_table"))
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
#' @param results_reactive reactive containing analysis results
#' @param delete_event_callback Function to call when deleting an event (optional)
#' @export
mod_status_server <- function(id, status_reactive, events_reactive, results_reactive = NULL, delete_event_callback = NULL) {
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

        return(NULL)
      }

      # Include event_id as first column and rename for better display
      display_events <- events |>
        dplyr::select("event_id", dplyr::everything()) |>
        dplyr::rename(
          "Event ID" = "event_id",
          "Hazard Type" = "hazard_type",
          "Hazard Name" = "hazard_name",
          "Scenario" = "scenario_name",
          "Return Period (years)" = "hazard_return_period",
          "Shock Year" = "event_year"
        )

      DT::datatable(
        display_events,
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          dom = "t"
        ),
        selection = "none",
        rownames = FALSE
      )
    })

    # Clear all events button
    shiny::observeEvent(input$clear_all_events, {
      if (!is.null(delete_event_callback)) {
        delete_event_callback("all")
      }
    })
    
    # Assets results table
    output$results_assets_table <- DT::renderDataTable({
      if (is.null(results_reactive)) {
        return(NULL)
      }
      
      results <- results_reactive()
      if (is.null(results) || is.null(results$assets_factors)) {
        return(NULL)
      }
      
      message("[mod_status] Rendering assets table, nrows=", nrow(results$assets_factors))
      
      DT::datatable(
        results$assets_factors,
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          dom = "Bfrtip",
          buttons = c("copy", "csv", "excel")
        ),
        extensions = "Buttons",
        rownames = FALSE
      )
    })
    
    # Companies results table
    output$results_companies_table <- DT::renderDataTable({
      if (is.null(results_reactive)) {
        return(NULL)
      }
      
      results <- results_reactive()
      if (is.null(results) || is.null(results$companies)) {
        return(NULL)
      }
      
      message("[mod_status] Rendering companies table, nrows=", nrow(results$companies))
      
      DT::datatable(
        results$companies,
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          dom = "Bfrtip",
          buttons = c("copy", "csv", "excel")
        ),
        extensions = "Buttons",
        rownames = FALSE
      )
    })

    # Return list with deletion trigger
    return(list(
      clear_trigger = shiny::reactive(input$clear_all_events)
    ))
  })
}
