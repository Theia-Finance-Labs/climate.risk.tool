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
        ),
        shiny::div(
          class = "status-repro-wrapper",
          shiny::tags$details(
            class = "hazard-panel status-repro-panel",
            shiny::tags$summary(
              class = "hazard-panel__summary status-repro-panel__summary",
              shiny::tags$span(
                class = "status-repro-panel__title",
                shiny::icon("code"),
                "Reproduction Code"
              )
            ),
            shiny::div(
              class = "hazard-panel__table status-repro-panel__body",
              shiny::div(
                class = "status-repro-toolbar",
                shiny::tags$button(
                  id = ns("copy_repro_code"),
                  type = "button",
                  class = "btn btn-secondary btn-sm status-repro-copy-btn",
                  `data-copy-target` = ns("run_repro_code"),
                  onclick = "copyStatusReproCode(this);",
                  shiny::icon("copy"),
                  "Copy to Clipboard"
                )
              ),
              shiny::p(
                "R code to reproduce the current live analysis configuration.",
                class = "text-muted status-repro-description"
              ),
              shiny::div(
                class = "status-repro-code-shell",
                shiny::verbatimTextOutput(ns("run_repro_code"), placeholder = TRUE)
              )
            )
          )
        )
      )
    )
  )
}

#' @noRd
format_spatial_selection <- function(level, region_codes, region_labels) {
  parse_vals <- function(x) {
    if (is.null(x) || length(x) == 0 || is.na(x) || !nzchar(trimws(as.character(x)))) {
      return(character(0))
    }
    vals <- unlist(strsplit(as.character(x), "[|;,]"))
    vals <- trimws(vals)
    vals[nzchar(vals)]
  }

  lvl <- tolower(trimws(as.character(level)))
  if (is.na(lvl) || !nzchar(lvl) || lvl == "brazil") {
    return("Brazil (whole)")
  }

  labels <- parse_vals(region_labels)
  codes <- parse_vals(region_codes)

  level_label <- dplyr::case_when(
    lvl == "state" ~ "States",
    lvl == "municipality" ~ "Municipalities",
    lvl == "macro" ~ "Macro regions",
    lvl == "meso" ~ "Meso regions",
    lvl == "micro" ~ "Micro regions",
    TRUE ~ stringr::str_to_title(lvl)
  )

  values <- if (length(labels) > 0) labels else codes
  if (length(values) == 0) {
    return(level_label)
  }

  preview <- paste(utils::head(values, 3), collapse = ", ")
  if (length(values) > 3) {
    paste0(level_label, ": ", preview, " (+", length(values) - 3, " more)")
  } else {
    paste0(level_label, ": ", preview)
  }
}

#' status Server Functions
#'
#' @param id Internal parameter for shiny
#' @param status_reactive reactive containing current status message
#' @param events_reactive reactive containing configured events
#' @param delete_event_callback function to delete an event by event_id
#' @param run_repro_code_reactive optional reactive containing the generated run reproduction code
#' @export
mod_status_server <- function(id, status_reactive, events_reactive, delete_event_callback = NULL, run_repro_code_reactive = NULL) {
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

    output$run_repro_code <- shiny::renderText({
      code <- if (is.null(run_repro_code_reactive)) {
        "Reproduction code will appear here once the current run inputs are available."
      } else {
        run_repro_code_reactive()
      }

      session$userData$status_run_repro_code <- code
      code
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

        session$userData$status_events_table <- NULL

        # Return empty table with message
        return(
          DT::datatable(
            tibble::tibble(Message = "No events configured - Add new hazard events from the sidebar"),
            options = list(dom = "t"),
            rownames = FALSE
          )
        )
      }

      # Prepare display data (exclude season column - it's now embedded in hazard_name)
      if (!"spatial_level" %in% names(events)) events$spatial_level <- NA_character_
      if (!"spatial_region_codes" %in% names(events)) events$spatial_region_codes <- NA_character_
      if (!"spatial_region_labels" %in% names(events)) events$spatial_region_labels <- NA_character_

      display_data <- events |>
        dplyr::mutate(
          spatial_selection = mapply(
            FUN = format_spatial_selection,
            level = .data$spatial_level,
            region_codes = .data$spatial_region_codes,
            region_labels = .data$spatial_region_labels,
            SIMPLIFY = TRUE,
            USE.NAMES = FALSE
          )
        ) |>
        dplyr::select("event_id", "hazard_type", "hazard_name", "scenario_name", "return_period", "event_year", "spatial_selection") |>
        dplyr::rename(
          "Event ID" = "event_id",
          "Hazard Type" = "hazard_type",
          "Hazard Name" = "hazard_name",
          "Scenario" = "scenario_name",
          "Return Period (years)" = "return_period",
          "Shock Year" = "event_year",
          "Spatial Separation" = "spatial_selection"
        )

      # Add delete buttons column
      if (nrow(display_data) > 0 && !is.null(delete_event_callback)) {
        # Create delete buttons for each row
        delete_buttons <- purrr::map_chr(seq_len(nrow(events)), function(i) {
          event_id <- events$event_id[i]
          # Create button HTML with onclick handler
          paste0(
            '<button class="btn btn-danger btn-sm" onclick="Shiny.setInputValue(\'',
            ns("delete_event"),
            "', '",
            event_id,
            '\', {priority: \'event\'});" style="padding: 2px 8px; margin: 0;">',
            '<i class="fa fa-trash"></i>',
            "</button>"
          )
        })
        display_data$Actions <- delete_buttons
      }

      session$userData$status_events_table <- display_data

      DT::datatable(
        display_data,
        escape = FALSE,
        selection = "none",
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          dom = "ftp"
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
