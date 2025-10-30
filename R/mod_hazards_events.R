#' hazards_events UI Function
#'
#' @description Shiny module to build a list of hazard events to apply. Allows adding
#' multiple events with type, hazard name, and event year.
#' @param id,input,output,session Internal parameters for {shiny}
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

    # Add event button
    shiny::observeEvent(input$add_event, {
      k <- counter()
      # Get all four filter selections
      haz_type <- input[[paste0("hazard_type_", k)]]
      haz_indicator <- input[[paste0("hazard_indicator_", k)]]
      scenario <- input[[paste0("scenario_name_", k)]]
      return_period <- input[[paste0("return_period_", k)]]

      if (is.null(haz_type) || is.null(haz_indicator) || is.null(scenario) || is.null(return_period)) {
        # If the current form is incomplete, just increment counter to create a new empty form
        counter(k + 1L)
        return()
      }

      # Derive hazard_name from the four filters
      inv <- try(hazards_inventory(), silent = TRUE)
      hazard_name_val <- NA_character_
      if (!inherits(inv, "try-error") && (tibble::is_tibble(inv) || is.data.frame(inv)) && nrow(inv) > 0) {
        matched <- inv |>
          dplyr::filter(
            .data$hazard_type == haz_type,
            .data$hazard_indicator == haz_indicator,
            .data$scenario_name == scenario,
            .data$hazard_return_period == return_period
          )
        if (nrow(matched) > 0) {
          # Both TIF and NC have hazard_name column with ensemble suffix
          hazard_name_val <- matched$hazard_name[1]
        }
      } else if (inherits(inv, "try-error")) {
        # Log error information to console
        log_module_error(
          error = attr(inv, "condition"),
          module_name = "mod_hazards_events_server",
          function_name = "add_hazard_event"
        )
      }

      if (is.na(hazard_name_val)) {
        message("Could not determine hazard_name for: ", haz_type, ", ", haz_indicator, ", ", scenario, ", ", return_period)
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
        hazard_indicator = haz_indicator,
        hazard_name = hazard_name_val,
        scenario_name = scenario,
        hazard_return_period = return_period,
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

      inv <- try(hazards_inventory(), silent = TRUE)
      hazard_type_choices <- character(0)
      hazard_indicator_choices <- character(0)
      scenario_choices <- character(0)
      return_period_choices <- numeric(0)

      if (!inherits(inv, "try-error") && (tibble::is_tibble(inv) || is.data.frame(inv)) && nrow(inv) > 0) {
        hazard_type_choices <- unique(inv$hazard_type)
        if (length(hazard_type_choices) > 0) {
          # Get indicators for first hazard type
          first_hazard <- hazard_type_choices[[1]]
          hazard_indicator_choices <- unique(inv$hazard_indicator[inv$hazard_type == first_hazard])
          if (length(hazard_indicator_choices) > 0) {
            # Get scenarios for first hazard type and indicator
            first_indicator <- hazard_indicator_choices[[1]]
            scenario_choices <- unique(inv$scenario_name[
              inv$hazard_type == first_hazard & inv$hazard_indicator == first_indicator
            ])
            if (length(scenario_choices) > 0) {
              # Get return periods for first hazard type, indicator, and scenario
              first_scenario <- scenario_choices[[1]]
              return_period_choices <- unique(inv$hazard_return_period[
                inv$hazard_type == first_hazard &
                  inv$hazard_indicator == first_indicator &
                  inv$scenario_name == first_scenario
              ])
            }
          }
        }
      }

      # Only show form for current event (index k)
      shiny::wellPanel(
        shiny::selectInput(ns(paste0("hazard_type_", k)), "Hazard Type",
          choices = hazard_type_choices,
          selected = if (length(hazard_type_choices) > 0) hazard_type_choices[[1]] else NULL
        ),
        shiny::uiOutput(ns(paste0("hazard_indicator_ui_", k))),
        shiny::uiOutput(ns(paste0("scenario_name_ui_", k))),
        shiny::uiOutput(ns(paste0("return_period_ui_", k))),
        shiny::uiOutput(ns(paste0("season_ui_", k))),
        shiny::numericInput(ns(paste0("year_", k)), label = "Shock year", value = 2030, min = 2025, max = 2100, step = 1)
      )
    })

    # Create cascading dropdowns: hazard_type -> hazard_indicator -> scenario_name -> return_period
    shiny::observe({
      k <- counter()
      if (k == 0) {
        return()
      }

      # Hazard indicator UI reacts to hazard type selection
      output[[paste0("hazard_indicator_ui_", k)]] <- shiny::renderUI({
        inv <- try(hazards_inventory(), silent = TRUE)
        hazard_type_val <- input[[paste0("hazard_type_", k)]]
        indicator_vec <- character(0)
        if (!inherits(inv, "try-error") && (tibble::is_tibble(inv) || is.data.frame(inv)) && !is.null(hazard_type_val)) {
          indicator_vec <- unique(inv$hazard_indicator[inv$hazard_type == hazard_type_val])
        }
        shiny::selectInput(ns(paste0("hazard_indicator_", k)), "Hazard Indicator",
          choices = indicator_vec,
          selected = if (length(indicator_vec) > 0) indicator_vec[[1]] else NULL
        )
      })

      # Scenario name UI reacts to hazard type and indicator selection
      output[[paste0("scenario_name_ui_", k)]] <- shiny::renderUI({
        inv <- try(hazards_inventory(), silent = TRUE)
        hazard_type_val <- input[[paste0("hazard_type_", k)]]
        hazard_indicator_val <- input[[paste0("hazard_indicator_", k)]]
        scenario_vec <- character(0)
        if (!inherits(inv, "try-error") && (tibble::is_tibble(inv) || is.data.frame(inv)) &&
          !is.null(hazard_type_val) && !is.null(hazard_indicator_val)) {
          scenario_vec <- unique(inv$scenario_name[
            inv$hazard_type == hazard_type_val & inv$hazard_indicator == hazard_indicator_val
          ])
        }
        shiny::selectInput(ns(paste0("scenario_name_", k)), "Scenario",
          choices = scenario_vec,
          selected = if (length(scenario_vec) > 0) scenario_vec[[1]] else NULL
        )
      })

      # Return period UI reacts to hazard type, indicator, and scenario selection
      output[[paste0("return_period_ui_", k)]] <- shiny::renderUI({
        inv <- try(hazards_inventory(), silent = TRUE)
        hazard_type_val <- input[[paste0("hazard_type_", k)]]
        hazard_indicator_val <- input[[paste0("hazard_indicator_", k)]]
        scenario_val <- input[[paste0("scenario_name_", k)]]
        return_period_vec <- numeric(0)
        if (!inherits(inv, "try-error") && (tibble::is_tibble(inv) || is.data.frame(inv)) &&
          !is.null(hazard_type_val) && !is.null(hazard_indicator_val) && !is.null(scenario_val)) {
          return_period_vec <- unique(inv$hazard_return_period[
            inv$hazard_type == hazard_type_val &
              inv$hazard_indicator == hazard_indicator_val &
              inv$scenario_name == scenario_val
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

    # Return
    return(list(events = events_rv))
  })
}
