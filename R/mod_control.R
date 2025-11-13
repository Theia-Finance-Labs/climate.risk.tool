#' control UI Function
#'
#' @description Sidebar control module for company upload, hazard selection, and analysis controls
#' @param id Internal parameter for shiny
#' @export
mod_control_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::div(
      class = "control-section",
      shiny::h4("Data Upload", class = "section-header"),
      shiny::fileInput(
        ns("company_file"),
        "Company Excel File:",
        accept = c(".xlsx", ".xls"),
        placeholder = "Choose company.xlsx file"
      )
    ),
    shiny::div(
      class = "control-section",
      mod_hazards_events_ui(ns("hazards"), title = "Hazard Events")
    ),
    shiny::div(
      class = "control-section",
      shiny::h4("Financial Parameters", class = "section-header"),
      shiny::p("Adjust these rates to reflect your financial assumptions:", class = "text-muted", style = "font-size: 0.9em; margin-bottom: 10px;"),
      shiny::sliderInput(
        ns("growth_rate"),
        "Revenue Growth (%):",
        value = 2.0,
        min = 0,
        max = 10,
        step = 0.1,
        ticks = FALSE
      ),
      shiny::sliderInput(
        ns("discount_rate"),
        "Discount Rate (%):",
        value = 5.0,
        min = 0,
        max = 15,
        step = 0.1,
        ticks = FALSE
      ),
      shiny::sliderInput(
        ns("risk_free_rate"),
        "Risk-Free Rate (%):",
        value = 2.0,
        min = 0,
        max = 10,
        step = 0.1,
        ticks = FALSE
      )
    ),
    shiny::div(
      class = "control-section run-section",
      shiny::actionButton(
        ns("run_analysis"),
        "Run Climate Risk Analysis",
        class = "btn-primary btn-lg btn-block",
        icon = shiny::icon("play")
      )
    )
  )
}

#' control Server Functions
#'
#' @param id Internal parameter for shiny
#' @param base_dir_reactive reactive containing base directory path
#' @return list with reactive values for company_file, events, run_trigger, growth_rate, discount_rate, risk_free_rate, results_ready, and get_hazards_at_factor
#' @export
mod_control_server <- function(id, base_dir_reactive) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive values
    values <- shiny::reactiveValues(
      results_ready = FALSE,
      results = NULL
    )

    # Load hazards and inventory (unified loader)
    hazards_and_inventory <- shiny::reactive({
      base_dir <- base_dir_reactive()
      if (is.null(base_dir) || base_dir == "") {
        return(NULL)
      }

      dir_hz <- file.path(base_dir, "hazards")
      if (!dir.exists(dir_hz)) {
        message("Hazards directory not found at: ", dir_hz)
        return(NULL)
      }

      # Use aggregation factor of 1 (no aggregation) in the app
      agg_factor <- 1L

      result <- try(
        load_hazards_and_inventory(
          hazards_dir = dir_hz,
          aggregate_factor = as.integer(agg_factor)
        ),
        silent = TRUE
      )

      if (inherits(result, "try-error") || !is.list(result)) {
        log_module_error(
          error = if (inherits(result, "try-error")) attr(result, "condition") else simpleError("Unknown error"),
          module_name = "mod_control_server",
          function_name = "hazards_and_inventory reactive"
        )

        message("Error loading hazards: ", if (inherits(result, "try-error")) attr(result, "condition")$message else "unknown")
        return(NULL)
      }

      return(result)
    })

    # Extract flattened hazards for compute
    get_hazards_at_factor <- shiny::reactive({
      result <- hazards_and_inventory()
      if (is.null(result)) {
        return(NULL)
      }

      # Flatten into a single named list for the analysis pipeline
      flat <- c(result$hazards$tif, result$hazards$nc, result$hazards$csv)
      return(flat)
    })

    # Extract inventory for UI
    hazards_inventory <- shiny::reactive({
      result <- hazards_and_inventory()
      if (is.null(result)) {
        return(tibble::tibble())
      }

      return(result$inventory)
    })


    # Hazards events module
    hz_mod <- mod_hazards_events_server("hazards", hazards_inventory = hazards_inventory)

    # Results ready output for conditional panel
    output$results_ready <- shiny::reactive({
      values$results_ready
    })
    shiny::outputOptions(output, "results_ready", suspendWhenHidden = FALSE)

    # Return reactive values and functions
    return(list(
      company_file = shiny::reactive({
        input$company_file
      }),
      events = hz_mod$events,
      delete_event = hz_mod$delete_event,
      run_trigger = shiny::reactive({
        input$run_analysis
      }),
      growth_rate = shiny::reactive({
        # Convert percentage to decimal (e.g., 2.0% -> 0.02)
        input$growth_rate / 100
      }),
      discount_rate = shiny::reactive({
        # Convert percentage to decimal (e.g., 5.0% -> 0.05)
        input$discount_rate / 100
      }),
      risk_free_rate = shiny::reactive({
        # Convert percentage to decimal (e.g., 2.0% -> 0.02)
        input$risk_free_rate / 100
      }),
      results_ready = shiny::reactive({
        values$results_ready
      }),
      results = shiny::reactive({
        values$results
      }),
      hazards_inventory = hazards_inventory,
      get_hazards_at_factor = get_hazards_at_factor,
      hazards_and_inventory = hazards_and_inventory,
      set_results = function(results) {
        values$results <- results
        values$results_ready <- !is.null(results)
      }
    ))
  })
}
