#' control UI Function
#'
#' @description Sidebar control module for company upload, hazard selection, and analysis controls
#' @param id,input,output,session Internal parameters for {shiny}
#' @export
mod_control_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::div(
      class = "control-section",
      shiny::h4("Data Upload", class = "section-header"),
      shiny::fileInput(
        ns("company_file"),
        "Company CSV File:",
        accept = ".csv",
        placeholder = "Choose company.csv file"
      )
    ),
    shiny::div(
      class = "control-section",
      shiny::h4("Hazard Resolution", class = "section-header"),
      shiny::selectInput(
        ns("agg_factor"),
        "Aggregation Factor:",
        choices = c("1" = 1, "16" = 16, "64" = 64, "128" = 128),
        selected = 1
      ),
      shiny::helpText("Higher values = faster processing, lower spatial resolution")
    ),
    shiny::div(
      class = "control-section",
      mod_hazards_events_ui(ns("hazards"), title = "Hazard Events")
    ),
    shiny::div(
      class = "control-section run-section",
      shiny::actionButton(
        ns("run_analysis"),
        "Run Climate Risk Analysis",
        class = "btn-primary btn-lg btn-block",
        icon = shiny::icon("play")
      )
    ),
    shiny::div(
      class = "control-section download-section",
      shiny::conditionalPanel(
        condition = "output.results_ready",
        ns = ns,
        shiny::downloadButton(
          ns("download_results"),
          "Download Results",
          class = "btn-success btn-block",
          icon = shiny::icon("download")
        )
      )
    )
  )
}

#' control Server Functions
#'
#' @param base_dir_reactive reactive containing base directory path
#' @return list with reactive values for company_file, events, run_trigger, results_ready, aggregation_factor, and get_hazards_at_factor
#' @export
mod_control_server <- function(id, base_dir_reactive) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive values
    values <- shiny::reactiveValues(
      results_ready = FALSE,
      results = NULL
    )

    get_hazards_at_factor <- shiny::reactive({
      base_dir <- base_dir_reactive()
      if (is.null(base_dir) || base_dir == "") {
        return(NULL)
      }

      dir_hz <- file.path(base_dir, "hazards")
      if (!dir.exists(dir_hz)) {
        return(NULL)
      }

      # Use the aggregation factor from the slider
      agg_factor <- input$agg_factor
      if (is.null(agg_factor)) agg_factor <- 16L

      load_hazards(dir_hz, aggregate_factor = as.integer(agg_factor))
    })

    # Hazards inventory
    hazards_inventory <- shiny::reactive({
      base_dir <- base_dir_reactive()
      if (is.null(base_dir) || base_dir == "") {
        return(data.frame())
      }
      haz <- get_hazards_at_factor()
      if (is.null(haz) || length(haz) == 0) {
        return(data.frame())
      }
      inv <- try(list_hazard_inventory(haz), silent = TRUE)
      if (inherits(inv, "try-error")) data.frame() else inv
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
      run_trigger = shiny::reactive({
        input$run_analysis
      }),
      results_ready = shiny::reactive({
        values$results_ready
      }),
      results = shiny::reactive({
        values$results
      }),
      aggregation_factor = shiny::reactive({
        agg <- input$agg_factor
        if (is.null(agg)) 16L else as.integer(agg)
      }),
      hazards_inventory = hazards_inventory,
      get_hazards_at_factor = get_hazards_at_factor,
      set_results = function(results) {
        values$results <- results
        values$results_ready <- !is.null(results)
      }
    ))
  })
}
