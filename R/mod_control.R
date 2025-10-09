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
#' @return list with reactive values for company_file, events, run_trigger, results_ready, and get_hazards_at_factor
#' @export
mod_control_server <- function(id, base_dir_reactive) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive values
    values <- shiny::reactiveValues(
      results_ready = FALSE,
      results = NULL
    )

    # Read hazards mapping
    hazards_mapping <- shiny::reactive({
      base_dir <- base_dir_reactive()
      if (is.null(base_dir) || base_dir == "") {
        return(NULL)
      }

      mapping_file <- file.path(base_dir, "hazards_metadata.csv")
      if (!file.exists(mapping_file)) {
        message("Hazards mapping file not found at: ", mapping_file)
        return(NULL)
      }
      
      mapping_df <- try(read_hazards_mapping(mapping_file), silent = TRUE)
      if (inherits(mapping_df, "try-error")) {
        message("Error reading hazards mapping: ", attr(mapping_df, "condition")$message)
        return(NULL)
      }
      
      return(mapping_df)
    })
    
    # Load hazards rasters
    get_hazards_at_factor <- shiny::reactive({
      base_dir <- base_dir_reactive()
      if (is.null(base_dir) || base_dir == "") {
        return(NULL)
      }

      mapping_df <- hazards_mapping()
      if (is.null(mapping_df)) {
        return(NULL)
      }
      
      dir_hz <- file.path(base_dir, "hazards")
      if (!dir.exists(dir_hz)) {
        message("Hazards directory not found at: ", dir_hz)
        return(NULL)
      }
      
      # Use aggregation factor of 1 (no aggregation) in the app
      agg_factor <- 1L
      
      rasters <- try(
        load_hazards_from_mapping(
          mapping_df = mapping_df,
          hazards_dir = dir_hz,
          aggregate_factor = as.integer(agg_factor)
        ),
        silent = TRUE
      )
      
      if (inherits(rasters, "try-error")) {
        message("Error loading hazards: ", attr(rasters, "condition")$message)
        return(NULL)
      }
      
      return(rasters)
    })

    # Hazards inventory
    hazards_inventory <- shiny::reactive({
      mapping_df <- hazards_mapping()
      if (is.null(mapping_df)) {
        return(tibble::tibble())
      }
      
      inv <- try(list_hazard_inventory_from_metadata(mapping_df), silent = TRUE)
      if (inherits(inv, "try-error")) {
        message("Error creating inventory: ", attr(inv, "condition")$message)
        return(tibble::tibble())
      }
      
      return(inv)
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
      hazards_inventory = hazards_inventory,
      get_hazards_at_factor = get_hazards_at_factor,
      set_results = function(results) {
        values$results <- results
        values$results_ready <- !is.null(results)
      }
    ))
  })
}
