#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Reactive values to track state
  values <- reactiveValues(
    data_loaded = FALSE,
    results_ready = FALSE,
    results = NULL,
    status = "Ready to load data",
    hazards_loaded = NULL,
    hazards_cache = list() # cache downscaled hazards by factor
  )
  
  # Create the reactive variables expected by tests
  data_loaded <- reactive({ values$data_loaded })
  results_ready <- reactive({ values$results_ready })
  results <- reactive({ values$results })
  
  # Get base_dir from golem options only
  get_base_dir <- reactive({
    # Only use golem options (when called via run_app with base_dir)
    golem_base_dir <- golem::get_golem_options("base_dir")
    if (!is.null(golem_base_dir) && golem_base_dir != "") {
      return(golem_base_dir)
    }
    return(NULL)
  })
  
  # Status output
  output$status_text <- renderText({
    values$status
  })
  
  # Helper: get hazards at selected resolution factor (1 = original)
  get_hazards_at_factor <- reactive({
    base_dir <- get_base_dir()
    if (is.null(base_dir) || base_dir == "") return(NULL)
    factor <- as.integer(shiny::req(input$hazard_resolution))
    # Load original once
    if (is.null(values$hazards_loaded)) {
      dir_hz <- file.path(base_dir, "hazards")
      if (!dir.exists(dir_hz)) return(NULL)
      values$hazards_loaded <- load_hazards(dir_hz)
    }
    if (isTRUE(factor <= 1L)) return(values$hazards_loaded)
    # Cached?
    if (!is.null(values$hazards_cache[[as.character(factor)]])) {
      return(values$hazards_cache[[as.character(factor)]])
    }
    # Downscale and cache
    hz_ds <- downscale_hazard_rasters(values$hazards_loaded, factor = factor)
    values$hazards_cache[[as.character(factor)]] <- hz_ds
    hz_ds
  })
  
  # Prepare hazards inventory and module
  # Load hazards inventory on startup (without waiting for Run)
  hazards_inventory <- reactive({
    base_dir <- get_base_dir()
    if (is.null(base_dir) || base_dir == "") return(data.frame())
    haz <- get_hazards_at_factor()
    if (is.null(haz)) return(data.frame())
    inv <- try(list_hazard_inventory(haz), silent = TRUE)
    if (inherits(inv, "try-error")) data.frame() else inv
  })
  hz_mod <- mod_hazards_events_server("hazards", hazards_inventory = hazards_inventory)

  # Run analysis when button is clicked
  observeEvent(input$run_analysis, {
    base_dir <- get_base_dir()
    
    # Guard clauses
    if (is.null(base_dir) || base_dir == "") {
      values$status <- "Error: Base directory is not set. Please restart the app with a valid base_dir."
      return()
    }
    if (is.null(input$company_file) || is.null(input$company_file$datapath) || input$company_file$datapath == "") {
      values$status <- "Error: Please upload a company.csv file before running the analysis."
      return()
    }

    values$status <- "Loading data..."
    
    tryCatch({
      # Load inputs separately
      assets <- read_assets(base_dir)
      companies <- read_companies(input$company_file$datapath)
      
      hazards <- get_hazards_at_factor()
      if (is.null(hazards)) stop("Hazards could not be loaded")
      areas <- load_location_areas(
        file.path(base_dir, "areas", "municipality"),
        file.path(base_dir, "areas", "province")
      )
      damage_factors <- read_damage_cost_factors(base_dir)
      
      values$data_loaded <- TRUE
      values$status <- "Data loaded. Running analysis..."
      
      # Build events from module inputs; if none, default to single acute event 2030 using first hazard
      ev_df <- try(hz_mod$events(), silent = TRUE)
      if (inherits(ev_df, "try-error") || !is.data.frame(ev_df) || nrow(ev_df) == 0) {
        # Provide a default using first hazard_type and its first scenario
        inv <- list_hazard_inventory(hazards)
        default_ht <- unique(inv$hazard_type)[1]
        default_sc <- inv$scenario[inv$hazard_type == default_ht][1]
        ev_df <- data.frame(event_id = "ev1", hazard_type = default_ht, scenario = default_sc, event_year = 2030L, chronic = FALSE, stringsAsFactors = FALSE)
      }
      
      # Run the complete climate risk analysis using the same approach as the console
      results <- compute_risk(
        assets = assets,
        companies = companies,
        hazards = hazards,
        areas = areas,
        damage_factors = damage_factors,
        events = ev_df,
        growth_rate = 0.02,
        net_profit_margin = 0.1,
        discount_rate = 0.05
      )
      
      values$results_ready <- TRUE
      values$results <- results
      values$status <- "Analysis complete. Download results or inspect tables."
      
      # Outputs
      output$results_summary <- renderTable({
        if (!values$results_ready) return(NULL)
        head(values$results$companies)
      })
      output$results_table <- DT::renderDataTable({
        if (!values$results_ready) return(NULL)
        DT::datatable(values$results$assets)
      })
    }, error = function(e) {
      values$status <- paste0("Error during analysis: ", conditionMessage(e))
    })
  })
}
