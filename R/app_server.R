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
    results = NULL,
    status = "Ready to load data"
  )
  
  # Create the reactive variables expected by tests
  data_loaded <- reactive({ values$data_loaded })
  results_ready <- reactive({ !is.null(values$results) })
  results <- reactive({ values$results })
  
  # Get base_dir from golem options
  get_base_dir <- reactive({
    golem_base_dir <- golem::get_golem_options("base_dir")
    if (!is.null(golem_base_dir) && golem_base_dir != "") {
      return(golem_base_dir)
    }
    return(NULL)
  })
  
  # Initialize control module
  control <- mod_control_server("control", base_dir_reactive = get_base_dir)
  
  # Initialize status module
  mod_status_server(
    "status",
    status_reactive = reactive({ values$status }),
    events_reactive = control$events
  )
  
  # Initialize results modules
  mod_results_assets_server("results_assets", results_reactive = results)
  mod_results_companies_server("results_companies", results_reactive = results)
  
  # Check and precompute assets factors on startup
  observe({
    base_dir <- get_base_dir()
    if (!is.null(base_dir) && base_dir != "") {
      # Check if precomputed assets factors exist and are complete
      hazards_dir <- file.path(base_dir, "hazards")
      precomputed_file <- file.path(hazards_dir, "assets_factors_precomputed.rds")
      
      if (!file.exists(precomputed_file)) {
        values$status <- "Precomputing assets factors (this may take a while on first run)..."
        
        # Load required data for precomputation
        tryCatch({
          assets <- read_assets(base_dir)
          hazards <- load_hazards(hazards_dir)
          areas <- load_location_areas(
            file.path(base_dir, "areas", "municipality"),
            file.path(base_dir, "areas", "province")
          )
          damage_factors <- read_damage_cost_factors(base_dir)
          
          # Progress callback for UI updates
          progress_callback <- function(processed, total, message) {
            progress_pct <- round((processed / total) * 100, 1)
            values$status <- paste0("Precomputing assets factors: ", message, " (", processed, "/", total, " - ", progress_pct, "%)")
          }
          
          # Precompute assets factors
          precomputed_file <- precompute_assets_factors(
            assets = assets,
            hazards = hazards,
            areas = areas,
            damage_factors = damage_factors,
            hazards_dir = hazards_dir,
            progress_callback = progress_callback
          )
          
          values$status <- "Assets factors precomputed successfully. Ready to run analysis."
          
        }, error = function(e) {
          values$status <- paste0("Error precomputing assets factors: ", conditionMessage(e))
        })
      } else {
        # Check if precomputed file is complete
        tryCatch({
          cached_data <- readRDS(precomputed_file)
          if (is.data.frame(cached_data) && nrow(cached_data) > 0) {
            values$status <- "Using cached assets factors. Ready to run analysis."
          } else {
            # File exists but is incomplete, recompute
            values$status <- "Cached assets factors incomplete, recomputing..."
            # Trigger recomputation by removing the file
            file.remove(precomputed_file)
          }
        }, error = function(e) {
          # File corrupted, recompute
          values$status <- "Cached assets factors corrupted, recomputing..."
          file.remove(precomputed_file)
        })
      }
    }
  })
  
  # Run analysis when button is clicked
  observeEvent(control$run_trigger(), {
    base_dir <- get_base_dir()
    company_file <- control$company_file()
    
    # Guard clauses
    if (is.null(base_dir) || base_dir == "") {
      values$status <- "Error: Base directory is not set. Please restart the app with a valid base_dir."
      return()
    }
    if (is.null(company_file) || is.null(company_file$datapath) || company_file$datapath == "") {
      values$status <- "Error: Please upload a company.csv file before running the analysis."
      return()
    }

    values$status <- "Loading data..."
    
    tryCatch({
      # Load inputs
      assets <- read_assets(base_dir)
      companies <- read_companies(company_file$datapath)
      
      hazards <- control$get_hazards_at_factor()
      if (is.null(hazards)) stop("Hazards could not be loaded")
      
      areas <- load_location_areas(
        file.path(base_dir, "areas", "municipality"),
        file.path(base_dir, "areas", "province")
      )
      damage_factors <- read_damage_cost_factors(base_dir)
      
      values$data_loaded <- TRUE
      values$status <- "Data loaded. Running analysis..."
      
      # Build events from control module
      ev_df <- try(control$events()(), silent = TRUE)
      if (inherits(ev_df, "try-error") || !is.data.frame(ev_df) || nrow(ev_df) == 0) {
        # Provide a default using first hazard_type and its first scenario
        inv <- list_hazard_inventory(hazards)
        default_ht <- unique(inv$hazard_type)[1]
        default_sc <- inv$scenario[inv$hazard_type == default_ht][1]
        ev_df <- data.frame(
          event_id = "ev1", 
          hazard_type = default_ht, 
          scenario = default_sc, 
          event_year = 2030L, 
          chronic = FALSE, 
          stringsAsFactors = FALSE
        )
      }
      
      # Use precomputed assets factors for faster analysis
      precomputed_file <- file.path(file.path(base_dir, "hazards"), "assets_factors_precomputed.rds")
      
      # Run the complete climate risk analysis using precomputed assets factors
      results <- compute_risk(
        assets = assets,
        companies = companies,
        events = ev_df,
        precomputed_assets_factors = precomputed_file,
        growth_rate = 0.02,
        net_profit_margin = 0.1,
        discount_rate = 0.05
      )
      
      values$results <- results
      control$set_results(results)
      values$status <- "Analysis complete. Check the Asset and Company Analysis tabs for detailed results."
      
      # Switch to results tab after completion
      updateTabsetPanel(session, "main_tabs", selected = "assets")
      
    }, error = function(e) {
      values$status <- paste0("Error during analysis: ", conditionMessage(e))
    })
  })
}
