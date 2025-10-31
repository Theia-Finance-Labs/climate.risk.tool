#' The application server-side
#'
#' @param input,output,session Internal parameters for shiny.
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
  data_loaded <- reactive({
    values$data_loaded
  })
  results_ready <- reactive({
    !is.null(values$results)
  })
  results <- reactive({
    values$results
  })

  # Get base_dir from golem options
  get_base_dir <- reactive({
    golem_base_dir <- golem::get_golem_options("base_dir")
    if (!is.null(golem_base_dir) && golem_base_dir != "") {
      return(golem_base_dir)
    }

    # Fallback: try to get from environment variable (useful for testing)
    env_base_dir <- Sys.getenv("CLIMATE_RISK_BASE_DIR")
    if (env_base_dir != "") {
      return(env_base_dir)
    }

    return(NULL)
  })

  # Initialize control module
  control <- mod_control_server("control", base_dir_reactive = get_base_dir)

  # Event deletion callback
  delete_event_callback <- function(event_id) {
    if (event_id == "all") {
      # Clear all events
      message("[app_server] Clearing all events")
      control$clear_events()
      values$status <- "All events cleared. Add new events and run analysis."
    }
  }

  # Initialize status module
  status_module <- mod_status_server(
    "status",
    status_reactive = reactive({
      values$status
    }),
    events_reactive = control$events,
    results_reactive = results,
    delete_event_callback = delete_event_callback
  )

  # Initialize new visualization modules
  mod_hazard_maps_server(
    "hazard_maps",
    results_reactive = results,
    events_reactive = control$events,
    hazards_reactive = control$get_hazards_at_factor,
    base_dir_reactive = get_base_dir
  )

  mod_profit_pathways_server(
    "profit_pathways",
    results_reactive = results
  )

  mod_company_analysis_server(
    "company_analysis",
    results_reactive = results
  )

  # Check data directory on startup
  observe({
    base_dir <- get_base_dir()
    if (!is.null(base_dir) && base_dir != "") {
      values$status <- "Ready to run analysis."
    } else {
      values$status <- "Please set base directory to get started."
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
      values$status <- "Error: Please upload a company.xlsx file before running the analysis."
      return()
    }

    values$status <- "Loading data..."

    tryCatch(
      {
        # Load inputs
        assets <- read_assets(base_dir)
        companies <- read_companies(company_file$datapath)

        hazards <- control$get_hazards_at_factor()
        if (is.null(hazards) || length(hazards) == 0) stop("Hazards could not be loaded")

        precomputed_hazards <- read_precomputed_hazards(base_dir)
        damage_factors <- read_damage_cost_factors(base_dir)
        cnae_exposure <- read_cnae_labor_productivity_exposure(base_dir)

        # Load ADM1 and ADM2 boundaries for province assignment and validation
        province_path <- file.path(base_dir, "areas", "province", "geoBoundaries-BRA-ADM1_simplified.geojson")
        municipality_path <- file.path(base_dir, "areas", "municipality", "geoBoundaries-BRA-ADM2_simplified.geojson")

        adm1_boundaries <- sf::st_read(province_path, quiet = TRUE)

        adm2_boundaries <- sf::st_read(municipality_path, quiet = TRUE)

        values$data_loaded <- TRUE
        values$status <- "Data loaded. Running analysis..."

        # Build events from control module (single call; events is a reactiveVal)
        ev_df <- try(control$events(), silent = TRUE)
        if (inherits(ev_df, "try-error") || !(tibble::is_tibble(ev_df) || is.data.frame(ev_df)) || nrow(ev_df) == 0) {
          values$status <- "Error: Please select at least one hazard event before running the analysis. Use the 'Add hazard' button to configure hazard events."
          return()
        }

        # Reconcile events with currently loaded hazards using inventory
        # For TIF: inventory.hazard_name matches event.hazard_name (new format)
        # For NC: inventory.hazard_name matches event.hazard_name (base event without ensemble)
        if ("hazard_name" %in% names(ev_df)) {
          inventory_hazard_names <- control$hazards_inventory()$hazard_name
          keep <- ev_df$hazard_name %in% inventory_hazard_names
          if (any(!keep)) {
            missing <- unique(ev_df$hazard_name[!keep])
            message("[app_server] Dropping events with missing hazards: ", paste(missing, collapse = ", "))
          }
          ev_df <- ev_df[keep, , drop = FALSE]
        }

        # Run the complete climate risk analysis
        results <- compute_risk(
          assets = assets,
          companies = companies,
          events = ev_df,
          hazards = hazards,
          hazards_inventory = control$hazards_inventory(),
          precomputed_hazards = precomputed_hazards,
          damage_factors = damage_factors,
          cnae_exposure = cnae_exposure,
          adm1_boundaries = adm1_boundaries,
          adm2_boundaries = adm2_boundaries,
          validate_inputs = TRUE,
          growth_rate = 0.02,
          net_profit_margin = 0.1,
          discount_rate = 0.05,
          aggregation_method = "mean" # Default aggregation method
        )

        values$results <- results
        control$set_results(results)
        values$status <- "Analysis complete. Check the Hazard Maps, Profit Pathways, and Company Analysis tabs for detailed results."

        # Switch to maps tab after completion
        updateTabsetPanel(session, "main_tabs", selected = "maps")
      },
      error = function(e) {
        log_error_to_console(e, "Main app analysis")
        values$status <- paste0("Error during analysis: ", conditionMessage(e))
      }
    )
  })
}
