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

  # Initialize status module
  mod_status_server(
    "status",
    status_reactive = reactive({
      values$status
    }),
    events_reactive = control$events
  )

  # Initialize results modules
  mod_results_assets_server("results_assets", results_reactive = results)
  mod_results_companies_server("results_companies", results_reactive = results)

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
      values$status <- "Error: Please upload a company.csv file before running the analysis."
      return()
    }

    values$status <- "Loading data..."

    tryCatch(
      {
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

        # Build events from control module (single call; events is a reactiveVal)
        ev_df <- try(control$events(), silent = TRUE)
        if (inherits(ev_df, "try-error") || !is.data.frame(ev_df) || nrow(ev_df) == 0) {
          # Provide a default using first hazard_type and its first scenario
          inv <- list_hazard_inventory(hazards)
          default_ht <- unique(inv$hazard_type)[1]
          default_hn <- inv$hazard_name[inv$hazard_type == default_ht][1]
          ev_df <- data.frame(
            event_id = "ev1",
            hazard_type = default_ht,
            hazard_name = default_hn,
            event_year = 2030L,
            chronic = FALSE,
            stringsAsFactors = FALSE
          )
        }

        # Reconcile events with currently loaded hazards (exact name match)
        haz_names <- names(hazards)
        if ("hazard_name" %in% names(ev_df)) {
          keep <- ev_df$hazard_name %in% haz_names
          if (any(!keep)) {
            missing <- unique(ev_df$hazard_name[!keep])
            message("[app_server] Dropping events with missing hazards at current resolution: ", paste(missing, collapse = ", "))
          }
          ev_df <- ev_df[keep, , drop = FALSE]
        }

        # Run the complete climate risk analysis
        results <- compute_risk(
          assets = assets,
          companies = companies,
          events = ev_df,
          hazards = hazards,
          areas = areas,
          damage_factors = damage_factors,
          growth_rate = 0.02,
          net_profit_margin = 0.1,
          discount_rate = 0.05
        )

        values$results <- results
        control$set_results(results)
        values$status <- "Analysis complete. Check the Asset and Company Analysis tabs for detailed results."

        # Switch to results tab after completion
        updateTabsetPanel(session, "main_tabs", selected = "assets")
      },
      error = function(e) {
        values$status <- paste0("Error during analysis: ", conditionMessage(e))
      }
    )
  })
}
