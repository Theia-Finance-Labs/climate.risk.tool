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
    status = "Ready to load data",
    # Store all loaded data files
    assets = NULL,
    hazards = NULL,
    hazards_inventory = NULL,
    precomputed_hazards = NULL,
    damage_factors = NULL,
    cnae_exposure = NULL,
    land_cover_legend = NULL,
    adm1_boundaries = NULL,
    adm2_boundaries = NULL,
    region_name_mapping = NULL
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
    events_reactive = control$events,
    delete_event_callback = control$delete_event
  )

  # Initialize results modules
  mod_results_assets_server(
    "results_assets",
    results_reactive = results,
    name_mapping_reactive = reactive({
      values$region_name_mapping
    }),
    cnae_exposure_reactive = reactive({
      values$cnae_exposure
    })
  )
  # Initialize plot modules
  mod_profit_pathways_server(
    "profit_pathways",
    results_reactive = results,
    cnae_exposure_reactive = reactive({
      values$cnae_exposure
    })
  )

  mod_company_analysis_server(
    "company_analysis",
    results_reactive = results
  )

  # Load all static data files (everything except companies which comes from file upload)
  # Reuses hazards already loaded by control module to avoid duplicate loading
  load_all_static_files <- function(base_dir) {
    tryCatch(
      {
        values$status <- "Loading data files..."

        # Load all input files
        values$assets <- read_assets(base_dir)

        # Reuse hazards and inventory from control module (already loaded for UI)
        hazards_result <- try(control$hazards_and_inventory(), silent = TRUE)
        if (inherits(hazards_result, "try-error") || is.null(hazards_result)) {
          stop("Hazards could not be loaded from control module")
        }
        values$hazards <- c(hazards_result$hazards$tif, hazards_result$hazards$nc, hazards_result$hazards$csv)
        values$hazards_inventory <- hazards_result$inventory

        # Load supporting data files
        values$precomputed_hazards <- read_precomputed_hazards(base_dir)
        values$damage_factors <- read_damage_cost_factors(base_dir)
        values$cnae_exposure <- read_cnae_labor_productivity_exposure(base_dir)
        values$land_cover_legend <- read_land_cover_legend(base_dir)

        # Load ADM1 and ADM2 boundaries for province assignment and validation
        province_path <- file.path(base_dir, "areas", "province", "geoBoundaries-BRA-ADM1_simplified.geojson")
        municipality_path <- file.path(base_dir, "areas", "municipality", "geoBoundaries-BRA-ADM2_simplified.geojson")
        values$adm1_boundaries <- sf::st_read(province_path, quiet = TRUE)
        values$adm2_boundaries <- sf::st_read(municipality_path, quiet = TRUE)

        # Load region name mapping for displaying original names in frontend
        values$region_name_mapping <- load_region_name_mapping(base_dir)

        values$status <- "Data files loaded. Ready to run analysis."
        values$data_loaded <- TRUE
      },
      error = function(e) {
        log_error_to_console(e, "Loading static data files")
        values$status <- paste0("Error loading data files: ", conditionMessage(e))
        values$data_loaded <- FALSE
      }
    )
  }

  # Load all static files when base_dir is set and hazards are available from control module
  # This loads all files immediately when the app starts with a valid base_dir
  observe({
    base_dir <- get_base_dir()
    hazards_result <- try(control$hazards_and_inventory(), silent = TRUE)

    if (!is.null(base_dir) && base_dir != "" &&
      !inherits(hazards_result, "try-error") &&
      !is.null(hazards_result)) {
      # Only load if we haven't loaded yet or if base_dir changed
      if (!values$data_loaded || is.null(values$hazards)) {
        load_all_static_files(base_dir)
      }
    } else if (!is.null(base_dir) && base_dir != "") {
      values$status <- "Loading hazards..."
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
    if (!values$data_loaded || is.null(values$hazards) || length(values$hazards) == 0) {
      values$status <- "Error: Data files not loaded. Please wait for data to finish loading."
      return()
    }

    values$status <- "Running analysis..."

    tryCatch(
      {
        # Load companies file from the uploaded file
        company_file_path <- company_file$datapath
        companies <- read_companies(company_file_path)

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
          inventory_hazard_names <- values$hazards_inventory$hazard_name
          keep <- ev_df$hazard_name %in% inventory_hazard_names
          if (any(!keep)) {
            missing <- unique(ev_df$hazard_name[!keep])
            message("[app_server] Dropping events with missing hazards: ", paste(missing, collapse = ", "))
          }
          ev_df <- ev_df[keep, , drop = FALSE]
        }

        # Run the complete climate risk analysis using pre-loaded data
        results <- compute_risk(
          assets = values$assets,
          companies = companies,
          events = ev_df,
          hazards = values$hazards,
          hazards_inventory = values$hazards_inventory,
          precomputed_hazards = values$precomputed_hazards,
          damage_factors = values$damage_factors,
          cnae_exposure = values$cnae_exposure,
          land_cover_legend = values$land_cover_legend,
          adm1_boundaries = values$adm1_boundaries,
          adm2_boundaries = values$adm2_boundaries,
          validate_inputs = TRUE,
          growth_rate = control$growth_rate(),
          discount_rate = control$discount_rate(),
          risk_free_rate = control$risk_free_rate(),
          aggregation_method = "median" # Default aggregation method
        )

        values$results <- results
        control$set_results(results)
        values$status <- "Analysis complete. Check the Profit Pathways and Company Analysis tabs for detailed results."

        # Switch to pathways tab after completion
        updateTabsetPanel(session, "main_tabs", selected = "assets")
      },
      error = function(e) {
        log_error_to_console(e, "Main app analysis")
        values$status <- paste0("Error during analysis: ", conditionMessage(e))
      }
    )
  })
}
