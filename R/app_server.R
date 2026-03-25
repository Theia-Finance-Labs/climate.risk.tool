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
    hazard_configs = NULL,
    cnae_exposure = NULL,
    adm1_boundaries = NULL,
    adm2_boundaries = NULL,
    adm_codes = NULL,
    region_name_mapping = NULL
  )
  settings_modal_open <- shiny::reactiveVal(FALSE)

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

  overrides_reload <- shiny::reactiveVal(0L)

  settings_configs <- shiny::reactive({
    base_dir <- get_base_dir()
    overrides_reload()
    if (is.null(base_dir) || base_dir == "") {
      return(NULL)
    }

    hazards_dir <- file.path(base_dir, "hazards", "config")
    if (!dir.exists(hazards_dir)) {
      return(NULL)
    }

    load_hazard_configs(
      hazards_dir = hazards_dir,
      hazards_override_path = file.path(hazards_dir, "config_overrides.yml")
    )
  })

  # Initialize settings module
  settings <- mod_settings_server(
    "settings",
    base_dir_reactive = get_base_dir,
    hazard_configs_reactive = settings_configs,
    inventory_reactive = control$hazards_inventory
  )

  shiny::observeEvent(settings$reload_trigger(), {
    overrides_reload(settings$reload_trigger())
  })

  shiny::observeEvent(input$open_settings, {
    shiny::showModal(
      shiny::modalDialog(
        title = "Hazards Settings",
        mod_settings_ui("settings"),
        size = "l",
        easyClose = TRUE,
        footer = NULL,
        class = "settings-modal"
      )
    )
    settings_modal_open(TRUE)
  })

  shiny::observeEvent(input$main_tabs, {
    if (isTRUE(settings_modal_open())) {
      shiny::removeModal()
      settings_modal_open(FALSE)
    }
  }, ignoreInit = TRUE)

  # Initialize control module
  control <- mod_control_server(
    "control",
    base_dir_reactive = get_base_dir,
    overrides_reload = overrides_reload
  )

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

  # Load all static data files (everything except assets and companies which come from user-selected folder)
  # Reuses hazards already loaded by control module to avoid duplicate loading
  load_all_static_files <- function(base_dir) {
    tryCatch(
      {
        values$status <- "Loading data files..."

        # NOTE: Assets are NOT loaded here - they come from the user-selected input folder
        # when "Run Analysis" is clicked

        # Reuse hazards and inventory from control module (already loaded for UI)
        hazards_result <- try(control$hazards_and_inventory(), silent = TRUE)
        if (inherits(hazards_result, "try-error") || is.null(hazards_result)) {
          stop("Hazards could not be loaded from control module")
        }
        values$hazards <- hazards_result$hazards
        values$hazards_inventory <- hazards_result$inventory
        values$hazard_configs <- hazards_result$configs

        # Load supporting data files from base_dir
        # Pass hazard_configs to read_precomputed_hazards to ensure overrides are applied
        values$precomputed_hazards <- read_precomputed_hazards(base_dir, hazard_configs = values$hazard_configs)
        
        # Load cnae_exposure from config if Heat hazard is present
        if ("Heat" %in% names(values$hazard_configs)) {
          heat_config <- values$hazard_configs[["Heat"]]
          if (!is.null(heat_config$mappings) && "cnae_exposure" %in% names(heat_config$mappings)) {
            values$cnae_exposure <- load_mapping_from_config(base_dir, values$hazard_configs, "Heat", "cnae_exposure")
          } else {
            values$cnae_exposure <- NULL
          }
        } else {
          values$cnae_exposure <- NULL
        }

        # Load ADM1 and ADM2 boundaries for state assignment and validation
        state_path <- file.path(base_dir, "areas", "state", "geoBoundaries-BRA-ADM1_simplified.geojson")
        municipality_path <- file.path(base_dir, "areas", "municipality", "geoBoundaries-BRA-ADM2_simplified.geojson")
        values$adm1_boundaries <- sf::st_read(state_path, quiet = TRUE)
        values$adm2_boundaries <- sf::st_read(municipality_path, quiet = TRUE)
        values$adm_codes <- load_adm_codes(base_dir)

        # Load region name mapping for displaying original names in frontend
        # Pass already loaded boundaries to avoid redundant file reads
        values$region_name_mapping <- load_region_name_mapping(
          base_dir, 
          adm1_sf = values$adm1_boundaries, 
          adm2_sf = values$adm2_boundaries
        )

        values$status <- "Data files loaded. Ready to select input folder and run analysis."
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
      
      # If we haven't loaded static files yet, load everything
      if (!values$data_loaded) {
        load_all_static_files(base_dir)
      } else {
        # If already loaded, just update the hazard-related parts that can be changed by overrides
        # This ensures that when user saves overrides in the settings tab, the analysis uses the new config
        values$hazards <- hazards_result$hazards
        values$hazards_inventory <- hazards_result$inventory
        values$hazard_configs <- hazards_result$configs
        
        # ALSO reload precomputed_hazards with new configs to ensure ensemble names and keys are updated
        # This is critical for assets without coordinates to match the new inventory
        values$precomputed_hazards <- read_precomputed_hazards(base_dir, hazard_configs = values$hazard_configs)
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
    input_folder <- control$input_folder()

    # Guard clauses
    if (is.null(base_dir) || base_dir == "") {
      values$status <- "Error: Base directory is not set. Please restart the app with a valid base_dir."
      return()
    }
    if (is.null(input_folder) || input_folder == "") {
      values$status <- "Error: Please select an input folder containing asset_information.xlsx or asset_information.csv, and company_information.xlsx or company_information.csv files."
      return()
    }
    
    # Check that both required files exist in the selected folder (Excel or CSV)
    asset_xlsx <- file.path(input_folder, "asset_information.xlsx")
    asset_csv <- file.path(input_folder, "asset_information.csv")
    company_xlsx <- file.path(input_folder, "company_information.xlsx")
    company_csv <- file.path(input_folder, "company_information.csv")
    
    asset_has_xlsx <- file.exists(asset_xlsx)
    asset_has_csv <- file.exists(asset_csv)
    company_has_xlsx <- file.exists(company_xlsx)
    company_has_csv <- file.exists(company_csv)
    
    # Check for conflicts (both formats exist)
    if ((asset_has_xlsx && asset_has_csv) || (company_has_xlsx && company_has_csv)) {
      conflicts <- c()
      if (asset_has_xlsx && asset_has_csv) conflicts <- c(conflicts, "asset_information")
      if (company_has_xlsx && company_has_csv) conflicts <- c(conflicts, "company_information")
      values$status <- paste0("Error: Both Excel and CSV formats found for: ", paste(conflicts, collapse = ", "), ". Please use only one format per file type.")
      return()
    }
    
    # Check that at least one format exists for each file
    if ((!asset_has_xlsx && !asset_has_csv) || (!company_has_xlsx && !company_has_csv)) {
      missing <- c()
      if (!asset_has_xlsx && !asset_has_csv) missing <- c(missing, "asset_information.xlsx or asset_information.csv")
      if (!company_has_xlsx && !company_has_csv) missing <- c(missing, "company_information.xlsx or company_information.csv")
      values$status <- paste0("Error: Missing required files in selected folder: ", paste(missing, collapse = ", "))
      return()
    }
    
    if (!values$data_loaded || is.null(values$hazards) || length(values$hazards) == 0) {
      values$status <- "Error: Data files not loaded. Please wait for data to finish loading."
      return()
    }

    values$status <- "Running analysis..."

    tryCatch(
      {
        # Load asset and company files from the selected folder
        values$assets <- read_assets(input_folder)
        companies <- read_companies(input_folder)

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

        spatial_cfg <- control$spatial_separation()
        if (isTRUE(spatial_cfg$enabled) && length(spatial_cfg$selected_codes) == 0) {
          values$status <- "Error: Please select at least one state or municipality in Spatial Separation."
          return()
        }

        # Run the complete climate risk analysis using pre-loaded data
        results <- compute_risk(
          assets = values$assets,
          companies = companies,
          events = ev_df,
          hazards = values$hazards,
          hazards_inventory = values$hazards_inventory,
          precomputed_hazards = values$precomputed_hazards,
          hazard_configs = values$hazard_configs,
          hazards_dir = file.path(base_dir, "hazards", "config"),
          adm1_boundaries = values$adm1_boundaries,
          adm2_boundaries = values$adm2_boundaries,
          adm_codes = values$adm_codes,
          validate_inputs = TRUE,
          growth_rate = control$growth_rate(),
          discount_rate = control$discount_rate(),
          risk_free_rate = control$risk_free_rate(),
          spatial_separation = spatial_cfg,
          aggregation_method = "mean" # Default aggregation method
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
