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
      shiny::div(
        style = "margin-bottom: 15px;",
        shinyFiles::shinyDirButton(
          id = ns("select_folder"),
          label = "Select Input Folder",
          title = "Select folder containing asset_information.xlsx and company_information.xlsx",
          icon = shiny::icon("folder-open"),
          class = "btn-primary btn-block"
        ),
        shiny::div(
          id = ns("folder_status"),
          style = "margin-top: 10px; padding: 8px; border-radius: 4px; font-size: 0.9em;",
          shiny::textOutput(ns("folder_path_display"))
        )
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
#' @param overrides_reload reactive trigger for hazard override reload
#' @return list with reactive values for input_folder, events, run_trigger, growth_rate, discount_rate, risk_free_rate, results_ready, and get_hazards_at_factor
#' @export
mod_control_server <- function(id, base_dir_reactive, overrides_reload) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive values
    values <- shiny::reactiveValues(
      results_ready = FALSE,
      results = NULL,
      selected_folder = NULL
    )
    
    # Set up folder selection using shinyFiles (cross-platform)
    # On Windows: includes drive letters (C:, D:, etc.)
    # On Mac/Linux: includes home directory and system volumes
    # Include base_dir as a volume so the picker opens there by default
    # Compute volumes reactively to include base_dir when available
    volumes <- shiny::reactive({
      base_dir <- base_dir_reactive()
      vol_list <- c(Home = path.expand("~"), shinyFiles::getVolumes()())
      # Add base_dir as a named volume if it exists and is valid
      if (!is.null(base_dir) && base_dir != "" && dir.exists(base_dir)) {
        # Use a descriptive name for the base_dir volume
        vol_list <- c("Base Directory" = base_dir, vol_list)
      }
      return(vol_list)
    })
    
    # Initialize shinyDirChoose with initial volumes
    # Update it when base_dir becomes available
    shiny::observe({
      vol_list <- volumes()
      shinyFiles::shinyDirChoose(
        input = input,
        id = "select_folder",
        roots = vol_list,
        session = session,
        filetypes = character(0),  # Allow all file types (we're selecting folders)
        allowDirCreate = FALSE  # Disable "create new folder" button
      )
    })
    
    # Observe folder selection
    shiny::observeEvent(input$select_folder, {
      if (!is.integer(input$select_folder)) {
        folder_path <- shinyFiles::parseDirPath(volumes(), input$select_folder)
        if (length(folder_path) > 0 && dir.exists(folder_path)) {
          values$selected_folder <- as.character(folder_path)
        }
      }
    })
    
    # Display selected folder path and file status
    output$folder_path_display <- shiny::renderText({
      if (is.null(values$selected_folder)) {
        return("No folder selected")
      }
      
      folder <- values$selected_folder
      asset_file <- file.path(folder, "asset_information.xlsx")
      company_file <- file.path(folder, "company_information.xlsx")
      
      asset_exists <- file.exists(asset_file)
      company_exists <- file.exists(company_file)
      
      if (asset_exists && company_exists) {
        paste0("[OK] Folder: ", folder, "\n[OK] Found: asset_information.xlsx, company_information.xlsx")
      } else {
        missing <- c()
        if (!asset_exists) missing <- c(missing, "asset_information.xlsx")
        if (!company_exists) missing <- c(missing, "company_information.xlsx")
        paste0("[X] Folder: ", folder, "\n[X] Missing: ", paste(missing, collapse = ", "))
      }
    })

    # Load hazards and inventory (unified loader)
    hazards_and_inventory <- shiny::reactive({
      overrides_reload()
      base_dir <- base_dir_reactive()
      if (is.null(base_dir) || base_dir == "") {
        return(NULL)
      }

      hazards_dir <- file.path(base_dir, "hazards", "config")
      indicators_dir <- file.path(base_dir, "hazards", "indicators")
      if (!dir.exists(hazards_dir)) {
        message("Hazards directory not found at: ", hazards_dir)
        return(NULL)
      }
      if (!dir.exists(indicators_dir)) {
        message("Hazard indicators directory not found at: ", indicators_dir)
        return(NULL)
      }

      # Use aggregation factor of 1 (no aggregation) in the app
      agg_factor <- 1L

      result <- try(
        load_hazards_and_inventory(
          hazards_dir = hazards_dir,
          hazard_indicators_dir = indicators_dir,
          hazards_override_path = file.path(hazards_dir, "config_overrides.yml"),
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

      # Return hazards as a single named list for the analysis pipeline
      return(result$hazards)
    })

    # Extract inventory for UI
    hazards_inventory <- shiny::reactive({
      result <- hazards_and_inventory()
      if (is.null(result)) {
        return(tibble::tibble())
      }

      return(result$inventory)
    })

    hazard_configs <- shiny::reactive({
      result <- hazards_and_inventory()
      if (is.null(result)) {
        return(NULL)
      }

      return(result$configs)
    })


    # Hazards events module
    hz_mod <- mod_hazards_events_server(
      "hazards",
      hazards_inventory = hazards_inventory,
      hazard_configs = hazard_configs
    )

    # Results ready output for conditional panel
    output$results_ready <- shiny::reactive({
      values$results_ready
    })
    shiny::outputOptions(output, "results_ready", suspendWhenHidden = FALSE)

    # Return reactive values and functions
    return(list(
      input_folder = shiny::reactive({
        values$selected_folder
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
