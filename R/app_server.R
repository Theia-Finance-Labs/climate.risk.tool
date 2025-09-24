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
    status = "Ready to load data"
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
  
  # Run analysis when button is clicked
  observeEvent(input$run_analysis, {
    base_dir <- get_base_dir()
    
    if (is.null(base_dir) || base_dir == "") {
      values$status <- "Error: No base directory provided. Please run the app with run_app(base_dir = 'path/to/data')"
      showNotification("No base directory provided. Please run the app with run_app(base_dir = 'path/to/data')", type = "error")
      return()
    }
    
    if (!dir.exists(base_dir)) {
      values$status <- paste("Error: Directory does not exist:", base_dir)
      showNotification("Directory does not exist", type = "error")
      return()
    }
    
    # Check if company file is uploaded
    if (is.null(input$company_file) || is.null(input$company_file$datapath)) {
      values$status <- "Error: Please upload a company CSV file"
      showNotification("Please upload a company CSV file", type = "error")
      return()
    }
    
    # Update status
    values$status <- "Loading data..."
    
    tryCatch({
      # Load inputs separately
      assets <- read_assets(base_dir)
      companies <- read_companies(input$company_file$datapath)
      
      hazards <- load_hazards(file.path(base_dir, "hazards"))
      areas <- load_location_areas(
        file.path(base_dir, "areas", "municipality"),
        file.path(base_dir, "areas", "province")
      )
      damage_factors_path <- file.path(base_dir, "damage_and_cost_factors.csv")
      
      values$data_loaded <- TRUE
      values$status <- "Data loaded. Running analysis..."
      
      # Run the complete analysis
      results_data <- compute_risk(
        assets = assets,
        companies = companies,
        hazards = hazards,
        areas = areas,
        damage_factors = damage_factors_path,
        shock_year = 2030,
        growth_rate = 0.02,
        net_profit_margin = 0.1,
        discount_rate = 0.05,
        verbose = FALSE
      )
      
      values$results <- results_data
      values$results_ready <- TRUE
      values$status <- "Analysis completed successfully!"
      
      showNotification("Analysis completed!", type = "success")
      
    }, error = function(e) {
      values$status <- paste("Error during analysis:", e$message)
      showNotification(paste("Error:", e$message), type = "error")
    })
  })
  
  # Results summary table
  output$results_summary <- renderTable({
    if (!values$results_ready || is.null(values$results)) {
      return(data.frame(Message = "No results available"))
    }
    
    res <- values$results
    summary_data <- data.frame(
      Metric = c("Number of Assets", "Number of Companies", "Scenarios"),
      Value = c(
        if (!is.null(res$assets)) nrow(res$assets) else 0,
        if (!is.null(res$companies)) nrow(res$companies) else 0,
        if (!is.null(res$companies)) length(unique(res$companies$scenario)) else 0
      )
    )
    summary_data
  })
  
  # Results data table
  output$results_table <- DT::renderDataTable({
    if (!values$results_ready || is.null(values$results)) {
      return(DT::datatable(data.frame(Message = "No results available")))
    }
    
    DT::datatable(
      values$results$companies,
      options = list(
        scrollX = TRUE,
        pageLength = 10
      )
    )
  })
  
  # Download handler
  output$download_results <- downloadHandler(
    filename = function() {
      paste0("climate_risk_results_", Sys.Date(), ".csv")
    },
    content = function(file) {
      if (!values$results_ready || is.null(values$results)) {
        write.csv(data.frame(Message = "No results available"), file, row.names = FALSE)
      } else {
        write.csv(values$results$companies, file, row.names = FALSE)
      }
    }
  )
}
