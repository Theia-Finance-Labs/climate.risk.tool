#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    fluidPage(
      titlePanel("Climate Risk Analysis Tool"),
      
      sidebarLayout(
        sidebarPanel(
          fileInput("company_file", 
                   "Upload Company CSV File:",
                   accept = ".csv",
                   placeholder = "Choose company.csv file"),
          selectInput("hazard_resolution",
                      "Hazard resolution (aggregation factor)",
                      choices = c(1, 2, 4, 8, 16, 32, 64),
                      selected = 1),
          
          actionButton("run_analysis", 
                      "Run Analysis", 
                      class = "btn-primary",
                      style = "margin-top: 10px;"),
          
          br(), br(),
          
          # Hazard events module UI
          mod_hazards_events_ui("hazards"),

          downloadButton("download_results", 
                        "Download Results", 
                        class = "btn-success")
        ),
        
        mainPanel(
          tabsetPanel(
            tabPanel("Status", 
                    verbatimTextOutput("status_text")),
            tabPanel("Results Summary", 
                    tableOutput("results_summary")),
            tabPanel("Results Table", 
                    DT::dataTableOutput("results_table"))
          )
        )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "climate.risk.tool"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
