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
      class = "climate-risk-app",
      
      # Header
      div(
        class = "app-header",
        h1("Climate Risk Analysis Tool", class = "app-title"),
        p("Comprehensive climate risk assessment for financial portfolios", class = "app-subtitle")
      ),
      
      sidebarLayout(
        # Sidebar with controls
        sidebarPanel(
          class = "app-sidebar",
          width = 3,
          mod_control_ui("control")
        ),
        
        # Main content with tabs
        mainPanel(
          class = "app-main",
          width = 9,
          tabsetPanel(
            id = "main_tabs",
            type = "pills",
            
            # Tab 1: Parameters and Status
            tabPanel(
              title = "Parameters & Status",
              value = "status",
              icon = icon("cog"),
              mod_status_ui("status")
            ),
            
            # Tab 2: Asset Results (shown only after results)
            tabPanel(
              title = "Asset Analysis",
              value = "assets",
              icon = icon("building"),
              mod_results_assets_ui("results_assets")
            ),
            
            # Tab 3: Company Results (shown only after results)
            tabPanel(
              title = "Company Analysis", 
              value = "companies",
              icon = icon("chart-line"),
              mod_results_companies_ui("results_companies")
            )
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
    ),
    # Add custom CSS
    tags$link(
      rel = "stylesheet",
      type = "text/css",
      href = "www/custom.css"
    ),
    # Add Font Awesome for icons
    tags$link(
      rel = "stylesheet",
      href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0/css/all.min.css"
    )
  )
}
