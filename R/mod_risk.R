#' risk UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_risk_ui <- function(id) {
  ns <- NS(id)
  tagList(
 
  )
}
    
#' risk Server Functions
#'
#' @noRd 
mod_risk_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_risk_ui("risk_1")
    
## To be copied in the server
# mod_risk_server("risk_1")
