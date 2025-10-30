#' results_companies UI Function
#'
#' @description Module to display company-level results table with financials
#' @param id Internal parameter for {shiny}
#' @export
mod_results_companies_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::div(
      class = "results-container",
      shiny::h3("Company Financials", class = "results-title"),
      DT::dataTableOutput(ns("companies_table"))
    )
  )
}

#' results_companies Server Functions
#'
#' @param id Internal parameter for {shiny}
#' @param results_reactive reactive containing analysis results
#' @export
mod_results_companies_server <- function(id, results_reactive) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Companies table
    output$companies_table <- DT::renderDataTable({
      results <- results_reactive()
      if (is.null(results) || is.null(results$companies)) {
        return(NULL)
      }

      companies <- results$companies

      # Format numeric columns for better display
      for (col in names(companies)) {
        if (is.numeric(companies[[col]])) {
          if (grepl("_pct$|_change_pct$", col, ignore.case = TRUE)) {
            # Percentage change columns (already in percentage format)
            companies[[col]] <- paste0(sprintf("%.2f", companies[[col]]), "%")
          } else if (grepl("^pd_|_pd$", col, ignore.case = TRUE)) {
            # Probability of default columns (need to multiply by 100)
            companies[[col]] <- paste0(sprintf("%.4f", companies[[col]] * 100), "%")
          } else if (grepl("npv|loss", col, ignore.case = TRUE)) {
            # NPV and loss columns (currency format)
            companies[[col]] <- paste0("$", format(round(companies[[col]]), big.mark = ","))
          }
        }
      }

      DT::datatable(
        companies,
        options = list(
          pageLength = 25,
          scrollX = TRUE,
          dom = "Bfrtip",
          buttons = c("copy", "csv", "excel")
        ),
        extensions = "Buttons",
        rownames = FALSE
      )
    })
  })
}
