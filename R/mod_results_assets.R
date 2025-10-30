#' results_assets UI Function
#'
#' @description Module to display asset-level results table with exposures
#' @param id Internal parameter for shiny
#' @export
mod_results_assets_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::div(
      class = "results-container",
      shiny::h3("Asset Exposures", class = "results-title"),
      DT::dataTableOutput(ns("assets_table"))
    )
  )
}

#' results_assets Server Functions
#'
#' @param id Internal parameter for shiny
#' @param results_reactive reactive containing analysis results
#' @export
mod_results_assets_server <- function(id, results_reactive) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Assets table
    output$assets_table <- DT::renderDataTable({
      results <- results_reactive()
      if (is.null(results) || is.null(results$assets_factors)) {
        return(NULL)
      }

      assets <- results$assets_factors

      # Format numeric columns for better display
      numeric_cols <- sapply(assets, is.numeric)
      for (col in names(assets)[numeric_cols]) {
        if (grepl("ratio|intensity", col)) {
          assets[[col]] <- round(assets[[col]], 4)
        } else if (grepl("cost|value", col)) {
          assets[[col]] <- round(assets[[col]], 0)
        }
      }

      # Reorder columns to show key columns prominently
      priority_cols <- c("asset", "company", "event_id", "matching_method", "hazard_return_period", "event_year")
      existing_priority <- intersect(priority_cols, names(assets))
      other_cols <- setdiff(names(assets), existing_priority)

      if (length(existing_priority) > 0) {
        col_order <- c(existing_priority, other_cols)
        assets <- assets[, col_order, drop = FALSE]
      }

      DT::datatable(
        assets,
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
