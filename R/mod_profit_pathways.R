#' profit_pathways UI Function
#'
#' @description Module to display asset profit pathways with interactive highlighting
#' @param id Internal parameter for shiny
#' @export
mod_profit_pathways_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::div(
      class = "pathways-container",
      shiny::h3("Asset Profit Pathways", class = "results-title"),
      shiny::p(
        "Compare baseline and shock scenarios for asset profitability over time. ",
        "Click on table rows below to highlight specific assets in the charts.",
        class = "text-muted",
        style = "margin-bottom: 2rem;"
      ),
      
      # Two plots side by side
      shiny::fluidRow(
        shiny::column(
          width = 6,
          shiny::div(
            class = "chart-container",
            shiny::h4("Baseline Scenario", class = "chart-title"),
            plotly::plotlyOutput(ns("profit_baseline"), height = "500px")
          )
        ),
        shiny::column(
          width = 6,
          shiny::div(
            class = "chart-container",
            shiny::h4("Shock Scenario", class = "chart-title"),
            plotly::plotlyOutput(ns("profit_shock"), height = "500px")
          )
        )
      ),
      
      # Asset results table
      shiny::div(
        class = "results-section",
        style = "margin-top: 3rem;",
        shiny::h4("Asset Exposure Details", class = "section-header"),
        shiny::p(
          "Click on rows to highlight the corresponding asset trajectories in the charts above.",
          class = "text-muted",
          style = "margin-bottom: 1rem;"
        ),
        DT::dataTableOutput(ns("assets_table"))
      )
    )
  )
}

#' profit_pathways Server Functions
#'
#' @param id Internal parameter for shiny
#' @param results_reactive reactive containing analysis results
#' @export
mod_profit_pathways_server <- function(id, results_reactive) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Store selected assets
    selected_assets <- shiny::reactiveVal(character(0))

    # Track if we have results
    has_results <- shiny::reactiveVal(FALSE)

    # Observe results changes and trigger updates
    shiny::observe({
      results <- results_reactive()
      new_has_results <- !is.null(results) && !is.null(results$assets_yearly)
      has_results(new_has_results)
      message("[mod_profit_pathways] has_results updated to: ", new_has_results)
    })

    # Prepare baseline profit trajectories
    baseline_data <- shiny::reactive({
      message("[mod_profit_pathways] baseline_data reactive triggered")
      results <- results_reactive()
      if (is.null(results)) {
        message("[mod_profit_pathways] Results is NULL")
        return(NULL)
      }
      if (is.null(results$assets_yearly)) {
        message("[mod_profit_pathways] results$assets_yearly is NULL")
        message("[mod_profit_pathways] Available result names: ", paste(names(results), collapse = ", "))
        return(NULL)
      }

      message("[mod_profit_pathways] Processing baseline data, nrows=", nrow(results$assets_yearly))
      prepare_profit_trajectories(results$assets_yearly, "baseline")
    })

    # Prepare shock profit trajectories (first non-baseline scenario)
    shock_data <- shiny::reactive({
      message("[mod_profit_pathways] shock_data reactive triggered")
      results <- results_reactive()
      if (is.null(results) || is.null(results$assets_yearly)) {
        return(NULL)
      }

      # Get first shock scenario (not baseline)
      scenarios <- unique(results$assets_yearly$scenario)
      message("[mod_profit_pathways] Available scenarios: ", paste(scenarios, collapse = ", "))
      shock_scenario <- scenarios[scenarios != "baseline"][1]

      if (is.na(shock_scenario)) {
        message("[mod_profit_pathways] No shock scenario found")
        return(NULL)
      }

      message("[mod_profit_pathways] Using shock scenario: ", shock_scenario)
      prepare_profit_trajectories(results$assets_yearly, shock_scenario)
    })
    
    # Create baseline plot
    output$profit_baseline <- plotly::renderPlotly({
      message("[mod_profit_pathways] Baseline plot render triggered - has_results: ", has_results())

      # Depend on has_results to trigger re-rendering
      if (!has_results()) {
        message("[mod_profit_pathways] Returning empty baseline plot - no results")
        return(
          plotly::plot_ly() |>
            plotly::add_text(
              x = 0.5, y = 0.5,
              text = "No data available. Run analysis first.",
              textposition = "middle center",
              showlegend = FALSE
            ) |>
            plotly::layout(
              xaxis = list(showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE),
              yaxis = list(showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE)
            )
        )
      }

      # Get results and selected assets
      results <- results_reactive()
      selected <- selected_assets()

      message("[mod_profit_pathways] Baseline plot - processing data")
      data <- prepare_profit_trajectories(results$assets_yearly, "baseline")

      if (is.null(data) || nrow(data) == 0) {
        message("[mod_profit_pathways] No baseline data to plot")
        return(
          plotly::plot_ly() |>
            plotly::add_text(
              x = 0.5, y = 0.5,
              text = "No baseline data available",
              textposition = "middle center",
              showlegend = FALSE
            ) |>
            plotly::layout(
              xaxis = list(showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE),
              yaxis = list(showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE)
            )
        )
      }

      message("[mod_profit_pathways] Creating baseline plot with ", nrow(data), " rows")
      create_profit_plot(data, selected, "Baseline")
    })
    
    # Create shock plot
    output$profit_shock <- plotly::renderPlotly({
      message("[mod_profit_pathways] Shock plot render triggered - has_results: ", has_results())

      # Depend on has_results to trigger re-rendering
      if (!has_results()) {
        message("[mod_profit_pathways] Returning empty shock plot - no results")
        return(
          plotly::plot_ly() |>
            plotly::add_text(
              x = 0.5, y = 0.5,
              text = "No data available. Run analysis first.",
              textposition = "middle center",
              showlegend = FALSE
            ) |>
            plotly::layout(
              xaxis = list(showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE),
              yaxis = list(showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE)
            )
        )
      }

      # Get results and selected assets
      results <- results_reactive()
      selected <- selected_assets()

      # Get shock scenario
      scenarios <- unique(results$assets_yearly$scenario)
      shock_scenario <- scenarios[scenarios != "baseline"][1]

      if (is.na(shock_scenario)) {
        message("[mod_profit_pathways] No shock scenario found")
        return(
          plotly::plot_ly() |>
            plotly::add_text(
              x = 0.5, y = 0.5,
              text = "No shock scenario available",
              textposition = "middle center",
              showlegend = FALSE
            ) |>
            plotly::layout(
              xaxis = list(showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE),
              yaxis = list(showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE)
            )
        )
      }

      message("[mod_profit_pathways] Shock plot - processing data for scenario: ", shock_scenario)
      data <- prepare_profit_trajectories(results$assets_yearly, shock_scenario)

      if (is.null(data) || nrow(data) == 0) {
        message("[mod_profit_pathways] No shock data to plot")
        return(
          plotly::plot_ly() |>
            plotly::add_text(
              x = 0.5, y = 0.5,
              text = "No shock data available",
              textposition = "middle center",
              showlegend = FALSE
            ) |>
            plotly::layout(
              xaxis = list(showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE),
              yaxis = list(showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE)
            )
        )
      }

      message("[mod_profit_pathways] Creating shock plot with ", nrow(data), " rows")
      create_profit_plot(data, selected, "Shock")
    })
    
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
        if (grepl("ratio|intensity|factor", col)) {
          assets[[col]] <- round(assets[[col]], 4)
        } else if (grepl("cost|value|revenue", col)) {
          assets[[col]] <- round(assets[[col]], 0)
        }
      }
      
      # Reorder columns to show key columns prominently
      priority_cols <- c("asset", "company", "event_id", "hazard_type", "matching_method", "hazard_intensity", "damage_factor")
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
        rownames = FALSE,
        selection = list(mode = "multiple", target = "row")
      )
    })
    
    # Update selected assets when table rows are clicked
    shiny::observeEvent(input$assets_table_rows_selected, {
      results <- results_reactive()
      if (is.null(results) || is.null(results$assets_factors)) {
        return()
      }
      
      selected_rows <- input$assets_table_rows_selected
      if (length(selected_rows) == 0) {
        selected_assets(character(0))
      } else {
        assets <- results$assets_factors
        # Get unique asset names from selected rows
        selected <- unique(assets$asset[selected_rows])
        selected_assets(selected)
        
        message("[mod_profit_pathways] Selected assets: ", paste(selected, collapse = ", "))
      }
    }, ignoreNULL = FALSE)
  })
}

#' Create profit pathway plot
#'
#' @param data Data frame with asset, year, profit columns
#' @param highlighted_assets Character vector of assets to highlight
#' @param title Character. Plot title
#' @return plotly object
#' @noRd
create_profit_plot <- function(data, highlighted_assets, title) {
  if (is.null(data) || nrow(data) == 0) {
    return(plotly::plot_ly())
  }
  
  # Get unique assets
  unique_assets <- unique(data$asset)
  
  # Create base plot
  p <- plotly::plot_ly()
  
  # Add a trace for each asset
  for (asset_name in unique_assets) {
    asset_data <- data |>
      dplyr::filter(.data$asset == !!asset_name)
    
    # Determine if this asset is highlighted
    is_highlighted <- asset_name %in% highlighted_assets
    
    p <- p |>
      plotly::add_trace(
        data = asset_data,
        x = ~year,
        y = ~profit,
        type = "scatter",
        mode = "lines",
        name = asset_name,
        line = list(
          width = if (is_highlighted) 4 else 1,
          color = if (is_highlighted) "#e74c3c" else "#95a5a6"
        ),
        opacity = if (is_highlighted) 1 else 0.3,
        hovertemplate = paste0(
          "<b>", asset_name, "</b><br>",
          "Year: %{x}<br>",
          "Profit: $%{y:,.0f}<br>",
          "<extra></extra>"
        )
      )
  }
  
  # Layout
  p <- p |>
    plotly::layout(
      title = list(
        text = title,
        font = list(size = 16, color = "#2c3e50")
      ),
      xaxis = list(
        title = "Year",
        showgrid = TRUE,
        gridcolor = "#ecf0f1"
      ),
      yaxis = list(
        title = "Profit ($)",
        showgrid = TRUE,
        gridcolor = "#ecf0f1"
      ),
      hovermode = "closest",
      showlegend = if (length(unique_assets) <= 20) TRUE else FALSE,
      legend = list(
        orientation = "v",
        yanchor = "top",
        y = 1,
        xanchor = "left",
        x = 1.02
      ),
      margin = list(l = 60, r = 150, t = 50, b = 60)
    )
  
  p
}

