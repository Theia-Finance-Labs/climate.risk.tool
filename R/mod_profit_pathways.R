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
        "Select assets from the table below to highlight their trajectories.",
        class = "text-muted",
        style = "margin-bottom: 1rem;"
      ),
      
      # Controls: Log scale toggle
      shiny::div(
        style = "margin-bottom: 2rem;",
        shiny::checkboxInput(
          ns("log_scale"),
          "Use logarithmic scale for Y-axis",
          value = FALSE
        )
      ),
      
      # Asset selection table at top
      shiny::div(
        class = "asset-selection-section",
        style = "margin-bottom: 2rem;",
        shiny::h4("Asset Selection", class = "section-header"),
        shiny::p(
          "Click on rows to select assets and highlight their trajectories in the charts below.",
          class = "text-muted",
          style = "margin-bottom: 1rem; font-size: 0.9em;"
        ),
        DT::dataTableOutput(ns("assets_selection_table"))
      ),
      
      # Baseline plot on top
      shiny::div(
        class = "chart-container",
        style = "margin-bottom: 2rem;",
        shiny::h4("Baseline Scenario", class = "chart-title"),
        plotly::plotlyOutput(ns("profit_baseline"), height = "500px")
      ),
      
      # Shock plot below
      shiny::div(
        class = "chart-container",
        shiny::h4("Shock Scenario", class = "chart-title"),
        plotly::plotlyOutput(ns("profit_shock"), height = "500px")
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

    # Prepare baseline profit trajectories
    baseline_data <- shiny::reactive({
      results <- results_reactive()
      if (is.null(results) || is.null(results$assets_yearly)) {
        return(NULL)
      }
      prepare_profit_trajectories(results$assets_yearly, "baseline")
    })

    # Prepare shock profit trajectories (first non-baseline scenario)
    shock_data <- shiny::reactive({
      results <- results_reactive()
      if (is.null(results) || is.null(results$assets_yearly)) {
        return(NULL)
      }

      # Get first shock scenario (not baseline)
      scenarios <- unique(results$assets_yearly$scenario)
      shock_scenario <- scenarios[scenarios != "baseline"][1]

      if (is.na(shock_scenario)) {
        return(NULL)
      }

      prepare_profit_trajectories(results$assets_yearly, shock_scenario)
    })
    
    # Create baseline plot
    output$profit_baseline <- plotly::renderPlotly({
      data <- baseline_data()
      if (is.null(data) || nrow(data) == 0) {
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
      
      create_profit_plot(
        data, 
        selected_assets(), 
        "Baseline",
        log_scale = input$log_scale
      )
    })
    
    # Create shock plot
    output$profit_shock <- plotly::renderPlotly({
      data <- shock_data()
      if (is.null(data) || nrow(data) == 0) {
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
      
      create_profit_plot(
        data, 
        selected_assets(), 
        "Shock",
        log_scale = input$log_scale
      )
    })
    
    # Asset selection table - unique assets with metadata
    output$assets_selection_table <- DT::renderDataTable({
      results <- results_reactive()
      if (is.null(results) || is.null(results$assets_factors)) {
        return(NULL)
      }
      
      # Get unique assets with their metadata
      # Check which columns exist before selecting
      available_cols <- names(results$assets_factors)
      cols_to_select <- c("asset", "asset_category")
      
      # Add optional columns if they exist
      if ("asset_subtype" %in% available_cols) {
        cols_to_select <- c(cols_to_select, "asset_subtype")
      }
      if ("cnae" %in% available_cols) {
        cols_to_select <- c(cols_to_select, "cnae")
      } else if ("sector" %in% available_cols) {
        cols_to_select <- c(cols_to_select, "sector")
      }
      
      assets_unique <- results$assets_factors |>
        dplyr::select(dplyr::all_of(cols_to_select)) |>
        dplyr::distinct() |>
        dplyr::arrange(.data$asset)
      
      # Rename columns for display
      display_names <- c("Asset", "Category")
      if ("asset_subtype" %in% cols_to_select) {
        display_names <- c(display_names, "Subtype")
      }
      if ("cnae" %in% cols_to_select || "sector" %in% cols_to_select) {
        display_names <- c(display_names, "CNAE/Sector")
      }
      colnames(assets_unique) <- display_names
      
      DT::datatable(
        assets_unique,
        options = list(
          pageLength = 15,
          scrollX = TRUE,
          dom = "ftp"
        ),
        rownames = FALSE,
        selection = "multiple"
      )
    })
    
    # Update selected assets when table rows are clicked
    shiny::observeEvent(input$assets_selection_table_rows_selected, {
      results <- results_reactive()
      if (is.null(results) || is.null(results$assets_factors)) {
        return()
      }
      
      selected_rows <- input$assets_selection_table_rows_selected
      if (length(selected_rows) == 0) {
        selected_assets(character(0))
      } else {
        # Get unique assets from the selection table
        available_cols <- names(results$assets_factors)
        cols_to_select <- c("asset")
        if ("asset_subtype" %in% available_cols) {
          cols_to_select <- c(cols_to_select, "asset_subtype")
        }
        if ("cnae" %in% available_cols) {
          cols_to_select <- c(cols_to_select, "cnae")
        } else if ("sector" %in% available_cols) {
          cols_to_select <- c(cols_to_select, "sector")
        }
        
        assets_unique <- results$assets_factors |>
          dplyr::select(dplyr::all_of(cols_to_select)) |>
          dplyr::distinct() |>
          dplyr::arrange(.data$asset)
        
        selected <- assets_unique$asset[selected_rows]
        selected_assets(selected)
      }
    })
  })
}

#' Create profit pathway plot
#'
#' @param data Data frame with asset, year, profit columns
#' @param highlighted_assets Character vector of assets to highlight
#' @param title Character. Plot title
#' @param log_scale Logical. Whether to use logarithmic scale for y-axis (default: FALSE)
#' @return plotly object
#' @noRd
create_profit_plot <- function(data, highlighted_assets, title, log_scale = FALSE) {
  if (is.null(data) || nrow(data) == 0) {
    return(plotly::plot_ly())
  }
  
  # Get unique assets
  unique_assets <- unique(data$asset)
  
  # Handle log scale: filter out non-positive values
  if (log_scale) {
    data <- data |>
      dplyr::filter(.data$profit > 0)
    if (nrow(data) == 0) {
      return(
        plotly::plot_ly() |>
          plotly::add_text(
            x = 0.5, y = 0.5,
            text = "No positive profit values available for log scale",
            textposition = "middle center",
            showlegend = FALSE
          ) |>
          plotly::layout(
            xaxis = list(showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE),
            yaxis = list(showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE)
          )
      )
    }
  }
  
  # Create base plot
  p <- plotly::plot_ly()
  
  # Add a trace for each asset
  for (asset_name in unique_assets) {
    asset_data <- data |>
      dplyr::filter(.data$asset == !!asset_name)
    
    if (nrow(asset_data) == 0) {
      next
    }
    
    # Determine if this asset is highlighted
    is_highlighted <- asset_name %in% highlighted_assets
    
    # Format hover template based on scale
    if (log_scale) {
      hovertemplate_str <- paste0(
        "<b>", asset_name, "</b><br>",
        "Year: %{x}<br>",
        "Profit: R$%{y:,.2f}<br>",
        "<extra></extra>"
      )
    } else {
      hovertemplate_str <- paste0(
        "<b>", asset_name, "</b><br>",
        "Year: %{x}<br>",
        "Profit: R$%{y:,.0f}<br>",
        "<extra></extra>"
      )
    }
    
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
        hovertemplate = hovertemplate_str
      )
  }
  
  # Configure y-axis based on scale type
  yaxis_config <- list(
    title = if (log_scale) "Profit (R$, log scale)" else "Profit (R$)",
    showgrid = TRUE,
    gridcolor = "#ecf0f1"
  )
  
  if (log_scale) {
    yaxis_config$type <- "log"
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
      yaxis = yaxis_config,
      hovermode = "closest",
      showlegend = if (length(unique_assets) <= 20) TRUE else FALSE,
      legend = list(
        orientation = "v",
        yanchor = "top",
        y = 1,
        xanchor = "left",
        x = 1.02
      ),
      margin = list(l = 80, r = 150, t = 50, b = 60)
    )
  
  p
}

