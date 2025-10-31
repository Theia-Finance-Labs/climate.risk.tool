#' company_analysis UI Function
#'
#' @description Module to display company-level analysis with visualizations
#' @param id Internal parameter for shiny
#' @export
mod_company_analysis_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::div(
      class = "company-analysis-container",
      shiny::h3("Company Financial Analysis", class = "results-title"),
      
      # Expected Loss Change Plot
      shiny::div(
        class = "chart-section",
        style = "margin-bottom: 3rem;",
        shiny::h4("Expected Loss % Change by Company", class = "section-header"),
        shiny::p(
          "Companies sorted by percentage change in expected loss from baseline to shock scenario.",
          class = "text-muted",
          style = "margin-bottom: 1rem;"
        ),
        shiny::div(
          class = "chart-container",
          plotly::plotlyOutput(ns("expected_loss_change_plot"), height = "500px")
        )
      ),
      
      # Portfolio Summary Plot
      shiny::div(
        class = "chart-section",
        style = "margin-bottom: 3rem;",
        shiny::h4("Portfolio-Level Expected Loss Summary", class = "section-header"),
        shiny::p(
          "Total expected loss across all companies in baseline and shock scenarios.",
          class = "text-muted",
          style = "margin-bottom: 1rem;"
        ),
        shiny::div(
          class = "chart-container",
          plotly::plotlyOutput(ns("portfolio_summary_plot"), height = "400px")
        )
      ),
      
      # Company Results Table
      shiny::div(
        class = "results-section",
        style = "margin-top: 3rem;",
        shiny::h4("Company Financial Details", class = "section-header"),
        DT::dataTableOutput(ns("companies_table"))
      )
    )
  )
}

#' company_analysis Server Functions
#'
#' @param id Internal parameter for shiny
#' @param results_reactive reactive containing analysis results
#' @export
mod_company_analysis_server <- function(id, results_reactive) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Expected Loss Change Plot
    output$expected_loss_change_plot <- plotly::renderPlotly({
      results <- results_reactive()
      if (is.null(results) || is.null(results$companies)) {
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
      
      message("[mod_company_analysis] Rendering expected loss plot, nrows=", nrow(results$companies))
      create_expected_loss_change_plot(results$companies)
    })
    
    # Portfolio Summary Plot
    output$portfolio_summary_plot <- plotly::renderPlotly({
      results <- results_reactive()
      if (is.null(results) || is.null(results$companies)) {
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
      
      message("[mod_company_analysis] Computing portfolio summary")
      summary_data <- compute_portfolio_summary(results$companies)
      message("[mod_company_analysis] Summary data: ", nrow(summary_data), " rows")
      create_portfolio_summary_plot(summary_data)
    })
    
    # Companies table
    output$companies_table <- DT::renderDataTable({
      results <- results_reactive()
      if (is.null(results) || is.null(results$companies)) {
        return(NULL)
      }
      
      companies <- results$companies
      
      # Create a display copy for formatting
      companies_display <- companies
      
      # Format numeric columns for better display
      for (col in names(companies_display)) {
        if (is.numeric(companies_display[[col]])) {
          if (grepl("_pct$|_change_pct$", col, ignore.case = TRUE)) {
            # Percentage change columns (already in percentage format)
            companies_display[[col]] <- paste0(sprintf("%.2f", companies_display[[col]]), "%")
          } else if (grepl("^pd_|_pd$", col, ignore.case = TRUE)) {
            # Probability of default columns (need to multiply by 100)
            companies_display[[col]] <- paste0(sprintf("%.4f", companies_display[[col]] * 100), "%")
          } else if (grepl("npv|loss", col, ignore.case = TRUE)) {
            # NPV and loss columns (currency format)
            companies_display[[col]] <- paste0("$", format(round(companies_display[[col]]), big.mark = ","))
          }
        }
      }
      
      DT::datatable(
        companies_display,
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

#' Create Expected Loss Change Plot
#'
#' @param companies_df Data frame with company results
#' @return plotly object
#' @noRd
create_expected_loss_change_plot <- function(companies_df) {
  if (is.null(companies_df) || nrow(companies_df) == 0) {
    return(plotly::plot_ly())
  }
  
  # Check if Expected_loss_change_pct column exists
  if (!"Expected_loss_change_pct" %in% names(companies_df)) {
    return(
      plotly::plot_ly() |>
        plotly::add_text(
          x = 0.5, y = 0.5,
          text = "Expected_loss_change_pct not found in data",
          textposition = "middle center",
          showlegend = FALSE
        ) |>
        plotly::layout(
          xaxis = list(showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE),
          yaxis = list(showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE)
        )
    )
  }
  
  # Sort by Expected_loss_change_pct descending
  plot_data <- companies_df |>
    dplyr::arrange(dplyr::desc(.data$Expected_loss_change_pct)) |>
    dplyr::mutate(company = factor(.data$company, levels = .data$company))
  
  # Determine bar colors (red for positive change, green for negative)
  bar_colors <- ifelse(plot_data$Expected_loss_change_pct > 0, "#e74c3c", "#27ae60")
  
  # Create plot
  p <- plotly::plot_ly(
    data = plot_data,
    x = ~company,
    y = ~Expected_loss_change_pct,
    type = "bar",
    marker = list(color = bar_colors),
    hovertemplate = paste0(
      "<b>%{x}</b><br>",
      "Expected Loss Change: %{y:.2f}%<br>",
      "<extra></extra>"
    )
  ) |>
    plotly::layout(
      xaxis = list(
        title = "Company",
        tickangle = -45
      ),
      yaxis = list(
        title = "Expected Loss Change (%)",
        showgrid = TRUE,
        gridcolor = "#ecf0f1"
      ),
      hovermode = "closest",
      margin = list(l = 60, r = 20, t = 40, b = 120)
    )
  
  p
}

#' Create Portfolio Summary Plot
#'
#' @param summary_data Data frame with metric and value columns
#' @return plotly object
#' @noRd
create_portfolio_summary_plot <- function(summary_data) {
  if (is.null(summary_data) || nrow(summary_data) == 0) {
    return(plotly::plot_ly())
  }
  
  # Define colors for each metric
  bar_colors <- c(
    "Baseline" = "#3498db",
    "Shock" = "#e74c3c",
    "Difference" = "#f39c12"
  )
  
  colors_vec <- sapply(summary_data$metric, function(m) bar_colors[m])
  
  # Create plot
  p <- plotly::plot_ly(
    data = summary_data,
    x = ~metric,
    y = ~value,
    type = "bar",
    marker = list(color = colors_vec),
    hovertemplate = paste0(
      "<b>%{x}</b><br>",
      "Total Expected Loss: $%{y:,.0f}<br>",
      "<extra></extra>"
    )
  ) |>
    plotly::layout(
      xaxis = list(
        title = "",
        categoryorder = "array",
        categoryarray = c("Baseline", "Shock", "Difference")
      ),
      yaxis = list(
        title = "Total Expected Loss ($)",
        showgrid = TRUE,
        gridcolor = "#ecf0f1"
      ),
      hovermode = "closest",
      margin = list(l = 80, r = 20, t = 40, b = 60)
    )
  
  p
}

