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

      # Company Results Table
      shiny::div(
        class = "results-section",
        shiny::div(
          class = "results-downloads",
          shiny::downloadButton(
            ns("download_companies_csv"),
            "Download Companies (CSV)",
            class = "btn btn-info"
          ),
          shiny::downloadButton(
            ns("download_companies_excel"),
            "Download Companies (Excel)",
            class = "btn btn-info"
          )
        ),
        DT::dataTableOutput(ns("companies_table"))
      ),

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
        ),
        # Portfolio-level percentage change display
        shiny::div(
          style = "margin-top: 1.5rem; padding: 1rem; background-color: #f8f9fa; border-radius: 8px; border-left: 4px solid #002776;",
          shiny::div(
            style = "font-size: 0.9rem; color: #6c757d; margin-bottom: 0.5rem;",
            "Portfolio Expected Loss % Change"
          ),
          shiny::div(
            style = "font-size: 2rem; font-weight: bold;",
            shiny::textOutput(ns("portfolio_pct_change"), inline = TRUE)
          )
        )
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

    # Track if we have results
    has_results <- shiny::reactiveVal(FALSE)

    # Observe results changes and trigger updates
    shiny::observe({
      results <- results_reactive()
      new_has_results <- !is.null(results) && !is.null(results$companies)
      has_results(new_has_results)
      message("[mod_company_analysis] has_results updated to: ", new_has_results)
    })

    # Expected Loss Change Plot
    output$expected_loss_change_plot <- plotly::renderPlotly({
      message("[mod_company_analysis] Expected loss plot render triggered - has_results: ", has_results())

      # Depend on has_results to trigger re-rendering
      if (!has_results()) {
        message("[mod_company_analysis] Returning empty expected loss plot - no results")
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

      results <- results_reactive()
      message("[mod_company_analysis] Rendering expected loss plot, nrows=", nrow(results$companies))
      create_expected_loss_change_plot(results$companies)
    })

    # Portfolio Summary Plot
    output$portfolio_summary_plot <- plotly::renderPlotly({
      message("[mod_company_analysis] Portfolio summary plot render triggered - has_results: ", has_results())

      # Depend on has_results to trigger re-rendering
      if (!has_results()) {
        message("[mod_company_analysis] Returning empty portfolio plot - no results")
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

      results <- results_reactive()
      message("[mod_company_analysis] Computing portfolio summary")
      summary_data <- compute_portfolio_summary(results$companies)
      message("[mod_company_analysis] Summary data: ", nrow(summary_data), " rows")
      create_portfolio_summary_plot(summary_data)
    })

    # Portfolio percentage change text output
    output$portfolio_pct_change <- shiny::renderText({
      if (!has_results()) {
        return("—")
      }

      results <- results_reactive()
      summary_data <- compute_portfolio_summary(results$companies)
      
      # Extract the percentage change from the "Difference" row
      pct_change_row <- summary_data |>
        dplyr::filter(.data$metric == "Difference")
      
      if (nrow(pct_change_row) == 0 || is.na(pct_change_row$pct_change[1])) {
        return("N/A")
      }
      
      pct_value <- pct_change_row$pct_change[1]
      
      # Format with color based on positive/negative
      formatted <- sprintf("%+.2f%%", pct_value)
      return(formatted)
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
            companies_display[[col]] <- paste0("R$", format(round(companies_display[[col]]), big.mark = ","))
          }
        }
      }

      DT::datatable(
        companies_display,
        options = list(
          pageLength = 25,
          scrollX = TRUE
        ),
        rownames = FALSE
      )
    })

    companies_download_data <- shiny::reactive({
      results <- results_reactive()
      if (is.null(results) || is.null(results$companies) || nrow(results$companies) == 0) {
        return(NULL)
      }

      results$companies
    })

    output$download_companies_csv <- shiny::downloadHandler(
      filename = function() {
        paste0("company_results_", Sys.Date(), ".csv")
      },
      content = function(file) {
        data <- companies_download_data()
        if (is.null(data) || nrow(data) == 0) {
          utils::write.csv(data.frame(message = "No company results available"), file, row.names = FALSE)
        } else {
          utils::write.csv(as.data.frame(data), file, row.names = FALSE)
        }
      }
    )

    output$download_companies_excel <- shiny::downloadHandler(
      filename = function() {
        paste0("company_results_", Sys.Date(), ".xlsx")
      },
      content = function(file) {
        data <- companies_download_data()
        if (is.null(data) || nrow(data) == 0) {
          writexl::write_xlsx(data.frame(message = "No company results available"), path = file)
        } else {
          writexl::write_xlsx(as.data.frame(data), path = file)
        }
      }
    )
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

  palette_brazil <- list(
    green = "#009C3B",
    yellow = "#FFDF00",
    blue = "#002776"
  )

  # Determine bar colors (yellow for increase, green for decrease)
  bar_colors <- ifelse(plot_data$Expected_loss_change_pct > 0, palette_brazil$yellow, palette_brazil$green)

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
        gridcolor = "#DDE5EC"
      ),
      hovermode = "closest",
      margin = list(l = 60, r = 20, t = 40, b = 120)
    )

  p
}

#' Create Portfolio Summary Plot
#'
#' @param summary_data Data frame with metric, value, and pct_change columns
#' @return plotly object
#' @noRd
create_portfolio_summary_plot <- function(summary_data) {
  if (is.null(summary_data) || nrow(summary_data) == 0) {
    return(plotly::plot_ly())
  }

  # Define colors for each metric
  palette_brazil <- list(
    green = "#009C3B",
    yellow = "#FFDF00",
    blue = "#002776"
  )

  bar_colors <- c(
    "Baseline" = palette_brazil$blue,
    "Shock" = palette_brazil$green,
    "Difference" = palette_brazil$yellow
  )

  colors_vec <- sapply(summary_data$metric, function(m) bar_colors[m])

  # Build hover text with conditional % change display
  hover_text <- sapply(seq_len(nrow(summary_data)), function(i) {
    row <- summary_data[i, ]
    base_text <- paste0(
      "<b>", row$metric, "</b><br>",
      "Total Expected Loss: R$", format(round(row$value), big.mark = ",")
    )
    
    # Add % change only for "Difference" row
    if (row$metric == "Difference" && !is.na(row$pct_change)) {
      base_text <- paste0(
        base_text, "<br>",
        "% Change: ", sprintf("%.2f", row$pct_change), "%"
      )
    }
    
    paste0(base_text, "<br><extra></extra>")
  })

  # Create plot
  p <- plotly::plot_ly(
    data = summary_data,
    x = ~metric,
    y = ~value,
    type = "bar",
    marker = list(color = colors_vec),
    hovertemplate = hover_text
  ) |>
    plotly::layout(
      xaxis = list(
        title = "",
        categoryorder = "array",
        categoryarray = c("Baseline", "Shock", "Difference")
      ),
      yaxis = list(
        title = "Total Expected Loss (R$)",
        showgrid = TRUE,
        gridcolor = "#DDE5EC"
      ),
      hovermode = "closest",
      margin = list(l = 80, r = 20, t = 40, b = 60)
    )

  p
}
