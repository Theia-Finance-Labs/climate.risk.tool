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
          "Showing top 20 companies by absolute expected loss change. Search to add more companies to the chart.",
          class = "text-muted",
          style = "margin-bottom: 1rem;"
        ),
        shiny::div(
          style = "margin-bottom: 1rem;",
          shiny::uiOutput(ns("company_search_ui"))
        ),
        shiny::div(
          class = "chart-container",
          plotly::plotlyOutput(ns("expected_loss_change_plot"), height = "500px")
        )
      ),

      # FI Expected Loss Change Plot (only rendered when FI column is present)
      shiny::uiOutput(ns("fi_plot_section")),

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
#' @param results_reactive reactive containing analysis results (median)
#' @param uncertainty_mode_reactive optional reactive returning logical
#' @param uncertainty_results_reactive optional reactive returning list(median, p10, p90)
#' @export
mod_company_analysis_server <- function(id, results_reactive,
                                        uncertainty_mode_reactive = NULL,
                                        uncertainty_results_reactive = NULL) {
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

    # Render the company search dropdown (only when results are available)
    output$company_search_ui <- shiny::renderUI({
      if (!has_results()) return(NULL)
      results <- results_reactive()
      all_companies <- sort(results$companies$company)
      shiny::selectizeInput(
        ns("extra_companies"),
        label = NULL,
        choices = all_companies,
        selected = NULL,
        multiple = TRUE,
        options = list(
          placeholder = "Search and add companies to the chart...",
          maxOptions = 50
        )
      )
    })

    get_uncertainty_mode <- function() {
      if (is.null(uncertainty_mode_reactive)) return(FALSE)
      isTRUE(uncertainty_mode_reactive())
    }

    get_uncertainty_results <- function() {
      if (is.null(uncertainty_results_reactive)) return(NULL)
      uncertainty_results_reactive()
    }

    # Expected Loss Change Plot
    output$expected_loss_change_plot <- plotly::renderPlotly({
      if (!has_results()) {
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
      extra <- input$extra_companies
      if (get_uncertainty_mode()) {
        unc <- get_uncertainty_results()
        create_expected_loss_change_plot(
          results$companies,
          unc$p10$companies,
          unc$p90$companies,
          extra_companies = extra
        )
      } else {
        create_expected_loss_change_plot(results$companies, extra_companies = extra)
      }
    })

    # FI Expected Loss Change Plot - conditionally shown
    output$fi_plot_section <- shiny::renderUI({
      if (!has_results()) return(NULL)

      results <- results_reactive()
      companies <- results$companies

      # Only render when at least one non-NA/non-empty FI value exists
      if (!"fi" %in% names(companies)) return(NULL)
      fi_vals <- trimws(as.character(companies$fi))
      if (all(is.na(companies$fi) | fi_vals == "" | fi_vals == "NA")) return(NULL)

      shiny::div(
        class = "chart-section",
        style = "margin-bottom: 3rem;",
        shiny::h4("Change in Expected Loss per FI", class = "section-header"),
        shiny::p(
          paste0(
            "Percentage change in total expected loss from baseline to shock, ",
            "aggregated by Financial Institution. ",
            "Companies with no FI specified are grouped as 'Unknown'."
          ),
          class = "text-muted",
          style = "margin-bottom: 1rem;"
        ),
        shiny::div(
          class = "chart-container",
          plotly::plotlyOutput(ns("fi_el_change_plot"), height = "400px")
        )
      )
    })

    output$fi_el_change_plot <- plotly::renderPlotly({
      if (!has_results()) return(plotly::plot_ly())
      results <- results_reactive()
      if (get_uncertainty_mode()) {
        unc <- get_uncertainty_results()
        create_fi_expected_loss_plot(results$companies, unc$p10$companies, unc$p90$companies)
      } else {
        create_fi_expected_loss_plot(results$companies)
      }
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
      if (get_uncertainty_mode()) {
        unc <- get_uncertainty_results()
        summary_median <- compute_portfolio_summary(results$companies)
        summary_p10    <- compute_portfolio_summary(unc$p10$companies)
        summary_p90    <- compute_portfolio_summary(unc$p90$companies)
        create_portfolio_summary_plot(summary_median, summary_p10, summary_p90)
      } else {
        summary_data <- compute_portfolio_summary(results$companies)
        message("[mod_company_analysis] Summary data: ", nrow(summary_data), " rows")
        create_portfolio_summary_plot(summary_data)
      }
    })

    # Portfolio percentage change text output
    output$portfolio_pct_change <- shiny::renderText({
      if (!has_results()) {
        return("-")
      }

      results <- results_reactive()

      get_pct <- function(companies_df) {
        sd <- compute_portfolio_summary(companies_df)
        row <- dplyr::filter(sd, .data$metric == "Difference")
        if (nrow(row) == 0 || is.na(row$pct_change[1])) NA_real_ else row$pct_change[1]
      }

      pct_median <- get_pct(results$companies)

      if (get_uncertainty_mode()) {
        unc <- get_uncertainty_results()
        pct_p10 <- get_pct(unc$p10$companies)
        pct_p90 <- get_pct(unc$p90$companies)
        if (is.na(pct_median)) return("N/A")
        sprintf("%+.2f%% [P10: %+.2f%%, P90: %+.2f%%]", pct_median, pct_p10, pct_p90)
      } else {
        if (is.na(pct_median)) return("N/A")
        sprintf("%+.2f%%", pct_median)
      }
    })

    # Companies table
    output$companies_table <- DT::renderDataTable({
      results <- results_reactive()
      if (is.null(results) || is.null(results$companies)) {
        return(NULL)
      }

      companies <- results$companies

      if (get_uncertainty_mode()) {
        unc <- get_uncertainty_results()
        companies <- build_uncertainty_companies_table(
          companies,
          unc$p10$companies,
          unc$p90$companies
        )
      }

      # Create a display copy for formatting
      companies_display <- companies

      # Format numeric columns for better display
      for (col in names(companies_display)) {
        if (is.numeric(companies_display[[col]])) {
          if (grepl("_pct$|_change_pct$", col, ignore.case = TRUE)) {
            companies_display[[col]] <- paste0(sprintf("%.2f", companies_display[[col]]), "%")
          } else if (grepl("^pd_|_pd$", col, ignore.case = TRUE)) {
            companies_display[[col]] <- paste0(sprintf("%.4f", companies_display[[col]] * 100), "%")
          } else if (grepl("npv|loss", col, ignore.case = TRUE)) {
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

      if (get_uncertainty_mode()) {
        unc <- get_uncertainty_results()
        build_uncertainty_companies_table(
          results$companies,
          unc$p10$companies,
          unc$p90$companies
        )
      } else {
        results$companies
      }
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
#' @param companies_df Data frame with median company results
#' @param companies_p10 optional data frame with P10 company results
#' @param companies_p90 optional data frame with P90 company results
#' @param extra_companies optional character vector of additional companies to show
#' @return plotly object
#' @noRd
create_expected_loss_change_plot <- function(companies_df,
                                             companies_p10 = NULL,
                                             companies_p90 = NULL,
                                             extra_companies = NULL) {
  if (is.null(companies_df) || nrow(companies_df) == 0) {
    return(plotly::plot_ly())
  }

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

  palette_brazil <- list(
    green = "#009C3B",
    yellow = "#FFDF00",
    blue = "#002776",
    orange = "#FF6B35"
  )

  max_bars <- 20
  total_companies <- nrow(companies_df)

  top_data <- companies_df |>
    dplyr::arrange(dplyr::desc(abs(.data$Expected_loss_change_pct))) |>
    utils::head(max_bars)

  top_companies <- top_data$company

  extra_in_data <- if (!is.null(extra_companies) && length(extra_companies) > 0) {
    companies_df |>
      dplyr::filter(.data$company %in% extra_companies & !(.data$company %in% top_companies))
  } else {
    companies_df[0, ]
  }

  plot_data <- dplyr::bind_rows(
    top_data |>
      dplyr::arrange(dplyr::desc(.data$Expected_loss_change_pct)) |>
      dplyr::mutate(.highlighted = FALSE),
    extra_in_data |>
      dplyr::arrange(dplyr::desc(.data$Expected_loss_change_pct)) |>
      dplyr::mutate(.highlighted = TRUE)
  ) |>
    dplyr::mutate(company = factor(.data$company, levels = .data$company))

  bar_colors <- dplyr::case_when(
    plot_data$.highlighted ~ palette_brazil$orange,
    plot_data$Expected_loss_change_pct > 0 ~ palette_brazil$yellow,
    TRUE ~ palette_brazil$green
  )

  n_shown <- nrow(top_data)
  n_extra <- nrow(extra_in_data)
  chart_title <- if (total_companies > max_bars) {
    extra_note <- if (n_extra > 0) paste0(" + ", n_extra, " selected") else ""
    paste0(
      "Top ", n_shown, " companies by absolute Expected Loss Change", extra_note,
      " (", total_companies, " total)"
    )
  } else {
    "Expected Loss % Change by Company"
  }

  show_uncertainty <- !is.null(companies_p10) && !is.null(companies_p90) &&
    "Expected_loss_change_pct" %in% names(companies_p10) &&
    "Expected_loss_change_pct" %in% names(companies_p90)

  if (show_uncertainty) {
    p10_vals <- companies_p10[
      match(as.character(plot_data$company), companies_p10$company),
      "Expected_loss_change_pct",
      drop = TRUE
    ]
    p90_vals <- companies_p90[
      match(as.character(plot_data$company), companies_p90$company),
      "Expected_loss_change_pct",
      drop = TRUE
    ]

    error_minus <- plot_data$Expected_loss_change_pct - p10_vals
    error_plus  <- p90_vals - plot_data$Expected_loss_change_pct

    hover_text <- paste0(
      "<b>", plot_data$company, "</b><br>",
      "Median: ", sprintf("%.2f", plot_data$Expected_loss_change_pct), "%<br>",
      "P10: ", sprintf("%.2f", p10_vals), "% | P90: ", sprintf("%.2f", p90_vals), "%"
    )

    p <- plotly::plot_ly(
      data = plot_data,
      x = ~company,
      y = ~Expected_loss_change_pct,
      type = "bar",
      name = "Median",
      marker = list(color = bar_colors),
      error_y = list(
        type = "data",
        symmetric = FALSE,
        array = error_plus,
        arrayminus = error_minus,
        color = "#555555",
        thickness = 1.5,
        width = 4
      ),
      hovertext = hover_text,
      hovertemplate = "%{hovertext}<extra></extra>"
    )
  } else {
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
    )
  }

  p |>
    plotly::layout(
      title = list(text = chart_title, font = list(size = 13)),
      xaxis = list(title = "Company", tickangle = -45),
      yaxis = list(
        title = "Expected Loss Change (%)",
        showgrid = TRUE,
        gridcolor = "#DDE5EC"
      ),
      hovermode = "closest",
      margin = list(l = 60, r = 20, t = 50, b = 150)
    )
}

#' Create Portfolio Summary Plot
#'
#' @param summary_data Data frame with metric, value, and pct_change columns (median)
#' @param summary_p10 optional Data frame for P10 results
#' @param summary_p90 optional Data frame for P90 results
#' @return plotly object
#' @noRd
create_portfolio_summary_plot <- function(summary_data,
                                          summary_p10 = NULL,
                                          summary_p90 = NULL) {
  if (is.null(summary_data) || nrow(summary_data) == 0) {
    return(plotly::plot_ly())
  }

  palette_brazil <- list(green = "#009C3B", yellow = "#FFDF00", blue = "#002776")

  show_uncertainty <- !is.null(summary_p10) && !is.null(summary_p90)

  if (show_uncertainty) {
    metrics_order <- c("Baseline", "Shock", "Difference")

    bar_colors <- c(
      "Baseline"   = palette_brazil$blue,
      "Shock"      = palette_brazil$green,
      "Difference" = palette_brazil$yellow
    )

    get_val <- function(sd, metric_name) {
      sd$value[match(metric_name, sd$metric)]
    }

    med_vals  <- sapply(metrics_order, function(m) get_val(summary_data, m))
    p10_vals  <- sapply(metrics_order, function(m) get_val(summary_p10,  m))
    p90_vals  <- sapply(metrics_order, function(m) get_val(summary_p90,  m))

    hover_text <- mapply(function(metric, med, p10, p90, pct_row) {
      base <- paste0(
        "<b>", metric, "</b><br>",
        "Median EL: R$", formatC(round(med), format="f", digits=0, big.mark=","), "<br>",
        "P10: R$", formatC(round(p10), format="f", digits=0, big.mark=","),
        " | P90: R$", formatC(round(p90), format="f", digits=0, big.mark=",")
      )
      if (metric == "Difference") {
        pct_med <- summary_data$pct_change[summary_data$metric == "Difference"]
        pct_p10 <- summary_p10$pct_change[summary_p10$metric == "Difference"]
        pct_p90 <- summary_p90$pct_change[summary_p90$metric == "Difference"]
        if (length(pct_med) && !is.na(pct_med)) {
          base <- paste0(base, "<br>Change: ", sprintf("%.2f%%", pct_med),
                         " [P10: ", sprintf("%.2f%%", pct_p10),
                         " | P90: ", sprintf("%.2f%%", pct_p90), "]")
        }
      }
      paste0(base, "<extra></extra>")
    }, metrics_order, med_vals, p10_vals, p90_vals, SIMPLIFY = TRUE)

    p <- plotly::plot_ly(
      x = metrics_order,
      y = med_vals,
      type = "bar",
      marker = list(color = sapply(metrics_order, function(m) bar_colors[m])),
      error_y = list(
        type = "data",
        symmetric = FALSE,
        array = p90_vals - med_vals,
        arrayminus = med_vals - p10_vals,
        color = "#555555",
        thickness = 1.5,
        width = 6
      ),
      hovertemplate = hover_text
    ) |>
      plotly::layout(
        xaxis = list(
          title = "",
          categoryorder = "array",
          categoryarray = metrics_order
        ),
        yaxis = list(
          title = "Total Expected Loss (R$)",
          showgrid = TRUE,
          gridcolor = "#DDE5EC"
        ),
        hovermode = "closest",
        margin = list(l = 80, r = 20, t = 40, b = 60)
      )
    return(p)
  }

  bar_colors <- c(
    "Baseline"   = palette_brazil$blue,
    "Shock"      = palette_brazil$green,
    "Difference" = palette_brazil$yellow
  )
  colors_vec <- sapply(summary_data$metric, function(m) bar_colors[m])

  hover_text <- sapply(seq_len(nrow(summary_data)), function(i) {
    row <- summary_data[i, ]
    base_text <- paste0(
      "<b>", row$metric, "</b><br>",
      "Total Expected Loss: R$", format(round(row$value), big.mark = ",")
    )
    if (row$metric == "Difference" && !is.na(row$pct_change)) {
      base_text <- paste0(base_text, "<br>% Change: ", sprintf("%.2f", row$pct_change), "%")
    }
    paste0(base_text, "<br><extra></extra>")
  })

  plotly::plot_ly(
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
}

#' Create FI Expected Loss Change Plot
#'
#' @param companies_df Data frame with median company results
#' @param companies_p10 optional Data frame with P10 company results
#' @param companies_p90 optional Data frame with P90 company results
#' @return plotly object
#' @noRd
create_fi_expected_loss_plot <- function(companies_df,
                                         companies_p10 = NULL,
                                         companies_p90 = NULL) {
  if (is.null(companies_df) || nrow(companies_df) == 0) {
    return(plotly::plot_ly())
  }
  if (!all(c("fi", "Expected_loss_baseline", "Expected_loss_shock") %in% names(companies_df))) {
    return(plotly::plot_ly())
  }

  palette_brazil <- list(green = "#009C3B", yellow = "#FFDF00", blue = "#002776")

  normalise_fi <- function(df) {
    df |>
      dplyr::mutate(
        fi_display = dplyr::case_when(
          is.na(.data$fi) | trimws(as.character(.data$fi)) %in% c("", "NA") ~ "Unknown",
          TRUE ~ trimws(as.character(.data$fi))
        )
      ) |>
      dplyr::group_by(.data$fi_display) |>
      dplyr::summarise(
        EL_baseline = sum(.data$Expected_loss_baseline, na.rm = TRUE),
        EL_shock    = sum(.data$Expected_loss_shock,    na.rm = TRUE),
        .groups = "drop"
      ) |>
      dplyr::mutate(
        EL_change_pct = dplyr::if_else(
          .data$EL_baseline == 0 | is.na(.data$EL_baseline),
          NA_real_,
          (.data$EL_shock - .data$EL_baseline) / .data$EL_baseline * 100
        )
      ) |>
      dplyr::filter(!is.na(.data$EL_change_pct))
  }

  fi_median <- normalise_fi(companies_df) |>
    dplyr::arrange(dplyr::desc(.data$EL_change_pct)) |>
    dplyr::mutate(fi_display = factor(.data$fi_display, levels = .data$fi_display))

  if (nrow(fi_median) == 0) return(plotly::plot_ly())

  show_uncertainty <- !is.null(companies_p10) && !is.null(companies_p90) &&
    all(c("fi", "Expected_loss_baseline", "Expected_loss_shock") %in% names(companies_p10)) &&
    all(c("fi", "Expected_loss_baseline", "Expected_loss_shock") %in% names(companies_p90))

  bar_colors <- ifelse(fi_median$EL_change_pct > 0, palette_brazil$yellow, palette_brazil$green)

  if (show_uncertainty) {
    fi_p10 <- normalise_fi(companies_p10)
    fi_p90 <- normalise_fi(companies_p90)

    match_pct <- function(fi_df, fi_levels) {
      fi_df$EL_change_pct[match(as.character(fi_levels), fi_df$fi_display)]
    }

    p10_vals <- match_pct(fi_p10, fi_median$fi_display)
    p90_vals <- match_pct(fi_p90, fi_median$fi_display)

    hover_text <- paste0(
      "<b>", fi_median$fi_display, "</b><br>",
      "Median: ", sprintf("%.2f", fi_median$EL_change_pct), "%<br>",
      "P10: ", sprintf("%.2f", p10_vals), "% | P90: ", sprintf("%.2f", p90_vals), "%"
    )

    p <- plotly::plot_ly(
      data = fi_median,
      x = ~fi_display,
      y = ~EL_change_pct,
      type = "bar",
      name = "Median",
      marker = list(color = bar_colors),
      error_y = list(
        type = "data",
        symmetric = FALSE,
        array = p90_vals - fi_median$EL_change_pct,
        arrayminus = fi_median$EL_change_pct - p10_vals,
        color = "#555555",
        thickness = 1.5,
        width = 4
      ),
      hovertext = hover_text,
      hovertemplate = "%{hovertext}<extra></extra>"
    )
  } else {
    hover_text <- paste0(
      "<b>", fi_median$fi_display, "</b><br>",
      "EL Baseline: R$", format(round(fi_median$EL_baseline), big.mark = ","), "<br>",
      "EL Shock: R$",    format(round(fi_median$EL_shock),    big.mark = ","), "<br>",
      "Change: ", sprintf("%.2f", fi_median$EL_change_pct), "%"
    )

    p <- plotly::plot_ly(
      data = fi_median,
      x = ~fi_display,
      y = ~EL_change_pct,
      type = "bar",
      marker = list(color = bar_colors),
      hovertext = hover_text,
      hovertemplate = "%{hovertext}<extra></extra>"
    )
  }

  p |>
    plotly::layout(
      xaxis = list(title = "Financial Institution", tickangle = -30),
      yaxis = list(
        title = "Expected Loss Change (%)",
        showgrid = TRUE,
        gridcolor = "#DDE5EC"
      ),
      hovermode = "closest",
      margin = list(l = 60, r = 20, t = 40, b = 100)
    )
}

#' Build an uncertainty-aware companies table
#'
#' Merges median, P10, and P90 company results into a single wide table where
#' key metrics have "(P10 | Median | P90)" formatted columns.
#'
#' @param companies_median Data frame - median results
#' @param companies_p10 Data frame - P10 results
#' @param companies_p90 Data frame - P90 results
#' @return tibble with uncertainty range columns
#' @noRd
build_uncertainty_companies_table <- function(companies_median, companies_p10, companies_p90) {
  key_cols <- c(
    "NPV_baseline", "NPV_shock", "NPV_change_pct",
    "PD_baseline", "PD_shock",
    "Expected_loss_baseline", "Expected_loss_shock", "Expected_loss_change_pct"
  )

  format_range <- function(p10, med, p90, fmt_fn) {
    paste0(fmt_fn(med), " [", fmt_fn(p10), " - ", fmt_fn(p90), "]")
  }

  fmt_pct  <- function(x) sprintf("%.2f%%", x)
  fmt_pd   <- function(x) sprintf("%.4f%%", x * 100)
  fmt_curr <- function(x) paste0("R$", formatC(round(x), format = "f", digits = 0, big.mark = ","))

  result <- companies_median |>
    dplyr::select(dplyr::any_of(c("company", "fi")))

  add_range_col <- function(df, col, fmt_fn) {
    med <- companies_median[[col]]
    p10 <- companies_p10[[col]]
    p90 <- companies_p90[[col]]
    if (is.null(med) || is.null(p10) || is.null(p90)) return(df)
    df[[col]] <- format_range(p10, med, p90, fmt_fn)
    df
  }

  for (col in c("NPV_baseline", "NPV_shock", "Expected_loss_baseline", "Expected_loss_shock")) {
    result <- add_range_col(result, col, fmt_curr)
  }
  for (col in c("NPV_change_pct", "Expected_loss_change_pct")) {
    result <- add_range_col(result, col, fmt_pct)
  }
  for (col in c("PD_baseline", "PD_shock")) {
    result <- add_range_col(result, col, fmt_pd)
  }

  result
}
