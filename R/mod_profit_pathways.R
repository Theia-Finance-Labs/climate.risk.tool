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
      shiny::div(
        class = "pathways-actions",
        shiny::downloadButton(
          ns("download_profit_pathways_csv"),
          "Download Profit Pathways (CSV)",
          class = "btn btn-info"
        ),
        shiny::downloadButton(
          ns("download_profit_pathways_excel"),
          "Download Profit Pathways (Excel)",
          class = "btn btn-info"
        )
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
      shiny::div(
        id = ns("log_scale_note"),
        class = "text-muted",
        style = "margin-top: -1rem; margin-bottom: 2.5rem; font-size: 0.85em;",
        "Log scale requires positive profits. Zero or negative profits are displayed just above zero; hover to see the original value."
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
#' @param cnae_exposure_reactive reactive returning CNAE exposure lookup table
#' @export
mod_profit_pathways_server <- function(id, results_reactive, cnae_exposure_reactive = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Store selected assets
    selected_assets <- shiny::reactiveVal(character(0))

    resolve_cnae_exposure <- function() {
      if (is.null(cnae_exposure_reactive)) {
        return(NULL)
      }
      cnae_exposure_reactive()
    }

    # Asset metadata with sector names and share of economic activity
    asset_metadata <- shiny::reactive({
      results <- results_reactive()
      if (is.null(results) || is.null(results$assets_factors)) {
        return(tibble::tibble())
      }

      metadata <- results$assets_factors |>
        dplyr::select(
          dplyr::any_of(c(
            "asset",
            "company",
            "share_of_economic_activity",
            "asset_category",
            "asset_subtype",
            "cnae",
            "sector"
          ))
        ) |>
        attach_sector_metadata(resolve_cnae_exposure())

      metadata |>
        dplyr::select(
          dplyr::any_of(c(
            "asset",
            "company",
            "share_of_economic_activity",
            "asset_category",
            "asset_subtype",
            "sector_name",
            "sector_code"
          ))
        ) |>
        dplyr::distinct() |>
        dplyr::arrange(.data$asset)
    })

    # Prepare baseline profit trajectories
    baseline_data <- shiny::reactive({
      results <- results_reactive()
      if (is.null(results) || is.null(results$assets_yearly)) {
        return(NULL)
      }
      prepare_profit_trajectories(
        results$assets_yearly,
        "baseline",
        asset_metadata()
      )
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

      prepare_profit_trajectories(
        results$assets_yearly,
        shock_scenario,
        asset_metadata()
      )
    })

    download_data <- shiny::reactive({
      results <- results_reactive()
      metadata <- asset_metadata()
      if (is.null(results) || is.null(results$assets_yearly) || nrow(results$assets_yearly) == 0) {
        return(NULL)
      }

      join_columns <- metadata |>
        dplyr::select(
          dplyr::any_of(c(
            "asset",
            "share_of_economic_activity",
            "sector_name",
            "sector_code",
            "asset_category",
            "asset_subtype"
          ))
        )

      results$assets_yearly |>
        dplyr::left_join(join_columns, by = "asset")
    })

    session$userData$download_profit_pathways_data <- download_data

    output$download_profit_pathways_csv <- shiny::downloadHandler(
      filename = function() {
        paste0("profit_pathways_", Sys.Date(), ".csv")
      },
      content = function(file) {
        data <- download_data()
        if (is.null(data) || nrow(data) == 0) {
          utils::write.csv(data.frame(message = "No profit pathways available"), file, row.names = FALSE)
        } else {
          utils::write.csv(as.data.frame(data), file, row.names = FALSE)
        }
      }
    )

    output$download_profit_pathways_excel <- shiny::downloadHandler(
      filename = function() {
        paste0("profit_pathways_", Sys.Date(), ".xlsx")
      },
      content = function(file) {
        data <- download_data()
        if (is.null(data) || nrow(data) == 0) {
          writexl::write_xlsx(data.frame(message = "No profit pathways available"), path = file)
        } else {
          writexl::write_xlsx(as.data.frame(data), path = file)
        }
      }
    )

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
      metadata <- asset_metadata()
      if (nrow(metadata) == 0) {
        session$userData$profit_pathways_assets_table <- NULL
        return(NULL)
      }

      display <- metadata |>
        dplyr::mutate(
          share_pct = dplyr::if_else(
            !is.na(.data$share_of_economic_activity),
            sprintf("%.1f%%", .data$share_of_economic_activity * 100),
            NA_character_
          )
        ) |>
        dplyr::select(
          asset,
          company,
          sector_name,
          share_pct,
          dplyr::any_of(c("asset_category", "asset_subtype"))
        ) |>
        dplyr::rename(
          Asset = asset,
          Company = company,
          Sector = sector_name,
          `Share of economic activity` = share_pct
        )

      if ("asset_category" %in% names(display)) {
        display <- display |>
          dplyr::rename(Category = asset_category)
      }

      if ("asset_subtype" %in% names(display)) {
        display <- display |>
          dplyr::rename(Subtype = asset_subtype)
      }

      session$userData$profit_pathways_assets_table <- display

      DT::datatable(
        display,
        options = list(
          pageLength = 15,
          scrollX = TRUE
        ),
        rownames = FALSE,
        selection = "multiple"
      )
    })

    # Update selected assets when table rows are clicked
    shiny::observeEvent(input$assets_selection_table_rows_selected, {
      metadata <- asset_metadata()
      if (nrow(metadata) == 0) {
        return()
      }

      selected_rows <- input$assets_selection_table_rows_selected
      if (length(selected_rows) == 0) {
        selected_assets(character(0))
      } else {
        selected <- metadata$asset[selected_rows]
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

  # Prepare columns used for plotting and hovering
  data <- data |>
    dplyr::mutate(
      profit_plot = .data$profit,
      profit_for_hover = .data$profit,
      profit_clipped_text = ""
    )

  # Handle log scale: clip non-positive values while keeping hover information
  if (log_scale) {
    positive_values <- data |>
      dplyr::filter(!is.na(.data$profit) & .data$profit > 0)

    if (nrow(positive_values) == 0) {
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

    min_positive <- min(positive_values$profit, na.rm = TRUE)
    replacement_value <- min_positive / 10

    data <- data |>
      dplyr::mutate(
        profit_plot = dplyr::if_else(
          !is.na(.data$profit) & .data$profit > 0,
          .data$profit,
          replacement_value
        ),
        profit_for_hover = .data$profit,
        profit_clipped_text = ""
      )
  }

  palette_brazil <- list(
    green = "#009C3B",
    yellow = "#FFDF00",
    blue = "#002776",
    white = "#FFFFFF"
  )

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

    company_label <- if ("company" %in% names(asset_data)) asset_data$company[1] else NULL
    company_label <- dplyr::coalesce(company_label, "")

    sector_label <- if ("sector_name" %in% names(asset_data)) asset_data$sector_name[1] else NULL
    sector_label <- dplyr::coalesce(sector_label, "")

    share_value <- if ("share_of_economic_activity" %in% names(asset_data)) asset_data$share_of_economic_activity[1] else NA_real_
    share_text <- if (!is.na(share_value)) sprintf("Share: %.1f%%<br>", share_value * 100) else ""

    # Format hover template based on scale
    if (log_scale) {
      hovertemplate_str <- paste0(
        "<b>", asset_name, "</b><br>",
        if (nzchar(company_label)) paste0("Company: ", company_label, "<br>") else "",
        if (nzchar(sector_label)) paste0("Sector: ", sector_label, "<br>") else "",
        share_text,
        "Year: %{x}<br>",
        "Profit: R$%{customdata:,.2f}%{text}",
        "<extra></extra>"
      )
    } else {
      hovertemplate_str <- paste0(
        "<b>", asset_name, "</b><br>",
        if (nzchar(company_label)) paste0("Company: ", company_label, "<br>") else "",
        if (nzchar(sector_label)) paste0("Sector: ", sector_label, "<br>") else "",
        share_text,
        "Year: %{x}<br>",
        "Profit: R$%{customdata:,.0f}<br>",
        "<extra></extra>"
      )
    }

    p <- p |>
      plotly::add_trace(
        data = asset_data,
        x = ~year,
        y = ~profit_plot,
        type = "scatter",
        mode = "lines",
        name = asset_name,
        line = list(
          width = if (is_highlighted) 4 else 1,
          color = if (is_highlighted) palette_brazil$yellow else palette_brazil$blue
        ),
        opacity = if (is_highlighted) 1 else 0.35,
        customdata = ~profit_for_hover,
        text = ~profit_clipped_text,
        hovertemplate = hovertemplate_str
      )
  }

  # Configure y-axis based on scale type
  yaxis_config <- list(
    title = if (log_scale) "Profit (R$, log scale)" else "Profit (R$)",
    showgrid = TRUE,
    gridcolor = "#DDE5EC"
  )

  if (log_scale) {
    yaxis_config$type <- "log"
  }

  # Layout
  p <- p |>
    plotly::layout(
      title = list(
        text = title,
        font = list(size = 16, color = palette_brazil$blue)
      ),
      xaxis = list(
        title = "Year",
        showgrid = TRUE,
        gridcolor = "#DDE5EC"
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
