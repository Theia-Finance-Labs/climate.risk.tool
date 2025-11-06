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
      shiny::p(
        "Expand a hazard to review the associated assets and impact metrics.",
        class = "text-muted",
        style = "margin-bottom: 1.5rem;"
      ),
      shiny::div(
        class = "results-downloads",
        shiny::downloadButton(
          ns("download_assets_csv"),
          "Download Assets (CSV)",
          class = "btn btn-info"
        ),
        shiny::downloadButton(
          ns("download_assets_excel"),
          "Download Assets (Excel)",
          class = "btn btn-info"
        )
      ),
      shiny::uiOutput(ns("hazard_tables"))
    )
  )
}

#' results_assets Server Functions
#'
#' @param id Internal parameter for shiny
#' @param results_reactive reactive containing analysis results
#' @param name_mapping_reactive reactive containing region name mapping dictionary
#' @param cnae_exposure_reactive reactive returning CNAE exposure lookup table
#' @export
mod_results_assets_server <- function(id, results_reactive, name_mapping_reactive = NULL, cnae_exposure_reactive = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    resolve_cnae_exposure <- function() {
      if (is.null(cnae_exposure_reactive)) {
        return(NULL)
      }
      cnae_exposure_reactive()
    }

    format_assets_table <- function(assets_df, name_mapping, cnae_exposure) {
      if (is.null(assets_df) || nrow(assets_df) == 0) {
        return(assets_df)
      }

      # Convert normalized province/municipality names back to original names for display
      if (!is.null(name_mapping)) {
        if ("province" %in% names(assets_df) && !is.null(name_mapping$province) && length(name_mapping$province) > 0) {
          province_lookup <- name_mapping$province
          assets_df <- assets_df |>
            dplyr::mutate(
              province = dplyr::if_else(
                !is.na(.data$province) & .data$province %in% names(province_lookup),
                province_lookup[.data$province],
                .data$province
              )
            )
        }

        if ("municipality" %in% names(assets_df) && !is.null(name_mapping$municipality) && length(name_mapping$municipality) > 0) {
          municipality_lookup <- name_mapping$municipality
          assets_df <- assets_df |>
            dplyr::mutate(
              municipality = dplyr::if_else(
                !is.na(.data$municipality) & .data$municipality %in% names(municipality_lookup),
                municipality_lookup[.data$municipality],
                .data$municipality
              )
            )
        }
      }

      numeric_cols <- vapply(assets_df, is.numeric, logical(1))
      numeric_col_names <- names(assets_df)[numeric_cols]
      for (col in numeric_col_names) {
        if (grepl("ratio|intensity", col)) {
          assets_df[[col]] <- round(assets_df[[col]], 4)
        } else if (grepl("cost|value", col)) {
          assets_df[[col]] <- round(assets_df[[col]], 0)
        }
      }

      if ("share_of_economic_activity" %in% names(assets_df)) {
        assets_df <- assets_df |>
          dplyr::mutate(
            share_of_economic_activity = dplyr::if_else(
              !is.na(.data$share_of_economic_activity),
              sprintf("%.1f%%", .data$share_of_economic_activity * 100),
              NA_character_
            )
          )
      }

      original_has_sector <- "sector" %in% names(assets_df)

      assets_df <- attach_sector_metadata(assets_df, cnae_exposure)

      assets_df <- if (original_has_sector) {
        assets_df |>
          dplyr::mutate(
            sector = dplyr::coalesce(.data$sector_code, as.character(.data$sector))
          )
      } else {
        assets_df |>
          dplyr::mutate(
            sector = .data$sector_code
          )
      }

      assets_df <- assets_df |>
        dplyr::select(-dplyr::any_of("cnae"))

      priority_cols <- c("asset", "company", "sector", "sector_name", "sector_code", "share_of_economic_activity", "event_id", "hazard_name", "hazard_type", "matching_method", "hazard_return_period", "event_year")
      existing_priority <- intersect(priority_cols, names(assets_df))
      other_cols <- setdiff(names(assets_df), existing_priority)

      if (length(existing_priority) > 0) {
        assets_df <- assets_df[, c(existing_priority, other_cols), drop = FALSE]
      }

      assets_df
    }

    extract_unique_hazards <- function(assets_df) {
      hazard_name_exists <- "hazard_name" %in% names(assets_df)
      hazard_type_exists <- "hazard_type" %in% names(assets_df)

      assets_df |>
        dplyr::mutate(
          hazard_label = dplyr::case_when(
            hazard_name_exists & !is.na(.data$hazard_name) & nzchar(.data$hazard_name) ~ .data$hazard_name,
            hazard_type_exists & !is.na(.data$hazard_type) & nzchar(.data$hazard_type) ~ .data$hazard_type,
            TRUE ~ "Unknown hazard"
          )
        ) |>
        dplyr::select(
          "hazard_label",
          dplyr::any_of("hazard_name"),
          dplyr::any_of("hazard_type")
        ) |>
        dplyr::distinct()
    }

    output$hazard_tables <- shiny::renderUI({
      results <- results_reactive()
      if (is.null(results) || is.null(results$assets_factors) || nrow(results$assets_factors) == 0) {
        return(shiny::wellPanel(shiny::p("Asset results will appear here once the analysis completes.")))
      }

      assets <- results$assets_factors
      unique_hazards <- extract_unique_hazards(assets)

      if (nrow(unique_hazards) == 0) {
        return(shiny::wellPanel(shiny::p("No hazards available for display.")))
      }

      hazard_blocks <- purrr::imap(unique_hazards$hazard_label, function(label, idx) {
        table_output <- DT::dataTableOutput(ns(paste0("assets_table_", idx)))

        if (idx == 1) {
          shiny::tags$details(
            class = "hazard-panel",
            open = NA,
            shiny::tags$summary(class = "hazard-panel__summary", label),
            shiny::tags$div(class = "hazard-panel__table", table_output)
          )
        } else {
          shiny::tags$details(
            class = "hazard-panel",
            shiny::tags$summary(class = "hazard-panel__summary", label),
            shiny::tags$div(class = "hazard-panel__table", table_output)
          )
        }
      })

      shiny::tagList(hazard_blocks)
    })

    shiny::observe({
      results <- results_reactive()
      if (is.null(results) || is.null(results$assets_factors) || nrow(results$assets_factors) == 0) {
        session$userData$hazard_tables_data <- NULL
        return(NULL)
      }

      assets <- results$assets_factors
      unique_hazards <- extract_unique_hazards(assets)

      if (nrow(unique_hazards) == 0) {
        return(NULL)
      }

      session$userData$hazard_tables_data <- vector("list", length = nrow(unique_hazards))

      purrr::iwalk(unique_hazards$hazard_label, function(label, idx) {
        hazard_name_val <- if ("hazard_name" %in% names(unique_hazards)) unique_hazards$hazard_name[[idx]] else NA_character_
        hazard_type_val <- if ("hazard_type" %in% names(unique_hazards)) unique_hazards$hazard_type[[idx]] else NA_character_

        output[[paste0("assets_table_", idx)]] <- DT::renderDataTable({
          current_assets <- assets

          if (!is.na(hazard_name_val) && !is.null(hazard_name_val)) {
            current_assets <- current_assets |>
              dplyr::filter(.data$hazard_name == !!hazard_name_val)
          } else if (!is.na(hazard_type_val) && !is.null(hazard_type_val)) {
            current_assets <- current_assets |>
              dplyr::filter(.data$hazard_type == !!hazard_type_val)
          }

          name_mapping <- if (!is.null(name_mapping_reactive)) name_mapping_reactive() else NULL
          cnae_exposure <- resolve_cnae_exposure()
          formatted_assets <- format_assets_table(current_assets, name_mapping, cnae_exposure)

          if (is.null(formatted_assets) || nrow(formatted_assets) == 0) {
            session$userData$hazard_tables_data[[idx]] <- NULL
            return(DT::datatable(
              tibble::tibble(Message = "No assets available for this hazard."),
              options = list(dom = "t"),
              rownames = FALSE
            ))
          }

          session$userData$hazard_tables_data[[idx]] <- formatted_assets

          DT::datatable(
            formatted_assets,
            options = list(
              pageLength = 25,
              scrollX = TRUE
            ),
            rownames = FALSE
          )
        })
      })
    })

    assets_download_data <- shiny::reactive({
      results <- results_reactive()
      if (is.null(results) || is.null(results$assets_factors) || nrow(results$assets_factors) == 0) {
        return(NULL)
      }

      name_mapping <- if (!is.null(name_mapping_reactive)) name_mapping_reactive() else NULL
      format_assets_table(results$assets_factors, name_mapping, resolve_cnae_exposure())
    })

    output$download_assets_csv <- shiny::downloadHandler(
      filename = function() {
        paste0("asset_results_", Sys.Date(), ".csv")
      },
      content = function(file) {
        data <- assets_download_data()
        if (is.null(data) || nrow(data) == 0) {
          utils::write.csv(data.frame(message = "No asset results available"), file, row.names = FALSE)
        } else {
          utils::write.csv(as.data.frame(data), file, row.names = FALSE)
        }
      }
    )

    output$download_assets_excel <- shiny::downloadHandler(
      filename = function() {
        paste0("asset_results_", Sys.Date(), ".xlsx")
      },
      content = function(file) {
        data <- assets_download_data()
        if (is.null(data) || nrow(data) == 0) {
          writexl::write_xlsx(data.frame(message = "No asset results available"), path = file)
        } else {
          writexl::write_xlsx(as.data.frame(data), path = file)
        }
      }
    )
  })
}
