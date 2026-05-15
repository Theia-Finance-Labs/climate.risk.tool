#' status UI Function
#'
#' @description Module to display analysis status and configured events
#' @param id Internal parameter for shiny
#' @export
mod_status_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::div(
      class = "status-container",

      # Status section
      shiny::div(
        class = "status-section",
        shiny::h3("Analysis Status", class = "status-title"),
        shiny::div(
          class = "status-indicator",
          shiny::div(
            class = "status-badge",
            shiny::textOutput(ns("status_badge"))
          ),
          shiny::div(
            class = "status-message",
            shiny::textOutput(ns("status_message"))
          )
        )
      ),

      # Configured events section
      shiny::div(
        class = "events-section",
        shiny::h3("Configured Hazard Events", class = "section-header"),
        shiny::p(
          "Click the delete button to remove individual events.",
          class = "text-muted",
          style = "margin-bottom: 1rem; font-size: 0.9em;"
        ),
        shiny::div(
          class = "events-table-container",
          DT::dataTableOutput(ns("events_table"))
        ),
        shiny::div(
          class = "status-map-wrapper",
          shiny::h4("Selected Event Areas in Brazil", class = "section-header status-map-title"),
          shiny::p(
            "Highlighted areas update automatically. Use event buttons to show or hide each event layer.",
            class = "text-muted status-map-description"
          ),
          shiny::div(
            class = "status-map-shell",
            plotly::plotlyOutput(ns("events_map"), height = "420px")
          ),
          shiny::div(
            class = "status-map-toggle-container",
            shiny::uiOutput(ns("events_map_toggles"))
          )
        ),
        shiny::div(
          class = "status-repro-wrapper",
          shiny::tags$details(
            class = "hazard-panel status-repro-panel",
            shiny::tags$summary(
              class = "hazard-panel__summary status-repro-panel__summary",
              shiny::tags$span(
                class = "status-repro-panel__title",
                shiny::icon("code"),
                "Reproduction Code"
              )
            ),
            shiny::div(
              class = "hazard-panel__table status-repro-panel__body",
              shiny::div(
                class = "status-repro-toolbar",
                shiny::tags$button(
                  id = ns("copy_repro_code"),
                  type = "button",
                  class = "btn btn-secondary btn-sm status-repro-copy-btn",
                  `data-copy-target` = ns("run_repro_code"),
                  onclick = "copyStatusReproCode(this);",
                  shiny::icon("copy"),
                  "Copy to Clipboard"
                )
              ),
              shiny::p(
                "R code to reproduce the current live analysis configuration.",
                class = "text-muted status-repro-description"
              ),
              shiny::div(
                class = "status-repro-code-shell",
                shiny::verbatimTextOutput(ns("run_repro_code"), placeholder = TRUE)
              )
            )
          )
        )
      )
    )
  )
}

#' @noRd
format_spatial_selection <- function(level, region_codes, region_labels) {
  parse_vals <- function(x) {
    if (is.null(x) || length(x) == 0 || is.na(x) || !nzchar(trimws(as.character(x)))) {
      return(character(0))
    }
    vals <- unlist(strsplit(as.character(x), "[|;,]"))
    vals <- trimws(vals)
    vals[nzchar(vals)]
  }

  lvl <- tolower(trimws(as.character(level)))
  if (is.na(lvl) || !nzchar(lvl) || lvl == "brazil") {
    return("Brazil (whole)")
  }

  labels <- parse_vals(region_labels)
  codes <- parse_vals(region_codes)

  level_label <- dplyr::case_when(
    lvl == "state" ~ "States",
    lvl == "municipality" ~ "Municipalities",
    lvl == "macro" ~ "Macro regions",
    lvl == "meso" ~ "Meso regions",
    lvl == "micro" ~ "Micro regions",
    TRUE ~ stringr::str_to_title(lvl)
  )

  values <- if (length(labels) > 0) labels else codes
  if (length(values) == 0) {
    return(level_label)
  }

  preview <- paste(utils::head(values, 3), collapse = ", ")
  if (length(values) > 3) {
    paste0(level_label, ": ", preview, " (+", length(values) - 3, " more)")
  } else {
    paste0(level_label, ": ", preview)
  }
}

#' @noRd
parse_status_spatial_values <- function(x) {
  if (is.null(x) || length(x) == 0) {
    return(character(0))
  }
  x_chr <- as.character(x[[1]])
  if (is.na(x_chr) || !nzchar(trimws(x_chr))) {
    return(character(0))
  }
  vals <- unlist(strsplit(x_chr, "[|;,]"))
  vals <- trimws(vals)
  vals[nzchar(vals)]
}

#' @noRd
infer_status_spatial_scheme <- function(level, scheme) {
  lvl <- tolower(trimws(as.character(level)))
  sch <- tolower(trimws(as.character(scheme)))

  if (is.na(sch) || !nzchar(sch) || !sch %in% c("adm_regions", "hydro_regions")) {
    if (lvl %in% c("macro", "meso", "micro")) {
      return("hydro_regions")
    }
    return("adm_regions")
  }
  sch
}

#' @noRd
ensure_status_map_crs <- function(sf_obj) {
  if (is.null(sf_obj) || !inherits(sf_obj, "sf") || nrow(sf_obj) == 0) {
    return(sf_obj)
  }

  crs <- try(sf::st_crs(sf_obj), silent = TRUE)
  if (!inherits(crs, "try-error") && !is.null(crs) && !is.na(crs) && !is.null(crs$epsg) && crs$epsg != 4326) {
    return(sf::st_transform(sf_obj, 4326))
  }
  sf_obj
}

#' @noRd
build_status_brazil_geometry <- function(spatial_data) {
  if (is.null(spatial_data)) {
    return(NULL)
  }

  candidates <- list(
    spatial_data$adm$state,
    spatial_data$adm$municipality,
    spatial_data$hydro$macro,
    spatial_data$hydro$meso,
    spatial_data$hydro$micro
  )

  source_layer <- NULL
  for (candidate in candidates) {
    if (inherits(candidate, "sf") && nrow(candidate) > 0) {
      source_layer <- candidate
      break
    }
  }

  if (is.null(source_layer)) {
    return(NULL)
  }

  source_layer <- ensure_status_map_crs(source_layer)

  union_geom <- try(sf::st_union(sf::st_geometry(source_layer)), silent = TRUE)
  if (inherits(union_geom, "try-error") || length(union_geom) == 0) {
    return(NULL)
  }

  brazil_sf <- sf::st_sf(
    region_code = "BRAZIL",
    region_label = "Brazil (whole)",
    geometry = union_geom
  )
  sf::st_crs(brazil_sf) <- sf::st_crs(source_layer)
  ensure_status_map_crs(brazil_sf)
}

#' @noRd
resolve_status_event_geometry <- function(event_row, spatial_data, brazil_geometry = NULL) {
  if (is.null(event_row) || nrow(event_row) == 0 || is.null(spatial_data)) {
    return(NULL)
  }

  event_id <- as.character(event_row$event_id[[1]])
  if (is.na(event_id) || !nzchar(trimws(event_id))) {
    return(NULL)
  }

  level <- tolower(trimws(as.character(event_row$spatial_level[[1]])))
  if (is.na(level) || !nzchar(level)) {
    level <- "brazil"
  }

  scheme <- infer_status_spatial_scheme(level, event_row$spatial_scheme[[1]])

  if (identical(level, "brazil")) {
    if (is.null(brazil_geometry) || !inherits(brazil_geometry, "sf") || nrow(brazil_geometry) == 0) {
      return(NULL)
    }
    return(
      brazil_geometry |>
        dplyr::mutate(event_id = event_id, .before = 1)
    )
  }

  layer <- if (identical(scheme, "hydro_regions")) spatial_data$hydro[[level]] else spatial_data$adm[[level]]
  if (is.null(layer) || !inherits(layer, "sf") || nrow(layer) == 0 || !"region_code" %in% names(layer)) {
    return(NULL)
  }

  selected_codes <- parse_status_spatial_values(event_row$spatial_region_codes)
  selected_labels <- parse_status_spatial_values(event_row$spatial_region_labels)

  resolved_codes <- resolve_selected_region_codes(
    spatial_data = spatial_data,
    scheme = scheme,
    level = level,
    selected_codes = selected_codes,
    selected_labels = selected_labels
  )

  if (length(resolved_codes) == 0) {
    return(NULL)
  }

  selected_layer <- layer |>
    dplyr::filter(as.character(.data$region_code) %in% resolved_codes)

  if (nrow(selected_layer) == 0) {
    return(NULL)
  }

  selected_layer <- ensure_status_map_crs(selected_layer)

  selected_layer |>
    dplyr::mutate(event_id = event_id, .before = 1)
}

#' @noRd
normalize_status_events_for_map <- function(events) {
  if (is.null(events) || !is.data.frame(events) || nrow(events) == 0) {
    return(tibble::tibble())
  }

  out <- tibble::as_tibble(events)
  if (!"event_id" %in% names(out)) {
    out$event_id <- paste0("event_", seq_len(nrow(out)))
  }

  if (!"spatial_level" %in% names(out)) out$spatial_level <- "brazil"
  if (!"spatial_region_codes" %in% names(out)) out$spatial_region_codes <- NA_character_
  if (!"spatial_region_labels" %in% names(out)) out$spatial_region_labels <- NA_character_
  if (!"spatial_scheme" %in% names(out)) out$spatial_scheme <- NA_character_

  out <- out |>
    dplyr::mutate(
      event_id = as.character(.data$event_id),
      spatial_level = dplyr::if_else(
        is.na(.data$spatial_level) | !nzchar(trimws(as.character(.data$spatial_level))),
        "brazil",
        tolower(trimws(as.character(.data$spatial_level)))
      )
    ) |>
    dplyr::filter(!is.na(.data$event_id), nzchar(trimws(.data$event_id)))

  out$spatial_scheme <- vapply(
    seq_len(nrow(out)),
    function(i) infer_status_spatial_scheme(out$spatial_level[[i]], out$spatial_scheme[[i]]),
    character(1)
  )

  out
}

#' @noRd
build_status_event_geometries <- function(events, spatial_data) {
  events_norm <- normalize_status_events_for_map(events)
  if (nrow(events_norm) == 0 || is.null(spatial_data)) {
    return(list())
  }

  brazil_geom <- build_status_brazil_geometry(spatial_data)
  out <- list()

  for (i in seq_len(nrow(events_norm))) {
    ev_row <- events_norm[i, , drop = FALSE]
    event_id <- as.character(ev_row$event_id[[1]])
    ev_geom <- resolve_status_event_geometry(
      event_row = ev_row,
      spatial_data = spatial_data,
      brazil_geometry = brazil_geom
    )
    if (!is.null(ev_geom) && inherits(ev_geom, "sf") && nrow(ev_geom) > 0) {
      out[[event_id]] <- ev_geom
    }
  }

  out
}

#' @noRd
reconcile_event_toggle_states <- function(event_ids, current_states = NULL) {
  ids <- unique(as.character(event_ids))
  ids <- ids[!is.na(ids) & nzchar(trimws(ids))]

  if (length(ids) == 0) {
    return(stats::setNames(logical(0), character(0)))
  }

  next_states <- stats::setNames(rep(TRUE, length(ids)), ids)
  if (!is.null(current_states) && length(current_states) > 0) {
    current <- as.list(current_states)
    shared <- intersect(ids, names(current))
    if (length(shared) > 0) {
      next_states[shared] <- vapply(shared, function(id) isTRUE(current[[id]]), logical(1))
    }
  }

  next_states
}

#' @noRd
empty_status_map_plot <- function(message_text) {
  plotly::plot_ly(type = "scatter", mode = "lines") |>
    plotly::add_text(
      x = 0.5,
      y = 0.5,
      text = message_text,
      textposition = "middle center",
      showlegend = FALSE,
      hoverinfo = "skip"
    ) |>
    plotly::layout(
      xaxis = list(showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE, fixedrange = TRUE),
      yaxis = list(showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE, fixedrange = TRUE)
    )
}

#' status Server Functions
#'
#' @param id Internal parameter for shiny
#' @param status_reactive reactive containing current status message
#' @param events_reactive reactive containing configured events
#' @param delete_event_callback function to delete an event by event_id
#' @param run_repro_code_reactive optional reactive containing the generated run reproduction code
#' @param spatial_data_reactive optional reactive containing loaded spatial separation layers
#' @export
mod_status_server <- function(id, status_reactive, events_reactive, delete_event_callback = NULL, run_repro_code_reactive = NULL, spatial_data_reactive = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    map_toggle_states <- shiny::reactiveVal(stats::setNames(logical(0), character(0)))

    events_for_map <- shiny::reactive({
      events <- try(events_reactive(), silent = TRUE)
      if (inherits(events, "try-error")) {
        return(tibble::tibble())
      }
      normalize_status_events_for_map(events)
    })

    spatial_data_for_map <- shiny::reactive({
      if (is.null(spatial_data_reactive)) {
        return(NULL)
      }
      loaded <- try(spatial_data_reactive(), silent = TRUE)
      if (inherits(loaded, "try-error")) {
        return(NULL)
      }
      loaded
    })

    # Status badge
    output$status_badge <- shiny::renderText({
      status <- status_reactive()
      if (grepl("Error", status, ignore.case = TRUE)) {
        "ERROR"
      } else if (grepl("complete|ready", status, ignore.case = TRUE)) {
        "READY"
      } else if (grepl("running|loading", status, ignore.case = TRUE)) {
        "RUNNING"
      } else {
        "WAITING"
      }
    })

    # Status message
    output$status_message <- shiny::renderText({
      status_reactive()
    })

    output$run_repro_code <- shiny::renderText({
      code <- if (is.null(run_repro_code_reactive)) {
        "Reproduction code will appear here once the current run inputs are available."
      } else {
        run_repro_code_reactive()
      }

      session$userData$status_run_repro_code <- code
      code
    })

    shiny::observe({
      events <- events_for_map()
      ids <- if (nrow(events) > 0) as.character(events$event_id) else character(0)
      merged_states <- reconcile_event_toggle_states(ids, map_toggle_states())
      if (!identical(merged_states, map_toggle_states())) {
        map_toggle_states(merged_states)
      }
      session$userData$status_map_toggle_states <- merged_states
    })

    shiny::observeEvent(input$toggle_map_event, {
      event_id <- as.character(input$toggle_map_event)
      if (is.na(event_id) || !nzchar(trimws(event_id))) {
        return()
      }

      current <- map_toggle_states()
      if (!event_id %in% names(current)) {
        return()
      }

      current[[event_id]] <- !isTRUE(current[[event_id]])
      map_toggle_states(current)
      session$userData$status_map_toggle_states <- current
    })

    output$events_map_toggles <- shiny::renderUI({
      events <- events_for_map()
      if (nrow(events) == 0) {
        return(
          shiny::tags$p(
            "No events to toggle yet. Add hazards from the sidebar.",
            class = "text-muted status-map-toggle-empty"
          )
        )
      }

      states <- map_toggle_states()
      buttons <- lapply(seq_len(nrow(events)), function(i) {
        event_id <- as.character(events$event_id[[i]])
        # Use `[` instead of `[[` so missing names resolve to NA (not an error)
        # during transient reactive ordering when toggle state is still initializing.
        is_on <- isTRUE(unname(states[event_id][[1]]))
        state_label <- if (is_on) "ON" else "OFF"
        btn_class <- paste(
          "btn btn-sm status-map-toggle-btn",
          if (is_on) "status-map-toggle-btn--on" else "status-map-toggle-btn--off"
        )

        shiny::tags$button(
          type = "button",
          class = btn_class,
          onclick = paste0(
            "Shiny.setInputValue('",
            ns("toggle_map_event"),
            "', '",
            event_id,
            "', {priority: 'event'});"
          ),
          `aria-pressed` = if (is_on) "true" else "false",
          shiny::tags$span(class = "status-map-toggle-btn__id", event_id),
          shiny::tags$span(class = "status-map-toggle-btn__state", state_label)
        )
      })

      shiny::tagList(
        shiny::tags$p(
          class = "status-map-toggle-legend",
          "Toggle status: ",
          shiny::tags$strong("ON"),
          " = shown on map, ",
          shiny::tags$strong("OFF"),
          " = hidden."
        ),
        shiny::tags$div(class = "status-map-toggle-row", buttons)
      )
    })

    output$events_map <- plotly::renderPlotly({
      spatial_data <- spatial_data_for_map()
      if (is.null(spatial_data)) {
        return(
          empty_status_map_plot("Map unavailable: spatial boundaries are not loaded yet.")
        )
      }

      events <- events_for_map()
      brazil_sf <- build_status_brazil_geometry(spatial_data)
      if (is.null(brazil_sf) || nrow(brazil_sf) == 0) {
        return(
          empty_status_map_plot("Map unavailable: Brazil boundary geometry is not available.")
        )
      }

      event_geometries <- build_status_event_geometries(events, spatial_data)
      session$userData$status_map_event_geometry_ids <- names(event_geometries)
      states <- map_toggle_states()
      visible_ids <- names(states)[vapply(states, isTRUE, logical(1))]
      visible_ids <- visible_ids[visible_ids %in% names(event_geometries)]
      session$userData$status_visible_map_event_ids <- visible_ids

      map_plot <- plotly::plot_ly(type = "scatter", mode = "lines") |>
        plotly::add_sf(
          data = brazil_sf,
          split = ~region_code,
          color = I("#B8C2CC"),
          alpha = 0.35,
          hoverinfo = "skip",
          showlegend = FALSE
        )

      if (length(visible_ids) > 0) {
        for (event_id in visible_ids) {
          event_sf <- event_geometries[[event_id]]
          if (is.null(event_sf) || nrow(event_sf) == 0) {
            next
          }

          map_plot <- map_plot |>
            plotly::add_sf(
              data = event_sf |> dplyr::mutate(map_label = paste0("Event: ", .data$event_id)),
              split = ~event_id,
              color = I("#009C3B"),
              alpha = 0.32,
              hoverinfo = "text",
              text = ~map_label,
              showlegend = FALSE
            )
        }
      }

      map_plot |>
        plotly::layout(
          margin = list(l = 0, r = 0, t = 4, b = 0),
          xaxis = list(showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE, fixedrange = TRUE),
          yaxis = list(showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE, fixedrange = TRUE, scaleanchor = "x", scaleratio = 1)
        ) |>
        plotly::config(
          staticPlot = TRUE,
          displayModeBar = FALSE,
          scrollZoom = FALSE
        )
    })

    # Events table with delete buttons
    output$events_table <- DT::renderDataTable({
      events <- try(events_reactive(), silent = TRUE)
      if (inherits(events, "try-error") || is.null(events) || nrow(events) == 0) {
        # Log error information to console
        if (inherits(events, "try-error")) {
          log_module_error(
            error = attr(events, "condition"),
            module_name = "mod_status_server",
            function_name = "events_table renderDataTable"
          )
        }

        session$userData$status_events_table <- NULL

        # Return empty table with message
        return(
          DT::datatable(
            tibble::tibble(Message = "No events configured - Add new hazard events from the sidebar"),
            options = list(dom = "t"),
            rownames = FALSE
          )
        )
      }

      # Prepare display data (exclude season column - it's now embedded in hazard_name)
      if (!"spatial_level" %in% names(events)) events$spatial_level <- NA_character_
      if (!"spatial_region_codes" %in% names(events)) events$spatial_region_codes <- NA_character_
      if (!"spatial_region_labels" %in% names(events)) events$spatial_region_labels <- NA_character_

      display_data <- events |>
        dplyr::mutate(
          spatial_selection = mapply(
            FUN = format_spatial_selection,
            level = .data$spatial_level,
            region_codes = .data$spatial_region_codes,
            region_labels = .data$spatial_region_labels,
            SIMPLIFY = TRUE,
            USE.NAMES = FALSE
          )
        ) |>
        dplyr::select("event_id", "hazard_type", "hazard_name", "scenario_name", "return_period", "event_year", "spatial_selection") |>
        dplyr::rename(
          "Event ID" = "event_id",
          "Hazard Type" = "hazard_type",
          "Hazard Name" = "hazard_name",
          "Scenario" = "scenario_name",
          "Return Period (years)" = "return_period",
          "Shock Year" = "event_year",
          "Spatial Separation" = "spatial_selection"
        )

      # Add delete buttons column
      if (nrow(display_data) > 0 && !is.null(delete_event_callback)) {
        # Create delete buttons for each row
        delete_buttons <- purrr::map_chr(seq_len(nrow(events)), function(i) {
          event_id <- events$event_id[i]
          # Create button HTML with onclick handler
          paste0(
            '<button class="btn btn-danger btn-sm" onclick="Shiny.setInputValue(\'',
            ns("delete_event"),
            "', '",
            event_id,
            '\', {priority: \'event\'});" style="padding: 2px 8px; margin: 0;">',
            '<i class="fa fa-trash"></i>',
            "</button>"
          )
        })
        display_data$Actions <- delete_buttons
      }

      session$userData$status_events_table <- display_data

      DT::datatable(
        display_data,
        escape = FALSE,
        selection = "none",
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          dom = "ftp"
        ),
        rownames = FALSE
      )
    })

    # Handle delete event action
    shiny::observeEvent(input$delete_event, {
      event_id <- input$delete_event
      if (!is.null(delete_event_callback) && !is.null(event_id) && event_id != "") {
        delete_event_callback(event_id)
      }
    })
  })
}
