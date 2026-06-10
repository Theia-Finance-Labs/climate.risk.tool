#' settings UI Function
#'
#' @description Settings module for hazard configuration settings
#' @param id Internal parameter for shiny
#' @export
mod_settings_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h4("Hazard Configurations", class = "section-header"),
    shiny::p(
      "Review all hazards below and adjust available settings using dropdowns.",
      class = "text-muted",
      style = "font-size: 0.9em; margin-bottom: 10px;"
    ),
    shiny::uiOutput(ns("settings_body")),
    shiny::div(
      style = "margin-top: 15px; display: flex; gap: 8px; flex-wrap: wrap;",
      shiny::actionButton(
        ns("save_overrides"),
        "Save Config",
        class = "btn-primary"
      ),
      shiny::actionButton(
        ns("reset_overrides"),
        "Reset to Defaults",
        class = "btn-danger",
        style = "background-color: #C21807 !important; color: white !important; border: none !important;"
      )
    ),
    shiny::div(
      style = "margin-top: 10px;",
      shiny::textOutput(ns("override_status"))
    )
  )
}

#' settings Server Functions
#'
#' @param id Internal parameter for shiny
#' @param base_dir_reactive reactive containing base directory path
#' @param hazard_configs_reactive reactive list from load_hazard_configs()
#' @param inventory_reactive reactive hazard inventory from load_hazards_and_inventory()
#' @return list with reload_trigger reactive counter
#' @export
mod_settings_server <- function(id, base_dir_reactive, hazard_configs_reactive, inventory_reactive) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    status_message <- shiny::reactiveVal("")
    reload_counter <- shiny::reactiveVal(0L)


    current_configs <- shiny::reactive({
      cfg <- hazard_configs_reactive()
      if (is.null(cfg) || length(cfg) == 0) {
        return(NULL)
      }
      cfg
    })

    override_path <- shiny::reactive({
      base_dir <- base_dir_reactive()
      if (is.null(base_dir) || base_dir == "") {
        return(NULL)
      }
      file.path(base_dir, "hazards", "config", "config_overrides.yml")
    })

    output$settings_body <- shiny::renderUI({
      cfg <- current_configs()
      if (is.null(cfg) || length(cfg) == 0) {
        return(shiny::helpText("No hazard configurations loaded yet."))
      }

      inventory_df <- inventory_reactive()
      base_dir <- base_dir_reactive()

      shiny::tagList(
        lapply(names(cfg), function(hazard_type) {
          hazard_cfg <- cfg[[hazard_type]]
          hazard_id <- safe_id(hazard_type)

          indicators_ui <- lapply(names(hazard_cfg$indicators), function(indicator_key) {
            indicator_cfg <- hazard_cfg$indicators[[indicator_key]]
            indicator_id <- safe_id(indicator_key)
            is_categorical <- isTRUE(indicator_cfg$categorical)

            if (is_categorical) {
              agg_choices <- c("mode", "closest")
            } else {
              agg_choices <- c("mean", "median", "p90", "p10", "min", "max", "closest")
            }

            if (!indicator_cfg$agg %in% agg_choices) {
              agg_choices <- unique(c(indicator_cfg$agg, agg_choices))
            }

            fixed_ui <- NULL
            if (!is.null(indicator_cfg$fixed) && length(indicator_cfg$fixed) > 0) {
              fixed_ui <- shiny::tagList(
                lapply(names(indicator_cfg$fixed), function(fixed_key) {
                  fixed_value <- indicator_cfg$fixed[[fixed_key]]
                  fixed_choices <- get_fixed_choices(
                    fixed_key,
                    fixed_value,
                    hazard_type,
                    indicator_key,
                    indicator_cfg,
                    inventory_df,
                    base_dir
                  )
                  if (length(fixed_choices) > 1) {
                    settings_row(
                      paste0("Fixed: ", fixed_key),
                      shiny::selectInput(
                        ns(paste0("fixed__", hazard_id, "__", indicator_id, "__", safe_id(fixed_key))),
                        label = NULL,
                        choices = fixed_choices,
                        selected = as.character(fixed_value),
                        width = "100%"
                      )
                    )
                  } else {
                    settings_row(paste0("Fixed: ", fixed_key), fixed_value)
                  }
                })
              )
            }

            shiny::div(
              class = "indicator-config",
              style = "padding: 12px; margin-bottom: 15px; background: #ffffff; border: 1px solid #e5e7eb; border-radius: 6px;",
              shiny::h5(indicator_key, style = "color: #002776; margin-top: 0; font-weight: 700;"),
              shiny::div(
                style = "margin-bottom: 10px;",
                settings_row("File", indicator_cfg$file),
                settings_row("Variable", indicator_cfg$variable),
                settings_row("Index", paste(indicator_cfg$index, collapse = ", ")),
                if (is_categorical) {
                  settings_row("Categorical", "TRUE")
                }
              ),
              settings_row(
                "Aggregation",
                shiny::selectInput(
                  ns(paste0("indicator_agg__", hazard_id, "__", indicator_id)),
                  label = NULL,
                  choices = agg_choices,
                  selected = indicator_cfg$agg,
                  width = "100%"
                )
              ),
              if (!is.null(fixed_ui)) fixed_ui
            )
          })

          mappings_ui <- NULL
          if (!is.null(hazard_cfg$mappings) && length(hazard_cfg$mappings) > 0) {
            mappings_ui <- lapply(names(hazard_cfg$mappings), function(mapping_key) {
              mapping_cfg <- hazard_cfg$mappings[[mapping_key]]
              intensity_match <- mapping_cfg$intensity_match

              join_cfg <- mapping_cfg$join
              on_indicator_intensity <- character(0)
              on_indicator_index <- character(0)
              on_assets <- character(0)
              if (!is.null(join_cfg)) {
                if (length(join_cfg$on_indicator_intensity) > 0) {
                  on_indicator_intensity <- join_cfg$on_indicator_intensity
                }
                if (length(join_cfg$on_indicator_index) > 0) {
                  on_indicator_index <- join_cfg$on_indicator_index
                }
                if (length(join_cfg$on_assets) > 0) on_assets <- join_cfg$on_assets
              }

              shiny::div(
                class = "mapping-config",
                style = "padding: 12px; margin-bottom: 15px; background: #ffffff; border: 1px solid #e5e7eb; border-radius: 6px;",
                shiny::h5(mapping_key, style = "color: #009C3B; margin-top: 0; font-weight: 700;"),
                shiny::div(
                  style = "margin-bottom: 10px;",
                  settings_row("File", mapping_cfg$file),
                  settings_row("Variables", mapping_cfg$variables),
                  settings_row("Indicator intensity keys", on_indicator_intensity),
                  settings_row("Indicator index keys", on_indicator_index),
                  settings_row("Asset keys", on_assets)
                ),
                get_mapping_preview(base_dir, mapping_cfg),
                if (!is.null(intensity_match)) {
                  display_val <- if (length(on_indicator_intensity) > 0) {
                    as.character(intensity_match)
                  } else {
                    paste0(intensity_match, " (No intensity keys)")
                  }
                  settings_row("Intensity match", display_val)
                }
              )
            })
          }

          shocks_ui <- NULL
          if (!is.null(hazard_cfg$shocks) && length(hazard_cfg$shocks) > 0) {
            # Collect variables for highlighting
            # Include both indicator keys and their variable names
            indicator_vars <- unique(c(
              names(hazard_cfg$indicators),
              vapply(hazard_cfg$indicators, function(ind) {
                if (!is.null(ind$variable) && nzchar(ind$variable)) ind$variable else ""
              }, character(1))
            ))
            indicator_vars <- indicator_vars[nzchar(indicator_vars)]
            
            mapping_vars <- character(0)
            asset_keys <- character(0)
            if (!is.null(hazard_cfg$mappings) && length(hazard_cfg$mappings) > 0) {
              mapping_vars <- unlist(lapply(hazard_cfg$mappings, function(m) m$variables), use.names = FALSE)
              asset_keys <- unlist(lapply(hazard_cfg$mappings, function(m) m$join$on_assets), use.names = FALSE)
            }
            input_vars <- unique(asset_keys)
            
            shocks_ui <- list(
              shiny::div(
                style = "margin-bottom: 15px; display: flex; gap: 15px; font-size: 0.8em; color: #64748b; font-style: italic;",
                shiny::span("Variable legend:"),
                shiny::span(style = "color: #002776; font-weight: 600;", "* Indicator"),
                shiny::span(style = "color: #009C3B; font-weight: 600;", "* Mapping"),
                shiny::span(style = "color: #9333ea; font-weight: 600;", "* Input/Asset"),
                shiny::span(style = "color: #64748b; font-weight: 600;", "* Constant")
              ),
              lapply(names(hazard_cfg$shocks), function(shock_type) {
                shock_cfg <- hazard_cfg$shocks[[shock_type]]
                lapply(shock_cfg$equations, function(eq) {
                  constant_vars <- if (!is.null(eq$constants)) names(eq$constants) else character(0)
                  highlighted_eq <- highlight_formula(
                    eq$equation,
                    indicator_vars = indicator_vars,
                    mapping_vars = mapping_vars,
                    constant_vars = constant_vars,
                    input_vars = input_vars
                  )
                  highlighted_when <- if (!is.null(eq$when)) {
                    highlight_formula(
                      eq$when,
                      indicator_vars = indicator_vars,
                      mapping_vars = mapping_vars,
                      constant_vars = constant_vars,
                      input_vars = input_vars
                    )
                  } else {
                    NULL
                  }
                  
                  shiny::div(
                    class = "shock-config",
                    style = "padding: 12px; margin-bottom: 15px; background: #ffffff; border: 1px solid #e5e7eb; border-radius: 6px;",
                    shiny::h5(eq$name, style = "color: #C21807; margin-top: 0; font-weight: 700;"),
                    shiny::div(
                      style = "margin-bottom: 10px;",
                      settings_row("Shock Type", shock_type),
                      if (!is.null(eq$when)) {
                        settings_row(
                          "Condition",
                          shiny::tags$code(
                            style = "background: #f1f5f9; padding: 2px 4px; border-radius: 4px; color: #1e293b;",
                            shiny::HTML(highlighted_when)
                          )
                        )
                      },
                      shiny::tags$div(
                        style = "margin-top: 12px;",
                        shiny::tags$div(style = "font-weight: 600; color: #4b5563; margin-bottom: 6px;", "Equation:"),
                        shiny::tags$div(
                          style = "background: #f8fafc; padding: 15px; border-radius: 6px; border: 1px solid #e2e8f0; font-family: monospace; font-size: 0.95em; line-height: 1.5; white-space: pre-wrap; color: #1e293b; box-shadow: inset 0 1px 2px rgba(0,0,0,0.05);",
                          shiny::HTML(highlighted_eq)
                        )
                      )
                    ),
                    if (length(eq$constants) > 0) {
                      shiny::tags$div(
                        style = "margin-top: 10px; padding-top: 10px; border-top: 1px dashed #e2e8f0;",
                        shiny::tags$div(style = "font-size: 0.85em; font-weight: 600; color: #64748b; margin-bottom: 5px;", "Constants:"),
                        shiny::tags$div(
                          style = "display: flex; gap: 10px; flex-wrap: wrap;",
                          lapply(names(eq$constants), function(cname) {
                            shiny::tags$span(
                              style = "font-size: 0.85em; background: #f1f5f9; padding: 1px 6px; border-radius: 4px; border: 1px solid #e2e8f0;",
                              shiny::tags$span(style = "color: #64748b; font-weight: 600;", cname),
                              " = ",
                              eq$constants[[cname]]
                            )
                          })
                        )
                      )
                    }
                  )
                })
              })
            )
          }

          shiny::tags$details(
            class = "hazard-panel",
            shiny::tags$summary(
              class = "hazard-panel__summary",
              hazard_type
            ),
            shiny::div(
              class = "hazard-panel__table",
              style = "padding: 20px; background: #f9fafb;",
              
              settings_row("Index Indicator", hazard_cfg$index_indicator),
              shiny::hr(style = "margin-top: 10px; margin-bottom: 20px; border-top: 1px solid #e5e7eb;"),
              
              # Indicators Section
              shiny::div(
                style = "margin-top: 0; padding: 15px; background: #f0f4f8; border-radius: 8px; border: 1px solid #d1d5db;",
                shiny::tags$h4("Indicators", style = "margin-top: 0; margin-bottom: 15px; color: #002776; font-size: 1.1em; font-weight: 700; border-bottom: 2px solid #002776; display: inline-block; padding-bottom: 2px;"),
                indicators_ui
              ),
              
              # Mappings Section
              if (!is.null(mappings_ui)) {
                shiny::div(
                  style = "margin-top: 20px; padding: 15px; background: #f0fdf4; border-radius: 8px; border: 1px solid #d1fae5;",
                  shiny::tags$h4("Mappings", style = "margin-top: 0; margin-bottom: 15px; color: #009C3B; font-size: 1.1em; font-weight: 700; border-bottom: 2px solid #009C3B; display: inline-block; padding-bottom: 2px;"),
                  mappings_ui
                )
              },

              # Shocks Section
              if (!is.null(shocks_ui)) {
                shiny::div(
                  style = "margin-top: 20px; padding: 15px; background: #fef2f2; border-radius: 8px; border: 1px solid #fee2e2;",
                  shiny::tags$h4("Shocks", style = "margin-top: 0; margin-bottom: 15px; color: #C21807; font-size: 1.1em; font-weight: 700; border-bottom: 2px solid #C21807; display: inline-block; padding-bottom: 2px;"),
                  shocks_ui
                )
              }
            )
          )
        })
      )
    })

    shiny::observeEvent(input$save_overrides, {
      base_dir <- base_dir_reactive()
      cfg <- current_configs()
      path <- override_path()

      if (is.null(base_dir) || base_dir == "" || is.null(cfg)) {
        status_message("Cannot save config: base_dir or config not available.")
        return()
      }

      overrides <- list()
      for (hazard_type in names(cfg)) {
        hazard_cfg <- cfg[[hazard_type]]
        base_config <- read_hazard_config(hazard_cfg$path, hazard_type)
        hazard_id <- safe_id(hazard_type)

        hazard_override <- list()

        indicator_overrides <- list()
        for (indicator_key in names(base_config$indicators)) {
          indicator_id <- safe_id(indicator_key)
          base_indicator <- base_config$indicators[[indicator_key]]
          indicator_override <- list()

          agg_input <- input[[paste0("indicator_agg__", hazard_id, "__", indicator_id)]]
          if (!is.null(agg_input) && agg_input != base_indicator$agg) {
            indicator_override$agg <- agg_input
          }

          if (!is.null(base_indicator$fixed) && length(base_indicator$fixed) > 0) {
            fixed_override <- list()
            inventory_df <- inventory_reactive()
            base_dir <- base_dir_reactive()
            for (fixed_key in names(base_indicator$fixed)) {
              fixed_choices <- get_fixed_choices(
                fixed_key,
                base_indicator$fixed[[fixed_key]],
                hazard_type,
                indicator_key,
                base_indicator,
                inventory_df,
                base_dir
              )
              if (length(fixed_choices) > 1) {
                fixed_input <- input[[paste0("fixed__", hazard_id, "__", indicator_id, "__", safe_id(fixed_key))]]
                if (!is.null(fixed_input) && fixed_input != as.character(base_indicator$fixed[[fixed_key]])) {
                  # Try to preserve numeric type if base was numeric
                  if (is.numeric(base_indicator$fixed[[fixed_key]])) {
                    fixed_input_num <- suppressWarnings(as.numeric(fixed_input))
                    if (!is.na(fixed_input_num)) {
                      fixed_input <- fixed_input_num
                    }
                  }
                  fixed_override[[fixed_key]] <- fixed_input
                }
              }
            }
            if (length(fixed_override) > 0) {
              indicator_override$fixed <- fixed_override
            }
          }

          if (length(indicator_override) > 0) {
            indicator_overrides[[indicator_key]] <- indicator_override
          }
        }

        if (length(indicator_overrides) > 0) {
          hazard_override$indicators <- indicator_overrides
        }

        hazard_override <- drop_nulls(hazard_override)
        if (length(hazard_override) > 0) {
          overrides[[hazard_type]] <- hazard_override
        }
      }

      overrides <- drop_nulls(overrides)
      if (length(overrides) == 0) {
        if (!is.null(path) && file.exists(path)) {
          file.remove(path)
          status_message("Config cleared (no changes vs defaults).")
        } else {
          status_message("No config changes; defaults unchanged.")
        }
      } else {
        if (!is.null(path)) {
          dir_create <- dirname(path)
          if (!dir.exists(dir_create)) {
            dir.create(dir_create, recursive = TRUE, showWarnings = FALSE)
          }
          yaml::write_yaml(overrides, path)
          status_message("Config saved.")
        } else {
          status_message("Cannot save config: path not available.")
        }
      }

      reload_counter(reload_counter() + 1L)
    })

    shiny::observeEvent(input$reset_overrides, {
      path <- override_path()
      if (is.null(path)) {
        status_message("Cannot reset: base_dir not set.")
        return()
      }
      dir_create <- dirname(path)
      if (!dir.exists(dir_create)) {
        dir.create(dir_create, recursive = TRUE, showWarnings = FALSE)
      }
      yaml::write_yaml(list(), path)
      status_message("Config reset to defaults.")
      reload_counter(reload_counter() + 1L)
    })

    output$override_status <- shiny::renderText({
      path <- override_path()
      if (is.null(path)) {
        return("Config file: base_dir not set")
      }
      message <- status_message()
      if (nzchar(message)) {
        paste0("Config file: ", path, ". ", message)
      } else {
        paste0("Config file: ", path, ".")
      }
    })

    return(list(
      reload_trigger = shiny::reactive({
        reload_counter()
      })
    ))
  })
}
