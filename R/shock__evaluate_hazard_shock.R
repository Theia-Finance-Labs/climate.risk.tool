#' Evaluate hazard shock equations from configuration (internal)
#'
#' @param assets_event Data frame of asset factors for a single event
#' @param hazard_config Hazard configuration list for the hazard type
#' @param shock_type Character. "revenue" or "profit"
#' @param combine Character. "error" to fail on duplicates, "sum" to aggregate
#' @return Tibble with columns: asset, shock_value
#' @noRd
evaluate_hazard_shock <- function(assets_event, hazard_config, shock_type, combine = "error") {
  if (is.null(assets_event) || nrow(assets_event) == 0) {
    return(tibble::tibble(asset = character(), shock_value = numeric()))
  }
  if (is.null(hazard_config$shocks) || length(hazard_config$shocks) == 0) {
    return(tibble::tibble(asset = character(), shock_value = numeric()))
  }
  shock_block <- hazard_config$shocks[[shock_type]]
  if (is.null(shock_block) || is.null(shock_block$equations) || length(shock_block$equations) == 0) {
    return(tibble::tibble(asset = character(), shock_value = numeric()))
  }

  combine <- match.arg(combine, c("error", "sum"))
  results <- list()

  for (equation_name in names(shock_block$equations)) {
    equation_def <- shock_block$equations[[equation_name]]
    eq_data <- assets_event

    if (!is.null(equation_def$when)) {
      eq_data <- eq_data |>
        dplyr::filter(!!rlang::parse_expr(equation_def$when))
    }

    if (nrow(eq_data) == 0) {
      next
    }

    constants <- equation_def$constants
    if (is.null(constants)) {
      constants <- list()
    }

    # Support R formula style: revenue ~ ... or profit ~ ...
    raw_eq <- as.character(equation_def$equation)
    if (grepl("~", raw_eq)) {
      # Extract the right-hand side of the formula
      eq_parts <- strsplit(raw_eq, "~")[[1]]
      if (length(eq_parts) != 2) {
        stop("Invalid formula style in equation: ", raw_eq)
      }
      # The target (revenue or profit) is on the left, formula on the right
      # We evaluate the right side and assign it to shock_value
      eq_expr <- rlang::parse_expr(trimws(eq_parts[2]))
    } else {
      eq_expr <- rlang::parse_expr(raw_eq)
    }

    # Debug info for the error reported by user
    # message("Evaluating equation '", equation_name, "' for ", nrow(eq_data), " rows")
    
    value <- rlang::eval_tidy(eq_expr, data = eq_data, env = rlang::env(!!!constants))

    # Apply global pmax(0, ...) condition only to revenue shocks
    if (shock_type == "revenue") {
      value <- pmax(0, as.numeric(value))
    } else {
      # For profit shocks, we allow negative values (losses)
      # and we return the DELTA (the shock value to be subtracted)
      # If the formula was "profit ~ profit - ...", we want to return the "..." part
      # But our current logic in apply_acute_profit_shock expects shock_value to be the damage to SUBTRACT.
      # So if formula is "profit ~ profit - (damage)", we should return (damage).
      # If formula is "profit ~ damage", we should return (profit - damage).
      
      raw_eq <- as.character(equation_def$equation)
      if (grepl("~", raw_eq)) {
        eq_parts <- strsplit(raw_eq, "~")[[1]]
        target_var <- trimws(eq_parts[1])
        
        # If target is profit, we want to return (baseline_profit - new_profit)
        # so that apply_acute_profit_shock can subtract it.
        if (target_var == "profit") {
          baseline_profit <- eq_data$profit
          value <- as.numeric(baseline_profit) - as.numeric(value)
        } else {
          value <- as.numeric(value)
        }
      } else {
        value <- as.numeric(value)
      }
    }

    if (length(value) == 1) {
      value <- rep(value, nrow(eq_data))
    }
    
    if (length(value) != nrow(eq_data)) {
      # If we have a mismatch, it's often because of a join issue that was missed
      # or because the equation returned a vector of different size.
      # We provide more context in the error message.
      stop(
        "Shock equation '", equation_name, "' returned ", length(value),
        " values for ", nrow(eq_data), " rows. ",
        "Assets in eq_data: ", paste(head(unique(eq_data$asset), 5), collapse = ", "),
        ". This usually indicates a data join issue (many-to-many) in previous steps."
      )
    }

    results[[length(results) + 1]] <- eq_data |>
      dplyr::mutate(shock_value = as.numeric(value)) |>
      dplyr::select("asset", "shock_value")
  }

  if (length(results) == 0) {
    return(tibble::tibble(asset = character(), shock_value = numeric()))
  }

  combined <- dplyr::bind_rows(results)

  if (combine == "error") {
    # Check if duplicates exist within any single equation result
    for (i in seq_along(results)) {
      eq_name <- names(shock_block$equations)[i]
      if (is.null(eq_name)) eq_name <- paste0("Equation ", i)
      
      eq_assets <- results[[i]]$asset
      if (any(duplicated(eq_assets))) {
        stop(
          "Shock equation '", eq_name, "' produced multiple values for assets: ",
          paste(unique(eq_assets[duplicated(eq_assets)]), collapse = ", "),
          ". This usually indicates a data join issue (many-to-many)."
        )
      }
    }

    # Check if duplicates exist across different equations
    duplicated_assets <- combined$asset[duplicated(combined$asset)]
    if (length(duplicated_assets) > 0) {
      stop(
        "Multiple shock equations produced values for assets: ",
        paste(unique(duplicated_assets), collapse = ", "),
        ". Ensure 'when' conditions in config are mutually exclusive or set combine='sum'."
      )
    }
  } else if (combine == "sum") {
    combined <- combined |>
      dplyr::group_by(.data$asset) |>
      dplyr::summarize(shock_value = sum(.data$shock_value, na.rm = TRUE), .groups = "drop")
  }

  combined
}
