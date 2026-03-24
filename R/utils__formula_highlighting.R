#' Highlight formula variables with category-specific colors
#'
#' @param text Character scalar (formula or condition)
#' @param indicator_vars Character vector of indicator variable names
#' @param mapping_vars Character vector of mapping variable names
#' @param constant_vars Character vector of constant names
#' @param input_vars Optional character vector of input/asset variables
#' @return Character string with HTML span wrappers for highlighted variables
#' @noRd
highlight_formula <- function(text, indicator_vars, mapping_vars, constant_vars, input_vars = NULL) {
  if (is.null(text) || length(text) == 0 || all(is.na(text))) return("")
  if (length(text) > 1) {
    text <- paste(text, collapse = "\n")
  }
  text <- as.character(text[[1]])

  input_vars <- unique(c(get_input_columns(), input_vars))

  wrap_vars <- function(vars, color, label) {
    if (is.null(vars) || length(vars) == 0) return(character(0))
    vars <- vars[nzchar(vars)]
    if (length(vars) == 0) return(character(0))
    vars <- unique(vars)
    vars <- vars[order(nchar(vars), decreasing = TRUE)]

    setNames(
      sprintf('<span style="color: %s; font-weight: 600;" title="%s">%s</span>', color, label, vars),
      vars
    )
  }

  all_replacements <- c(
    wrap_vars(indicator_vars, "#002776", "Indicator"),
    wrap_vars(mapping_vars, "#009C3B", "Mapping"),
    wrap_vars(input_vars, "#9333ea", "Input/Asset"),
    wrap_vars(constant_vars, "#64748b", "Constant")
  )

  if (length(all_replacements) == 0) return(text)

  escape_regex <- function(value) {
    gsub("([][{}()^$|.*+?\\\\])", "\\\\\\1", value, perl = TRUE)
  }

  placeholders <- sprintf("___VAR_PLACEHOLDER_%d___", seq_along(all_replacements))
  temp_text <- text

  for (i in seq_along(all_replacements)) {
    var_name <- names(all_replacements)[i]
    pattern <- paste0("\\b", escape_regex(var_name), "\\b")
    temp_text <- gsub(pattern, placeholders[i], temp_text, perl = TRUE)
  }

  for (i in seq_along(all_replacements)) {
    temp_text <- gsub(placeholders[i], all_replacements[i], temp_text, fixed = TRUE)
  }

  temp_text
}

