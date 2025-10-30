#' Log error with minimal useful information
#'
#' @param error The error object
#' @param context Brief context (module name, function name, etc.)
#' @noRd
log_error_to_console <- function(error, context = NULL) {
  cat("\n=== ERROR ===\n")
  cat("Message:", conditionMessage(error), "\n")
  if (!is.null(context)) {
    cat("Context:", context, "\n")
  }

  # Get the actual line where the error occurred
  traceback_info <- sys.calls()
  if (length(traceback_info) > 0) {
    # Find the first non-error-handling function call
    for (i in length(traceback_info):1) {
      call_text <- deparse(traceback_info[[i]], width.cutoff = 100)
      if (!grepl("(log_error_to_console|tryCatch|error|handler)", call_text)) {
        cat("Location:", call_text, "\n")
        break
      }
    }
  }
  cat("=============\n\n")
}

#' Log module-specific error information
#'
#' @param error The error object
#' @param module_name Name of the module where error occurred
#' @param function_name Name of the function where error occurred
#' @noRd
log_module_error <- function(error, module_name, function_name) {
  context <- paste0(module_name, "::", function_name)
  log_error_to_console(error, context = context)
}

#' Log reactive error information
#'
#' @param error The error object
#' @param reactive_name Name of the reactive where error occurred
#' @noRd
log_reactive_error <- function(error, reactive_name) {
  log_error_to_console(error, context = reactive_name)
}
