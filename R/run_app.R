#' Run the Shiny Application
#'
#' @param base_dir Character. Path to folder containing climate risk data
#'   (hazards, areas, damage factors, user inputs). If provided, this will
#'   be used as the default data source for the application.
#' @param ... arguments to pass to golem_opts.
#' See `?golem::get_golem_options` for more details.
#' @inheritParams shiny::shinyApp
#'
#' @export
run_app <- function(
  base_dir = NULL,
  onStart = NULL,
  options = list(),
  enableBookmarking = NULL,
  uiPattern = "/",
  ...
) {
  golem::with_golem_options(
    app = shiny::shinyApp(
      ui = app_ui,
      server = app_server,
      onStart = onStart,
      options = options,
      enableBookmarking = enableBookmarking,
      uiPattern = uiPattern
    ),
    golem_opts = list(base_dir = base_dir, ...)
  )
}
