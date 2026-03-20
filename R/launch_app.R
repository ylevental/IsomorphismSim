#' Launch the Isomorphism Explorer Shiny App
#'
#' Opens an interactive Shiny application that lets users explore
#' the ant colony / random forest isomorphism with adjustable
#' parameters and real-time simulation.
#'
#' @param ... Additional arguments passed to \code{\link[shiny]{runApp}}
#' @export
#' @examples
#' if (interactive()) launch_app()
launch_app <- function(...) {
  app_dir <- system.file("shiny", package = "IsomorphismSim")
  if (app_dir == "") {
    stop("Shiny app not found. Try re-installing the IsomorphismSim package.",
         call. = FALSE)
  }
  shiny::runApp(app_dir, ...)
}
