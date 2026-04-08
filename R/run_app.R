#' Launch the AlgAware Shiny Application
#'
#' Start the interactive AlgAware application for IFCB phytoplankton
#' data processing, validation, and report generation.
#'
#' @param launch.browser Logical; open the app in a browser? Default TRUE.
#' @param reset_settings Logical; if TRUE, reset all settings to defaults
#'   before launching. Default FALSE.
#' @param ... Additional arguments passed to \code{\link[shiny]{runApp}}.
#' @return A Shiny app object (invisibly).
#' @export
#' @examples
#' \dontrun{
#'   algaware::launch_app()
#'   algaware::launch_app(reset_settings = TRUE)
#' }
launch_app <- function(launch.browser = TRUE, reset_settings = FALSE, ...) {
  if (isTRUE(reset_settings)) {
    save_settings(default_settings())
    message("Settings reset to defaults.")
  }
  app_dir <- system.file("app", package = "algaware")
  if (app_dir == "") {
    stop("Could not find app directory. Try re-installing `algaware`.",
         call. = FALSE)
  }
  if (!"algaware" %in% .packages()) {
    attachNamespace("algaware")
  }
  shiny::runApp(app_dir, launch.browser = launch.browser, ...)
}
