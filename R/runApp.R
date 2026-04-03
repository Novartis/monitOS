#' @title monitOS app
#'
#' @description Runs the Shiny application for exploring positivity thresholds
#' to monitor overall survival (OS).
#' @import shiny
#' @export
#' @return No return value, runs shiny app
# nocov start
run_app <- function() {
  app_www_dir <- system.file("app/www", package = "monitOS")
  shinyApp(
    ui = app_ui(app_www_dir = app_www_dir),
    server = app_server
  )
}
# nocov end
