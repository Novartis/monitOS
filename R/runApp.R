#' @title monitOS app
#'
#' @description Runs the Shiny application for exploring positivity thresholds
#' to monitor overall survival (OS).
#' @import shiny
#' @export
#' @return No return value, runs shiny app
# nocov start
run_app <- function() {
  shinyApp(
    ui = app_ui,
    server = app_server
  )
}
# nocov end
