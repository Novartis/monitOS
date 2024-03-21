#' @title monitOS app
#'
#' @description Runs the shiny app to guide user choice adequate settings to calculate
#' the positivity thresholds to monitor overall survival (OS)
#' @import shiny
#' @export
#' @return No return value, runs shiny app
# nocov start
run_app <- function() {
  shinyApp(
    ui = app_ui,
    server = app_server,
    # onStart = onStart,
    # options = options,
    # enableBookmarking = enableBookmarking,
    # uiPattern = uiPattern
  )
}
# nocov end
