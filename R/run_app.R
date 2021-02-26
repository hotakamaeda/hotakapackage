#' main server of app
#'
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#'
#' @return shiny server
#' @export

run_app <- function(options = list()) {
  shinyApp(ui = app_ui,
           server = app_server,
           options = options)

}
