#' Run the TIMCI Shiny app
#'
#' This function runs a Shiny web application dedicated to the Tools for Integrated Management of Childhood Illnesses (TIMCI) project.
#'
#' @import shiny
#' @export

run_app <- function() {

  appDir <- system.file("shiny", "myapp", package = "timci")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `timci`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")

}
