#' Run the TIMCI Shiny app
#'
#' This function runs a Shiny web application dedicated to the Tools for Integrated Management of Childhood Illnesses (TIMCI) project.
#'
#' @import shiny
#' @export

run_app <- function() {

  appDir <- system.file("shiny", "myapp", package = "timci")
  if (appDir == "") {
    stop("Could not find `myapp` directory. Try re-installing `timci`.", call. = FALSE)
  }

  shiny::runApp(appDir,
                launch.browser = TRUE,
                display.mode = "normal")

}

#' Run a minimalist Shiny app example
#'
#' This function runs a minimalist Shiny web application with a simple connection to ODK Central.
#'
#' @import shiny
#' @export

run_mini_app <- function() {

  appDir <- system.file("shiny", "mini", package = "timci")
  if (appDir == "") {
    stop("Could not find `mini` directory. Try re-installing `timci`.", call. = FALSE)
  }

  shiny::runApp(appDir,
                launch.browser = TRUE,
                display.mode = "normal")

}
