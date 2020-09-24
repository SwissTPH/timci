#' Module UI function
#' @param id Namespace for the Shiny module
#' @import shiny
#' @export

dummy_data_table_ui <- function(id) {
  # `NS(id)` returns a namespace function, which was saved as `ns` and will be invoked later.
  ns <- NS(id)

  shiny::tabPanel("Dummy data",
                  shiny::fluidRow(
                    shiny::column(12, shiny::tableOutput(ns("table")))
                  )
  )
}
