#' Module UI function
#' @param id Namespace for the Shiny module
#' @export
#' @import shiny

enrolment_tab_ui <- function(id) {
  # `NS(id)` returns a namespace function, which was saved as `ns` and will be invoked later.
  ns <- shiny::NS(id)

  shiny::tabPanel("Enrolment",

                  shiny::mainPanel(
                    shiny::fluidRow(
                      shiny::column(6, h2("Global enrolment"))
                    ),
                    shiny::fluidRow(
                      shiny::column(6, gaugeOutput(ns("global_gauge")))
                    ),
                    shiny::fluidRow(
                      shiny::column(6, plotOutput(ns("global_hist"))),
                      shiny::column(6, plotOutput(ns("week_hist")))
                    )
                  )

  )

}
