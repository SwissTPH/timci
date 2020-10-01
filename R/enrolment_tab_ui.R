#' Module UI function
#' @param id Namespace for the Shiny module
#' @export
#' @import shiny flexdashboard

enrolment_tab_ui <- function(id) {

  # `NS(id)` returns a namespace function, which was saved as `ns` and will be invoked later.
  ns <- shiny::NS(id)

  shiny::tagList(

    shiny::h2("Global enrolment"),
    flexdashboard::gaugeOutput(ns("global_gauge")),
    shiny::plotOutput(ns("global_hist"))
    #plotOutput(ns("week_hist"))

  )

}
