#' Module UI function
#' @param id Namespace for the Shiny module
#' @export
#' @import shiny

qr_code_image_ui <- function(id) {

  # `NS(id)` returns a namespace function, which was saved as `ns` and will be invoked later.
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::imageOutput(ns("qRCodeImage"))

  )
}
