#' ODK data info module server-side processing
#'
#' @param input,output,session standard \code{shiny} boilerplate
#' @param vars
#'
#' @return list with following components
#' \describe{
#'   \item{form}{reactive character string indicating x variable selection}
#' }
#' @export
odk_data_info_server <- function(input,
                                output,
                                session,
                                vars) {

  dataInput <- reactive({

    # Makes sure reactive value is available
    req(vars())
    return(vars()$cdata)

  })

  output$displayRecordNb <- renderUI({
    paste("Number of records: ", nrow(dataInput()))
  })

}

#' ODK data info module user interface
#' @param id, character used to specify namespace, see \code{shiny::\link[shiny]{NS}}
#' @export
#' @import shiny

odk_data_info_ui <- function(id) {

  # `NS(id)` returns a namespace function, which was saved as `ns` and will be invoked later.
  ns <- shiny::NS(id)

  tagList(
    # Display the number of records
    shiny::uiOutput(ns("displayRecordNb"))
  )

}
