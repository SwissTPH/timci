#' ODK data table module server-side processing
#'
#' This module displays the ODK data table corresponding to the form selected by the user.
#'
#' @param id character used to specify namespace, see \code{shiny::\link[shiny]{NS}}
#' @param vars parameter containing a reactive list with the following components:
#' \describe{
#'   \item{`citem`}{string indicating the current ODK form}
#'   \item{`cdata`}{dataframe containing the current ODK data}
#' }
#' @export

odk_data_table_server <- function(id,
                                  vars) {

  dataInput <- reactive({

    # Makes sure the reactive value `vars` is available
    req(vars())
    vars()$cdata

  })

  moduleServer(

    id,

    # Module function
    function(input, output, session){

      # Render the ODK data in a dynamic data table
      output$table <- shiny::renderDataTable({

        dataInput()

      })

    })

}

#' ODK data table module user interface
#'
#' @param id character used to specify namespace, see \code{shiny::\link[shiny]{NS}}
#'
#' @return a \code{shiny::\link[shiny]{tagList}} containing UI elements
#' @export
#' @import shiny

odk_data_table_ui <- function(id) {

  # Return a namespace function, which is saved as `ns` and will be invoked later
  ns <- NS(id)

  # Create an HTML tag definition
  shiny::tagList(

    shiny::dataTableOutput(ns("table"))

    )

}
