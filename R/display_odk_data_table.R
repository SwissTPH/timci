#' ODK data table module server-side processing
#'
#' This module displays the ODK data table for the form selected by the user.
#'
#' @param input,output,session standard \code{shiny} boilerplate
#' @param vars parameter containing reactive list (called `vars`) for selecting the ODK data table to be displayed
#' @export

odk_data_table_server <- function(input,
                                  output,
                                  session,
                                  vars) {

  dataInput <- reactive({

    # Makes sure reactive value is available
    req(vars())
    return(vars()$cdata)

  })

  output$table <- shiny::renderDataTable({

    return(dataInput())

  })

}

#' ODK data table module user interface
#'
#' @param id, character used to specify namespace, see \code{shiny::\link[shiny]{NS}}
#'
#' @return a \code{shiny::\link[shiny]{tagList}} containing UI elements
#' @export
#' @import shiny
odk_data_table_ui <- function(id) {

  ns <- NS(id)

  shiny::tagList(
    shiny::fluidRow(
      shiny::column(width = 6,
                    shiny::dataTableOutput(ns("table"))
      )
    )
  )

}
