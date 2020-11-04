#' Reactive ODK data info module server-side processing
#'
#' This module displays some global information about the ODK data table that is displayed.
#'
#' @param id character used to specify namespace, see \code{shiny::\link[shiny]{NS}}
#' @param vars parameter containing a reactive list with the following components:
#' \describe{
#'   \item{`citem`}{string indicating the current ODK form}
#'   \item{`cdata`}{dataframe containing the current ODK data}
#' }
#' @export

reactive_odk_data_info_server <- function(id,
                                          vars) {

  dataInput <- reactive({

    # Makes sure the reactive value `vars` is available
    req(vars())
    vars()$cdata

  })

  moduleServer(

    id,

    function(input, output, session){

      output$displayRecordNb <- renderText({
        paste("Number of entries: ", nrow(dataInput()))
      })

      output$feature_number <- renderText({
        paste("Number of features: ", ncol(dataInput()))
      })

      output$period <- renderText({
        if("date" %in% colnames(dataInput())){
          paste("Period: ", min(dataInput()$date), ' - ', max(dataInput()$date))}
        else{
          paste("")}
      })

    })

}

#' ODK data info module server-side processing
#'
#' This module displays some global information about the ODK data table that is displayed.
#'
#' @param id character used to specify namespace, see \code{shiny::\link[shiny]{NS}}
#' @param df dataframe containing the current ODK data
#' @export

odk_data_info_server <- function(id,
                                 df) {

  moduleServer(

    id,

    function(input, output, session){

      output$displayRecordNb <- renderText({
        paste("Number of entries: ", nrow(df))
      })

      output$feature_number <- renderText({
        paste("Number of features: ", ncol(df))
      })

      output$period <- renderText({
        if("date" %in% colnames(df)){
          paste("Period: ", min(df$date), ' - ', max(df$date))}
        else{
          paste("")}
      })

    })

}

#' ODK data info module user interface
#' @param id character used to specify namespace, see \code{shiny::\link[shiny]{NS}}
#' @export
#' @import shiny

odk_data_info_ui <- function(id) {

  # Return a namespace function, which is saved as `ns` and will be invoked later
  ns <- shiny::NS(id)

  # Create an HTML tag definition
  shiny::tagList(

    # Display the number of entries
    shiny::textOutput(ns("displayRecordNb")),

    # Display the number of features per entry
    shiny::textOutput(ns("feature_number")),

    # Display the period of entries
    shiny::textOutput(ns("period"))

  )

}
