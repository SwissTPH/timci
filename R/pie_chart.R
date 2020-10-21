#' Pie chart module server-side processing
#'
#' @param id character used to specify namespace, see \code{shiny::\link[shiny]{NS}}
#' @param data parameter containing a vector
#'
#' @export

pie_chart_server <- function(id, data) {

  moduleServer(

    id,

    # Module function
    function(input, output, session){

      p <- generate_pie_chart(data)

      # Render pie chart
      output$pie_chart <- renderPlot(p)

    })

}

#' Pie chart module user interface
#'
#' @param id character used to specify namespace, see \code{shiny::\link[shiny]{NS}}
#'
#' @export
#' @import shiny

pie_chart_ui <- function(id) {

  # Return a namespace function, which is saved as `ns` and will be invoked later
  ns <- shiny::NS(id)

  # Create an HTML tag definition
  tagList(

    # Download data button
    shiny::plotOutput(ns("pie_chart"))

  )

}
