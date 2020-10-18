#' CSV data download module server-side processing
#'
#' @param id character used to specify namespace, see \code{shiny::\link[shiny]{NS}}
#' @param vars parameter containing a reactive list with the following components:
#' \describe{
#'   \item{`citem`}{string indicating the current ODK form}
#'   \item{`cdata`}{dataframe containing the current ODK data}
#' }
#' @export
#' @importFrom utils write.csv

csv_download_server <- function(id,
                                vars) {

  nameInput <- reactive({

    # Makes sure the reactive value `vars` is available
    req(vars())
    vars()$citem

  })

  dataInput <- reactive({

    # Makes sure the reactive value `vars` is available
    req(vars())
    vars()$cdata

  })

  moduleServer(

    id,

    # Module function
    function(input, output, session){

      # Downloadable *.csv file of the selected ODK data
      output$downloadODKData <- downloadHandler(

        filename = function() {
          paste("Research data", nameInput(), Sys.Date(), ".csv", sep = "")
        },
        content = function(file) {
          write.csv(dataInput(), file, row.names = FALSE)

        })

    })

}

#' CSV data download module user interface
#'
#' @param id character used to specify namespace, see \code{shiny::\link[shiny]{NS}}
#'
#' @export
#' @import shiny

csv_download_ui <- function(id) {

  # Return a namespace function, which is saved as `ns` and will be invoked later
  ns <- shiny::NS(id)

  # Create an HTML tag definition
  tagList(

    # Download data button
    shiny::downloadButton(ns("downloadODKData"), "Download data as CSV")

  )

}
