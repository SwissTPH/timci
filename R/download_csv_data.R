#' CSV data download module server-side processing
#'
#' @param input,output,session standard \code{shiny} boilerplate
#' @param vars
#'
#' @return list with following components
#' \describe{
#'   \item{form}{reactive character string indicating x variable selection}
#' }
#' @export

csv_download_server <- function(input,
                                output,
                                session,
                                vars) {

  nameInput <- reactive({

    # Makes sure reactive value is available
    req(vars())
    return(vars()$citem)

  })

  dataInput <- reactive({

    # Makes sure reactive value is available
    req(vars())
    return(vars()$cdata)

  })

  # Downloadable *.csv file of the selected ODK data
  output$downloadODKData <- downloadHandler(

    filename = function() {
      paste("Research data", nameInput(), Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(dataInput(), file, row.names = FALSE)

    })

}

#' CSV data download module user interface
#' @param id, character used to specify namespace, see \code{shiny::\link[shiny]{NS}}
#' @export
#' @import shiny

csv_download_ui <- function(id) {

  # `NS(id)` returns a namespace function, which was saved as `ns` and will be invoked later.
  ns <- shiny::NS(id)

  tagList(
    # Download data button
    shiny::downloadButton(ns("downloadODKData"), "Download ODK data")
  )

}
