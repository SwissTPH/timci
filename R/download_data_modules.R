#' Reactive CSV data download module server-side processing
#'
#' @param id character used to specify namespace, see \code{shiny::\link[shiny]{NS}}
#' @param vars parameter containing a reactive list with the following components:
#' \describe{
#'   \item{`citem`}{string indicating the current ODK form}
#'   \item{`cdata`}{dataframe containing the current ODK data}
#' }
#' @export
#' @importFrom utils write.csv

reactive_csv_download_server <- function(id, vars) {

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
      output$download_ODK_data <- downloadHandler(

        filename = function() {
          paste(nameInput(), "_", Sys.Date(), ".csv", sep = "")
        },
        content = function(file) {
          write.csv(dataInput(), file, row.names = FALSE)

        })

    })

}

#' CSV data download module server-side processing
#'
#' @param id character used to specify namespace, see \code{shiny::\link[shiny]{NS}}
#' @param name string containing the name of the data export
#' @param df parameter containing a dataframe
#' @export
#' @importFrom utils write.csv

csv_download_server <- function(id, name, df) {

  moduleServer(

    id,

    # Module function
    function(input, output, session){

      # Downloadable *.csv file of the selected ODK data
      output$download_ODK_data <- downloadHandler(

        filename = function() {
          paste(name, "_", Sys.Date(), ".csv", sep = "")
        },
        content = function(file) {
          write.csv(df, file, row.names = FALSE)

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
    shiny::downloadButton(ns("download_ODK_data"), "Download data as CSV")

  )

}

#' Reactive Excel data download module server-side processing
#'
#' @param id character used to specify namespace, see \code{shiny::\link[shiny]{NS}}
#' @param vars parameter containing a reactive list with the following components:
#' \describe{
#'   \item{`citem`}{string indicating the current ODK form}
#'   \item{`cdata`}{dataframe containing the current ODK data}
#' }
#' @export
#' @import openxlsx

reactive_xlsx_download_server <- function(id, vars) {

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

      # Downloadable *.xlsx file of the selected ODK data
      output$download_ODK_data <- downloadHandler(

        filename = function() {
          paste(nameInput(), "_", Sys.Date(), ".xlsx", sep = "")
        },
        content = function(file) {
          write.xlsx(dataInput(), file, row.names = FALSE)

        })

    })

}

#' Excel data download module server-side processing
#'
#' @param id character used to specify namespace, see \code{shiny::\link[shiny]{NS}}
#' @param name string containing the name of the data export
#' @param df parameter containing a dataframe
#' @export
#' @import openxlsx

xlsx_download_server <- function(id, name, df) {

  moduleServer(

    id,

    # Module function
    function(input, output, session){

      # Downloadable *.xlsx file of the selected ODK data
      output$download_ODK_data <- downloadHandler(

        filename = function() {
          paste(name, "_", Sys.Date(), ".xlsx", sep = "")
        },
        content = function(file) {
          write.xlsx(df, file, row.names = FALSE)

        })

    })

}

#' Excel data download module user interface
#'
#' @param id character used to specify namespace, see \code{shiny::\link[shiny]{NS}}
#'
#' @export
#' @import shiny

xlsx_download_ui <- function(id) {

  # Return a namespace function, which is saved as `ns` and will be invoked later
  ns <- shiny::NS(id)

  # Create an HTML tag definition
  tagList(

    # Download data button
    shiny::downloadButton(ns("download_ODK_data"), "Download data as Excel")

  )

}

#' Report download module server-side processing
#'
#' @param id character used to specify namespace, see \code{shiny::\link[shiny]{NS}}
#' @export
#' @import rmarkdown

report_download_server <- function(id) {

  moduleServer(

    id,

    # Module function
    function(input, output, session){

      output$download_report <- downloadHandler(
        # For PDF output, change this to "report.pdf"
        filename = {
          paste("report_", Sys.Date(), ".html", sep = "")
        },
        content = function(file) {
          # Copy the report file to a temporary directory before processing it, in
          # case we don't have write permissions to the current working dir (which
          # can happen when deployed).
          temp_report <- file.path(tempdir(), "report.Rmd")
          file.copy(system.file("extdata", "report.Rmd", package = "timci"), temp_report, overwrite = TRUE)

          # Set up parameters to pass to Rmd document
          params <- list(n=0)

          # Knit the document, passing in the `params` list, and eval it in a
          # child of the global environment (this isolates the code in the document
          # from the code in this app).
          rmarkdown::render(temp_report, output_file = file,
                            params = params,
                            envir = new.env(parent = globalenv())
          )
        }
      )

    })

}

#' Report download module user interface
#'
#' @param id character used to specify namespace, see \code{shiny::\link[shiny]{NS}}
#'
#' @export
#' @import shiny

report_download_ui <- function(id) {

  # Return a namespace function, which is saved as `ns` and will be invoked later
  ns <- shiny::NS(id)

  # Create an HTML tag definition
  tagList(

    # Download data button
    shiny::downloadButton(ns("download_report"), "Download the monitoring report")

  )

}
