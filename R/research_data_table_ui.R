#' Module UI function
#' @param id Namespace for the Shiny module
#' @export
#' @import shiny

research_data_table_ui <- function(id) {
  # `NS(id)` returns a namespace function, which was saved as `ns` and will be invoked later.
  ns <- shiny::NS(id)

  shiny::tabPanel("ODK data",

                  # Sidebar layout with input and output definitions
                  shiny::sidebarLayout(

                    # Sidebar panel for inputs
                    shiny::sidebarPanel(

                      # Help text
                      shiny::helpText("Select form from which data needs to be extracted."),

                      # Select form button
                      shiny::uiOutput(ns("select_form")),

                      # Download data button
                      shiny::downloadButton(ns("download_ODK_data"), "Download ODK data")

                    ),

                    shiny::mainPanel(
                      shiny::fluidRow(
                        shiny::column(12, tableOutput(ns("table")))
                      )
                    )
                  )

  )
}
