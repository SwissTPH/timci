#' Module UI function
#' @param id Namespace for the Shiny module
#' @param odk_form_list ODK list of forms
#' @param odk_data ODK dataframe
#' @export
#' @import readxl hash scales

research_data_table_server <- function(id, odk_form_list, odk_data) {

  moduleServer(

    id,

    function(input, output, session) {

      output$select_form <- renderUI({

        ns <- session$ns
        selectInput(ns("form"),
                    label = "Select a form to display",
                    choices = odk_form_list,
                    selected = odk_form_list[1])

      })

      formInput <- reactive({
        return(input$form)
      })

      output$table <- renderTable({

        return(odk_data[[formInput()]])

      })

      # Downloadable *.csv file of the selected ODK data
      output$download_ODK_data <- downloadHandler(
        filename = function() {
          paste("Research data", formInput(), Sys.Date(), ".csv", sep = "")
        },
        content = function(file) {
          write.csv(odk_data[[formInput()]], file, row.names = FALSE)
        })

    })

}
