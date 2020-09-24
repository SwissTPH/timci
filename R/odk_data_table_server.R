#' Module UI function
#' @param id Namespace for the Shiny module
#' @param odk_form_list ODK list of forms
#' @param odk_data ODK dataframe
#' @export
#' @import readxl hash scales

odk_data_table_server <- function(id, odk_form_list, odk_data) {

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

    })

}
