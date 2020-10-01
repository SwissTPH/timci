#' Module UI function
#' @param id Namespace for the Shiny module
#' @param dummy_data Dummy data
#' @export

dummy_data_table_server <- function(id, dummy_data) {

  moduleServer(
    id,
    function(input, output, session) {

      output$table <- renderTable({

        return(dummy_data)

      })

    }

  )

}
