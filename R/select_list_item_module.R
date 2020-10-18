#' List item selection module server-side processing
#'
#' @param id character used to specify namespace, see \code{shiny::\link[shiny]{NS}}
#' @param list list of items (non-reactive) containing ODK forms
#' @param data hash structure (non-reactive) containing the dataframes from different ODK forms
#'
#' @return reactive list with following components
#' \describe{
#'   \item{item}{reactive character string indicating item selection}
#'   \item{`cdata`}{dataframe containing the current ODK data}
#' }
#' @export

select_list_item_server <- function(id,
                                    list,
                                    data) {

  moduleServer(

    id,

    # Module function
    function(input, output, session){

      output$select_list <- renderUI({

        ns <- session$ns
        selectInput(ns("item"),
                    label = "Select the item you want to display",
                    choices = list,
                    selected = list[1])

      })

      # Return a reactive list containing the name of the selected item `citem` and the corresponding dataframe `cdata`
      reactive({

        # Makes sure reactive value is available
        req(input$item,data[[input$item]])
        list(
          citem = input$item,
          cdata = data[[input$item]]
        )

      })

    })

}

#' List item selection module user interface
#'
#' @param id character used to specify namespace, see \code{shiny::\link[shiny]{NS}}
#'
#' @export
#' @import shiny

select_list_item_ui <- function(id) {

  # `NS(id)` returns a namespace function, which was saved as `ns` and will be invoked later.
  ns <- shiny::NS(id)

  # Create an HTML tag definition
  tagList(

    # Select form button
    shiny::uiOutput(ns("select_list"))

  )

}
