#' List item selection module server-side processing
#'
#' @param input,output,session standard \code{shiny} boilerplate
#' @param clist list of items
#' @param data hash structure (non-reactive) containing ODK dataframes from different forms
#'
#' @return list with following components
#' \describe{
#'   \item{item}{reactive character string indicating item selection}
#' }
#' @export
select_list_item_server <- function(input,
                                    output,
                                    session,
                                    list,
                                    data) {

  output$select_list <- renderUI({

    ns <- session$ns
    selectInput(ns("item"),
                label = "Select the item you want to display",
                choices = list,
                selected = list[1])

  })

  return(reactive({

    # Makes sure reactive value is available
    req(input$item, data[[input$item]])
    list(
      citem = input$item,
      cdata = data[[input$item]]
    )

  }))

}

#' List item selection module user interface
#' @param id, character used to specify namespace, see \code{shiny::\link[shiny]{NS}}
#' @export
#' @import shiny

select_list_item_ui <- function(id) {

  # `NS(id)` returns a namespace function, which was saved as `ns` and will be invoked later.
  ns <- shiny::NS(id)

  tagList(
    # Select form button
    shiny::uiOutput(ns("select_list"))
  )

}
