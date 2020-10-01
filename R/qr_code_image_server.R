#' Module UI function
#' @param id Namespace for the Shiny module
#' @export
#' @import grDevices
#' @import utils

qr_code_image_server <- function(id, outfile) {

  moduleServer(

    id,

    function(input, output, session) {

      output$qRCodeImage <- renderImage({

        ns <- session$ns

        list(src = outfile,
             contentType = 'image/png',
             width = 400,
             height = 400,
             alt = "This is alternate text")
      }, deleteFile = TRUE)

    })

}
