#' Module UI function
#' @param id Namespace for the Shiny module
#' @param data Data
#' @export
#' @import flexdashboard rmarkdown

enrolment_tab_server <- function(id, data) {

  moduleServer(

    id,

    function(input, output, session) {

      global_hist <- generate_enrolment_hist(data)

      # Global gauge
      output$global_gauge <- flexdashboard::renderGauge({
        m <- 26*60*2
        s <- c(25*60*2, 1000000)
        w <- c(8*250, 8*325)
        d <- c(0, 8*250)
        gauge(sum(data$x), min = 0, max = m, sectors = gaugeSectors(success = s, warning = w, danger = d))
      })

      # Global histogram
      output$global_hist <- renderPlot({
        return(global_hist)
      })

      # Download monitoring report
      output$report <- downloadHandler(
        filename = function(){paste("Report-", Sys.Date(), '.pdf',sep='')},
        content = function(file){
          ggsave(file, plot=global_hist)
        }
      )

    })

}
