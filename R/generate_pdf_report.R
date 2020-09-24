#' Generate PDF report
#'
#' This function generates a monitoring report.
#'
#' Description of generate_pdf_report
#' @param file File name
#' @param plot_list List of plots
#' @return This function generates a monitoring report.
#' @export

generate_pdf_report <- function(file, plot_list){

  pdf(file, paper="a4r", onefile = TRUE)
  for(plot in plot_list){
    print(plot)
  }
  dev.off()

}
