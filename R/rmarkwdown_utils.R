#' Generate a PDF report using Rmarkdown
#'
#' This function generates a standardised Rmarkdown report in a PDF format
#'
#' @param report_dir Path to the folder where the generated PDF Rmarkdown report is stored
#' @param rmd_fn Filename of the Rmarkdown file
#' @param report_fn Filename of the Rmarkdown rendered report
#' @param rmd_params List of parameters
#' @import rmarkdown
#' @export

generate_pdf_report <- function(report_dir, rmd_fn, report_fn, rmd_params=list()) {

  # Look for the Rmarkdown document in the package file system of the package
  report <- system.file("rmarkdown", rmd_fn, package = "timci")
  if (report == "") {
    stop(paste("Could not find `", rmd_fn, "`. Try re-installing `timci`."), call. = FALSE)
  }

  rmarkdown::render(report,
                    output_format = c("pdf_document"),
                    output_file = c(paste0(report_fn, '_',Sys.Date(),'.pdf')),
                    output_dir = report_dir,
                    params = rmd_params,
                    encoding = "UTF-8")

}

#' Generate a Microsoft Word report using Rmarkdown
#'
#' This function generates a standardised Rmarkdown report in a DOCX format
#'
#' @param report_dir Path to the folder where the generated DOCX Rmarkdown report is stored
#' @param rmd_fn Filename of the Rmarkdown file
#' @param report_fn Filename of the Rmarkdown rendered report
#' @param rmd_params List of parameters
#' @import rmarkdown
#' @export

generate_word_report <- function(report_dir, rmd_fn, report_fn, rmd_params=list()) {

  report <- system.file("rmarkdown", rmd_fn, package = "timci")
  if (report == "") {
    stop(paste("Could not find `", rmd_fn, "`. Try re-installing `timci`."), call. = FALSE)
  }

  rmarkdown::render(report,
                    output_format = c("word_document"),
                    output_file = c(paste0(report_fn, '_',Sys.Date(),'.docx')),
                    output_dir = report_dir,
                    params = rmd_params,
                    encoding = "UTF-8")

}
