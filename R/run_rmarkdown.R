#' Generate Rmarkdown file
#'
#' This function generate a standardised Rmarkdown report in two formats: html and docx.
#'
#' @param export_dir Path to the output folder for the generated Rmarkdown reports and database exports
#' @param rmd_fn Filename of the Rmarkdown file
#' @param output_fn Filename of the Rmarkdown rendered report
#' @import rmarkdown
#' @export

generate_report <- function(export_dir, rmd_fn, output_fn) {

  report <- system.file("rmarkdown", rmd_fn, package = "timci")
  if (report == "") {
    stop(paste("Could not find `", rmd_fn, "`. Try re-installing `timci`."), call. = FALSE)
  }

  rmarkdown::render(report,
                    output_format = c("html_document", "word_document"),
                    output_file = c(paste0(output_fn, '_',Sys.Date(),'.html'),
                                    paste0(output_fn, '_',Sys.Date(),'.docx')),
                    output_dir = export_dir,
                    params = list(output_dir = export_dir))

}

#' Run Rmarkdown files
#'
#' This function runs several Rmarkdown files to generate standardised automated reports for the Tools for Integrated Management of Childhood Illnesses (TIMCI) project.
#'
#' @param export_dir Path to the output folder for the generated Rmarkdown reports and database exports
#' @import rmarkdown
#' @export

run_rmarkdown <- function(export_dir) {

  ##############
  # RCT report #
  ##############

  generate_report(export_dir, "rct_report.Rmd", "rct_report")

  #######################
  # Day 7 follow-up log #
  #######################

  generate_report(export_dir, "day7_fu_log.Rmd", "day7_fu_log")

  ###################
  # PATH M&E report #
  ###################

  generate_report(export_dir, "path_report.Rmd", "path_report")

  #############################
  # Intervention pilot report #
  #############################

  generate_report(export_dir, "pilot_report.Rmd", "pilot_report")

}
