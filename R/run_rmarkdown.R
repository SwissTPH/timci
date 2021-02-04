#' Run Rmarkdown files
#'
#' This function runs several Rmarkdown files to generate standardised automated reports for the Tools for Integrated Management of Childhood Illnesses (TIMCI) project.
#'
#' @param export_dir Path to the output folder for the generated Rmarkdown reports and database exports
#' @import rmarkdown
#' @export

run_rmarkdown <- function(export_dir) {

  rmarkdown_dir <- system.file("rmarkdown", package = "timci")
  if (rmarkdown_dir == "") {
    stop("Could not find `rmarkdown directory`. Try re-installing `timci`.", call. = FALSE)
  }

  rct <- system.file("rmarkdown", "rct_report.Rmd", package = "timci")
  if (rct == "") {
    stop("Could not find `rct.Rmd`. Try re-installing `timci`.", call. = FALSE)
  }

  rmarkdown::render(rct,
                    output_format = c("html_document", "word_document"),
                    output_file = c(paste0('rct_report_',Sys.Date(),'.html'),
                                    paste0('rct_report_',Sys.Date(),'.docx')),
                    output_dir = export_dir,
                    params = list(output_dir = export_dir))

  day7_fu_log <- system.file("rmarkdown", "day7_fu_log.Rmd", package = "timci")
  if (day7_fu_log == "") {
    stop("Could not find `day7_fu_log.Rmd`. Try re-installing `timci`.", call. = FALSE)
  }

  rmarkdown::render(day7_fu_log,
                    output_format = c("html_document", "word_document"),
                    output_file = c(paste0('day7_fu_log_',Sys.Date(),'.html'),
                                    paste0('day7_fu_log_',Sys.Date(),'.docx')),
                    output_dir = export_dir)

}
