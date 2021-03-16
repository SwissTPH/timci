#' Generate Rmarkdown file
#'
#' This function generate a standardised Rmarkdown report in two formats HTML and Microsoft Word DOCX
#'
#' @param report_dir Path to the folder where the generated HTML and Microsoft Word DOCX Rmarkdown reports are stored
#' @param rmd_fn Filename of the Rmarkdown file
#' @param report_fn Filename of the Rmarkdown rendered report
#' @param rmd_params List of parameters
#' @import rmarkdown
#' @export

generate_report <- function(report_dir, rmd_fn, report_fn, rmd_params="") {

  report <- system.file("rmarkdown", rmd_fn, package = "timci")
  if (report == "") {
    stop(paste("Could not find `", rmd_fn, "`. Try re-installing `timci`."), call. = FALSE)
  }

  rmarkdown::render(report,
                    output_format = c("word_document"),
                    output_file = c(paste0(report_fn, '_',Sys.Date(),'.docx')),
                    output_dir = report_dir,
                    params = rmd_params)

}

#' Run Rmarkdown files
#'
#' This function runs several Rmarkdown files to generate standardised automated reports for the Tools for Integrated Management of Childhood Illnesses (TIMCI) project.
#'
#' @param report_dir Path to the output folder for the generated Rmarkdown reports
#' @param participant_zip Path to the encrypted zip archive that stores participant data
#' @param mdb_dir Path to the output folder for the RCT / LS database exports
#' @param fu_dir Path to the output folder for the follow-up exports
#' @param qual1_dir Path to the output folder for the caregiver IDI exports
#' @param qual2_dir Path to the output folder for the healthcare provider IDI exports
#' @param spa_db_dir Path to the output folder for the SPA database exports
#' @import rmarkdown
#' @export

run_rmarkdown <- function(report_dir, participant_zip, mdb_dir, fu_dir, qual1_dir, qual2_dir, spa_db_dir) {

  ###########################
  # RCT data quality report #
  ###########################

  params <- list(rctls_dir = mdb_dir,
                 participant_zip = participant_zip,
                 spa_dir = spa_db_dir,
                 qual1_dir = qual1_dir)
  generate_report(report_dir, "database_export.Rmd", "timci_data_export_report", params)

  #########################
  # RCT monitoring report #
  #########################

  generate_report(report_dir, "rct_monitoring_report.Rmd", "timci_rct_monitoring_report")

  #######################
  # Day 7 follow-up log #
  #######################

  params <- list(output_dir = fu_dir)
  generate_report(fu_dir, "day7_fu_log.Rmd", "timci_day7_fu_log", params)

  #################################
  # Hospitalisation follow-up log #
  #################################

  params <- list(output_dir = fu_dir)
  generate_report(fu_dir, "hospit_fu_log.Rmd", "timci_hospit_fu_log", params)

  #######################
  # Day 28 follow-up log #
  #######################

  params <- list(output_dir = fu_dir)
  generate_report(fu_dir, "day28_fu_log.Rmd", "timci_day28_fu_log", params)

  ###################################
  # Qualitative caregiver selection #
  ###################################

  params <- list(qual1_dir = qual1_dir)
  generate_report(fu_dir, "caregiver_selection.Rmd", "timci_caregiver_selection", params)

  ###################
  # PATH M&E report #
  ###################

  generate_report(report_dir, "path_report.Rmd", "timci_path_report")

  #############################
  # Intervention pilot report #
  #############################

  generate_report(report_dir, "pilot_report.Rmd", "timci_pilot_report")

}
