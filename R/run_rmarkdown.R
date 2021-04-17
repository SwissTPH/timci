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
#' @param research_facilities Dataframe that contains the research facilities
#' @param report_dir Path to the output folder for the generated Rmarkdown reports
#' @param participant_zip Path to the encrypted zip archive that stores participant data
#' @param mdb_dir Path to the output folder for the RCT / LS database exports
#' @param fu_dir Path to the output folder for the follow-up exports
#' @param qual1_dir Path to the output folder for the caregiver IDI exports
#' @param qual2_dir Path to the output folder for the healthcare provider IDI exports
#' @param spa_db_dir Path to the output folder for the SPA database exports
#' @param path_dir Path to the output folder for the M&E exports to be shared with PATH
#' @import rmarkdown ruODK
#' @export

run_rmarkdown <- function(research_facilities, report_dir, participant_zip, mdb_dir, fu_dir, qual1_dir, qual2_dir, spa_db_dir, path_dir) {

  ################
  # Set up ruODK #
  ################

  ruODK::ru_setup(
    svc = Sys.getenv("ODKC_SVC"),
    un = Sys.getenv("ODKC_UN"),
    pw = Sys.getenv("ODKC_PW"),
    tz = Sys.getenv("TZ"),
    verbose = TRUE # Can be switched to TRUE for demo or debugging
  )

  # List of projects visible with the credentials `ODKC_UN` and `ODKC_PW`
  odkc_project_list <- ruODK::project_list()$id

  #logs <- ruODK::audit_get()
  #print(logs)

  crf_facility_fid <- Sys.getenv("TIMCI_CRF_FACILITY_FID")
  print(crf_facility_fid)
  crf_day7_fid <- Sys.getenv("TIMCI_CRF_DAY7_FID")
  crf_hospit_fid <- Sys.getenv("TIMCI_CRF_HOSPIT_FID")
  if (Sys.getenv('TIMCI_IS_RCT') == 1) {
    crf_day28_fid <- Sys.getenv("TIMCI_CRF_DAY28_FID")
  }
  wd_fid <- Sys.getenv("TIMCI_WD_FID")
  crf_wfa_fid <- Sys.getenv("TIMCI_WEEKLY_FA_FID")
  main_study <- Sys.getenv("TIMCI_MAIN_STUDY")
  form_list <- ruODK::form_list()$fid

  #######################
  # Load TIMCI ODK data #
  #######################

  # List of forms visible in the RCT / LS project
  rct_ls_form_list <- ruODK::form_list()$fid

  # Load facility data
  print("Load facility data")
  raw_facility_data <- ruODK::odata_submission_get(fid = crf_facility_fid)
  facility_data <- timci::process_facility_data(raw_facility_data)

  pii <- timci::extract_enrolled_participants(facility_data)[[2]]

  # Load day 7 follow-up data
  print("Load day 7 follow-up data")
  raw_day7fu_data <- NULL
  if (crf_day7_fid %in% form_list) {
    raw_day7fu_data <- ruODK::odata_submission_get(fid = crf_day7_fid,
                                                   download = FALSE)
  }

  print("Load raw hospital visit data")
  raw_hospit_data <- NULL
  if (crf_hospit_fid %in% form_list) {
    raw_hospit_data <- ruODK::odata_submission_get(fid = crf_hospit_fid,
                                                   download = FALSE)
  }

  if (Sys.getenv('TIMCI_IS_RCT') == 1) {
    print("Load raw day 28 follow-up data")
    raw_day28fu_data <- NULL
    if (crf_day28_fid %in% form_list) {
      raw_day28fu_data <- ruODK::odata_submission_get(fid = crf_day28_fid,
                                                      download = FALSE)
    }
  }

  print("Load withdrawal data")
  if (wd_fid %in% form_list) {
    raw_withdrawal_data <- ruODK::odata_submission_get(fid = wd_fid,
                                                       download = FALSE)
  } else {
    raw_withdrawal_data <- NULL
  }

  print("Load weekly facility assessment data")
  if (crf_wfa_fid %in% rct_ls_form_list) {
    raw_wfa_data <- ruODK::odata_submission_get(fid = crf_wfa_fid,
                                                download = FALSE)
    wfa_data <- timci::process_weekly_fa_data(raw_wfa_data)
  } else {
    wfa_data <- NULL
  }

  ###########################
  # RCT data quality report #
  ###########################

  params <- list(rctls_dir = mdb_dir,
                 participant_zip = participant_zip,
                 spa_dir = spa_db_dir,
                 qual1_dir = qual1_dir,
                 odkc_project_list = odkc_project_list,
                 rct_ls_form_list = rct_ls_form_list,
                 facility_data = facility_data)
  generate_report(report_dir, "database_export.Rmd", "timci_data_export_report", params)

  #########################
  # RCT monitoring report #
  #########################

  params <- list(research_facilities = research_facilities,
                 facility_data = facility_data,
                 raw_day7fu_data = raw_day7fu_data)
  generate_report(report_dir, "rct_monitoring_report.Rmd", "timci_rct_monitoring_report", params)

  #######################
  # Day 7 follow-up log #
  #######################

  for (fid in research_facilities$facility_id) {
    params <- list(output_dir = fu_dir,
                   rct_ls_form_list = rct_ls_form_list,
                   pii = pii,
                   raw_day7fu_data = raw_day7fu_data,
                   raw_withdrawal_data = raw_withdrawal_data,
                   facility_id = fid)
    generate_report(fu_dir, "day7_fu_weekly_log.Rmd", paste0("timci_day7_fu_log_", fid), params)
  }
  params <- list(output_dir = fu_dir,
                 rct_ls_form_list = rct_ls_form_list,
                 pii = pii,
                 raw_day7fu_data = raw_day7fu_data,
                 raw_withdrawal_data = raw_withdrawal_data)
  generate_report(fu_dir, "day7_fu_daily_log.Rmd", "timci_day7_fu_daily_log", params)

  #################################
  # Hospitalisation follow-up log #
  #################################

  params <- list(output_dir = fu_dir,
                 pii = pii)
  generate_report(fu_dir, "hospit_fu_log.Rmd", "timci_hospit_fu_log", params)

  #######################
  # Day 28 follow-up log #
  #######################

  # Day 28 follow-up log is only generated if
  is_rct <- Sys.getenv("TIMCI_IS_RCT")
  if (is_rct == 1) {
    params <- list(output_dir = fu_dir,
                   rct_ls_form_list = rct_ls_form_list,
                   pii = pii,
                   raw_day28fu_data = raw_day28fu_data)
    generate_report(fu_dir, "day28_fu_log.Rmd", "timci_day28_fu_log", params)
  }

  ###################################
  # Qualitative caregiver selection #
  ###################################

  params <- list(qual1_dir = qual1_dir,
                 rct_ls_form_list = rct_ls_form_list,
                 pii = pii)
  generate_report(fu_dir, "caregiver_selection.Rmd", "timci_caregiver_selection", params)

  ###################
  # PATH M&E report #
  ###################

  params <- list(path_dir = path_dir,
                 facility_data = facility_data,
                 wfa_data = wfa_data)
  generate_report(path_dir, "path_report.Rmd", "timci_path_report", params)

  #############################
  # Intervention pilot report #
  #############################

  generate_report(report_dir, "pilot_report.Rmd", "timci_pilot_report")

}
