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

  #################################
  # Generate the folder structure #
  #################################

  #dir.create(file.path(output_dir, subdir, "01_rct_ls"), showWarnings = FALSE)
  #dir.create(file.path(output_dir, subdir, "01_rct_ls", "01_database"), showWarnings = FALSE)
  #dir.create(file.path(output_dir, subdir, "01_rct_ls", "02_followup"), showWarnings = FALSE)
  #dir.create(file.path(output_dir, subdir, "02_spa"), showWarnings = FALSE)
  #dir.create(file.path(output_dir, subdir, "02_spa", "01_database"), showWarnings = FALSE)
  #dir.create(file.path(output_dir, subdir, "03_qualitative"), showWarnings = FALSE)
  #dir.create(file.path(output_dir, subdir, "03_qualitative", "01_caregiver_idis"), showWarnings = FALSE)
  #dir.create(file.path(output_dir, subdir, "03_qualitative", "02_provider_idis"), showWarnings = FALSE)
  #dir.create(file.path(output_dir, subdir, "04_cost"), showWarnings = FALSE)
  #dir.create(file.path(output_dir, subdir, "05_reports"), showWarnings = FALSE)
  #dir.create(file.path(output_dir, subdir, "06_path"), showWarnings = FALSE)

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

  wd_fid <- Sys.getenv("TIMCI_WD_FID")

  # RCT / LS environment variables
  crf_facility_fid <- Sys.getenv("TIMCI_CRF_FACILITY_FID")
  crf_day7_fid <- Sys.getenv("TIMCI_CRF_DAY7_FID")
  crf_hospit_fid <- Sys.getenv("TIMCI_CRF_HOSPIT_FID")
  if (Sys.getenv('TIMCI_IS_RCT') == 1) {
    crf_day28_fid <- Sys.getenv("TIMCI_CRF_DAY28_FID")
  }
  crf_wfa_fid <- Sys.getenv("TIMCI_WEEKLY_FA_FID")

  # SPA environment variables
  spa_pid <- Sys.getenv("TIMCI_SPA_PID")
  cgei_fid <- Sys.getenv("TIMCI_SPA_CGEI_FID")
  fa_fid <- Sys.getenv("TIMCI_SPA_FA_FID")
  sco_fid <- Sys.getenv("TIMCI_SPA_SCO_FID")
  hcpi_fid <- Sys.getenv("TIMCI_SPA_HCPI_FID")

  # Qualitative environment variables
  qpid <- Sys.getenv("TIMCI_QUAL_PID")
  cgidi1_fid <- Sys.getenv("TIMCI_QUAL_CGIDI1_FID")
  cgidi2_fid <- Sys.getenv("TIMCI_QUAL_CGIDI2_FID")
  cgidi3_fid <- Sys.getenv("TIMCI_QUAL_CGIDI3_FID")

  #######################
  # Load TIMCI ODK data #
  #######################

  # List of forms visible in the RCT / LS project
  rct_ls_form_list <- ruODK::form_list()$fid

  # List of forms visible in the SPA project
  spa_form_list <- NULL
  if (spa_pid %in% odkc_project_list) {
    spa_form_list <- ruODK::form_list(pid = spa_pid)$fid
  }

  # List of forms visible in the qualitative project
  qual_form_list <- NULL
  if (qpid %in% odkc_project_list) {
    qual_form_list <- ruODK::form_list(pid = qpid)$fid
  }

  # Load facility data
  print("Load facility data")
  raw_facility_data <- ruODK::odata_submission_get(fid = crf_facility_fid)
  facility_data <- timci::process_facility_data(raw_facility_data)

  pii <- timci::extract_enrolled_participants(facility_data)[[2]]

  # Load day 7 follow-up data
  print("Load day 7 follow-up data")
  raw_day7fu_data <- NULL
  if (crf_day7_fid %in% rct_ls_form_list) {
    raw_day7fu_data <- ruODK::odata_submission_get(fid = crf_day7_fid,
                                                   download = FALSE)
  }

  # Load hospital visit follow-up data
  print("Load hospital visit data")
  raw_hospit_data <- NULL
  if (crf_hospit_fid %in% rct_ls_form_list) {
    raw_hospit_data <- ruODK::odata_submission_get(fid = crf_hospit_fid,
                                                   download = FALSE)
  }

  # Load day 28 follow-up data
  raw_day28fu_data <- NULL
  if (Sys.getenv('TIMCI_IS_RCT') == 1) {
    print("Load day 28 follow-up data")
    if (crf_day28_fid %in% rct_ls_form_list) {
      raw_day28fu_data <- ruODK::odata_submission_get(fid = crf_day28_fid,
                                                      download = FALSE)
    }
  }

  # Load widthdrawal data
  print("Load withdrawal data")
  raw_withdrawal_data <- NULL
  if (wd_fid %in% rct_ls_form_list) {
    raw_withdrawal_data <- ruODK::odata_submission_get(fid = wd_fid,
                                                       download = FALSE)
  }

  # Load weekly facility assessment data
  print("Load weekly facility assessment data")
  wfa_data <- NULL
  if (crf_wfa_fid %in% rct_ls_form_list) {
    raw_wfa_data <- ruODK::odata_submission_get(fid = crf_wfa_fid,
                                                download = FALSE)
    wfa_data <- timci::process_weekly_fa_data(raw_wfa_data)
  }

  # Load SPA data
  spa_cgei_data <- NULL
  spa_fa_data <- NULL
  spa_hcpi_data <- NULL
  spa_sco_data <- NULL

  if (spa_pid %in% odkc_project_list) {

    # Load SPA caregiver exit interview data
    print("Load SPA caregiver exit interview data")
    if (cgei_fid %in% spa_form_list) {
      raw_spa_cgei_data <- ruODK::odata_submission_get(pid = spa_pid, fid = cgei_fid)
      spa_cgei_data <- format_odk_metadata(raw_spa_cgei_data)
    }

    # Load SPA facility assessment data
    print("Load SPA facility assessment data")
    if (fa_fid %in% spa_form_list) {
      raw_spa_fa_data <- ruODK::odata_submission_get(pid = spa_pid, fid = fa_fid)
      spa_fa_data <- format_odk_metadata(raw_spa_fa_data)
    }

    # Load SPA healthcare provider interview data
    print("Load SPA healthcare provider interview data")
    if (hcpi_fid %in% spa_form_list) {
      raw_spa_hcpi_data <- ruODK::odata_submission_get(pid = spa_pid, fid = hcpi_fid)
      spa_hcpi_data <- format_odk_metadata(raw_spa_hcpi_data)
    }

    # Load SPA sick child observation protocol data
    print("Load SPA sick child observation protocol data")
    if (sco_fid %in% spa_form_list) {
      raw_spa_sco_data <- ruODK::odata_submission_get(pid = spa_pid, fid = sco_fid)
      spa_sco_data <- format_odk_metadata(raw_spa_sco_data)
    }

  }

  # Load qualitative data
  cgidi_invitation_data <- NULL
  cgidi_encryption_data <- NULL
  cgidi_interview_data <- NULL

  if (qpid %in% odkc_project_list) {

    # Load caregiver IDI invitation data
    print("Load caregiver IDI invitation data")
    if (cgidi1_fid %in% qual_form_list) {
      raw_cgidi_invitation_data <- ruODK::odata_submission_get(pid = qpid,
                                                               fid = cgidi1_fid)
      cgidi_invitation_data <- format_odk_metadata(raw_cgidi_invitation_data)
    }

    # Load caregiver IDI encryption list
    print("Load caregiver IDI encryption list")
    if (cgidi2_fid %in% qual_form_list) {
      raw_cgidi_encryption_data <- ruODK::odata_submission_get(pid = qpid,
                                                               fid = cgidi2_fid)
      cgidi_encryption_data <- format_odk_metadata(raw_cgidi_encryption_data)
    }

    # Load caregiver IDI interview data
    print("Load caregiver IDI interview data")
    if (cgidi3_fid %in% qual_form_list) {
      raw_cgidi_interview_zip <- ruODK::submission_export(local_dir = tempdir(),
                                                          pid = qpid,
                                                          fid = cgidi3_fid)
      cgidi_interview_data <- timci::extract_data_from_odk_zip(raw_cgidi_interview_zip, paste0(cgidi3_fid,".csv"))
    }

  }

  ###########################
  # RCT data quality report #
  ###########################

  params <- list(rctls_dir = mdb_dir,
                 participant_zip = participant_zip,
                 spa_dir = spa_db_dir,
                 qual1_dir = qual1_dir,
                 facility_data = facility_data,
                 raw_day7fu_data = raw_day7fu_data,
                 raw_hospit_data = raw_hospit_data,
                 raw_day28fu_data = raw_day28fu_data,
                 raw_withdrawal_data = raw_withdrawal_data,
                 spa_cgei_data = spa_cgei_data,
                 spa_fa_data = spa_fa_data,
                 spa_hcpi_data = spa_hcpi_data,
                 spa_sco_data = spa_sco_data,
                 cgidi_invitation_data = cgidi_invitation_data,
                 cgidi_encryption_data = cgidi_encryption_data,
                 cgidi_interview_data = cgidi_interview_data)
  generate_report(report_dir, "database_export.Rmd", "timci_data_export_report", params)

  #########################
  # RCT monitoring report #
  #########################

  params <- list(research_facilities = research_facilities,
                 facility_data = facility_data,
                 raw_day7fu_data = raw_day7fu_data,
                 raw_hospit_data = raw_hospit_data,
                 raw_day28fu_data = raw_day28fu_data,
                 raw_withdrawal_data = raw_withdrawal_data)
  generate_report(report_dir, "rct_monitoring_report.Rmd", "timci_rct_monitoring_report", params)

  #######################
  # Day 7 follow-up log #
  #######################

  day7fu_week_dir <- file.path(fu_dir, "day7_weekly_log")
  dir.create(day7fu_week_dir, showWarnings = FALSE)
  for (fid in research_facilities$facility_id) {
    params <- list(pii = pii,
                   fu_fid = crf_day7_fid,
                   raw_fu_data = raw_day7fu_data,
                   raw_withdrawal_data = raw_withdrawal_data,
                   facility_id = fid,
                   fu_start = 0,
                   fu_end = 9)
    generate_report(day7fu_week_dir, "fu_weekly_log.Rmd", paste0("timci_day7_fu_log_", fid), params)
  }
  params <- list(output_dir = fu_dir,
                 rct_ls_form_list = rct_ls_form_list,
                 pii = pii,
                 fu_fid = crf_day7_fid,
                 raw_fu_data = raw_day7fu_data,
                 raw_withdrawal_data = raw_withdrawal_data,
                 fu_start = 7,
                 fu_end = 9)
  generate_report(fu_dir, "fu_daily_log.Rmd", "timci_day7_fu_daily_log", params)

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
    day28fu_week_dir <- file.path(fu_dir, "day28_weekly_log")
    dir.create(day28fu_week_dir, showWarnings = FALSE)
    for (fid in research_facilities$facility_id) {
      params <- list(pii = pii,
                     fu_fid = crf_day28_fid,
                     raw_fu_data = raw_day28fu_data,
                     raw_withdrawal_data = raw_withdrawal_data,
                     facility_id = fid,
                     fu_start = 0,
                     fu_end = 32)
      generate_report(day28fu_week_dir, "fu_weekly_log.Rmd", paste0("timci_day28_fu_log_", fid), params)
    }
    params <- list(output_dir = fu_dir,
                   rct_ls_form_list = rct_ls_form_list,
                   pii = pii,
                   fu_fid = crf_day28_fid,
                   raw_fu_data = raw_day28fu_data,
                   raw_withdrawal_data = raw_withdrawal_data,
                   fu_start = 28,
                   fu_end = 32)
    generate_report(fu_dir, "fu_daily_log.Rmd", "timci_day28_fu_daily_log", params)

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
