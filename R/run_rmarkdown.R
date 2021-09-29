#' Run Rmarkdown files
#'
#' This function runs several Rmarkdown files to generate standardised automated reports for the Tools for Integrated Management of Childhood Illnesses (TIMCI) project.
#'
#' @param rctls_pid Numeric ID of the RCT/LS ODK Central project
#' @param rctls_pp Passphrase
#' @param spa_pid Numeric ID of the SPA / time-flow ODK Central project
#' @param qpid Numeric ID of the qualitative ODK Central project
#' @param qual_pp Passphrase
#' @param research_facilities Dataframe that contains the research facilities
#' @param report_dir Path to the output folder for the generated Rmarkdown reports
#' @param participant_zip Path to the encrypted zip archive that stores participant data
#' @param mdb_dir Path to the output folder for the RCT / LS database exports
#' @param fu_dir Path to the output folder for the follow-up exports
#' @param qual1_dir Path to the output folder for the caregiver IDI exports
#' @param qual2_dir Path to the output folder for the healthcare provider IDI exports
#' @param spa_db_dir Path to the output folder for the SPA database exports
#' @param path_dir Path to the output folder for the M&E exports to be shared with PATH
#' @param start_date RCT/LS data collection start date
#' @param end_date RCT/LS data collection end date
#' @param spa_start_date SPA data collection start date
#' @param lock_date RCT/LS data collection cleaning end date (for database lock)
#' @import rmarkdown ruODK
#' @export

run_rmarkdown_reportonly <- function(rctls_pid,
                                     rctls_pp,
                                     spa_pid,
                                     qpid,
                                     qual_pp,
                                     research_facilities,
                                     report_dir,
                                     participant_zip,
                                     mdb_dir,
                                     fu_dir,
                                     qual1_dir,
                                     qual2_dir,
                                     spa_db_dir,
                                     path_dir,
                                     start_date = NULL,
                                     end_date = NULL,
                                     spa_start_date = NULL,
                                     lock_date = NULL) {

  if (is.null(spa_start_date)) {
    spa_start_date = start_date
  }

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

  wd_fid <- Sys.getenv("TIMCI_WD_FID")
  problem_fid <- Sys.getenv("TIMCI_PROBLEM_FID")

  # RCT / LS environment variables
  crf_facility_fid <- Sys.getenv("TIMCI_CRF_FACILITY_FID")
  crf_day7_fid <- Sys.getenv("TIMCI_CRF_DAY7_FID")
  crf_hospit_fid <- Sys.getenv("TIMCI_CRF_HOSPIT_FID")
  if (Sys.getenv('TIMCI_COUNTRY') == "Tanzania" || Sys.getenv('TIMCI_COUNTRY') == "India") {
    crf_day28_fid <- Sys.getenv("TIMCI_CRF_DAY28_FID")
  }
  crf_wfa_fid <- Sys.getenv("TIMCI_WEEKLY_FA_FID")

  # SPA environment variables
  cgei_fid <- Sys.getenv("TIMCI_SPA_CGEI_FID")
  fa_fid <- Sys.getenv("TIMCI_SPA_FA_FID")
  sco_fid <- Sys.getenv("TIMCI_SPA_SCO_FID")
  hcpi_fid <- Sys.getenv("TIMCI_SPA_HCPI_FID")
  tf_fid <- Sys.getenv("TIMCI_TF_FID")

  # Qualitative environment variables
  cgidi1_fid <- Sys.getenv("TIMCI_QUAL_CGIDI1_FID")
  cgidi2_fid <- Sys.getenv("TIMCI_QUAL_CGIDI2_FID")
  cgidi3_fid <- Sys.getenv("TIMCI_QUAL_CGIDI3_FID")

  #######################
  # Load TIMCI ODK data #
  #######################

  # List of forms visible in the RCT / LS project
  rct_ls_form_list <- ruODK::form_list(pid = rctls_pid)$fid

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

  raw_facility_zip <- ruODK::submission_export(local_dir = tempdir(),
                                               pid = rctls_pid,
                                               fid = crf_facility_fid,
                                               pp = rctls_pp,
                                               media = FALSE)
  col_specs <- list(
    'a4_c_10a' = col_integer(),
    'crfs-t04a-b2_3' = col_integer(),
    'crfs-t03-m3_1b' = col_integer(),
    'crfs-t03-m3_3' = col_integer(),
    'crfs-t03-m3_3o' = col_character(),
    'crfs-t03-m3_4' = col_integer(),
    'crfs-t03-m3_6' = col_integer(),
    'crfs-t03-m3_7' = col_integer(),
    'crfs-t03-m3_8a' = col_integer(),
    'crfs-t03-m3_9a' = col_integer(),
    'crfs-t09a1-medication_injection' = col_integer(),
    'crfs-t09a1-injection_types' = col_integer(),
    'crfs-t09a1-injection_typeso' = col_character(),
    'crfs-t09a2-g3_1' = col_character(),
    'crfs-t09a2-g3_1o' = col_character(),
    'crfs-t09a2-i2_1' = col_integer(),
    'crfs-t09a2-i2_1a' = col_integer(),
    'crfs-t09a2-i2_1b' = col_integer(),
    'crfs-t09a2-i2_1o' = col_character(),
    'crfs-t09a2-j2_1' = col_integer(),
    'crfs-t09a2-j2_1c' = col_integer(),
    'crfs-t09a2-h2_2a' = col_character(),
    'crfs-t09a2-h2_2ao' = col_character(),
    'crfs-t07a-tt07a-e2_1' = col_integer(),
    'crfs-t07a-tt07a-e2_1a' = col_integer(),
    'crfs-t07a-tt07a-e2_2' = col_integer(),
    'crfs-t07a-tt07a-e2_2a' = col_integer(),
    'crfs-t07a-tt07a-e2_3' = col_integer(),
    'crfs-t07a-tt07a-e2_3a' = col_integer(),
    'crfs-t07a-tt07a-e2_4' = col_integer(),
    'crfs-t07a-tt07a-e2_4a' = col_integer(),
    'crfs-t06a-tt06a-d2_6' = col_integer(),
    'crfs-t06a-d2_6b' = col_integer(),
    'crfs-t06a-tt06a-d2_1' = col_integer(),
    'crfs-t06a-d2_1a' = col_integer(),
    'crfs-t06a-tt06a-d2_4' = col_integer(),
    'crfs-t06a-d2_4a' = col_integer(),
    'crfs-t06a-tt06a-d2_5' = col_integer(),
    'crfs-t06a-d2_5a' = col_integer(),
    'crfs-t06a-tt06a-d2_2' = col_integer(),
    'crfs-t06a-d2_2b' = col_integer(),
    'crfs-t06a-tt06a-d2_3' = col_integer(),
    'crfs-t06a-d2_3b' = col_integer(),
    'crfs-t08a-f2_1' = col_character(),
    'crfs-t08a-f2_1o' = col_character(),
    'crfs-t08a-f2_2' = col_integer(),
    'crfs-t08a-f2_3' = col_integer(),
    'crfs-t08a-f2_4' = col_integer(),
    'crfs-t08a-f2_5' = col_double(),
    'crfs-t08a-f2_6' = col_integer(),
    'crfs-t08a-f2_7' = col_integer(),
    'crfs-t08a-f2_8' = col_integer(),
    'crfs-t08a-f2_9' = col_integer(),
    'crfs-t08a-f2_10a' = col_integer(),
    'crfs-t05b-c3_1' = col_integer(),
    'crfs-t05b-c3_2' = col_integer(),
    'crfs-t05b-c3_3' = col_integer(),
    'crfs-t05b-c3_3a' = col_integer(),
    'crfs-t05b-c3_4' = col_integer(),
    'crfs-t05b-c3_6a' = col_integer(),
    'crfs-t05b-c3_6' = col_integer(),
    'crfs-t05b-c3_6o' = col_character()
  )
  raw_facility_data <- timci::extract_data_from_odk_zip(raw_facility_zip,
                                                        paste0(crf_facility_fid,".csv"),
                                                        start_date,
                                                        end_date,
                                                        col_specs)
  if (Sys.getenv('TIMCI_COUNTRY') == 'Tanzania') {
    facility_data <- timci::process_tanzania_facility_data(raw_facility_data)
  } else{
    facility_data <- timci::process_facility_data(raw_facility_data)
  }

  # Copy audit trail in folder
  facility_data_audit <- timci::extract_additional_data_from_odk_zip(raw_facility_zip, paste0(crf_facility_fid, " - audit.csv"))

  # Load day 7 follow-up data
  print("Load day 7 follow-up data")
  raw_day7fu_data <- NULL
  if (crf_day7_fid %in% rct_ls_form_list) {
    raw_day7fu_zip <- ruODK::submission_export(local_dir = tempdir(),
                                               pid = rctls_pid,
                                               fid = crf_day7_fid,
                                               pp = rctls_pp,
                                               media = FALSE)
    raw_day7fu_data <- timci::extract_data_from_odk_zip(raw_day7fu_zip,
                                                        paste0(crf_day7_fid,".csv"),
                                                        start_date,
                                                        end_date)
  }

  # Load hospital visit follow-up data
  print("Load hospital visit data")
  raw_hospit_data <- NULL
  if (crf_hospit_fid %in% rct_ls_form_list) {
    raw_hospit_zip <- ruODK::submission_export(local_dir = tempdir(),
                                               pid = rctls_pid,
                                               fid = crf_hospit_fid,
                                               pp = rctls_pp,
                                               media = FALSE)
    raw_hospit_data <- timci::extract_data_from_odk_zip(raw_hospit_zip,
                                                        paste0(crf_hospit_fid,".csv"),
                                                        start_date,
                                                        end_date)
  }

  # Load day 28 follow-up data
  raw_day28fu_data <- NULL
  if (Sys.getenv('TIMCI_COUNTRY') == "Tanzania" || Sys.getenv('TIMCI_COUNTRY') == "India") {
    print("Load day 28 follow-up data")
    if (crf_day28_fid %in% rct_ls_form_list) {
      raw_day28fu_zip <- ruODK::submission_export(local_dir = tempdir(),
                                                  pid = rctls_pid,
                                                  fid = crf_day28_fid,
                                                  pp = rctls_pp,
                                                  media = FALSE)
      raw_day28fu_data <- timci::extract_data_from_odk_zip(raw_day28fu_zip,
                                                           paste0(crf_day28_fid,".csv"),
                                                           start_date,
                                                           end_date)
    }
  }

  # Load widthdrawal data
  print("Load withdrawal data")
  raw_withdrawal_data <- NULL
  if (wd_fid %in% rct_ls_form_list) {
    raw_withdrawal_zip <- ruODK::submission_export(local_dir = tempdir(),
                                                   pid = rctls_pid,
                                                   fid = wd_fid,
                                                   pp = rctls_pp,
                                                   media = FALSE)
    raw_withdrawal_data <- timci::extract_data_from_odk_zip(raw_withdrawal_zip,
                                                            paste0(wd_fid,".csv"),
                                                            start_date,
                                                            end_date)
  }

  # Load weekly facility assessment data
  print("Load weekly facility assessment data")
  wfa_data <- NULL
  if (crf_wfa_fid %in% rct_ls_form_list) {
    raw_wfa_zip <- ruODK::submission_export(local_dir = tempdir(),
                                            pid = rctls_pid,
                                            fid = crf_wfa_fid,
                                            pp = rctls_pp,
                                            media = TRUE)
    raw_wfa_data <- timci::extract_data_from_odk_zip(raw_wfa_zip,
                                                     paste0(crf_wfa_fid,".csv"),
                                                     start_date,
                                                     end_date)
    if (!is.null(raw_wfa_data)) {
      wfa_data <- timci::process_weekly_fa_data(raw_wfa_data)
    }
  }

  # Load problem report data
  print("Load problem report data")
  raw_problem_data <- NULL
  if (problem_fid %in% rct_ls_form_list) {
    raw_problem_zip <- ruODK::submission_export(local_dir = tempdir(),
                                                pid = rctls_pid,
                                                fid = problem_fid,
                                                pp = rctls_pp,
                                                media = FALSE)
    raw_problem_data <- timci::extract_data_from_odk_zip(raw_problem_zip,
                                                         paste0(problem_fid,".csv"),
                                                         start_date,
                                                         end_date)
  }

  # Load SPA data
  spa_cgei_data <- NULL
  spa_fa_data <- NULL
  spa_hcpi_data <- NULL
  spa_sco_data <- NULL
  tf_data <- NULL
  tf_data_full <- NULL

  if (spa_pid %in% odkc_project_list) {

    # Load SPA caregiver exit interview data
    print("Load SPA caregiver exit interview data")
    if (cgei_fid %in% spa_form_list) {
      raw_spa_cgei_zip <- ruODK::submission_export(local_dir = tempdir(),
                                                   pid = spa_pid,
                                                   fid = cgei_fid,
                                                   media = FALSE)
      spa_cgei_data <- timci::extract_data_from_odk_zip(raw_spa_cgei_zip,
                                                        paste0(cgei_fid,".csv"),
                                                        spa_start_date,
                                                        end_date)
    }

    # Load SPA facility assessment data
    print("Load SPA facility assessment data")
    if (fa_fid %in% spa_form_list) {
      raw_spa_fa_zip <- ruODK::submission_export(local_dir = tempdir(),
                                                 pid = spa_pid,
                                                 fid = fa_fid,
                                                 media = FALSE)
      spa_fa_data <- timci::extract_data_from_odk_zip(raw_spa_fa_zip,
                                                      paste0(fa_fid,".csv"),
                                                      spa_start_date,
                                                      end_date)
    }

    # Load SPA healthcare provider interview data
    print("Load SPA healthcare provider interview data")
    if (hcpi_fid %in% spa_form_list) {
      raw_spa_hcpi_zip <- ruODK::submission_export(local_dir = tempdir(),
                                                   pid = spa_pid,
                                                   fid = hcpi_fid,
                                                   media = FALSE)
      spa_hcpi_data <- timci::extract_data_from_odk_zip(raw_spa_hcpi_zip,
                                                        paste0(hcpi_fid,".csv"),
                                                        spa_start_date,
                                                        end_date)
    }

    # Load SPA sick child observation protocol data
    print("Load SPA sick child observation protocol data")
    if (sco_fid %in% spa_form_list) {
      raw_spa_sco_zip <- ruODK::submission_export(local_dir = tempdir(),
                                                  pid = spa_pid,
                                                  fid = sco_fid,
                                                  media = FALSE)
      spa_sco_data <- timci::extract_data_from_odk_zip(raw_spa_sco_zip,
                                                       paste0(sco_fid,".csv"),
                                                       spa_start_date,
                                                       end_date)
    }

    # Load time-flow data
    print("Load time-flow data")
    if (tf_fid %in% spa_form_list) {
      raw_tf_zip <- ruODK::submission_export(local_dir = tempdir(),
                                             pid = spa_pid,
                                             fid = tf_fid,
                                             media = FALSE)
      tf_data <- timci::extract_data_from_odk_zip(raw_tf_zip,
                                                  paste0(tf_fid,".csv"),
                                                  spa_start_date,
                                                  end_date)
      # To improve with a constraint of no submission
      tf_data_audit <- timci::extract_additional_data_from_odk_zip(raw_tf_zip, paste0(tf_fid, " - audit.csv"))
      tf_data_steps <- timci::extract_additional_data_from_odk_zip(raw_tf_zip, paste0(tf_fid, "-steps.csv"))
      tf_data_full <- list(tf_data, tf_data_audit, tf_data_steps)
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
      raw_cgidi_invitation_zip <- ruODK::submission_export(local_dir = tempdir(),
                                                           pid = qpid,
                                                           fid = cgidi1_fid,
                                                           pp = qual_pp)
      cgidi_invitation_data <- timci::extract_data_from_odk_zip(raw_cgidi_invitation_zip,
                                                                paste0(cgidi1_fid,".csv"),
                                                                start_date,
                                                                end_date)
    }

    # Load caregiver IDI encryption list
    print("Load caregiver IDI encryption list")
    if (cgidi2_fid %in% qual_form_list) {
      raw_cgidi_encryption_zip <- ruODK::submission_export(local_dir = tempdir(),
                                                           pid = qpid,
                                                           fid = cgidi2_fid,
                                                           pp = qual_pp)
      cgidi_encryption_data <- timci::extract_data_from_odk_zip(raw_cgidi_encryption_zip,
                                                                paste0(cgidi2_fid,".csv"),
                                                                start_date,
                                                                end_date)
    }

    # Load caregiver IDI interview data
    print("Load caregiver IDI interview data")
    if (cgidi3_fid %in% qual_form_list) {
      raw_cgidi_interview_zip <- ruODK::submission_export(local_dir = tempdir(),
                                                          pid = qpid,
                                                          fid = cgidi3_fid,
                                                          pp = qual_pp)
      cgidi_interview_data <- timci::extract_data_from_odk_zip(raw_cgidi_interview_zip,
                                                               paste0(cgidi3_fid,".csv"),
                                                               start_date,
                                                               end_date)
    }

  }

  ###########################
  # RCT data quality report #
  ###########################

  params <- list(rctls_dir = mdb_dir,
                 research_facilities = research_facilities,
                 participant_zip = participant_zip,
                 spa_dir = spa_db_dir,
                 qual1_dir = qual1_dir,
                 facility_data = facility_data,
                 lock_date = lock_date,
                 facility_data_audit = facility_data_audit,
                 raw_day7fu_data = raw_day7fu_data,
                 raw_hospit_data = raw_hospit_data,
                 raw_day28fu_data = raw_day28fu_data,
                 raw_withdrawal_data = raw_withdrawal_data,
                 raw_problem_data = raw_problem_data,
                 spa_cgei_data = spa_cgei_data,
                 spa_fa_data = spa_fa_data,
                 spa_hcpi_data = spa_hcpi_data,
                 spa_sco_data = spa_sco_data,
                 tf_data = tf_data_full,
                 cgidi_invitation_data = cgidi_invitation_data,
                 cgidi_encryption_data = cgidi_encryption_data,
                 cgidi_interview_data = cgidi_interview_data,
                 lock_date = lock_date)
  generate_word_report(report_dir, "database_export.Rmd", "timci_data_export_report", params)

  #########################
  # RCT monitoring report #
  #########################

  params <- list(research_facilities = research_facilities,
                 facility_data = facility_data,
                 raw_day7fu_data = raw_day7fu_data,
                 raw_hospit_data = raw_hospit_data,
                 raw_day28fu_data = raw_day28fu_data,
                 raw_withdrawal_data = raw_withdrawal_data,
                 wfa_data = wfa_data)
  if (Sys.getenv('TIMCI_COUNTRY') == 'Tanzania' | Sys.getenv('TIMCI_COUNTRY') == 'Tanzania') {
    rname <- "timci_rct_monitoring_report"
  } else{
    rname <- "timci_ls_monitoring_report"
  }
  generate_pdf_report(report_dir, "rct_monitoring_report.Rmd", rname, params)

  #########################
  # SPA monitoring report #
  #########################

  if (!is.null(spa_sco_data)) {
    if (length(spa_sco_data) > 0) {
      if (nrow(spa_sco_data) > 0) {
        params <- list(research_facilities = research_facilities,
                       facility_data = facility_data,
                       spa_sco_data = spa_sco_data,
                       raw_withdrawal_data = raw_withdrawal_data)
        generate_pdf_report(report_dir, "spa_monitoring_report.Rmd", "timci_spa_monitoring_report", params)
      }
    }
  }

  #############################################
  # Process map / time-flow monitoring report #
  #############################################

  if (!is.null(tf_data)) {
    if (length(tf_data) > 0) {
      if (nrow(tf_data) > 0) {
        params <- list(research_facilities = research_facilities,
                       facility_data = facility_data,
                       tf_data = tf_data,
                       raw_withdrawal_data = raw_withdrawal_data)
        generate_pdf_report(report_dir, "pmtf_monitoring_report.Rmd", "timci_processmap_timeflow_monitoring_report", params)
      }
    }
  }


  ###################
  # PATH M&E report #
  ###################

  params <- list(path_dir = path_dir,
                 facility_data = facility_data,
                 research_facilities = research_facilities,
                 wfa_data = wfa_data)
  generate_pdf_report(path_dir, "path_report.Rmd", "TIMCI_M&E_RA_report_for_PATH", params)

}

#' General follow-up logs
#'
#' This function runs follow-up Rmarkdown files to generate standardised follow-up logs for the Tools for Integrated Management of Childhood Illnesses (TIMCI) project.
#'
#' @param rctls_pid numeric ID of the RCT/LS ODK Central project (mandatory parameter)
#' @param rctls_pp passphrase (mandatory parameter)
#' @param research_facilities dataframe that contains the research facilities (mandatory parameter)
#' @param fu_dir path to the output folder for the follow-up exports (mandatory parameter)
#' @param start_date start date (optional parameter)
#' @param end_date end date (optional parameter)
#' @import rmarkdown ruODK
#' @export

generate_fu_logs <- function(rctls_pid,
                             rctls_pp,
                             research_facilities,
                             fu_dir,
                             start_date = NULL,
                             end_date = NULL) {

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

  wd_fid <- Sys.getenv("TIMCI_WD_FID")

  # RCT / LS environment variables
  crf_facility_fid <- Sys.getenv("TIMCI_CRF_FACILITY_FID")
  crf_day7_fid <- Sys.getenv("TIMCI_CRF_DAY7_FID")
  crf_hospit_fid <- Sys.getenv("TIMCI_CRF_HOSPIT_FID")
  if (Sys.getenv('TIMCI_COUNTRY') == "Tanzania" || Sys.getenv('TIMCI_COUNTRY') == "India") {
    crf_day28_fid <- Sys.getenv("TIMCI_CRF_DAY28_FID")
  }
  crf_wfa_fid <- Sys.getenv("TIMCI_WEEKLY_FA_FID")

  #######################
  # Load TIMCI ODK data #
  #######################

  # List of forms visible in the RCT / LS project
  rct_ls_form_list <- ruODK::form_list(pid = rctls_pid)$fid

  # Load facility data
  print("Load facility data")

  raw_facility_zip <- ruODK::submission_export(local_dir = tempdir(),
                                               pid = rctls_pid,
                                               fid = crf_facility_fid,
                                               pp = rctls_pp,
                                               media = FALSE)
  raw_facility_data <- timci::extract_data_from_odk_zip(raw_facility_zip,
                                                        paste0(crf_facility_fid,".csv"),
                                                        start_date,
                                                        end_date)
  if (Sys.getenv('TIMCI_COUNTRY') == 'Tanzania') {
    facility_data <- timci::process_tanzania_facility_data(raw_facility_data)
  } else{
    facility_data <- timci::process_facility_data(raw_facility_data)
  }
  pii <- timci::extract_enrolled_participants(facility_data)[[2]]

  # Load day 7 follow-up data
  print("Load day 7 follow-up data")
  raw_day7fu_data <- NULL
  if (crf_day7_fid %in% rct_ls_form_list) {
    raw_day7fu_zip <- ruODK::submission_export(local_dir = tempdir(),
                                               pid = rctls_pid,
                                               fid = crf_day7_fid,
                                               pp = rctls_pp,
                                               media = FALSE)
    raw_day7fu_data <- timci::extract_data_from_odk_zip(raw_day7fu_zip,
                                                        paste0(crf_day7_fid,".csv"),
                                                        start_date,
                                                        end_date)
  }

  # Load hospital visit follow-up data
  print("Load hospital visit data")
  raw_hospit_data <- NULL
  if (crf_hospit_fid %in% rct_ls_form_list) {
    raw_hospit_zip <- ruODK::submission_export(local_dir = tempdir(),
                                               pid = rctls_pid,
                                               fid = crf_hospit_fid,
                                               pp = rctls_pp,
                                               media = FALSE)
    raw_hospit_data <- timci::extract_data_from_odk_zip(raw_hospit_zip,
                                                        paste0(crf_hospit_fid,".csv"),
                                                        start_date,
                                                        end_date)
  }

  # Load day 28 follow-up data
  raw_day28fu_data <- NULL
  if (Sys.getenv('TIMCI_COUNTRY') == "Tanzania" || Sys.getenv('TIMCI_COUNTRY') == "India") {
    print("Load day 28 follow-up data")
    if (crf_day28_fid %in% rct_ls_form_list) {
      raw_day28fu_zip <- ruODK::submission_export(local_dir = tempdir(),
                                                  pid = rctls_pid,
                                                  fid = crf_day28_fid,
                                                  pp = rctls_pp,
                                                  media = FALSE)
      raw_day28fu_data <- timci::extract_data_from_odk_zip(raw_day28fu_zip,
                                                           paste0(crf_day28_fid,".csv"),
                                                           start_date,
                                                           end_date)
    }
  }

  # Load widthdrawal data
  print("Load withdrawal data")
  raw_withdrawal_data <- NULL
  if (wd_fid %in% rct_ls_form_list) {
    raw_withdrawal_zip <- ruODK::submission_export(local_dir = tempdir(),
                                                   pid = rctls_pid,
                                                   fid = wd_fid,
                                                   pp = rctls_pp,
                                                   media = FALSE)
    raw_withdrawal_data <- timci::extract_data_from_odk_zip(raw_withdrawal_zip,
                                                            paste0(wd_fid,".csv"),
                                                            start_date,
                                                            end_date)
  }

  #######################
  # Day 7 follow-up log #
  #######################

  day7fu_dir <- file.path(fu_dir, "day7_log")
  dir.create(day7fu_dir, showWarnings = FALSE)

  fu7all <- timci::generate_fu_log(pii, raw_day7fu_data, 0, 12, 7, 10)
  timci::export_df2xlsx(fu7all, day7fu_dir, "02_timci_day7_fu_weekly_log_all")

  # Weekly log
  for (i in 1:nrow(research_facilities)) {
    fid <- research_facilities[[i, 'facility_id']]
    fname <- research_facilities[[i, 'facility_label']]
    params <- list(pii = pii,
                   fu_fid = crf_day7_fid,
                   raw_fu_data = raw_day7fu_data,
                   raw_withdrawal_data = raw_withdrawal_data,
                   facility_id = fid,
                   facility_label = fname,
                   fu_start = 0,
                   fu_end = 12,
                   fu_vstart = 7,
                   fu_vend = 10)
    generate_word_report(day7fu_dir, "fu_weekly_log.Rmd", paste0(fid, "_", fname, "_timci_day7_fu_weekly_log"), params)
  }

  # Daily log
  params <- list(output_dir = fu_dir,
                 rct_ls_form_list = rct_ls_form_list,
                 pii = pii,
                 rctls_pid = rctls_pid,
                 fu_fid = crf_day7_fid,
                 raw_fu_data = raw_day7fu_data,
                 raw_withdrawal_data = raw_withdrawal_data,
                 fu_start = 6,
                 fu_end = 12,
                 fu_vstart = 7,
                 fu_vend = 10)
  generate_pdf_report(day7fu_dir, "fu_daily_log.Rmd", "01_timci_day7_fu_daily_log", params)

  #################################
  # Hospitalisation follow-up log #
  #################################

  params <- list(output_dir = fu_dir,
                 rct_ls_form_list = rct_ls_form_list,
                 pii = pii,
                 rctls_pid = rctls_pid,
                 raw_day7fu_data = raw_day7fu_data,
                 raw_hospit_data = raw_hospit_data,
                 raw_withdrawal_data = raw_withdrawal_data,
                 fu_end = 12)
  generate_word_report(fu_dir, "hospit_fu_log.Rmd", "timci_hospit_fu_log", params)

  #######################
  # Day 28 follow-up log #
  #######################

  # Day 28 follow-up log is only generated for India and Tanzania

  if (Sys.getenv('TIMCI_COUNTRY') == "Tanzania" || Sys.getenv('TIMCI_COUNTRY') == "India") {
    day28fu_dir <- file.path(fu_dir, "day28_log")
    dir.create(day28fu_dir, showWarnings = FALSE)

    fu28all <- timci::generate_fu_log(pii, raw_day28fu_data, 0, 32, 28, 32)
    timci::export_df2xlsx(fu28all, day28fu_dir, "02_timci_day28_fu_weekly_log_all")

    for (i in 1:nrow(research_facilities)) {
      fid <- research_facilities[[i, 'facility_id']]
      fname <- research_facilities[[i, 'facility_label']]
      params <- list(pii = pii,
                     fu_fid = crf_day28_fid,
                     raw_fu_data = raw_day28fu_data,
                     raw_withdrawal_data = raw_withdrawal_data,
                     facility_id = fid,
                     facility_label = fname,
                     fu_start = 21,
                     fu_end = 35,
                     fu_vstart = 28,
                     fu_vend = 32)
      generate_word_report(day28fu_dir, "fu_weekly_log.Rmd", paste0(fid, "_", fname, "_timci_day28_fu_weekly_log"), params)
    }
    params <- list(output_dir = fu_dir,
                   rct_ls_form_list = rct_ls_form_list,
                   pii = pii,
                   rctls_pid = rctls_pid,
                   fu_fid = crf_day28_fid,
                   raw_fu_data = raw_day28fu_data,
                   raw_withdrawal_data = raw_withdrawal_data,
                   fu_start = 27,
                   fu_end = 35,
                   fu_vstart = 28,
                   fu_vend = 32)
    generate_pdf_report(day28fu_dir, "fu_daily_log.Rmd", "01_timci_day28_fu_daily_log", params)

  }

}
