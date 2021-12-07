#' Run Rmarkdown files
#'
#' This function runs several Rmarkdown files to generate standardised automated reports for the Tools for Integrated Management of Childhood Illnesses (TIMCI) project.
#'
#' @param rctls_pid Numeric ID of the RCT/LS ODK Central project
#' @param rctls_pp Passphrase
#' @param spa_pid Numeric ID of the SPA / time-flow / process mapping ODK Central project
#' @param cost_pid Numeric ID of the cost and cost-effectiveness ODK Central project (optional)
#' @param qpid Numeric ID of the qualitative ODK Central project
#' @param qual_pp Passphrase
#' @param research_facilities Dataframe that contains the research facilities
#' @param report_dir Path to the output folder for the generated Rmarkdown reports
#' @param participant_zip Path to the encrypted zip archive that stores participant data
#' @param mdb_dir Path to the output folder for the RCT / LS database exports
#' @param fu_dir Path to the output folder for the follow-up exports
#' @param qual1_dir Path to the output folder for the caregiver IDI exports
#' @param qual2_dir Path to the output folder for the healthcare provider IDI exports
#' @param spa_db_dir Path to the output folder for the SPA / time-flow / process mapping database exports
#' @param cost_dir Path to the output folder for the cost and cost effectiveness database exports (optional)
#' @param path_dir Path to the output folder for the M&E exports to be shared with PATH
#' @param start_date RCT/LS data collection start date (optional)
#' @param end_date RCT/LS data collection end date (optional)
#' @param spa_start_date SPA data collection start date (optional)
#' @param lock_date RCT/LS data collection cleaning end date for database lock (optional)
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
                                     lock_date = NULL,
                                     cost_pid = NULL,
                                     cost_dir = NULL) {


  if (is.null(spa_start_date)) {
    spa_start_date = start_date
  }

  ################
  # Set up ruODK #
  ################

  write(formats2h1("Connection to ODK Central through ruODK"), stderr())

  ruODK::ru_setup(
    svc = Sys.getenv("ODKC_SVC"),
    un = Sys.getenv("ODKC_UN"),
    pw = Sys.getenv("ODKC_PW"),
    tz = Sys.getenv("TZ"),
    verbose = FALSE # Can be switched to TRUE for demo or debugging
  )

  # List of projects ---------------------------
  # Only list projects visible with the credentials `ODKC_UN` and `ODKC_PW`
  odkc_project_list <- ruODK::project_list()$id

  # Environment variables which may be shared between different projects ---------------------------
  wd_fid <- Sys.getenv("TIMCI_WD_FID")
  problem_fid <- Sys.getenv("TIMCI_PROBLEM_FID")

  # RCT / LS environment variables ---------------------------
  crf_facility_fid <- Sys.getenv("TIMCI_CRF_FACILITY_FID")
  crf_day7_fid <- Sys.getenv("TIMCI_CRF_DAY7_FID")
  crf_hospit_fid <- Sys.getenv("TIMCI_CRF_HOSPIT_FID")
  if (Sys.getenv('TIMCI_COUNTRY') == "Tanzania" || Sys.getenv('TIMCI_COUNTRY') == "India") {
    crf_day28_fid <- Sys.getenv("TIMCI_CRF_DAY28_FID")
  }
  crf_wfa_fid <- Sys.getenv("TIMCI_WEEKLY_FA_FID")

  # SPA environment variables ---------------------------
  cgei_fid <- Sys.getenv("TIMCI_SPA_CGEI_FID")
  fa_fid <- Sys.getenv("TIMCI_SPA_FA_FID")
  sco_fid <- Sys.getenv("TIMCI_SPA_SCO_FID")
  hcpi_fid <- Sys.getenv("TIMCI_SPA_HCPI_FID")
  tf_fid <- Sys.getenv("TIMCI_TF_FID")
  pm_fid <- Sys.getenv("TIMCI_PM_FID")

  # Cost environment variables ---------------------------
  medical_cost_fid <- Sys.getenv("TIMCI_COST_MEDICAL_FID")
  hospital_cost_fid <- Sys.getenv("TIMCI_COST_HOSPITAL_FID")

  # Qualitative environment variables ---------------------------
  cgidi1_fid <- Sys.getenv("TIMCI_QUAL_CGIDI1_FID")
  cgidi2_fid <- Sys.getenv("TIMCI_QUAL_CGIDI2_FID")
  cgidi3_fid <- Sys.getenv("TIMCI_QUAL_CGIDI3_FID")

  #######################
  # Load TIMCI ODK data #
  #######################

  write(formats2h1("Load TIMCI ODK data"), stderr())

  # List RCT/LS forms ---------------------------
  rct_ls_form_list <- ruODK::form_list(pid = rctls_pid)$fid

  # List SPA forms ---------------------------
  spa_form_list <- NULL
  if (spa_pid %in% odkc_project_list) {
    spa_form_list <- ruODK::form_list(pid = spa_pid)$fid
  }

  # List cost forms ---------------------------
  cost_form_list <- NULL
  if (!is.null(cost_pid)) {
    if (cost_pid %in% odkc_project_list) {
      cost_form_list <- ruODK::form_list(pid = cost_pid)$fid
    }
  }

  # List qualitative forms ---------------------------
  qual_form_list <- NULL
  if (qpid %in% odkc_project_list) {
    qual_form_list <- ruODK::form_list(pid = qpid)$fid
  }

  # Load facility data ---------------------------
  write(formats2h2("Load RCT/LS facility data"), stderr())

  raw_facility_zip <- ruODK::submission_export(local_dir = tempdir(),
                                               pid = rctls_pid,
                                               fid = crf_facility_fid,
                                               pp = rctls_pp,
                                               media = FALSE)
  # Dirty fix - To be moved when time allows
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

  # SANITY CHECK: export raw LS/RCT data from ODK
  t <- tempdir()
  utils::unzip(raw_facility_zip, exdir = t)
  fs::dir_ls(t)
  if (!is.null(col_specs)) {
    raw_odk_data <- readr::with_edition(1, readr::read_csv(file.path(t, paste0(crf_facility_fid,".csv")), col_types = col_specs))
  } else{
    raw_odk_data <- readr::with_edition(1, readr::read_csv(file.path(t, paste0(crf_facility_fid,".csv"))))
  }
  fn <- timci::export_df2xlsx(raw_odk_data,
                              mdb_dir,
                              "01a_screening_raw")

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

  # Copy audit trail in folder: only enabled if MEDIA = TRUE when downloading the initial *.zip
  facility_data_audit <- timci::extract_additional_data_from_odk_zip(raw_facility_zip, paste0(crf_facility_fid, " - audit.csv"))

  # Load day 7 follow-up data
  write("Load day 7 follow-up data", stderr())
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
  write("Load hospital visit data", stderr())
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

  # [Tanzania and India only] Load day 28 follow-up data
  raw_day28fu_data <- NULL
  if (Sys.getenv('TIMCI_COUNTRY') == "Tanzania" || Sys.getenv('TIMCI_COUNTRY') == "India") {
    write("Load day 28 follow-up data", stderr())
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
  write("Load withdrawal data", stderr())
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
  write("Load weekly facility assessment data", stderr())
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

  # Load SPA data ---------------------------

  write("Load TIMCI SPA data", stderr())

  spa_cgei_data <- NULL
  spa_fa_data <- NULL
  spa_hcpi_data <- NULL
  spa_sco_data <- NULL
  tf_data <- NULL
  tf_data_full <- NULL
  pm_data <- NULL

  if (spa_pid %in% odkc_project_list) {

    # Load SPA caregiver exit interview data
    write("Load SPA caregiver exit interview data", stderr())
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
                                             media = TRUE)
      tf_data <- timci::extract_data_from_odk_zip(raw_tf_zip,
                                                  paste0(tf_fid,".csv"),
                                                  spa_start_date,
                                                  end_date)
      # Extract time-flow audit and single steps
      tf_data_audit <- timci::extract_additional_data_from_odk_zip(raw_tf_zip, paste0(tf_fid, " - audit.csv"))
      tf_data_steps <- timci::extract_additional_data_from_odk_zip(raw_tf_zip, paste0(tf_fid, "-steps.csv"))
      tf_data_full <- list(tf_data, tf_data_audit, tf_data_steps)
    }

    # Load process-mapping data
    print("Load time-flow data")
    if (pm_fid %in% spa_form_list) {
      raw_pm_zip <- ruODK::submission_export(local_dir = tempdir(),
                                             pid = spa_pid,
                                             fid = pm_fid,
                                             media = FALSE)
      pm_data <- timci::extract_data_from_odk_zip(raw_pm_zip,
                                                  paste0(pm_fid,".csv"),
                                                  spa_start_date,
                                                  end_date)
    }

  }

  # Load cost data ---------------------------
  medical_cost_data <- NULL
  hospital_cost_data <- NULL

  if (!is.null(cost_pid)) {
    if (cost_pid %in% odkc_project_list) {

      # Load medical cost data
      write("Load medical cost data", stderr())

      if (medical_cost_fid %in% cost_form_list) {
        medical_cost_zip <- ruODK::submission_export(local_dir = tempdir(),
                                                     pid = cost_pid,
                                                     fid = medical_cost_fid,
                                                     media = FALSE)
        medical_cost_data <- timci::extract_data_from_odk_zip(medical_cost_zip,
                                                              paste0(medical_cost_fid,".csv"),
                                                              spa_start_date,
                                                              end_date)
      }

      # Load hospital cost data
      write("Load hospital cost data", stderr())
      if (hospital_cost_fid %in% cost_form_list) {
        hospital_cost_zip <- ruODK::submission_export(local_dir = tempdir(),
                                                      pid = cost_pid,
                                                      fid = hospital_cost_fid,
                                                      media = FALSE)
        hospital_cost_data <- timci::extract_data_from_odk_zip(hospital_cost_zip,
                                                               paste0(hospital_cost_fid,".csv"),
                                                               spa_start_date,
                                                               end_date)
      }

    }
  }

  # Load qualitative data ---------------------------
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
                 cost_dir = cost_dir,
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
                 pm_data = pm_data,
                 medical_cost_data = medical_cost_data,
                 hospital_cost_data = hospital_cost_data,
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
