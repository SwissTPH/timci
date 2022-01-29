#' Set language settings
#'
#' This function sets country-specific language preferences to be used in the Tools for Integrated Management of Childhood Illnesses (TIMCI) project.
#' The function more specifically sets the environment variable `LANG` and teh current locale `LC_TIME`
#'
#' @export

set_language_settings <- function() {

  if (Sys.getenv('TIMCI_COUNTRY') == 'Senegal') {
    Sys.setenv(LANG = "fr")
    Sys.setlocale("LC_TIME", "French")
  } else {
    Sys.setenv(LANG = "en")
    Sys.setlocale("LC_TIME", "English")
  }

}

#' Set the connection to ODK Central through ruODK
#'
#' This function sets the connection to the country ODK Central server which stores data in the Tools for Integrated Management of Childhood Illnesses (TIMCI) project.
#' To this aim, the function relies on the `ruODK` package which is an R client to access and parse data from ODK Central.
#'
#' @param verbose Boolean (optional) that enables (TRUE) or disables (FALSE) verbose output. Defaut is set to FALSE.
#' @return list of ODK central project IDs
#' @export

set_odkc_connection <- function(verbose = FALSE) {

  write(formats2h1("Connection to ODK Central through ruODK"), stderr())

  ruODK::ru_setup(
    svc = Sys.getenv("ODKC_SVC"),
    un = Sys.getenv("ODKC_UN"),
    pw = Sys.getenv("ODKC_PW"),
    tz = Sys.getenv("TZ"),
    verbose = verbose # Can be switched to TRUE for demo or debugging
  )

  # List of projects ---------------------------
  # Only list projects visible with the credentials `ODKC_UN` and `ODKC_PW`
  odkc_project_list <- ruODK::project_list()$id

}

#' Run Rmarkdown files
#'
#' This function runs several Rmarkdown files to generate standardised automated reports for the Tools for Integrated Management of Childhood Illnesses (TIMCI) project.
#'
#' @param rctls_pid Numeric value that refers ID of the RCT/LS ODK Central project
#' @param rctls_pp Passphrase
#' @param spa_pid Numeric value that refers to the ID of the SPA / time-flow / process mapping project on the ODK Central server
#' @param cost_pid Numeric value (optional) that refers to the ID of the cost and cost-effectiveness project on the ODK Central server
#' @param qpid Numeric ID of the qualitative ODK Central project
#' @param qual_pp Passphrase
#' @param research_facilities Dataframe that contains the research facilities
#' @param report_dir Path to the output folder for the generated Rmarkdown reports
#' @param participant_zip Path to the encrypted zip archive that stores participant data
#' @param mdb_dir Path to the output folder for the RCT / LS database exports
#' @param fu_dir Path to the output folder for the follow-up exports
#' @param qualcg_dir Path to the output folder for the caregiver in-depth interview exports
#' @param qualhcp_dir Path to the output folder for the healthcare provider in-depth interview exports
#' @param qualkii_dir Path to the output folder for the key informant interview exports (optional)
#' @param qualos_dir Path to the output folder for the online survey exports (optional)
#' @param spa_db_dir Path to the output folder for the SPA / time-flow / process mapping database exports
#' @param cost_dir Path to the output folder for the cost and cost effectiveness database exports (optional)
#' @param path_dir Path to the output folder for the M&E exports to be shared with PATH
#' @param start_date RCT/LS data collection start date (optional)
#' @param end_date RCT/LS data collection end date (optional)
#' @param spa_start_date SPA data collection start date (optional)
#' @param lock_date RCT/LS data collection cleaning end date for database lock (optional)
#' @param sample_size Numeric value, sample size for RCT/LS enrolment
#' @param short Short version of the export
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
                                     qualcg_dir,
                                     qualhcp_dir,
                                     spa_db_dir,
                                     path_dir,
                                     start_date = NULL,
                                     end_date = NULL,
                                     spa_start_date = NULL,
                                     lock_date = NULL,
                                     cost_pid = NULL,
                                     cost_dir = NULL,
                                     qualkii_dir = NULL,
                                     qualos_dir = NULL,
                                     sample_size = 100000,
                                     short = FALSE) {

  ###########################
  # Set up current language #
  ###########################

  timci::set_language_settings()

  ######################
  # Set up study dates #
  ######################

  if (is.null(end_date)) {
    end_date = Sys.Date()
  } else{
    end_date = as.Date(end_date, "%Y-%m-%d")
  }

  day7fu_end_date <- min(as.Date(end_date + 14), Sys.Date())
  hospitfu_end_date <- min(as.Date(end_date + 42), Sys.Date())
  day28fu_end_date <- min(as.Date(end_date + 42), Sys.Date())
  withdrawal_end_date <- min(as.Date(end_date + 42), Sys.Date())
  wfa_end_date <- min(as.Date(end_date + 6), Sys.Date())

  if (is.null(spa_start_date)) {
    spa_start_date = start_date
  }

  ################
  # Set up ruODK #
  ################

  # Connection to ODK Central through ruODK
  odkc_project_list <- set_odkc_connection()

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
  hcpidi_fid <- Sys.getenv("TIMCI_QUAL_HCPIDI_FID")
  kii_fid <- Sys.getenv("TIMCI_QUAL_KII_FID")
  os_fid <- Sys.getenv("TIMCI_QUAL_OS_FID")

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

  write(formats2h3("Load day 0 data"), stderr())

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

  raw_facility_data <- timci::extract_data_from_odk_zip(odk_zip = raw_facility_zip,
                                                        csv_name = paste0(crf_facility_fid,".csv"),
                                                        start_date = start_date,
                                                        end_date = end_date,
                                                        local_dir = t,
                                                        col_specs = col_specs)
  if (Sys.getenv('TIMCI_COUNTRY') == 'Tanzania') {
    facility_data <- timci::process_tanzania_facility_data(raw_facility_data)
  } else{
    facility_data <- timci::process_facility_data(raw_facility_data)
  }

  # Copy audit trail in folder: only enabled if MEDIA = TRUE when downloading the initial *.zip
  facility_data_audit <- timci::extract_additional_data_from_odk_zip(odk_zip = raw_facility_zip,
                                                                     csv_name = paste0(crf_facility_fid, " - audit.csv"),
                                                                     local_dir = t)

  # Load day 7 follow-up data
  write(formats2h3("Load day 7 follow-up data"), stderr())
  raw_day7fu_data <- extract_data_from_odk_server(cpid = rctls_pid,
                                                  cpid_forms = rct_ls_form_list,
                                                  cpp = rctls_pp,
                                                  cfid = crf_day7_fid,
                                                  start_date = start_date,
                                                  end_date = day7fu_end_date,
                                                  verbose = TRUE)

  # Load hospital visit follow-up data
  write(formats2h3("Load hospital visit data"), stderr())
  raw_hospit_data <- extract_data_from_odk_server(cpid = rctls_pid,
                                                  cpid_forms = rct_ls_form_list,
                                                  cpp = rctls_pp,
                                                  cfid = crf_hospit_fid,
                                                  start_date = start_date,
                                                  end_date = hospitfu_end_date,
                                                  verbose = TRUE)

  # [Tanzania and India only] Load day 28 follow-up data
  raw_day28fu_data <- NULL
  if (Sys.getenv('TIMCI_COUNTRY') == "Tanzania" || Sys.getenv('TIMCI_COUNTRY') == "India") {
    write(formats2h3("Load day 28 follow-up data"), stderr())
    if (crf_day28_fid %in% rct_ls_form_list) {
      raw_day28fu_zip <- ruODK::submission_export(local_dir = tempdir(),
                                                  pid = rctls_pid,
                                                  fid = crf_day28_fid,
                                                  pp = rctls_pp,
                                                  media = FALSE)
      raw_day28fu_data <- timci::extract_data_from_odk_zip(odk_zip = raw_day28fu_zip,
                                                           csv_name = paste0(crf_day28_fid,".csv"),
                                                           start_date = start_date,
                                                           end_date = day28fu_end_date)
    }
  }

  # Load widthdrawal data
  write(formats2h3("Load withdrawal data"), stderr())
  raw_withdrawal_data <- extract_data_from_odk_server(cpid = rctls_pid,
                                                      cpid_forms = rct_ls_form_list,
                                                      cpp = rctls_pp,
                                                      cfid = wd_fid,
                                                      start_date = start_date,
                                                      end_date = withdrawal_end_date,
                                                      verbose = TRUE)

  # Load weekly facility assessment data
  write(formats2h3("Load weekly facility assessment data"), stderr())
  raw_wfa_data <- extract_data_from_odk_server(cpid = rctls_pid,
                                               cpid_forms = rct_ls_form_list,
                                               cpp = rctls_pp,
                                               cfid = crf_wfa_fid,
                                               start_date = start_date,
                                               end_date = wfa_end_date,
                                               verbose = TRUE)
  wfa_data <- NULL
  if (!is.null(raw_wfa_data)) {
    wfa_data <- timci::process_weekly_fa_data(raw_wfa_data)
  }

  # Load problem report data
  write(formats2h3("Load problem report data"), stderr())
  raw_problem_data <- extract_data_from_odk_server(cpid = rctls_pid,
                                                   cpid_forms = rct_ls_form_list,
                                                   cpp = rctls_pp,
                                                   cfid = problem_fid,
                                                   start_date = start_date,
                                                   end_date = end_date,
                                                   verbose = TRUE)

  # Load SPA data ---------------------------

  write(formats2h2("Load TIMCI SPA data"), stderr())

  spa_cgei_data <- NULL
  spa_fa_data <- NULL
  spa_hcpi_data <- NULL
  spa_sco_data <- NULL
  tf_data <- NULL
  pm_data <- NULL

  if (spa_pid %in% odkc_project_list) {

    # Load SPA caregiver exit interview data
    write(formats2h3("Load SPA caregiver exit interview data"), stderr())
    spa_cgei_data <- extract_data_from_odk_server(cpid = spa_pid,
                                                  cpid_forms = spa_form_list,
                                                  cfid = cgei_fid,
                                                  start_date = spa_start_date,
                                                  end_date = end_date,
                                                  verbose = TRUE)

    # Load SPA facility assessment data
    print("Load SPA facility assessment data")
    spa_fa_data <- extract_data_from_odk_server(cpid = spa_pid,
                                                cpid_forms = spa_form_list,
                                                cfid = fa_fid,
                                                start_date = spa_start_date,
                                                end_date = end_date,
                                                verbose = TRUE)

    # Load SPA healthcare provider interview data
    print("Load SPA healthcare provider interview data")
    spa_hcpi_data <- extract_data_from_odk_server(cpid = spa_pid,
                                                  cpid_forms = spa_form_list,
                                                  cfid = hcpi_fid,
                                                  start_date = spa_start_date,
                                                  end_date = end_date,
                                                  verbose = TRUE)

    # Load SPA sick child observation protocol data
    print("Load SPA sick child observation protocol data")
    spa_sco_data <- extract_data_from_odk_server(cpid = spa_pid,
                                                 cpid_forms = spa_form_list,
                                                 cfid = sco_fid,
                                                 start_date = spa_start_date,
                                                 end_date = end_date,
                                                 verbose = TRUE)

    # Load time-flow data
    write(formats2h3("Load time-flow data"), stderr())
    tf_data <- extract_complex_data_from_odk_server(cpid = spa_pid,
                                                    cpid_forms = spa_form_list,
                                                    cfid = tf_fid,
                                                    start_date = spa_start_date,
                                                    end_date = end_date,
                                                    verbose = TRUE)

    # Load process-mapping data
    write(formats2h3("Load process mapping data"), stderr())
    pm_data <- extract_complex_data_from_odk_server(cpid = spa_pid,
                                                    cpid_forms = spa_form_list,
                                                    cfid = pm_fid,
                                                    start_date = spa_start_date,
                                                    end_date = end_date,
                                                    verbose = TRUE)

  }

  # Load TIMCI cost data ---------------------------

  write(formats2h2("Load TIMCI cost data"), stderr())

  medical_cost_data <- NULL
  hospital_cost_data <- NULL

  if (!is.null(cost_pid)) {
    if (cost_pid %in% odkc_project_list) {

      # Load medical cost data
      write(formats2h3("Load medical cost data"), stderr())
      medical_cost_data <- extract_complex_data_from_odk_server(cpid = cost_pid,
                                                                cpid_forms = cost_form_list,
                                                                cfid = medical_cost_fid,
                                                                start_date = start_date,
                                                                end_date = end_date,
                                                                verbose = TRUE)

      # Load hospital cost data
      write(formats2h3("Load hospital cost data"), stderr())
      hospital_cost_data <- extract_complex_data_from_odk_server(cpid = cost_pid,
                                                                 cpid_forms = cost_form_list,
                                                                 cfid = hospital_cost_fid,
                                                                 start_date = start_date,
                                                                 end_date = end_date,
                                                                 verbose = TRUE)

    }
  }

  # Load TIMCI qualitative data ---------------------------

  write(formats2h2("Load TIMCI qualitative data"), stderr())

  cgidi_invitation_data <- NULL
  cgidi_encryption_data <- NULL
  cgidi_interview_data <- NULL
  hcpidi_interview_data <- NULL
  kii_interview_data <- NULL
  online_survey_data <- NULL

  if (qpid %in% odkc_project_list) {

    # Load caregiver IDI invitation data
    write(formats2h3("Load caregiver in-depth interview (IDI) invitation data"), stderr())
    cgidi_invitation_data <- timci::extract_data_from_odk_server(cpid = qpid,
                                                                 cpid_forms = qual_form_list,
                                                                 cfid = cgidi1_fid,
                                                                 cpp = qual_pp,
                                                                 start_date = start_date,
                                                                 end_date = end_date,
                                                                 verbose = TRUE)

    # Load caregiver IDI encryption list
    write(formats2h3("Load caregiver in-depth interview (IDI) encryption list"), stderr())
    cgidi_encryption_data <- timci::extract_data_from_odk_server(cpid = qpid,
                                                                 cpid_forms = qual_form_list,
                                                                 cfid = cgidi2_fid,
                                                                 cpp = qual_pp,
                                                                 start_date = start_date,
                                                                 end_date = end_date,
                                                                 verbose = TRUE)

    # Load caregiver in-depth interview (IDI) data
    write(formats2h3("Load caregiver in-depth interview (IDI) data"), stderr())
    cgidi_interview_data <- extract_data_from_odk_server(cpid = qpid,
                                                         cpid_forms = qual_form_list,
                                                         cfid = cgidi3_fid,
                                                         cpp = qual_pp,
                                                         start_date = start_date,
                                                         end_date = end_date,
                                                         verbose = TRUE)

    # Load healthcare provider in-depth interview (IDI) data
    write(formats2h3("Load healthcare provider in-depth interview (IDI) data"), stderr())
    hcpidi_interview_data <- extract_data_from_odk_server(cpid = qpid,
                                                          cpid_forms = qual_form_list,
                                                          cfid = hcpidi_fid,
                                                          cpp = qual_pp,
                                                          start_date = start_date,
                                                          end_date = end_date,
                                                          verbose = TRUE)

    # Load key informant interview (KII) data
    write(formats2h3("Load key informant interview (KII) data"), stderr())
    kii_interview_data <- extract_data_from_odk_server(cpid = qpid,
                                                       cpid_forms = qual_form_list,
                                                       cfid = kii_fid,
                                                       cpp = qual_pp,
                                                       start_date = start_date,
                                                       end_date = end_date,
                                                       verbose = TRUE)

    # Load online survey data
    write(formats2h3("Load online survey data"), stderr())
    online_survey_data <- extract_data_from_odk_server(cpid = qpid,
                                                       cpid_forms = qual_form_list,
                                                       cfid = os_fid,
                                                       cpp = qual_pp,
                                                       start_date = start_date,
                                                       end_date = end_date,
                                                       verbose = TRUE)

  }

  ###########################
  # RCT data quality report #
  ###########################

  write(formats2h1("Export data and generate data quality report"), stderr())

  params <- list(rctls_dir = mdb_dir,
                 research_facilities = research_facilities,
                 participant_zip = participant_zip,
                 spa_dir = spa_db_dir,
                 cost_dir = cost_dir,
                 qualcg_dir = qualcg_dir,
                 qualhcp_dir = qualhcp_dir,
                 qualkii_dir = qualkii_dir,
                 qualos_dir = qualos_dir,
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
                 tf_data = tf_data,
                 pm_data = pm_data,
                 medical_cost_data = medical_cost_data,
                 hospital_cost_data = hospital_cost_data,
                 cgidi_invitation_data = cgidi_invitation_data,
                 cgidi_encryption_data = cgidi_encryption_data,
                 cgidi_interview_data = cgidi_interview_data,
                 hcpidi_interview_data = hcpidi_interview_data,
                 kii_interview_data = kii_interview_data,
                 online_survey_data = online_survey_data,
                 lock_date = lock_date)
  generate_word_report(report_dir, "database_export.Rmd", "timci_data_export_report", params)

  #########################
  # RCT monitoring report #
  #########################

  write(formats2h1("Generate monitoring report"), stderr())

  params <- list(research_facilities = research_facilities,
                 start_date = start_date,
                 end_date = end_date,
                 sample_target = sample_size,
                 facility_data = facility_data,
                 raw_day7fu_data = raw_day7fu_data,
                 raw_hospit_data = raw_hospit_data,
                 raw_day28fu_data = raw_day28fu_data,
                 raw_withdrawal_data = raw_withdrawal_data,
                 wfa_data = wfa_data)
  if (Sys.getenv('TIMCI_COUNTRY') == 'Tanzania' || Sys.getenv('TIMCI_COUNTRY') == 'India') {
    rname <- "timci_rct_monitoring_report"
  } else{
    rname <- "timci_ls_monitoring_report"
  }
  generate_pdf_report(report_dir, "rct_monitoring_report.Rmd", rname, params)

  ###################
  # PATH M&E report #
  ###################

  write(formats2h1("Generate PATH M&E report"), stderr())

  params <- list(path_dir = path_dir,
                 facility_data = facility_data,
                 research_facilities = research_facilities,
                 wfa_data = wfa_data)
  generate_pdf_report(path_dir, "path_report.Rmd", "TIMCI_M&E_RA_report_for_PATH", params)

  #########################
  # SPA monitoring report #
  #########################

  write(formats2h1("Generate SPA report"), stderr())

  # Filter research facilities to only keep those in which SPA activities are conducted
  spa_research_facilities <- research_facilities %>%
    filter(spa == 1)

  # Check that at least one SPA database is not empty
  spa_sco_is_not_null <- !is.null(spa_sco_data)
  spa_cgei_is_not_null <- !is.null(spa_cgei_data)
  spa_fa_is_not_null <- !is.null(spa_fa_data)
  spa_hcpi_is_not_null <- !is.null(spa_hcpi_data)
  spa_report_condition <- spa_sco_is_not_null | spa_cgei_is_not_null | spa_fa_is_not_null | spa_hcpi_is_not_null

  # Only keep data that corresponds to SPA facilities
  if (spa_sco_is_not_null) {
    spa_sco_data <- spa_sco_data %>%
      merge(y = spa_research_facilities[, c("facility_id")],
            by.x = 'facility_identification-fcode',
            by.y = 'facility_id',
            all = FALSE)
  }

  if (spa_cgei_is_not_null) {
    spa_cgei_data <- spa_cgei_data %>%
      merge(y = spa_research_facilities[, c("facility_id")],
            by.x = 'b1-fcode',
            by.y = 'facility_id',
            all = FALSE)
  }

  if (spa_fa_is_not_null) {
    spa_fa_data <- spa_fa_data %>%
      merge(y = spa_research_facilities[, c("facility_id")],
            by.x = 'facility_identification-a1_2',
            by.y = 'facility_id',
            all = FALSE)
  }

  if (spa_hcpi_is_not_null) {
    spa_hcpi_data <- spa_hcpi_data %>%
      merge(y = spa_research_facilities[, c("facility_id")],
            by.x = 'a1-fcode',
            by.y = 'facility_id',
            all = FALSE)
  }

  # If at least one SPA database is not empty, generate the SPA report
  if (spa_report_condition) {
    params <- list(research_facilities = spa_research_facilities,
                   facility_data = facility_data,
                   spa_sco_data = spa_sco_data,
                   spa_cgei_data = spa_cgei_data,
                   spa_fa_data = spa_fa_data,
                   spa_hcpi_data = spa_hcpi_data,
                   raw_withdrawal_data = raw_withdrawal_data)
    generate_pdf_report(report_dir, "spa_monitoring_report.Rmd", "timci_spa_monitoring_report", params)
  }

  #############################################
  # Process map / time-flow monitoring report #
  #############################################

  write(formats2h1("Generate process map & time-flow report"), stderr())

  if (!is.null(tf_data) | !is.null(pm_data)) {
    if (length(tf_data) > 0 | length(pm_data) > 0) {
      if (length(tf_data[[1]]) > 0 | length(pm_data[[1]]) > 0) {
        params <- list(research_facilities = spa_research_facilities,
                       facility_data = facility_data,
                       tf_data = tf_data[[1]],
                       pm_data = NULL,
                       raw_withdrawal_data = raw_withdrawal_data)
        generate_pdf_report(report_dir, "pmtf_monitoring_report.Rmd", "timci_processmap_timeflow_monitoring_report", params)
      }
    }
  }

  #################################
  # Qualitative monitoring report #
  #################################

  write(formats2h1("Generate qualitative report"), stderr())

  if (!is.null(hcpidi_interview_data)) {
    if (length(hcpidi_interview_data) > 0) {
      params <- list(research_facilities = research_facilities,
                     facility_data = facility_data,
                     hcpidi_interview_data = hcpidi_interview_data,
                     raw_withdrawal_data = raw_withdrawal_data)
      generate_pdf_report(report_dir, "qual_monitoring_report.Rmd", "timci_qual_monitoring_report", params)
    }
  }

}

#' Run Rmarkdown files
#'
#' This function runs several Rmarkdown files to generate standardised automated reports for the Tools for Integrated Management of Childhood Illnesses (TIMCI) project.
#'
#' @param cost_pid Numeric ID of the cost and cost-effectiveness ODK Central project (optional)
#' @param research_facilities Dataframe that contains the research facilities
#' @param report_dir Path to the output folder for the generated Rmarkdown reports
#' @param cost_dir Path to the output folder for the cost and cost effectiveness database exports (optional)
#' @param cost_start_date SPA data collection start date (optional)
#' @param cost_end_date RCT/LS data collection end date (optional)
#' @param cost_lock_date RCT/LS data collection cleaning end date for database lock (optional)
#' @param short Short version of the export
#' @import rmarkdown ruODK
#' @export

export_cost_studies <- function(cost_pid,
                                cost_dir,
                                research_facilities,
                                report_dir,
                                cost_start_date = NULL,
                                cost_end_date = NULL,
                                cost_lock_date = NULL,
                                short = FALSE) {

  ###########################
  # Set up current language #
  ###########################

  timci::set_language_settings()

  ######################
  # Set up study dates #
  ######################

  if (is.null(cost_end_date)) {
    cost_end_date <- Sys.Date()
  } else{
    cost_end_date <- as.Date(cost_end_date, "%Y-%m-%d")
  }

  if (is.null(cost_start_date)) {
    cost_start_date <- cost_start_date
  }

  ################
  # Set up ruODK #
  ################

  # Connection to ODK Central through ruODK
  odkc_project_list <- set_odkc_connection()

  #######################
  # Load TIMCI ODK data #
  #######################

  write(formats2h1("Load TIMCI ODK data"), stderr())

  # Cost environment variables ---------------------------
  medical_cost_fid <- Sys.getenv("TIMCI_COST_MEDICAL_FID")
  hospital_cost_fid <- Sys.getenv("TIMCI_COST_HOSPITAL_FID")

  # Initialise variables ---------------------------
  cost_form_list <- NULL
  medical_cost_data <- NULL
  hospital_cost_data <- NULL

  # Load TIMCI cost data ---------------------------

  write(formats2h2("Load TIMCI cost data"), stderr())

  if (!is.null(cost_pid)) {

    if (cost_pid %in% odkc_project_list) {

      cost_form_list <- ruODK::form_list(pid = cost_pid)$fid

      # Load medical cost data
      write(formats2h3("Load medical cost data"), stderr())
      medical_cost_data <- extract_complex_data_from_odk_server(cpid = cost_pid,
                                                                cpid_forms = cost_form_list,
                                                                cfid = medical_cost_fid,
                                                                start_date = cost_start_date,
                                                                end_date = cost_end_date,
                                                                verbose = TRUE)

      # Load hospital cost data
      write(formats2h3("Load hospital cost data"), stderr())
      hospital_cost_data <- extract_complex_data_from_odk_server(cpid = cost_pid,
                                                                 cpid_forms = cost_form_list,
                                                                 cfid = hospital_cost_fid,
                                                                 start_date = cost_start_date,
                                                                 end_date = cost_end_date,
                                                                 verbose = TRUE)

    }
  }

  ###########################
  # RCT data quality report #
  ###########################

  write(formats2h1("Export data and generate data quality report"), stderr())
  write("No data exported for the moment - To be updated", stderr())

}
