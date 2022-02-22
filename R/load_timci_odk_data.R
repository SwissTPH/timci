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

#' Load RCT/LS data
#'
#' This function loads RCT/LS data for the Tools for Integrated Management of Childhood Illnesses (TIMCI) project.
#'
#' @param odkc_project_list List of ODK project IDs on the ODK Central server
#' @param rctls_pid Numeric value that refers to the ID of the RCT/LS project on the ODK Central server
#' @param rctls_pp Encryption passphrase associated with the RCT/LS ODK ODK Central project
#' @param rctls_start_date RCT/LS data collection start date
#' @param rctls_end_date RCT/LS data collection end date
#'
#' @return list of ODK central project IDs
#' @export

load_rctls_data <- function(odkc_project_list,
                            rctls_pid,
                            rctls_pp,
                            rctls_start_date,
                            rctls_end_date) {

  # RCT/LS environment variables ---------------------------
  crf_facility_fid <- Sys.getenv("TIMCI_CRF_FACILITY_FID")
  crf_day7_fid <- Sys.getenv("TIMCI_CRF_DAY7_FID")
  crf_hospit_fid <- Sys.getenv("TIMCI_CRF_HOSPIT_FID")
  if (Sys.getenv('TIMCI_COUNTRY') == "Tanzania" || Sys.getenv('TIMCI_COUNTRY') == "India") {
    crf_day28_fid <- Sys.getenv("TIMCI_CRF_DAY28_FID")
  }
  crf_wfa_fid <- Sys.getenv("TIMCI_WEEKLY_FA_FID")

  # List RCT/LS forms ---------------------------
  rct_ls_form_list <- NULL
  if (rctls_pid %in% odkc_project_list) {
    rct_ls_form_list <- ruODK::form_list(pid = rctls_pid)$fid
  }

  # Initialise RCT/LS datasets to NULL ---------------------------
  facility_data <- NULL
  raw_day7fu_data <- NULL
  raw_day28fu_data <- NULL
  raw_withdrawal_data <- NULL
  raw_wfa_data <- NULL

  # Load the RCT/LS datasets ---------------------------
  if (spa_pid %in% odkc_project_list) {
    write(formats2h2("Load RCT/LS data"), stderr())

    # Load RCT/LS Day 0 data
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
                                                          start_date = rctls_start_date,
                                                          end_date = rctls_end_date,
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
                                                    start_date = rctls_start_date,
                                                    end_date = day7fu_end_date,
                                                    verbose = TRUE)

    # Load hospital visit follow-up data
    write(formats2h3("Load hospital visit data"), stderr())
    raw_hospit_data <- extract_data_from_odk_server(cpid = rctls_pid,
                                                    cpid_forms = rct_ls_form_list,
                                                    cpp = rctls_pp,
                                                    cfid = crf_hospit_fid,
                                                    start_date = rctls_start_date,
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
                                                             start_date = rctls_start_date,
                                                             end_date = day28fu_end_date)
      }
    }
  }

  # Load widthdrawal data
  write(formats2h3("Load withdrawal data"), stderr())
  raw_withdrawal_data <- extract_data_from_odk_server(cpid = rctls_pid,
                                                      cpid_forms = rct_ls_form_list,
                                                      cpp = rctls_pp,
                                                      cfid = wd_fid,
                                                      start_date = rctls_start_date,
                                                      end_date = withdrawal_end_date,
                                                      verbose = TRUE)

  # Load weekly facility assessment data
  write(formats2h3("Load weekly facility assessment data"), stderr())
  raw_wfa_data <- extract_data_from_odk_server(cpid = rctls_pid,
                                               cpid_forms = rct_ls_form_list,
                                               cpp = rctls_pp,
                                               cfid = crf_wfa_fid,
                                               start_date = rctls_start_date,
                                               end_date = wfa_end_date,
                                               verbose = TRUE)
  wfa_data <- NULL
  if (!is.null(raw_wfa_data)) {
    wfa_data <- timci::process_weekly_fa_data(raw_wfa_data)
  }

  # Return RCT/LS datasets ---------------------------
  list(facility_data,
       wfa_data,)

}

#' Load Service Provision Assessment (SPA), process mapping & time-flow (PM&TF) data
#'
#' This function loads SPA data for the Tools for Integrated Management of Childhood Illnesses (TIMCI) project.
#'
#' @param odkc_project_list List of ODK project IDs on the ODK Central server
#' @param spa_pid Numeric value that refers to the ID of the SPA / time-flow / process mapping project on the ODK Central server
#' @param spa_start_date SPA data collection start date
#' @param spa_end_date SPA data collection end date
#'
#' @return list of ODK central project IDs
#' @export

load_spapmtf_data <- function(odkc_project_list,
                              spa_pid,
                              spa_start_date,
                              spa_end_date) {

  # SPA and PM&TF environment variables ---------------------------
  cgei_fid <- Sys.getenv("TIMCI_SPA_CGEI_FID")
  fa_fid <- Sys.getenv("TIMCI_SPA_FA_FID")
  sco_fid <- Sys.getenv("TIMCI_SPA_SCO_FID")
  hcpi_fid <- Sys.getenv("TIMCI_SPA_HCPI_FID")
  tf_fid <- Sys.getenv("TIMCI_TF_FID")
  pm_fid <- Sys.getenv("TIMCI_PM_FID")

  # List SPA and PM&TF forms ---------------------------
  spa_form_list <- NULL
  if (spa_pid %in% odkc_project_list) {
    spa_form_list <- ruODK::form_list(pid = spa_pid)$fid
  }

  # Initialise SPA and PM&TF datasets to NULL ---------------------------
  spa_cgei_data <- NULL
  spa_fa_data <- NULL
  spa_hcpi_data <- NULL
  spa_sco_data <- NULL
  tf_data <- NULL
  pm_data <- NULL

  # Load SPA and PM&TF datasets ---------------------------
  if (spa_pid %in% odkc_project_list) {

    # Load SPA caregiver exit interview data
    write(formats2h3("Load SPA caregiver exit interview data"), stderr())
    spa_cgei_data <- extract_data_from_odk_server(cpid = spa_pid,
                                                  cpid_forms = spa_form_list,
                                                  cfid = cgei_fid,
                                                  start_date = spa_start_date,
                                                  end_date = spa_end_date,
                                                  verbose = TRUE)

    # Load SPA facility assessment data
    write(formats2h3("Load SPA facility assessment data"), stderr())
    spa_fa_data <- extract_data_from_odk_server(cpid = spa_pid,
                                                cpid_forms = spa_form_list,
                                                cfid = fa_fid,
                                                start_date = spa_start_date,
                                                end_date = spa_end_date,
                                                verbose = TRUE)

    # Load SPA healthcare provider interview data
    write(formats2h3("Load SPA healthcare provider interview data"), stderr())
    spa_hcpi_data <- extract_data_from_odk_server(cpid = spa_pid,
                                                  cpid_forms = spa_form_list,
                                                  cfid = hcpi_fid,
                                                  start_date = spa_start_date,
                                                  end_date = spa_end_date,
                                                  verbose = TRUE)

    # Load SPA sick child observation protocol data
    write(formats2h3("Load SPA sick child observation protocol data"), stderr())
    spa_sco_data <- extract_data_from_odk_server(cpid = spa_pid,
                                                 cpid_forms = spa_form_list,
                                                 cfid = sco_fid,
                                                 start_date = spa_start_date,
                                                 end_date = spa_end_date,
                                                 verbose = TRUE)

    # Load time-flow data
    write(formats2h3("Load time-flow data"), stderr())
    tf_data <- extract_complex_data_from_odk_server(cpid = spa_pid,
                                                    cpid_forms = spa_form_list,
                                                    cfid = tf_fid,
                                                    start_date = spa_start_date,
                                                    end_date = spa_end_date,
                                                    verbose = TRUE)

    # Load process-mapping data
    write(formats2h3("Load process mapping data"), stderr())
    pm_data <- extract_complex_data_from_odk_server(cpid = spa_pid,
                                                    cpid_forms = spa_form_list,
                                                    cfid = pm_fid,
                                                    start_date = spa_start_date,
                                                    end_date = spa_end_date,
                                                    verbose = TRUE)

  }

  # Return SPA and PM&TF datasets ---------------------------
  list(spa_cgei_data,
       spa_fa_data,
       spa_hcpi_data,
       spa_sco_data,
       tf_data,
       pm_data)

}

#' Load RCT/LS data
#'
#' This function loads RCT/LS data for the Tools for Integrated Management of Childhood Illnesses (TIMCI) project.
#'
#' @param odkc_project_list List of ODK project IDs on the ODK Central server
#' @param qpid Numeric value that refers to the ID of the qualitative project on the ODK Central server
#' @param qual_pp Encryption passphrase associated with the qualitative ODK Central project
#' @param qual_start_date Qualitative data collection start date
#' @param qual_end_date Qualitative data collection end date
#'
#' @return list of ODK central project IDs
#' @export

load_qual_data <- function(odkc_project_list,
                           qpid,
                           qual_pp,
                           qual_start_date,
                           qual_end_date) {

  # Qualitative environment variables ---------------------------
  cgidi1_fid <- Sys.getenv("TIMCI_QUAL_CGIDI1_FID")
  cgidi2_fid <- Sys.getenv("TIMCI_QUAL_CGIDI2_FID")
  cgidi3_fid <- Sys.getenv("TIMCI_QUAL_CGIDI3_FID")
  hcpidi_fid <- Sys.getenv("TIMCI_QUAL_HCPIDI_FID")
  kii_fid <- Sys.getenv("TIMCI_QUAL_KII_FID")
  os_fid <- Sys.getenv("TIMCI_QUAL_OS_FID")

  # List qualitative forms ---------------------------
  qual_form_list <- NULL
  if (qpid %in% odkc_project_list) {
    qual_form_list <- ruODK::form_list(pid = qpid)$fid
  }

  # Initialise qualitative datasets to NULL ---------------------------
  cgidi_invitation_data <- NULL
  cgidi_encryption_data <- NULL
  cgidi_interview_data <- NULL
  hcpidi_interview_data <- NULL
  kii_interview_data <- NULL
  online_survey_data <- NULL

  # Load qualitative datasets ---------------------------
  if (qpid %in% odkc_project_list) {

    # Load caregiver IDI invitation data
    write(formats2h3("Load caregiver in-depth interview (IDI) invitation data"), stderr())
    cgidi_invitation_data <- timci::extract_data_from_odk_server(cpid = qpid,
                                                                 cpid_forms = qual_form_list,
                                                                 cfid = cgidi1_fid,
                                                                 cpp = qual_pp,
                                                                 start_date = qual_start_date,
                                                                 end_date = qual_end_date,
                                                                 verbose = TRUE)

    # Load caregiver IDI encryption list
    write(formats2h3("Load caregiver in-depth interview (IDI) encryption list"), stderr())
    cgidi_encryption_data <- timci::extract_data_from_odk_server(cpid = qpid,
                                                                 cpid_forms = qual_form_list,
                                                                 cfid = cgidi2_fid,
                                                                 cpp = qual_pp,
                                                                 start_date = qual_start_date,
                                                                 end_date = qual_end_date,
                                                                 verbose = TRUE)

    # Load caregiver in-depth interview (IDI) data
    write(formats2h3("Load caregiver in-depth interview (IDI) data"), stderr())
    cgidi_interview_data <- extract_data_from_odk_server(cpid = qpid,
                                                         cpid_forms = qual_form_list,
                                                         cfid = cgidi3_fid,
                                                         cpp = qual_pp,
                                                         start_date = qual_start_date,
                                                         end_date = qual_end_date,
                                                         verbose = TRUE)

    # Load healthcare provider in-depth interview (IDI) data
    write(formats2h3("Load healthcare provider in-depth interview (IDI) data"), stderr())
    hcpidi_interview_data <- extract_data_from_odk_server(cpid = qpid,
                                                          cpid_forms = qual_form_list,
                                                          cfid = hcpidi_fid,
                                                          cpp = qual_pp,
                                                          start_date = qual_start_date,
                                                          end_date = qual_end_date,
                                                          verbose = TRUE)

    # Load key informant interview (KII) data
    write(formats2h3("Load key informant interview (KII) data"), stderr())
    kii_interview_data <- extract_data_from_odk_server(cpid = qpid,
                                                       cpid_forms = qual_form_list,
                                                       cfid = kii_fid,
                                                       cpp = qual_pp,
                                                       start_date = qual_start_date,
                                                       end_date = qual_end_date,
                                                       verbose = TRUE)

    # Load online survey data
    write(formats2h3("Load online survey data"), stderr())
    kii_interview_data <- extract_data_from_odk_server(cpid = qpid,
                                                       cpid_forms = qual_form_list,
                                                       cfid = os_fid,
                                                       cpp = qual_pp,
                                                       start_date = qual_start_date,
                                                       end_date = qual_end_date,
                                                       verbose = TRUE)

  }

  # Return qualitative datasets ---------------------------
  list(cgidi_interview_data,
       hcpidi_interview_data,
       kii_interview_data,
       kii_interview_data,
       cgidi_invitation_data,
       cgidi_encryption_data)

}
