#' Run Rmarkdown files
#'
#' This function runs several Rmarkdown files to generate standardised automated reports for the Tools for Integrated Management of Childhood Illnesses (TIMCI) project.
#'
#' @param rctls_pid Numeric value that refers ID of the RCT/LS ODK Central project
#' @param rctls_pp Encryption passphrase associated with the RCT/LS ODK ODK Central project
#' @param spa_pid Numeric value that refers to the ID of the SPA / time-flow / process mapping project on the ODK Central server
#' @param spa_pid2 Numeric value (optional) that refers to another ID of the SPA / time-flow / process mapping project on the ODK Central server if data have been split between 2 projects
#' @param cost_pid Numeric value (optional) that refers to the ID of the cost and cost-effectiveness project on the ODK Central server
#' @param qpid Numeric ID of the qualitative ODK Central project
#' @param qual_pp Encryption passphrase associated with the qualitative ODK Central project
#' @param research_facilities Dataframe that contains the research facilities
#' @param report_dir Path to the output folder for the generated Rmarkdown reports
#' @param participant_zip Path to the encrypted zip archive that stores participant data
#' @param mdb_dir Path to the output folder for the RCT / LS database exports
#' @param fu_dir Path to the output folder for the follow-up exports
#' @param qc_dir Path to the output folder that contains the quality check findings
#' @param lock_dir Path to the output folder that contains the cleaned datasets
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
#' @param is_test Boolean that enables to export and process a small subset of data for running technical tests (optional, default set to FALSE)
#' @param operational_reports Boolean that enables to generate operational reports (optional, default set to TRUE)
#' @param is_pilot Boolean that enables to select the pilot mode for Tanzania and India (optional, default set to FALSE)
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
                                     qc_dir,
                                     lock_dir,
                                     qualcg_dir,
                                     qualhcp_dir,
                                     spa_db_dir,
                                     path_dir,
                                     start_date = NULL,
                                     end_date = NULL,
                                     spa_pid2 = NULL,
                                     spa_start_date = NULL,
                                     lock_date = NULL,
                                     cost_pid = NULL,
                                     cost_dir = NULL,
                                     qualkii_dir = NULL,
                                     qualos_dir = NULL,
                                     sample_size = 100000,
                                     is_test = FALSE,
                                     operational_reports = TRUE,
                                     is_pilot = FALSE) {

  #####################
  # Set TIMCI country #
  #####################

  is_tanzania <- Sys.getenv('TIMCI_COUNTRY') == 'Tanzania'
  is_india <- Sys.getenv('TIMCI_COUNTRY') == 'India'
  is_kenya <- Sys.getenv('TIMCI_COUNTRY') == 'Kenya'
  is_senegal <- Sys.getenv('TIMCI_COUNTRY') == 'Senegal'
  is_rct <- is_tanzania | is_india
  is_ls <- is_kenya | is_senegal

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

  filter <- NULL
  if (is_test) {
    odata_start_date <- lock_date - 30
    odata_end_date <- lock_date + 1
    filter <- paste0("__system/submissionDate ge ",
                     odata_start_date,
                     " and __system/submissionDate lt ",
                     odata_end_date)
  }

  if (!is_test) {

    ################
    # Set up ruODK #
    ################

    # Connection to ODK Central through ruODK
    odkc_project_list <- timci::set_odkc_connection()

    # Environment variables which may be shared between different projects ---------------------------
    wd_fid <- Sys.getenv("TIMCI_WD_FID")
    problem_fid <- Sys.getenv("TIMCI_PROBLEM_FID")

    # RCT / LS environment variables ---------------------------
    crf_facility_fid <- Sys.getenv("TIMCI_CRF_FACILITY_FID")
    crf_day7_fid <- Sys.getenv("TIMCI_CRF_DAY7_FID")
    crf_hospit_fid <- Sys.getenv("TIMCI_CRF_HOSPIT_FID")
    if (is_rct) {
      crf_day28_fid <- Sys.getenv("TIMCI_CRF_DAY28_FID")
    }
    crf_wfa_fid <- Sys.getenv("TIMCI_WEEKLY_FA_FID")

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
                                                 filter = filter,
                                                 delfields = FALSE,
                                                 group = TRUE,
                                                 split = FALSE,
                                                 media = FALSE)

    # Dirty fix - To be moved when time allows
    col_specs <- list(
      'a3-a3_a_5' = col_integer(),
      'crfs-t09a1-t08a-f2_9' = col_integer(),
      'crfs-t09a1-t08a-f2_8' = col_integer(),
      'crfs-t09a1-t06a-d2_3b' = col_double(),
      'crfs-t09a1-t06a-d2_2b' = col_double(),
      'crfs-t09a1-t05b-c3_6' = col_integer(),
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
    if (is_tanzania) {
      facility_data <- timci::process_tanzania_facility_data(raw_facility_data)
    } else{
      facility_data <- timci::process_facility_data(raw_facility_data)
    }

    # Copy audit trail in folder: only enabled if MEDIA = TRUE when downloading the initial *.zip
    facility_data_audit <- timci::extract_additional_data_from_odk_zip(odk_zip = raw_facility_zip,
                                                                       csv_name = paste0(crf_facility_fid, " - audit.csv"),
                                                                       local_dir = t)

    rm(raw_facility_data)

    # Load day 7 follow-up data
    write(formats2h3("Load day 7 follow-up data"), stderr())

    day7_col_specs <- list(
      'a1-enroldate' = col_date(),
      'o1-o1_2' = col_date(),
      'o1-o1_2a' = col_character(),
      'n1-o3_1a' = col_character()
    )

    raw_day7fu_data <- extract_data_from_odk_server(cpid = rctls_pid,
                                                    cpid_forms = rct_ls_form_list,
                                                    cpp = rctls_pp,
                                                    cfid = crf_day7_fid,
                                                    start_date = start_date,
                                                    end_date = day7fu_end_date,
                                                    col_specs = day7_col_specs,
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
    if (is_rct) {
      write(formats2h3("Load day 28 follow-up data"), stderr())

      day28_col_specs <- list(
        'a1-enroldate' = col_date(),
        'o1-o1_2' = col_date(),
        'o1-o1_2a' = col_character(),
        'n1-o3_1a' = col_character()
      )

      if (crf_day28_fid %in% rct_ls_form_list) {
        raw_day28fu_data <- extract_data_from_odk_server(cpid = rctls_pid,
                                                         cpid_forms = rct_ls_form_list,
                                                         cpp = rctls_pp,
                                                         cfid = crf_day28_fid,
                                                         start_date = start_date,
                                                         end_date = day28fu_end_date,
                                                         col_specs = day28_col_specs,
                                                         verbose = TRUE)
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
    raw_problem_data <- NULL
    # raw_problem_data <- extract_data_from_odk_server(cpid = rctls_pid,
    #                                                  cpid_forms = rct_ls_form_list,
    #                                                  cpp = rctls_pp,
    #                                                  cfid = problem_fid,
    #                                                  start_date = start_date,
    #                                                  end_date = end_date,
    #                                                  verbose = TRUE)

    # Load SPA data ---------------------------

    write(formats2h2("Load TIMCI SPA data"), stderr())

    spapmtf_dfs <- timci::load_spapmtf_data(odkc_project_list = odkc_project_list,
                                            spa_pid = spa_pid,
                                            spa_start_date = spa_start_date,
                                            spa_end_date = end_date)
    spa_cgei_data <- spapmtf_dfs[[1]]
    spa_fa_data <- spapmtf_dfs[[2]]
    spa_hcpi_data <- spapmtf_dfs[[3]]
    spa_sco_data <- spapmtf_dfs[[4]]
    tf_data <- spapmtf_dfs[[5]]
    pm_data <- spapmtf_dfs[[6]]

    # If SPA, process mapping and tine-flow data have been split between two different ODK projects, additionally extract data from the 2nd project and combine both datasets in a single dataset.
    if (!is.null(spa_pid2)) {

      write(formats2h3("SPA, process mapping and time-flow data split in 2 ODK projects"), stderr())
      write("Now loading data from the 2nd project", stderr())

      spapmtf_dfs2 <- timci::load_spapmtf_data(odkc_project_list = odkc_project_list,
                                              spa_pid = spa_pid2,
                                              spa_start_date = spa_start_date,
                                              spa_end_date = end_date)

      spa_cgei_data2 <- spapmtf_dfs2[[1]]
      spa_fa_data2 <- spapmtf_dfs2[[2]]
      spa_hcpi_data2 <- spapmtf_dfs2[[3]]
      spa_sco_data2 <- spapmtf_dfs2[[4]]
      tf_data2 <- spapmtf_dfs2[[5]]
      pm_data2 <- spapmtf_dfs2[[6]]

      write("Combine SPA CGEI data", stderr())
      spa_cgei_data <- timci::combine_dataframes(df1 = spa_cgei_data,
                                                 df2 = spa_cgei_data2,
                                                 verbose = TRUE)

      write("Combine SPA FA data", stderr())
      spa_fa_data <- timci::combine_dataframes(df1 = spa_fa_data,
                                               df2 = spa_fa_data2,
                                               verbose = TRUE)

      write("Combine SPA HCPI data", stderr())
      spa_hcpi_data <- timci::combine_dataframes(df1 = spa_hcpi_data,
                                                 df2 = spa_hcpi_data2,
                                                 verbose = TRUE)

      write("Combine SPA sick child observation data", stderr())
      spa_sco_data <- timci::combine_dataframes(df1 = spa_sco_data,
                                                df2 = spa_sco_data2,
                                                verbose = TRUE)

      write("Combine PM & TF data", stderr())
      tf_data <- lapply(seq_along(tf_data),
                        function(x) timci::combine_dataframes(df1 = tf_data[[x]],
                                                              df2 = tf_data2[[x]],
                                                              verbose = FALSE))


      pm_data <- lapply(seq_along(pm_data),
                        function(x) timci::combine_dataframes(df1 = pm_data[[x]],
                                                              df2 = pm_data2[[x]],
                                                              verbose = FALSE))

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

    qual_dfs <- timci::load_qual_data(odkc_project_list = odkc_project_list,
                                      qpid = qpid,
                                      qual_pp = qual_pp,
                                      qual_start_date = start_date,
                                      qual_end_date = end_date)

    cgidi_interview_data <- qual_dfs[[1]]
    hcpidi_interview_data <- qual_dfs[[2]]
    kii_interview_data <- qual_dfs[[3]]
    online_survey_data <- qual_dfs[[4]]
    cgidi_invitation_data <- qual_dfs[[5]]
    cgidi_encryption_data <- qual_dfs[[6]]

    #############
    # Save data #
    #############

    save(facility_data,
         facility_data_audit,
         raw_day7fu_data,
         raw_hospit_data,
         raw_day28fu_data,
         raw_withdrawal_data,
         raw_problem_data,
         spa_cgei_data,
         spa_fa_data,
         spa_hcpi_data,
         spa_sco_data,
         tf_data,
         pm_data,
         medical_cost_data,
         hospital_cost_data,
         cgidi_invitation_data,
         cgidi_encryption_data,
         cgidi_interview_data,
         hcpidi_interview_data,
         kii_interview_data,
         online_survey_data,
         file = "C:\\Users\\langhe\\Documents\\Tanzania\\Interim analysis\\timci_save_interim_analysis.rda")

  } else {
    load(file = "C:\\Users\\langhe\\Documents\\Tanzania\\Interim analysis\\timci_save_interim_analysis.rda")
  }

  ########################
  # RCT data lock report #
  ########################

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
                 qc_dir = qc_dir,
                 locked_db_dir = lock_dir,
                 facility_data = facility_data,
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
  generate_word_report(lock_dir, "database_export.Rmd", "timci_data_lock_report", params)

  if (operational_reports) {

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
    if (is_rct) {
      rname <- "timci_rct_monitoring_report"
    } else{
      rname <- "timci_ls_monitoring_report"
    }
    generate_pdf_report(report_dir, "rct_monitoring_report.Rmd", rname, params)

    #########################
    # SPA monitoring report #
    #########################

    write(formats2h1("Generate SPA report"), stderr())

    # Filter research facilities to only keep those in which SPA activities are conducted
    if (is_tanzania & !is_pilot) {
      spa_research_facilities <- research_facilities %>%
        filter(spa == 1 & pilot == 0)
    } else {
      spa_research_facilities <- research_facilities %>%
        filter(spa == 1)
    }
    spa_facility_ids <- spa_research_facilities[, c("facility_id")] %>%
      distinct()

    # Check that at least one SPA database is not empty
    spa_sco_is_not_null <- !is.null(spa_sco_data)
    spa_cgei_is_not_null <- !is.null(spa_cgei_data)
    spa_fa_is_not_null <- !is.null(spa_fa_data)
    spa_hcpi_is_not_null <- !is.null(spa_hcpi_data)
    spa_report_condition <- spa_sco_is_not_null | spa_cgei_is_not_null | spa_fa_is_not_null | spa_hcpi_is_not_null

    # Only keep data that corresponds to SPA facilities
    if (spa_sco_is_not_null) {
      spa_sco_data <- spa_sco_data %>%
        merge(y = spa_facility_ids,
              by.x = 'facility_identification-fcode',
              by.y = 'facility_id',
              all = FALSE)
    }

    if (spa_cgei_is_not_null) {
      spa_cgei_data <- spa_cgei_data %>%
        merge(y = spa_facility_ids,
              by.x = 'b1-fcode',
              by.y = 'facility_id',
              all = FALSE)
    }

    if (spa_fa_is_not_null) {
      spa_fa_data <- spa_fa_data %>%
        merge(y = spa_facility_ids,
              by.x = 'facility_identification-a1_2',
              by.y = 'facility_id',
              all = FALSE)
    }

    if (spa_hcpi_is_not_null) {
      spa_hcpi_data <- spa_hcpi_data %>%
        merge(y = spa_facility_ids,
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

    # Select relevant research facilities to only keep those in which process mapping & time-flow activities are conducted
    pmtf_research_facilities <- spa_research_facilities
    pmtf_facility_ids <- pmtf_research_facilities[, c("facility_id")] %>%
      distinct()

    # Check that at least one PMTF database is not empty
    pm_is_not_null <- !is.null(pm_data)
    if (pm_is_not_null) {
      pm_is_not_null <- length(pm_data) > 0
      if (pm_is_not_null) {
        pm_is_not_null <- !is.null(pm_data[[1]])
        if (pm_is_not_null) {
          pm_is_not_null <- length(pm_data[[1]]) > 0
        }
      }
    }
    tf_is_not_null <- !is.null(tf_data)
    if (tf_is_not_null) {
      tf_is_not_null <- length(tf_data) > 0
      if (tf_is_not_null) {
        tf_is_not_null <- !is.null(tf_data[[1]])
        if (tf_is_not_null) {
          tf_is_not_null <- length(tf_data[[1]]) > 0
        }
      }
    }
    pmtf_report_condition <- pm_is_not_null | tf_is_not_null

    # Only keep data that corresponds to relevant facilities
    pm_df <- NULL
    if (pm_is_not_null) {
      pm_df <- pm_data[[1]] %>%
        merge(y = pmtf_facility_ids,
              by.x = 'visit_start-fcode',
              by.y = 'facility_id',
              all = FALSE)
    }

    tf_df <- NULL
    if (tf_is_not_null) {
      tf_df <- tf_data[[1]] %>%
        merge(y = pmtf_facility_ids,
              by.x = 'visit_start-fcode',
              by.y = 'facility_id',
              all = FALSE)
    }

    if (pmtf_report_condition) {
      if (length(tf_df) > 0 | length(pm_df) > 0) {
        params <- list(research_facilities = pmtf_research_facilities,
                       facility_data = facility_data,
                       tf_data = tf_df,
                       pm_data = pm_df,
                       raw_withdrawal_data = raw_withdrawal_data)
        generate_pdf_report(report_dir, "pmtf_monitoring_report.Rmd", "timci_processmap_timeflow_monitoring_report", params)
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
                       cgidi_interview_data = cgidi_interview_data,
                       hcpidi_interview_data = hcpidi_interview_data,
                       kii_interview_data = kii_interview_data,
                       online_survey_data = online_survey_data,
                       raw_withdrawal_data = raw_withdrawal_data)
        generate_pdf_report(report_dir, "qual_monitoring_report.Rmd", "timci_qual_monitoring_report", params)
      }
    }

  }

  ###########################
  # RCT data quality report #
  ###########################

  full_export <- FALSE
  if(full_export){

    write(formats2h1("Explore Day 0 data"), stderr())

    # Load locked data from *.Rda
    wdir <- getwd()
    setwd(system.file(file.path('rmarkdown'), package = 'timci'))
    load(file = "timci_locked_data.rda")
    setwd(wdir)

    # Global data reports
    params <- list(locked_day0_data = locked_day0_data,
                   locked_allday7fu_data = locked_allday7fu_data,
                   locked_hospit_data = locked_hospit_data,
                   locked_spa_sco_data = locked_spa_sco_data,
                   locked_spa_hcpi_data = locked_spa_hcpi_data,
                   spa_cgei_data = spa_cgei_data,
                   spa_fa_data = spa_fa_data)
    if (is_rct) {
      params <- append(params,
                       list(locked_allday28fu_data = locked_allday28fu_data))
    }
    # Aggregated data report
    generate_word_report(report_dir,
                         "database_summary.Rmd",
                         "timci_data_summary_report",
                         params)

    # Data report disaggregated by facility
    generate_word_report(report_dir,
                         "database_summary_by_facility.Rmd",
                         "timci_data_by_facility_summary_report",
                         params)

    # Young infant data report
    generate_word_report(report_dir,
                         "database_summary_yi.Rmd",
                         "timci_young_infant_data_summary_report",
                         params)

    # Detailed data reports
    library(dataMaid)
    if (is_kenya | is_india) {
      dataMaid::makeDataReport(locked_day0_data,
                               file = file.path(report_dir, paste0("timci_rctls_day0_data_summary_report",'_',Sys.Date(),".Rmd")),
                               reportTitle = "TIMCI RCT/LS - Locked Day 0 dataset",
                               openResult = FALSE,
                               replace = TRUE)
      dataMaid::makeDataReport(locked_spa_sco_data,
                               file = file.path(report_dir, paste0("timci_spa_sco_data_summary_report", '_',Sys.Date(),".Rmd")),
                               reportTitle = "TIMCI SPA - Locked sick child observation dataset",
                               openResult = FALSE,
                               replace = TRUE)
      dataMaid::makeDataReport(spa_cgei_data,
                               file = file.path(report_dir, paste0("timci_spa_cgei_data_summary_report", '_',Sys.Date(),".Rmd")),
                               reportTitle = "TIMCI SPA - Caregiver exit interview dataset",
                               openResult = FALSE,
                               replace = TRUE)
      dataMaid::makeDataReport(locked_spa_hcpi_data,
                               file = file.path(report_dir, paste0("timci_spa_hcpi_data_summary_report", '_',Sys.Date(),".Rmd")),
                               reportTitle = "TIMCI SPA - Healthcare provider interview dataset",
                               openResult = FALSE,
                               replace = TRUE)
    }

    rm(locked_day0_data)
    gc() # Garbage collection

    ########
    # Maps #
    ########

    write(formats2h1("Generate map report"), stderr())

    params <- list(research_facilities = research_facilities,
                   start_date = start_date,
                   end_date = end_date,
                   facility_data = facility_data,
                   raw_day7fu_data = raw_day7fu_data,
                   raw_hospit_data = raw_hospit_data,
                   raw_day28fu_data = raw_day28fu_data,
                   raw_withdrawal_data = raw_withdrawal_data,
                   wfa_data = wfa_data)
    if (is_rct) {
      rname <- "timci_rct_map_report"
    } else{
      rname <- "timci_ls_map_report"
    }
    generate_pdf_report(report_dir, "generate_maps.Rmd", rname, params)

    gc() # Garbage collection

  }

}

#' Run Rmarkdown files
#'
#' This function runs several Rmarkdown files to generate standardised automated reports for the longitudinal study (LS) and the randomised controlled trial (RCT) for the Tools for Integrated Management of Childhood Illnesses (TIMCI) project.
#'
#' @param rctls_pid Numeric value that refers ID of the RCT/LS ODK Central project
#' @param rctls_pp Passphrase
#' @param research_facilities Dataframe that contains the research facilities
#' @param report_dir Path to the output folder for the generated Rmarkdown reports
#' @param participant_zip Path to the encrypted zip archive that stores participant data
#' @param mdb_dir Path to the output folder for the RCT / LS database exports
#' @param path_dir Path to the output folder for the M&E exports to be shared with PATH
#' @param start_date RCT/LS data collection start date (optional)
#' @param end_date RCT/LS data collection end date (optional)
#' @param lock_date RCT/LS data collection cleaning end date for database lock (optional)
#' @param sample_size Numeric value, sample size for RCT/LS enrolment
#' @param short Short version of the export
#' @import rmarkdown ruODK
#' @export

run_rmarkdown_rctls <- function(rctls_pid,
                                rctls_pp,
                                research_facilities,
                                report_dir,
                                participant_zip,
                                mdb_dir,
                                path_dir,
                                start_date = NULL,
                                end_date = NULL,
                                lock_date = NULL,
                                sample_size = 100000,
                                short = FALSE) {

  #####################
  # Set TIMCI country #
  #####################

  is_tanzania <- Sys.getenv('TIMCI_COUNTRY') == 'Tanzania'
  is_india <- Sys.getenv('TIMCI_COUNTRY') == 'India'
  is_kenya <- Sys.getenv('TIMCI_COUNTRY') == 'Kenya'
  is_senegal <- Sys.getenv('TIMCI_COUNTRY') == 'Senegal'
  is_rct <- is_tanzania | is_india
  is_ls <- is_kenya | is_senegal

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

  ################
  # Set up ruODK #
  ################

  # Connection to ODK Central through ruODK
  odkc_project_list <- timci::set_odkc_connection()

  # Environment variables which may be shared between different projects ---------------------------
  wd_fid <- Sys.getenv("TIMCI_WD_FID")
  problem_fid <- Sys.getenv("TIMCI_PROBLEM_FID")

  # RCT / LS environment variables ---------------------------
  crf_facility_fid <- Sys.getenv("TIMCI_CRF_FACILITY_FID")
  crf_day7_fid <- Sys.getenv("TIMCI_CRF_DAY7_FID")
  crf_hospit_fid <- Sys.getenv("TIMCI_CRF_HOSPIT_FID")
  if (is_rct) {
    crf_day28_fid <- Sys.getenv("TIMCI_CRF_DAY28_FID")
  }
  crf_wfa_fid <- Sys.getenv("TIMCI_WEEKLY_FA_FID")

  #######################
  # Load TIMCI ODK data #
  #######################

  write(formats2h1("Load TIMCI ODK data"), stderr())

  # List RCT/LS forms ---------------------------
  rct_ls_form_list <- ruODK::form_list(pid = rctls_pid)$fid

  # Load facility data ---------------------------
  write(formats2h2("Load RCT/LS facility data"), stderr())

  write(formats2h3("Load day 0 data"), stderr())

  raw_facility_zip <- ruODK::submission_export(local_dir = tempdir(),
                                               pid = rctls_pid,
                                               fid = crf_facility_fid,
                                               pp = rctls_pp,
                                               delfields = FALSE,
                                               group = TRUE,
                                               split = FALSE,
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
  if (is_tanzania) {
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
  if (is_rct) {
    write(formats2h3("Load day 28 follow-up data"), stderr())
    if (crf_day28_fid %in% rct_ls_form_list) {
      raw_day28fu_zip <- ruODK::submission_export(local_dir = tempdir(),
                                                  pid = rctls_pid,
                                                  fid = crf_day28_fid,
                                                  pp = rctls_pp,
                                                  delfields = FALSE,
                                                  group = TRUE,
                                                  split = FALSE,
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

  ###########################
  # RCT data quality report #
  ###########################

  write(formats2h1("Export data and generate data quality report"), stderr())

  params <- list(rctls_dir = mdb_dir,
                 research_facilities = research_facilities,
                 participant_zip = participant_zip,
                 facility_data = facility_data,
                 facility_data_audit = facility_data_audit,
                 raw_day7fu_data = raw_day7fu_data,
                 raw_hospit_data = raw_hospit_data,
                 raw_day28fu_data = raw_day28fu_data,
                 raw_withdrawal_data = raw_withdrawal_data,
                 raw_problem_data = raw_problem_data,
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
  if (is_rct) {
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

}

#' Run Rmarkdown files
#'
#' This function runs several Rmarkdown files to generate standardised automated reports for the Tools for Integrated Management of Childhood Illnesses (TIMCI) project.
#'
#' @param cost_pid Numeric ID of the cost and cost-effectiveness ODK Central project
#' @param research_facilities Dataframe that contains the research facilities
#' @param report_dir Path to the output folder for the generated Rmarkdown reports
#' @param cost_dir Path to the output folder for the cost and cost effectiveness database exports (optional)
#' @param cost_start_date Cost data collection start date (optional)
#' @param cost_end_date Cost data collection end date (optional)
#' @param cost_lock_date Cost data collection cleaning end date for database lock (optional)
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
  odkc_project_list <- timci::set_odkc_connection()

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

#' Load qualitative data and generate corresponding report
#'
#' This function loads qualitative data and run the qualitative Rmarkdown files to generate standardised automated reports for the qualitative studies in the Tools for Integrated Management of Childhood Illnesses (TIMCI) project.
#'
#' @param qpid Numeric ID of the qualitative ODK Central project
#' @param qual_pp Encryption passphrase associated with the qualitative ODK Central project
#' @param research_facilities Dataframe that contains the research facilities
#' @param report_dir Path to the output folder for the generated Rmarkdown reports
#' @param qual_start_date Qualitative data collection start date (optional)
#' @param qual_end_date Qualitative data collection end date (optional)
#' @export

load_qual_data_and_generate_report <- function(qpid,
                                               qual_pp,
                                               research_facilities,
                                               report_dir,
                                               qual_start_date = NULL,
                                               qual_end_date = NULL) {

  ###########################
  # Set up current language #
  ###########################

  timci::set_language_settings()

  ######################
  # Set up study dates #
  ######################

  if (is.null(qual_end_date)) {
    qual_end_date <- Sys.Date()
  } else{
    qual_end_date <- as.Date(qual_end_date, "%Y-%m-%d")
  }

  ################
  # Set up ruODK #
  ################

  # Connection to ODK Central through ruODK
  odkc_project_list <- timci::set_odkc_connection()

  #######################
  # Load TIMCI ODK data #
  #######################

  write(formats2h1("Load TIMCI qualitative data"), stderr())

  qual_dfs <- timci::load_qual_data(odkc_project_list = odkc_project_list,
                                    qpid = qpid,
                                    qual_pp = qual_pp,
                                    qual_start_date = qual_start_date,
                                    qual_end_date = qual_end_date)

  cgidi_interview_data <- qual_dfs[[1]]
  hcpidi_interview_data <- qual_dfs[[2]]
  kii_interview_data <- qual_dfs[[3]]
  online_survey_data <- qual_dfs[[4]]
  cgidi_invitation_data <- qual_dfs[[5]]
  cgidi_encryption_data <- qual_dfs[[6]]

  #################################
  # Qualitative monitoring report #
  #################################

  write(formats2h1("Generate qualitative report"), stderr())

  if (!is.null(hcpidi_interview_data)) {
    if (length(hcpidi_interview_data) > 0) {
      params <- list(research_facilities = research_facilities,
                     facility_data = NULL,
                     cgidi_interview_data = cgidi_interview_data,
                     hcpidi_interview_data = hcpidi_interview_data,
                     kii_interview_data = kii_interview_data,
                     online_survey_data = online_survey_data,
                     raw_withdrawal_data = NULL)
      generate_pdf_report(report_dir, "qual_monitoring_report.Rmd", "timci_qual_monitoring_report", params)
    }
  }

}
