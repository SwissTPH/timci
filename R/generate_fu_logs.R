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

  fu7all <- timci::generate_fu_log(pii, raw_day7fu_data, 0, 12, 7, 10, ext = TRUE, deidentify = FALSE)
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
                 facility_data = facility_data,
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

  hospitfu_dir <- file.path(fu_dir, "hospitalisation_log")
  dir.create(hospitfu_dir, showWarnings = FALSE)

  params <- list(output_dir = fu_dir,
                 rct_ls_form_list = rct_ls_form_list,
                 pii = pii,
                 day0_data = facility_data,
                 rctls_pid = rctls_pid,
                 raw_day7fu_data = raw_day7fu_data,
                 raw_hospit_data = raw_hospit_data,
                 raw_withdrawal_data = raw_withdrawal_data,
                 fu_end = 12)
  generate_word_report(hospitfu_dir, "hospit_fu_log.Rmd", "timci_hospit_fu_log", params)

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
                   facility_data = facility_data,
                   rctls_pid = rctls_pid,
                   fu_fid = crf_day28_fid,
                   raw_fu_data = raw_day28fu_data,
                   raw_oth_fu_data = raw_day7fu_data,
                   raw_withdrawal_data = raw_withdrawal_data,
                   fu_start = 27,
                   fu_end = 35,
                   fu_vstart = 28,
                   fu_vend = 32)
    generate_pdf_report(day28fu_dir, "fu_daily_log.Rmd", "01_timci_day28_fu_daily_log", params)

  }

}
