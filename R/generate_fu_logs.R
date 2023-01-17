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
#' @param for_today boolean that enables to generate the follow-up for today (defaut set to `FALSE`)
#' @param hospitfu_ok boolean that enables to generate the hospital follow-up for today (defaut set to `TRUE`)
#' @param is_pilot Boolean, default set to `FALSE`
#' @import rmarkdown ruODK
#' @export

generate_fu_logs <- function(rctls_pid,
                             rctls_pp,
                             research_facilities,
                             fu_dir,
                             start_date = NULL,
                             end_date = NULL,
                             for_today = FALSE,
                             hospitfu_ok = TRUE,
                             is_pilot = FALSE) {

  ###########################
  # Set up current language #
  ###########################

  timci::set_language_settings()

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

  thres_date <- Sys.Date() - 45
  filter <- paste0("__system/submissionDate ge ", thres_date)

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
                                               filter = filter,
                                               delfields = FALSE,
                                               group = TRUE,
                                               split = FALSE,
                                               media = FALSE)
  raw_facility_data <- timci::extract_data_from_odk_zip(raw_facility_zip,
                                                        paste0(crf_facility_fid,".csv"),
                                                        start_date,
                                                        end_date)
  write("Generate follow-up logs", stderr())


  if (Sys.getenv('TIMCI_COUNTRY') == 'Tanzania') {
    facility_data <- timci::process_tanzania_facility_data(raw_facility_data, is_pilot)
    facility_data <- timci::correct_day0_all(facility_data)
  } else if (Sys.getenv('TIMCI_COUNTRY') == 'Kenya') {
    facility_data <- timci::process_facility_data(raw_facility_data, is_pilot)
    facility_data <- timci::correct_day0_all(facility_data)
  } else {
    facility_data <- timci::process_facility_data(raw_facility_data, is_pilot)
    facility_data <- timci::correct_day0_all(facility_data)
  }

  pii <- timci::extract_enrolled_participants(facility_data, is_pilot)[[2]]

  # Load day 7 follow-up data
  print("Load day 7 follow-up data")
  raw_day7fu_data <- NULL
  if (crf_day7_fid %in% rct_ls_form_list) {
    raw_day7fu_zip <- ruODK::submission_export(local_dir = tempdir(),
                                               pid = rctls_pid,
                                               fid = crf_day7_fid,
                                               pp = rctls_pp,
                                               filter = filter,
                                               delfields = FALSE,
                                               group = TRUE,
                                               split = FALSE,
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
                                               delfields = FALSE,
                                               group = TRUE,
                                               split = FALSE,
                                               media = FALSE)
    raw_hospit_data <- timci::extract_data_from_odk_zip(raw_hospit_zip,
                                                        paste0(crf_hospit_fid,".csv"),
                                                        start_date,
                                                        end_date)
  }

  # Load day 28 follow-up data
  raw_day28fu_data <- NULL
  if (Sys.getenv('TIMCI_COUNTRY') == "Tanzania" || Sys.getenv('TIMCI_COUNTRY') == "India") {
    write("Load day 28 follow-up data", stderr())
    if (crf_day28_fid %in% rct_ls_form_list) {
      raw_day28fu_zip <- ruODK::submission_export(local_dir = tempdir(),
                                                  pid = rctls_pid,
                                                  fid = crf_day28_fid,
                                                  pp = rctls_pp,
                                                  filter = filter,
                                                  delfields = FALSE,
                                                  group = TRUE,
                                                  split = FALSE,
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
                                                   delfields = FALSE,
                                                   group = TRUE,
                                                   split = FALSE,
                                                   media = FALSE)
    raw_withdrawal_data <- timci::extract_data_from_odk_zip(raw_withdrawal_zip,
                                                            paste0(wd_fid,".csv"),
                                                            start_date,
                                                            end_date)
  }

  #######################
  # Day 7 follow-up log #
  #######################

  write("Generate the Day 7 follow-up log", stderr())

  day7fu_dir <- file.path(fu_dir, "day7_log")
  dir.create(day7fu_dir, showWarnings = FALSE)

  fu7all <- timci::generate_fu_log(pii, raw_day7fu_data, 0, 12, 7, 10, ext = TRUE, deidentify = FALSE)
  timci::export_df2xlsx(fu7all, day7fu_dir, "02_timci_day7_fu_weekly_log_all")

  # Daily log
  fu_start <- 6
  physicalfu_start <- 8
  physicalfu_end <- 14

  if (Sys.getenv('TIMCI_COUNTRY') == "Kenya") {
    fu_end <- 15
  } else {
    fu_end <- 12
  }

  if (for_today) {
    fu_start <- fu_start + 1
    physicalfu_start <- physicalfu_start + 1
    fu_end <- fu_end + 1
    physicalfu_end <- physicalfu_end + 1
  }

  params <- list(output_dir = fu_dir,
                 rct_ls_form_list = rct_ls_form_list,
                 facility_data = facility_data,
                 rctls_pid = rctls_pid,
                 fu_fid = crf_day7_fid,
                 raw_fu_data = raw_day7fu_data,
                 raw_withdrawal_data = raw_withdrawal_data,
                 fu_start = fu_start,
                 fu_end = fu_end,
                 fu_vstart = 7,
                 fu_vend = 10)

  if (Sys.getenv('TIMCI_COUNTRY') == "Tanzania") {
    physical_params <- list(physicalfu_start = physicalfu_start,
                            physicalfu_end = physicalfu_end)
    params <- append(params, physical_params)
  }

  generate_pdf_report(day7fu_dir, "fu_daily_log.Rmd", "01_timci_day7_fu_daily_log", params)

  # # Weekly log
  # for (i in 1:nrow(research_facilities)) {
  #   fid <- research_facilities[[i, 'facility_id']]
  #   fname <- research_facilities[[i, 'facility_label']]
  #   params <- list(pii = pii,
  #                  fu_fid = crf_day7_fid,
  #                  raw_fu_data = raw_day7fu_data,
  #                  raw_withdrawal_data = raw_withdrawal_data,
  #                  facility_id = fid,
  #                  facility_label = fname,
  #                  fu_start = 0,
  #                  fu_end = 12,
  #                  fu_vstart = 7,
  #                  fu_vend = 10)
  #   generate_word_report(day7fu_dir, "fu_weekly_log.Rmd", paste0(fid, "_", fname, "_timci_day7_fu_weekly_log"), params)
  # }

  #################################
  # Hospitalisation follow-up log #
  #################################

  if (hospitfu_ok){
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
  }

  #######################
  # Day 28 follow-up log #
  #######################

  # Day 28 follow-up log is only generated for India and Tanzania

  if (Sys.getenv('TIMCI_COUNTRY') == "Tanzania" || Sys.getenv('TIMCI_COUNTRY') == "India") {
    day28fu_dir <- file.path(fu_dir, "day28_log")
    dir.create(day28fu_dir, showWarnings = FALSE)

    fu28all <- timci::generate_fu_log(pii, raw_day28fu_data, 0, 32, 28, 32)
    timci::export_df2xlsx(fu28all, day28fu_dir, "02_timci_day28_fu_weekly_log_all")

    # Daily log
    fu_start <- 27
    physicalfu_start <- 29
    fu_end <- 35
    physicalfu_end <- 40

    if (for_today) {
      fu_start <- fu_start + 1
      physicalfu_start <- physicalfu_start + 1
      fu_end <- fu_end + 1
      physicalfu_end <- physicalfu_end + 1
    }

    params <- list(output_dir = fu_dir,
                   rct_ls_form_list = rct_ls_form_list,
                   facility_data = facility_data,
                   rctls_pid = rctls_pid,
                   fu_fid = crf_day28_fid,
                   raw_fu_data = raw_day28fu_data,
                   raw_oth_fu_data = raw_day7fu_data,
                   raw_withdrawal_data = raw_withdrawal_data,
                   fu_start = fu_start,
                   fu_end = fu_end,
                   fu_vstart = 28,
                   fu_vend = 32)

    if (Sys.getenv('TIMCI_COUNTRY') == "Tanzania") {
      physical_params <- list(physicalfu_start = physicalfu_start,
                              physicalfu_end = physicalfu_end)
      params <- append(params, physical_params)
    }

    generate_pdf_report(day28fu_dir, "fu_daily_log.Rmd", "01_timci_day28_fu_daily_log", params)

    # # Weekly log
    # for (i in 1:nrow(research_facilities)) {
    #   fid <- research_facilities[[i, 'facility_id']]
    #   fname <- research_facilities[[i, 'facility_label']]
    #   params <- list(pii = pii,
    #                  fu_fid = crf_day28_fid,
    #                  raw_fu_data = raw_day28fu_data,
    #                  raw_withdrawal_data = raw_withdrawal_data,
    #                  facility_id = fid,
    #                  facility_label = fname,
    #                  fu_start = 21,
    #                  fu_end = 35,
    #                  fu_vstart = 28,
    #                  fu_vend = 32)
    #   generate_word_report(day28fu_dir, "fu_weekly_log.Rmd", paste0(fid, "_", fname, "_timci_day28_fu_weekly_log"), params)
    # }

  }

}

#' Generate follow-up log (TIMCI-specific function)
#'
#' Generate a list of participants to be called in a time window after baseline between wmin and wmax
#' @param pii dataframe containing personally identifiable data
#' @param fudf dataframe containing the processed follow-up data
#' @param wmin numerical, start of the follow-up period (in days)
#' @param wmax numerical, end of the follow-up period (in days)
#' @param vwmin numerical, start of the follow-up period valid for the statistical analysis (in days)
#' @param vwmax numerical, end of the follow-up period valid for the statistical analysis (in days)
#' @param ext boolean, allow to export more information
#' @param deidentify boolean, enable the generation of a format with personally identifiable information (PII) for upload on ODK Central (if set to FALSE) or of a deidentified dataframe (if set to TRUE)
#' @return This function returns a dataframe.
#' @export
#' @import magrittr dplyr

generate_fu_log <- function(pii,
                            fudf,
                            wmin,
                            wmax,
                            vwmin,
                            vwmax,
                            ext = FALSE,
                            deidentify = FALSE) {

  fu_log <- pii
  fu_log$min_date <- as.Date(fu_log$date_visit) + wmin
  fu_log$max_date <- as.Date(fu_log$date_visit) + wmax
  if(Sys.getenv('TIMCI_COUNTRY') != 'Tanzania'){
    fu_log$label <- paste(fu_log$fs_name, fu_log$ls_name)
  } else{
    fu_log$label <- paste(fu_log$fs_name, fu_log$ms_name, fu_log$ls_name)
  }
  fu_log$caregiver <- paste(fu_log$cg_fs_name, fu_log$cg_ls_name)
  fu_log$mother <- paste(fu_log$mother_fs_name, fu_log$mother_ls_name)
  fu_log$sex <- ifelse(fu_log$sex == 1, "male", ifelse(fu_log$sex == 2, "female", "other"))

  # Exclude children who already underwent successful follow-up
  if (!is.null(fudf)) {
    if (nrow(fudf) > 0) {
      fudf <- fudf %>% dplyr::filter(proceed == 1)
      fu_log <- fu_log[!(fu_log$child_id %in% fudf$"a1-pid"),]
    }
  }

  # Exclude children who are outside of the follow-up window period
  fu_log <- fu_log[fu_log$min_date <= Sys.Date() & fu_log$max_date >= Sys.Date(),]

  # Order columns
  if (deidentify) {
    col_order <- c('fid',
                   'child_id',
                   'sex',
                   'date_visit',
                   'main_cg_lbl')
  } else{
    col_order <- c('fid',
                   'child_id',
                   'label',
                   'sex',
                   'date_visit',
                   'caregiver',
                   'main_cg_lbl',
                   'mother',
                   'phone_nb',
                   'phone_nb2',
                   'location',
                   'cmty_contact')
    if (Sys.getenv('TIMCI_COUNTRY') == "Tanzania") {
      col_order <- c(col_order, 'physical_fu_instructions')
    }
  }
  # If ext is set to TRUE, also add the district and facility name
  if (ext) {
    col_order <- c('district',
                   'facility',
                   col_order)
  }
  # Select only a subset of columns
  fu_log <- fu_log[, col_order]

  # Order entries by date
  fu_log <- fu_log %>%
    dplyr::arrange(fid, date_visit = as.Date(date_visit, "%Y-%m-%d"))

  # Add valid window in export
  if ( is_not_empty(fu_log) ) {
    fu_log$date_visit <- paste0("From ", as.Date(fu_log$'date_visit', "%Y-%m-%d") + vwmin, " to ", as.Date(fu_log$'date_visit', "%Y-%m-%d") + vwmax, " [enrolled on ", fu_log$date_visit, "]")
  }

  # Add a first generic row

  # If ext is set to TRUE, do not add
  if (!ext & !deidentify) {
    first_row <- data.frame('fid' = 'F0000',
                            'child_id' = 'X-F0000-P0000',
                            'label' = 'CHILD NAME',
                            'sex' = 'SEX',
                            'date_visit' = 'VALID WINDOW [ENROLMENT DATE]',
                            'caregiver' = 'CAREGIVER NAME',
                            'main_cg_lbl' = 'RELATIONSHIP',
                            'mother' = 'MOTHER NAME',
                            'phone_nb' = 'PHONE NB 1',
                            'phone_nb2' = 'PHONE NB 2',
                            'location' = 'LOCATION',
                            'cmty_contact' = 'CONTACT')
    if (Sys.getenv('TIMCI_COUNTRY') == "Tanzania") {
      first_row$physical_fu_instructions <- 'INSTRUCTIONS'
    }
    fu_log <- rbind(first_row,
                    fu_log)
  }

  if (!deidentify) {
    fu_log <- fu_log %>% dplyr::rename('name' = 'child_id',
                                       'enroldate' = 'date_visit',
                                       'relationship' = 'main_cg_lbl',
                                       'phonenb1' = 'phone_nb',
                                       'phonenb2' = 'phone_nb2',
                                       'contact' = 'cmty_contact')
  }

  fu_log

}

#' Generate follow-up log CSV for upload on ODK Central (TIMCI-specific function)
#'
#' Generate a list of participants to be called in a time window after baseline between wmin and wmax
#' @param pii dataframe containing personally identifiable data
#' @param fudf dataframe containing the processed follow-up data
#' @param wmin numerical, start of the follow-up period (in days)
#' @param wmax numerical, end of the follow-up period (in days)
#' @param vwmin numerical, start of the follow-up period valid for the statistical analysis (in days)
#' @param vwmax numerical, end of the follow-up period valid for the statistical analysis (in days)
#' @return This function returns a dataframe.
#' @export
#' @import magrittr dplyr

generate_fu_log_csv <- function(pii,
                                fudf,
                                wmin,
                                wmax,
                                vwmin,
                                vwmax) {

  fu_log <- pii
  fu_log$min_date <- as.Date(fu_log$date_visit) + wmin
  fu_log$max_date <- as.Date(fu_log$date_visit) + wmax
  if(Sys.getenv('TIMCI_COUNTRY') != 'Tanzania'){
    fu_log$child_name <- paste(fu_log$fs_name, fu_log$ls_name)
    fu_log$label <- paste0(fu_log$child_name,
                           " (",
                           as.Date(fu_log$date_visit, "%Y-%m-%d") + vwmin,
                           " to ",
                           as.Date(fu_log$date_visit, "%Y-%m-%d") + vwmax,
                           " )")
  }
  else{
    fu_log$child_name <- paste(fu_log$fs_name, fu_log$ms_name, fu_log$ls_name)
    fu_log$label <- paste0(fu_log$child_name,
                           " (",
                           as.Date(fu_log$date_visit, "%Y-%m-%d") + vwmin,
                           " to ",
                           as.Date(fu_log$date_visit, "%Y-%m-%d") + vwmax,
                           " ) - ",
                           fu_log$ra_name)
  }
  fu_log$caregiver <- paste(fu_log$cg_fs_name, fu_log$cg_ls_name)
  fu_log$mother <- paste(fu_log$mother_fs_name, fu_log$mother_ls_name)
  fu_log$sex <- ifelse(fu_log$sex == 1, "male", ifelse(fu_log$sex == 2, "female", "other"))

  # Exclude children who already underwent successful follow-up
  if (!is.null(fudf)) {
    if (nrow(fudf) > 0) {
      fudf <- fudf %>% dplyr::filter(proceed == 1)
      fu_log <- fu_log[!(fu_log$child_id %in% fudf$"a1-pid"),]
    }
  }

  # Exclude children who are outside of the follow-up window period
  fu_log <- fu_log[fu_log$min_date <= Sys.Date() & fu_log$max_date >= Sys.Date(),]

  if (nrow(fu_log) > 0) {
    fu_log$population <- "all"
    fu_log$type <- "1"

    if(Sys.getenv('TIMCI_COUNTRY') != 'Tanzania'){
      fu_log$physical_fu_instructions <- ""
    }

    # Order columns
    col_order <- c('fid',
                   'device_id',
                   'child_id',
                   'child_name',
                   'sex',
                   'date_visit',
                   'caregiver',
                   'main_cg_lbl',
                   'mother',
                   'phone_nb',
                   'phone_nb2',
                   'location',
                   'physical_fu_instructions',
                   'cmty_contact',
                   'label',
                   'sys_submit_id',
                   'population',
                   'type')
    fu_log <- fu_log[, col_order]

    # Order entries by date
    fu_log <- fu_log %>%
      dplyr::arrange(fid, sys_submit_id, date_visit = as.Date(date_visit, "%Y-%m-%d"))

    # Add a first generic row
    fu_log <- rbind(data.frame('fid' = 'F0000',
                               'device_id' = 'none',
                               'child_id' = 'X-F0000-P0000',
                               'child_name' = 'CHILD NAME',
                               'sex' = 'SEX',
                               'date_visit' = 'ENROLMENT DATE',
                               'caregiver' = 'CAREGIVER NAME',
                               'main_cg_lbl' = 'RELATIONSHIP',
                               'mother' = 'MOTHER NAME',
                               'phone_nb' = 'PHONE NB 1',
                               'phone_nb2' = 'PHONE NB 2',
                               'location' = 'LOCATION',
                               'physical_fu_instructions' = 'INSTRUCTIONS',
                               'cmty_contact' = 'CONTACT',
                               'label' = 'CHILD NAME (VALID WINDOW)',
                               'sys_submit_id' = 'SUBMITTER',
                               'population' = 'POPULATION',
                               'type' = 'MANUAL'),
                    fu_log)

    fu_log <- fu_log %>% dplyr::rename('name' = 'child_id',
                                       'enroldate' = 'date_visit',
                                       'relationship' = 'main_cg_lbl',
                                       'phonenb1' = 'phone_nb',
                                       'phonenb2' = 'phone_nb2',
                                       'contact' = 'cmty_contact',
                                       'submitter' = 'sys_submit_id')

  }

  fu_log

}

#' Generate physical follow-up log CSV for upload on ODK Central (TIMCI-specific function, Tanzania only)
#'
#' Generate a list of participants to be called in a time window after baseline between wmin and wmax
#' @param pii dataframe containing personally identifiable data
#' @param fudf dataframe containing the processed follow-up data
#' @param wmin_nophone numerical, start of the follow-up period if the caregiver has no phone number (in days)
#' @param wmin_phone numerical, start of the follow-up period if the caregiver has a phone number (in days)
#' @param wmax numerical, end of the follow-up period (in days)
#' @param vwmin numerical, start of the follow-up period valid for the statistical analysis (in days)
#' @param vwmax numerical, end of the follow-up period valid for the statistical analysis (in days)
#' @return This function returns a dataframe.
#' @export
#' @import magrittr dplyr

generate_physical_fu_log_csv <- function(pii,
                                         fudf,
                                         wmin_nophone,
                                         wmin_phone,
                                         wmax,
                                         vwmin,
                                         vwmax) {

  fu_log <- pii
  fu_log$min_nophone_date <- as.Date(fu_log$date_visit) + wmin_nophone
  fu_log$min_phone_date <- as.Date(fu_log$date_visit) + wmin_phone
  fu_log$max_date <- as.Date(fu_log$date_visit) + wmax
  fu_log$child_name <- paste(fu_log$fs_name, fu_log$ms_name, fu_log$ls_name)
  fu_log$caregiver <- paste(fu_log$cg_fs_name, fu_log$cg_ls_name)
  fu_log$mother <- paste(fu_log$mother_fs_name, fu_log$mother_ls_name)
  fu_log$sex <- ifelse(fu_log$sex == 1, "male", ifelse(fu_log$sex == 2, "female", "other"))
  fu_log$label <- paste0(fu_log$child_name,
                         " (",
                         as.Date(fu_log$date_visit, "%Y-%m-%d") + vwmin,
                         " to ",
                         as.Date(fu_log$date_visit, "%Y-%m-%d") + vwmax,
                         " )")

  # Exclude children who already underwent successful follow-up
  if (!is.null(fudf)) {
    if (nrow(fudf) > 0) {
      fudf <- fudf %>% dplyr::filter(proceed == 1)
      fu_log <- fu_log[!(fu_log$child_id %in% fudf$"a1-pid"),]
    }
  }

  #########################################################################
  # Select children for whom the caregiver did not provide a phone number #
  #########################################################################

  fu_log_nophone <- fu_log %>%
    dplyr::filter(phone_nb_avail == 0) %>% # Include only children for whom the caregiver did not provide a phone number
    dplyr::filter(min_nophone_date <= Sys.Date() & max_date >= Sys.Date()) # Exclude children who are outside of the physical follow-up window period
    if (nrow(fu_log_nophone)){
      fu_log_nophone$population <- "no phone number available"
      fu_log_nophone$type <- "2"
    } else {
      fu_log_nophone$population <- character()
      fu_log_nophone$type <- character()
    }



  #######################################################################################################
  # Select children for whom the caregiver provided a phone number and agreed with a physical follow-up #
  #######################################################################################################

  # Add a constraints on having at least 1 recorded failed attempt

  fu_log_phone <- fu_log %>%
    dplyr::filter(phone_nb_avail == 1 & physical_fu == 1) %>% # Include only children for whom the caregiver provided a phone number and agreed with physical follow-up
    dplyr::filter(min_phone_date <= Sys.Date() & max_date >= Sys.Date()) # Exclude children who are outside of the physical follow-up window period
  if (nrow(fu_log_phone)) {
    fu_log_phone$population <- "phone number available"
    fu_log_phone$type <- "2"
  } else {
    fu_log_phone$population <- character()
    fu_log_phone$type <- character()
  }

  #########################################################
  # Combine both selections of children in one single log #
  #########################################################

  #if (!is.null(fu_log_nophone)) {
   # if (nrow(fu_log_nophone) > 0) {
    #  if (!is.null(fu_log_phone)) {
    #    fu_log <- dplyr::bind_rows(fu_log_nophone, fu_log_phone)
    #  } else{
     #   fu_log <- fu_log_nophone
     # }
   # }
  #}
  fu_log <- dplyr::bind_rows(fu_log_nophone,fu_log_phone)

  # Order columns
  col_order <- c('fid',
                 'device_id',
                 'child_id',
                 'child_name',
                 'sex',
                 'date_visit',
                 'caregiver',
                 'main_cg_lbl',
                 'mother',
                 'phone_nb',
                 'phone_nb2',
                 'location',
                 'location_lvl1',
                 'location_lvl2',
                 'location_lvl3',
                 'location_lvl4',
                 'physical_fu_instructions',
                 'cg_common_name',
                 'cmty_contact',
                 'label',
                 'sys_submit_id',
                 'population',
                 'type')
  fu_log <- fu_log[, col_order]

  # Order entries by date
  fu_log <- fu_log %>%
    dplyr::arrange(date_visit = as.Date(date_visit, "%Y-%m-%d"))

  # Add a first generic row
  fu_log <- rbind(data.frame('fid' = 'F0000',
                             'device_id' = 'none',
                             'child_id' = 'X-F0000-P0000',
                             'child_name' = 'CHILD NAME',
                             'sex' = 'SEX',
                             'date_visit' = 'ENROLMENT DATE',
                             'caregiver' = 'CAREGIVER NAME',
                             'main_cg_lbl' = 'RELATIONSHIP',
                             'mother' = 'MOTHER NAME',
                             'phone_nb' = 'PHONE NB 1',
                             'phone_nb2' = 'PHONE NB 2',
                             'location' = 'LOCATION',
                             'location_lvl1' = 'LOCATION_DIVISION',
                             'location_lvl2' = 'LOCATION_WARD',
                             'location_lvl3' = 'LOCATION_LVL3',
                             'location_lvl4' = 'LOCATION_LVL4',
                             'cmty_contact' = 'CONTACT',
                             'physical_fu_instructions' = 'INSTRUCTIONS',
                             'cg_common_name' = 'CAREGIVER COMMON NAME',
                             'label' = 'CHILD NAME (VALID WINDOW)',
                             'sys_submit_id' = 'SUBMITTER',
                             'population' = 'POPULATION',
                             'type' = 'MANUAL'),
                  fu_log)

  fu_log %>% dplyr::rename('name' = 'child_id',
                           'enroldate' = 'date_visit',
                           'relationship' = 'main_cg_lbl',
                           'phonenb1' = 'phone_nb',
                           'phonenb2' = 'phone_nb2',
                           'contact' = 'cmty_contact',
                           'instructions' = 'physical_fu_instructions',
                           'submitter' = 'sys_submit_id')

}

#' Generate lost to follow-up log (TIMCI-specific function)
#'
#' Generate a list of participants who are lost to follow-up after wmax
#' @param df dataframe
#' @param fudf dataframe containing the processed follow-up data
#' @param end_date numerical, end of the follow-up period (in days)
#' @param raw boolean flag for selecting raw (TRUE) or processed (FALSE) data
#' @return This function returns a dataframe.
#' @export
#' @import magrittr dplyr

generate_ltfu_log <- function(df,
                              fudf,
                              end_date,
                              raw = TRUE) {

  ltfu_log <- df
  ltfu_log$date_maxfu <- as.Date(ltfu_log$date_visit) + end_date

  # If raw data
  if (raw) {
    fudf <- fudf %>%
      dplyr::rename(child_id = "a1-pid",
                    proceed_day7 = proceed)
  }

  # Exclude children who already underwent successful follow-up
  if (!is.null(fudf)) {
    if (nrow(fudf) > 0) {
      suc_fudf <- fudf %>%
        dplyr::filter(proceed_day7 == 1)
      un_fudf <- fudf %>%
        dplyr::filter(proceed_day7 == 0)
      # Count the number of unsuccessful attempts
      attempt_df <- un_fudf %>%
        group_by(child_id) %>%
        count
      ltfu_log <- ltfu_log[!(ltfu_log$child_id %in% suc_fudf$child_id),]
      ltfu_log <- merge(ltfu_log, attempt_df, by = 'child_id', all.x = TRUE) %>%
        dplyr::rename(fu_attempts = n)
    }
  }

  # If fu_attempts does not exist, create a column and fill out values with zeroes
  if(!("fu_attempts" %in% colnames(ltfu_log))) {
    ltfu_log$fu_attempts <- 0
  }

  # Exclude children who are still within or before the follow-up window period
  ltfu_log <- ltfu_log[ltfu_log$date_maxfu < Sys.Date(),]

  # Order entries by date
  ltfu_log <- ltfu_log %>%
    dplyr::arrange(date_visit = as.Date(date_visit, "%Y-%m-%d"))

  # Order columns
  ltfu_log$date_maxfu <- as.Date(ltfu_log$date_maxfu, "%Y-%m-%d")
  if(Sys.getenv('TIMCI_COUNTRY') == 'India'){
    col_order <- c('child_id',
                   'date_visit',
                   'date_maxfu',
                   'fu_attempts',
                   'age_mo',
                   'yg_infant',
                   'who_age_ctg',
                   'fid',
                   'facility',
                   'district',
                   'referral_cg',
                   'referral_hf',
                   'urg_referral_hf',
                   'ref_facility',
                   'ref_facility_oth',
                   'device_id',
                   'sys_submit_id')
  } else{
    col_order <- c('child_id',
                   'date_visit',
                   'date_maxfu',
                   'fu_attempts',
                   'age_mo',
                   'yg_infant',
                   'who_age_ctg',
                   'fid',
                   'facility',
                   'district',
                   'phone_nb_avail',
                   'referral_cg',
                   'referral_hf',
                   'urg_referral_hf',
                   'ref_facility',
                   'ref_facility_oth',
                   'device_id',
                   'sys_submit_id')
  }
  ltfu_log <- ltfu_log[, col_order]

}
