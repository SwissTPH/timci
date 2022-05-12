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
  fu_log$date_visit <- paste0("From ", as.Date(fu_log$'date_visit', "%Y-%m-%d") + vwmin, " to ", as.Date(fu_log$'date_visit', "%Y-%m-%d") + vwmax, " [enrolled on ", fu_log$date_visit, "]")

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
  fu_log_nophone$population <- "no phone number available"
  fu_log_nophone$type <- "2"

  #######################################################################################################
  # Select children for whom the caregiver provided a phone number and agreed with a physical follow-up #
  #######################################################################################################

  # Add a constraints on having at least 1 recorded failed attempt

  fu_log_phone <- fu_log %>%
    dplyr::filter(phone_nb_avail == 1 & physical_fu == 1) %>% # Include only children for whom the caregiver provided a phone number and agreed with physical follow-up
    dplyr::filter(min_phone_date <= Sys.Date() & max_date >= Sys.Date()) # Exclude children who are outside of the physical follow-up window period
  fu_log_phone$population <- "phone number available"
  fu_log_phone$type <- "2"

  #########################################################
  # Combine both selections of children in one single log #
  #########################################################

  if (!is.null(fu_log_nophone)) {
    if (nrow(fu_log_nophone) > 0) {
      if (!is.null(fu_log_phone)) {
        fu_log <- dplyr::bind_rows(fu_log_nophone, fu_log_phone)
      } else{
        fu_log <- fu_log_nophone
      }
    }
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
                   'yg_infant_ctg',
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
                   'yg_infant_ctg',
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

#' Process day 7 follow-up data (TIMCI-specific function)
#'
#' @param df dataframe containing the non de-identified (raw) ODK data collected during the Day 7 follow-up call
#' @return This function returns a formatted dataframe for future display and analysis.
#' @export
#' @import dplyr magrittr lubridate

format_day7_data <- function(df) {

  # Set the dictionary to be used
  day7_dict <- "day7_dict.xlsx"
  if(Sys.getenv('TIMCI_COUNTRY') == 'Tanzania'){
    day7_dict <- "day7_dict_tanzania.xlsx"
  }

  # Replace the space between different answers by a semicolon in multiple select questions
  sep <- ";"
  multi_cols = c("n1_o3_1a",
                 "n1_o3_1b",
                 "n1_o3_2b",
                 "n1_o3_3")
  df <- format_multiselect_asws(df, multi_cols, sep)

  # Separate submissions that relate to complete Day 7 follow-up and unsuccessful attempts
  successful_day7_df <- df[df$proceed == 1,]
  fail_day7_df <- df[df$proceed == 0,]

  # Match column names with names from dictionary
  dictionary <- readxl::read_excel(system.file(file.path('extdata', day7_dict), package = 'timci'))
  sub <- subset(dictionary, deidentified == 1)

  day7_df <- match_from_xls_dict(df, day7_dict)
  day7_df <- day7_df[sub$new]

  # Change multiple date formats
  dates <- lubridate::date(day7_df$date_day0)
  mdyv <- lubridate::mdy(day7_df$date_day0)
  dmyv <- lubridate::dmy(day7_df$date_day0)
  mdyv[is.na(mdyv)] <- dmyv[is.na(mdyv)] # some dates are ambiguous, here we give mdy precedence over dmy
  day7_df$date_day0 <- mdyv

  # Format dates
  day7_df$date_death_day7 <- strftime(day7_df$date_death_day7,"%Y-%m-%d")

  day7_df <- day7_df %>%
    dplyr::mutate(days = as.Date(date_call) - as.Date(date_day0), na.rm = TRUE)

  successful_day7_df <- match_from_xls_dict(successful_day7_df, day7_dict)
  successful_day7_df <- successful_day7_df[sub$new]
  successful_day7_df <- successful_day7_df %>%
    dplyr::mutate(days = as.Date(date_call) - as.Date(date_day0), na.rm = TRUE)

  fail_day7_df <- match_from_xls_dict(fail_day7_df, day7_dict)

  list(successful_day7_df, fail_day7_df, day7_df)

}

#' Process day 28 follow-up data (TIMCI-specific function)
#'
#' @param df dataframe containing the non de-identified (raw) ODK data collected during the Day 28 follow-up call
#' @return This function returns a formatted dataframe for future display and analysis.
#' @export
#' @import dplyr magrittr

format_day28_data <- function(df) {

  # Replace the space between different answers by a semicolon in multiple select questions
  sep <- ";"
  multi_cols = c("n1_o3_1a",
                 "n1_o3_1b",
                 "n1_o3_2b")
  df <- format_multiselect_asws(df, multi_cols, sep)

  # Separate submissions that relate to complete Day 28 follow-up and unsuccessful attempts
  successful_day28_df <- df[df$proceed == 1,]
  fail_day28_df <- df[df$proceed == 0,]

  # Match column names with names from dictionary
  dictionary <- readxl::read_excel(system.file(file.path('extdata', "day28_dict.xlsx"), package = 'timci'))
  sub <- subset(dictionary, deidentified == 1)

  day28_df <- match_from_xls_dict(df, "day28_dict.xlsx")
  day28_df <- day28_df[sub$new]
  day28_df <- day28_df %>%
    dplyr::mutate(days = as.Date(date_call) - as.Date(date_day0), na.rm = TRUE)

  successful_day28_df <- match_from_xls_dict(successful_day28_df, "day28_dict.xlsx")
  successful_day28_df <- successful_day28_df[sub$new]
  successful_day28_df <- successful_day28_df %>%
    dplyr::mutate(days = as.Date(date_call) - as.Date(date_day0), na.rm = TRUE)

  fail_day28_df <- match_from_xls_dict(fail_day28_df, "day28_dict.xlsx")

  list(successful_day28_df, fail_day28_df, day28_df)

}

#' Clean follow-up data for estimating rough follow-up rate (TIMCI-specific function)
#'
#' @param day0_df dataframe containing the non de-identified (raw) ODK data collected at Day 0
#' @param fu_df dataframe containing follow-up data that correspond to successful (either Day 7 Day 28) calls
#' @return out
#' @import dplyr magrittr
#' @export
#'
#' @examples
clean_followup_for_rate_estimation <- function(day0_df, fu_df) {

  # Extract only follow-ups that corresponds to an enrolled child
  # and delete duplicated IDs
  fu_df[fu_df$child_id %in% day0_df$child_id, ] %>%
    dplyr::distinct_at(dplyr::vars(child_id),
                       .keep_all = TRUE)

}
