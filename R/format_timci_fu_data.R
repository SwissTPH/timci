#' Generate follow-up log (TIMCI-specific function)
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

generate_fu_log <- function(pii,
                            fudf,
                            wmin,
                            wmax,
                            vwmin,
                            vwmax) {

  fu_log <- pii
  fu_log$min_date <- as.Date(fu_log$date_visit) + wmin
  fu_log$max_date <- as.Date(fu_log$date_visit) + wmax
  fu_log$label <- paste(fu_log$fs_name, fu_log$ls_name)
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
                 'phone_nb3',
                 'location',
                 'cmty_contact')
  fu_log <- fu_log[, col_order]

  # Order entries by date
  fu_log <- fu_log %>%
    dplyr::arrange(date_visit = as.Date(date_visit, "%Y-%m-%d"))

  # Add valid window in export
  fu_log$date_visit <- paste0("From ", as.Date(fu_log$'date_visit', "%Y-%m-%d") + vwmin, " to ", as.Date(fu_log$'date_visit', "%Y-%m-%d") + vwmax, " [enrolled on ", fu_log$date_visit, "]")

  # Add a first generic row
  fu_log <- rbind(data.frame('fid' = 'F0000',
                             'child_id' = 'X-F0000-P0000',
                             'label' = 'CHILD NAME',
                             'sex' = 'SEX',
                             'date_visit' = 'VALID WINDOW [ENROLMENT DATE]',
                             'caregiver' = 'CAREGIVER NAME',
                             'main_cg_lbl' = 'RELATIONSHIP',
                             'mother' = 'MOTHER NAME',
                             'phone_nb' = 'PHONE NB 1',
                             'phone_nb2' = 'PHONE NB 2',
                             'phone_nb3' = 'PHONE NB 3',
                             'location' = 'LOCATION',
                             'cmty_contact' = 'CONTACT'),
                  fu_log)

  fu_log %>% dplyr::rename('name' = 'child_id',
                           'enroldate' = 'date_visit',
                           'relationship' = 'main_cg_lbl',
                           'phonenb1' = 'phone_nb',
                           'phonenb2' = 'phone_nb2',
                           'phonenb3' = 'phone_nb3',
                           'contact' = 'cmty_contact')

}

#' Generate follow-up log 2 (TIMCI-specific function)
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

generate_fu_log2 <- function(pii,
                             fudf,
                             wmin,
                             wmax,
                             vwmin,
                             vwmax) {

  fu_log <- pii
  fu_log$min_date <- as.Date(fu_log$date_visit) + wmin
  fu_log$max_date <- as.Date(fu_log$date_visit) + wmax
  fu_log$child_name <- paste(fu_log$fs_name, fu_log$ls_name)
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

  # Exclude children who are outside of the follow-up window period
  fu_log <- fu_log[fu_log$min_date <= Sys.Date() & fu_log$max_date >= Sys.Date(),]

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
                 'phone_nb3',
                 'location',
                 'cmty_contact',
                 'label')
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
                             'phone_nb3' = 'PHONE NB 3',
                             'location' = 'LOCATION',
                             'cmty_contact' = 'CONTACT',
                             'label' = 'CHILD NAME (VALID WINDOW)'),
                  fu_log)

  fu_log %>% dplyr::rename('name' = 'child_id',
                           'enroldate' = 'date_visit',
                           'relationship' = 'main_cg_lbl',
                           'phonenb1' = 'phone_nb',
                           'phonenb2' = 'phone_nb2',
                           'phonenb3' = 'phone_nb3',
                           'contact' = 'cmty_contact')

}

#' Generate lost to follow-up log (TIMCI-specific function)
#'
#' Generate a list of participants who are lost to follow-up after wmax
#' @param df dataframe
#' @param fudf dataframe containing the processed follow-up data
#' @param dltfu numerical, end of the follow-up period (in days)
#' @return This function returns a dataframe.
#' @export
#' @import magrittr dplyr

generate_ltfu_log <- function(df,
                              fudf,
                              dltfu) {

  ltfu_log <- df
  ltfu_log$date_maxfu <- as.Date(ltfu_log$date_visit) + dltfu

  # Exclude children who already underwent successful follow-up
  if (!is.null(fudf)) {
    if (nrow(fudf) > 0) {
      suc_fudf <- fudf %>%
        dplyr::filter(proceed == 1)
      un_fudf <- fudf %>%
        dplyr::filter(proceed == 0) %>%
        dplyr::rename(child_id = "a1-pid")
      # Count the number of unsuccessful attempts
      attempt_nb <- un_fudf %>%
        group_by(child_id) %>%
        count
      ltfu_log <- ltfu_log[!(ltfu_log$child_id %in% suc_fudf$'a1-pid'),]
      ltfu_log <- merge(ltfu_log, attempt_nb, by = 'child_id', all.x = TRUE) %>%
        dplyr::rename(fu_attempts = n)
    }
  }

  # Exclude children who are still within or before the follow-up window period
  ltfu_log <- ltfu_log[ltfu_log$date_maxfu < Sys.Date(),]

  # Order entries by date
  ltfu_log <- ltfu_log %>%
    dplyr::arrange(date_visit = as.Date(date_visit, "%Y-%m-%d"))

  # Order columns
  ltfu_log$date_maxfu <- as.Date(ltfu_log$date_maxfu, "%Y-%m-%d")
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
  ltfu_log <- ltfu_log[, col_order]

}

#' Process day 7 follow-up data (TIMCI-specific function)
#'
#' @param df dataframe containing the non de-identified (raw) ODK data collected during the Day 7 follow-up call
#' @return This function returns a formatted dataframe for future display and analysis.
#' @export
#' @import dplyr magrittr

format_day7_data <- function(df) {

  # Replace the space between different answers by a semicolon in multiple select questions
  sep <- ";"
  multi_cols = c("n1_o3_1a",
                 "n1_o3_1b",
                 "n1_o3_2b")
  df <- format_multiselect_asws(df, multi_cols, sep)

  # Separate submissions that relate to complete Day 7 follow-up and unsuccessful attempts
  successful_day7_df <- df[df$proceed == 1,]
  fail_day7_df <- df[df$proceed == 0,]

  # Match column names with names from dictionary
  dictionary <- readxl::read_excel(system.file(file.path('extdata', "day7_dict.xlsx"), package = 'timci'))
  sub <- subset(dictionary, deidentified == 1)

  day7_df <- match_from_xls_dict(df, "day7_dict.xlsx")
  day7_df <- day7_df[sub$new]
  day7_df <- day7_df %>%
    dplyr::mutate(days = as.Date(date_call) - as.Date(date_day0), na.rm = TRUE)

  successful_day7_df <- match_from_xls_dict(successful_day7_df, "day7_dict.xlsx")
  successful_day7_df <- successful_day7_df[sub$new]
  successful_day7_df <- successful_day7_df %>%
    dplyr::mutate(days = as.Date(date_call) - as.Date(date_day0), na.rm = TRUE)

  fail_day7_df <- match_from_xls_dict(fail_day7_df, "day7_dict.xlsx")

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
