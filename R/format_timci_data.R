#' Process facility data (TIMCI-specific function)
#'
#' @param df dataframe containing the non de-identified (raw) ODK data collected at the facility level
#' @return This function returns a formatted dataframe for future display and analysis.
#' @export
#' @import dplyr magrittr stringr

process_facility_data <- function(df) {

  cols <- colnames(df)

  if ('a3-a3_a_7' %in% cols) {
    # Create a deidentified version of the date of birth with a month and year accuracy for export
    df <- df %>% dplyr::mutate(ymdob = ifelse(!is.na(df$'a3-a3_a_7'), strftime(df$'a3-a3_a_7',"%Y-%m"), ''))
    # Format the date of birth
    df$'a3-a3_a_7' <- ifelse(!is.na(df$'a3-a3_a_7'), strftime(df$'a3-a3_a_7',"%Y-%m-%d"), '')
  }

  # Format the dates of birth collected with a month and year accuracy
  if ('a3-a3_a_4' %in% cols) {
    df$'a3-a3_a_4' <- ifelse(!is.na(df$'a3-a3_a_4'), strftime(df$'a3-a3_a_4',"%Y-%m"), '')
  }
  if ('a3-yob' %in% cols) {
    df$'a3-yob' <- ifelse(!is.na(df$'a3-yob'), strftime(df$'a3-yob',"%Y"), '')
  }

  # Combine exact and approximate options to get the age in years
  if ('a3-a3_a_3' %in% cols) {
    df$'a3-a3_a_3' <- ifelse(!is.na(df$'a3-a3_a_3'), df$'a3-a3_a_3', df$'a3-a3_a_2a')
  } else {
    df$'a3-a3_a_3' <- df$'a3-a3_a_2a'
  }

  # Combine exact and approximate options to get the age in months
  if ('a3-a3_a_6' %in% cols) {
    df$'a3-a3_a_6' <- ifelse(!is.na(df$'a3-a3_a_6'),
                             df$'a3-a3_a_6',
                             ifelse(!is.na(df$'a3-a3_a_6b'),
                                    df$'a3-a3_a_6b',
                                    ifelse(df$'a3-a3_a_5' != 98, df$'a3-a3_a_5', NA)))
  } else if ('a3-a3_a_6b' %in% cols) {
    df$'a3-a3_a_6' <- ifelse(!is.na(df$'a3-a3_a_6b'),
                             df$'a3-a3_a_6b',
                             ifelse(df$'a3-a3_a_5' != 98, df$'a3-a3_a_5', NA))
  } else if ('a3-a3_a_5' %in% cols) {
    df$'a3-a3_a_6' <- ifelse(df$'a3-a3_a_5' != 98, df$'a3-a3_a_5', NA)
  }

  if ('a3-a3_a_5' %in% cols) {
    df$'a3-a3_a_5' <- ifelse(df$'a3-a3_a_5' == 98 | (df$'a3-dobk' == 98 & df$'a3-a3_a_3' > 1), 0, 1)
  }

  if ('a3-a3_a_5' %in% cols) {
    df$'a3-a3_a_5' <- ifelse(df$'a3-a3_a_5' == 98 | (df$'a3-dobk' == 98 & df$'a3-a3_a_3' > 1), 0, 1)
  }

  if ('crfs-t02b-a4_c_4' %in% cols) {
    if ('crfs-t02b-a4_c_4a' %in% cols) {
      if ('crfs-t02b-a4_c_4b' %in% cols) {
        df$'crfs-t02b-a4_c_4' <- ifelse(!is.na(df$'crfs-t02b-a4_c_4') | df$'crfs-t02b-a4_c_4' != 98,
                                        ifelse(!is.na(df$'crfs-t02b-a4_c_4a') | df$'crfs-t02b-a4_c_4a' != 98,
                                               ifelse(!is.na(df$'crfs-t02b-a4_c_4b') | df$'crfs-t02b-a4_c_4b' != 98,
                                                      paste0(df$'crfs-t02b-a4_c_4b', " (", df$'crfs-t02b-a4_c_4a', " / ", df$'crfs-t02b-a4_c_4', ")"),
                                                      paste0(df$'crfs-t02b-a4_c_4a', " (", df$'crfs-t02b-a4_c_4', ")")),
                                               df$'crfs-t02b-a4_c_4'),
                                        '')
      } else {
        df$'crfs-t02b-a4_c_4' <- ifelse(!is.na(df$'crfs-t02b-a4_c_4'),
                                        ifelse(!is.na(df$'crfs-t02b-a4_c_4a') | df$'crfs-t02b-a4_c_4a' != 98,
                                               paste0(df$'crfs-t02b-a4_c_4a', " (", df$'crfs-t02b-a4_c_4', ")"),
                                               df$'crfs-t02b-a4_c_4'),
                                        '')
      }
    } else {
      df$'crfs-t02b-a4_c_4' <- ifelse(!is.na(df$'crfs-t02b-a4_c_4') | df$'crfs-t02b-a4_c_4' != 98,
                                      ifelse(df$'crfs-t02b-a4_c_4' == 96,
                                             "Outside the district",
                                             df$'crfs-t02b-a4_c_4'),
                                      '')
    }
  }

  # Replace the space between different answers by a semicolon in multiple select questions
  sep <- ";"
  multi_cols = c("visit_reason-a3_c_1",
                 "crfs-t05a-c1_a_11",
                 "crfs-t04a-b1_2",
                 "crfs-t04a-b1_2a",
                 "crfs-t04a-b1_2b",
                 "crfs-t04a-b1_4",
                 "crfs-t03-m1_3",
                 "crfs-t09a1-injection_types",
                 "crfs-t09a1-h2_2",
                 "crfs-t09a2-g3_1",
                 "crfs-t09a2-h2_2a",
                 "crfs-t08a-f2_1",
                 "crfs-t05b-c3_6")
  df <- format_multiselect_asws(df, multi_cols, sep)

  # Match column names with names from dictionary
  df <- match_from_xls_dict(df, "main_dict.xlsx")

  # Format dates
  df$date_prev <- strftime(df$date_prev,"%Y-%m-%d")

  df

  # Malaria test done
  #df <- df %>% dplyr::mutate(malaria = ("1" %in% df$'dx_tests'))

}

#' Extract screening data (TIMCI-specific function)
#'
#' @param df dataframe containing the processed facility data
#' @return This function returns a dataframe containing screening data only
#' @export
#' @import dplyr magrittr

extract_screening_data <- function(df) {

  dictionary <- readxl::read_excel(system.file(file.path('extdata', "main_dict.xlsx"), package = 'timci'))
  sub <- subset(dictionary, screening == 1)
  df[sub$new]

}

#' Extract enrolled participants (TIMCI-specific function)
#'
#' @param df dataframe containing the processed facility data
#' @return This function returns a dataframe containing data of enrolled participants only
#' @export
#' @import dplyr magrittr

extract_enrolled_participants <- function(df) {

  df %>%
    dplyr::filter(enrolled == 1) %>%
    extract_pii()

}

#' Extract personally identifiable information (TIMCI-specific function)
#'
#' @param df dataframe containing the processed facility data
#' @return This function returns a list of 2 dataframes: 1 dataframe with pii and 1 dataframe with deidentified demographic data
#' @export
#' @import dplyr magrittr

extract_pii <- function(df) {

  # Extract de-identified baseline data
  # Merge dictionaries
  dictionary <- readxl::read_excel(system.file(file.path('extdata', "main_dict.xlsx"), package = 'timci'))
  sub <- subset(dictionary, day0 == 1)
  demog <- df[sub$new]

  # Extract personally identifiable information
  sub <- subset(dictionary, contact == 1)
  pii <- df[sub$new]

  # Return a list
  list(demog, pii)

}

#' Extract visits (TIMCI-specific function)
#'
#' @param df dataframe containing the processed facility data
#' @return This function returns a dataframe containing data of all baseline and repeat visits
#' @export
#' @import dplyr magrittr

extract_all_visits <- function(df) {

  dictionary <- readxl::read_excel(system.file(file.path('extdata', "main_dict.xlsx"), package = 'timci'))
  sub <- subset(dictionary, visits == 1)
  df <- df[sub$new]
  df %>%
    dplyr::filter((repeat_consult == 1) | (repeat_consult == 0 & enrolled == 1))

}

#' Extract baseline visits (TIMCI-specific function)
#'
#' @param df dataframe containing the processed facility data
#' @return This function returns a dataframe containing data of baseline visits only
#' @export
#' @import dplyr magrittr

extract_baseline_visits <- function(df) {

  df %>%
    dplyr::filter(repeat_consult == 0)

}

#' Extract repeat visits (TIMCI-specific function)
#'
#' @param df dataframe containing the processed facility data
#' @return This function returns a dataframe containing data of repeat visits only
#' @export
#' @import dplyr magrittr

extract_repeat_visits <- function(df) {

  df %>%
    dplyr::filter(repeat_consult == 1)

}

#' Extract referrals (TIMCI-specific function)
#'
#' @param df dataframe containing the processed facility data
#' @return This function returns a dataframe containing data of children who were referred at Day 0 only
#' @export
#' @import dplyr magrittr

extract_referrals <- function(df) {

  df %>% dplyr::filter(referral_hf == 1)

}

#' Extract hypoxaemia (TIMCI-specific function)
#'
#' @param df Dataframe containing the processed facility data
#' @return This function returns a dataframe containing data of hypoxemic children only
#' @export
#' @import dplyr magrittr

extract_hypoxaemia <- function(df) {

  df %>% dplyr::filter(spo2_meas1_day0 <= 90)

}

#' Get summary statistics grouped by facility (TIMCI-specific function)
#'
#' @param df Dataframe containing the processed facility data
#' @return This function returns a dataframe containing summary statistics grouped by health facility
#' @export
#' @import dplyr magrittr

get_summary_by_facility <- function(df) {

  df1 <- df %>%
    dplyr::group_by(device_id) %>%
    dplyr::summarise(recruitment_start = min(date_visit),
                     recruitment_last = max(date_visit),
                     n = dplyr::n())

  enrolled <- extract_enrolled_participants(df)

  df2 <- enrolled[[1]] %>%
    dplyr::count(device_id)

  df3 <- enrolled[[1]] %>%
    dplyr::filter(sex == 2) %>%
    dplyr::count(device_id)

  df4 <- enrolled[[1]] %>%
    dplyr::filter(yg_infant == 1) %>%
    dplyr::count(device_id)

  df5 <- enrolled[[1]] %>%
    dplyr::filter((yg_infant == 1) & (sex == 2)) %>%
    dplyr::count(device_id)

  # Merge and rename
  res <- merge(x = df1, y = df2, by = 'device_id', all.x = TRUE)
  res <- res %>% dplyr::rename('screened' = 'n.x',
                               'children' = 'n.y')
  res <- merge(x = res, y = df3, by = 'device_id', all.x = TRUE)
  res <- res %>% dplyr::rename('female' = 'n')
  res <- merge(x = res, y = df4, by = 'device_id', all.x = TRUE)
  res <- res %>% dplyr::rename('yg_infant' = 'n')
  res <- merge(x = res, y = df5, by = 'device_id', all.x = TRUE)
  res %>% dplyr::rename('yg_female' = 'n')

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
  fudf <- fudf %>% dplyr::filter(proceed == 1)
  if (!is.null(fudf)) {
    if (nrow(fudf) > 0) {
      fu_log <- fu_log[!(fu_log$child_id %in% fudf$a1_pid),]
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
  fudf <- fudf %>% dplyr::filter(proceed == 1)
  if (!is.null(fudf)) {
    if (nrow(fudf) > 0) {
      fu_log <- fu_log[!(fu_log$child_id %in% fudf$a1_pid),]
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
  day7_df <- df[df$proceed == 1,]
  fail_df <- df[df$proceed == 0,]

  # Match column names with names from dictionary
  day7_df <- match_from_xls_dict(day7_df, "day7_dict.xlsx")
  fail_df <- match_from_xls_dict(fail_df, "day7_dict.xlsx")

  list(day7_df, fail_df)

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

  # Separate submissions that relate to complete Day 7 follow-up and unsuccessful attempts
  day28_df <- df[df$proceed == 1,]
  fail_df <- df[df$proceed == 0,]

  # Match column names with names from dictionary
  day28_df <- match_from_xls_dict(day28_df, "day28_dict.xlsx")
  fail_df <- match_from_xls_dict(fail_df, "day28_dict.xlsx")

  list(day28_df, fail_df)

}

#' Process hospital data (TIMCI-specific function)
#'
#' @param df dataframe containing the non de-identified (raw) ODK data collected at the referral level
#' @return This function returns a formatted dataframe for future display and analysis.
#' @export
#' @import dplyr magrittr

process_hospital_data <- function(df) {

  # Replace the space between different answers by a semicolon in multiple select questions
  sep <- ";"
  multi_cols = c("n4_n4_1")
  df <- format_multiselect_asws(df, multi_cols, sep)

  # Match column names with names from dictionary
  df %>% match_from_xls_dict("hospit_dict.xlsx")

}

#' Count the occurrence of a specific value in a column (TIMCI-specific function)
#'
#' @param df dataframe containing the processed facility data
#' @return This function returns de-identified data.
#' @export
#' @import magrittr dplyr

count_screening <- function(df) {

  cp <- df %>% dplyr::select('age_incl',
                             'age_excl',
                             'sickness',
                             'inpatient',
                             'repeat_consult',
                             'consent')

  # Above 5 years
  n_incl1 <- sum(cp$'age_incl' == 0)

  # First day of life
  cp <- cp %>%
    dplyr::filter(cp$'age_incl' == 1)
  n_excl1 <- sum(cp$'age_excl'  == 1)

  # Inpatient admission
  cp <- cp %>%
    dplyr::filter(cp$'age_excl' == 0)
  n_excl3 <- sum(cp$'inpatient' == 1)

  # No illness
  cp <- cp %>%
    dplyr::filter(cp$'inpatient' == 0)
  n_incl2 <- sum(cp$'sickness' == 0)

  # Repeat visit
  cp <- cp %>%
    dplyr::filter(cp$'sickness' == 1)
  n_rep <- sum(cp$'repeat_consult' == 1)

  # Consent withdrawal
  cp <- cp %>%
    dplyr::filter(cp$'repeat_consult' == 0)
  n_con <- sum(cp$'consent' == 0)

  data.frame(group = c("Above 5 years",
                       "First day of life",
                       "Inpatient admission",
                       "No illness",
                       "Repeat visit",
                       "Consent withdrawal"),
             value = c(n_incl1,
                       n_excl1,
                       n_excl3,
                       n_incl2,
                       n_rep,
                       n_con))

}
