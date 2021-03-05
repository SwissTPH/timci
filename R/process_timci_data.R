#' Process facility data (TIMCI-specific function)
#'
#' @param df dataframe containing the non de-identified (raw) ODK data collected at the facility level
#' @return This function returns a formatted dataframe for future display and analysis.
#' @export
#' @import dplyr magrittr stringr

process_facility_data <- function(df) {

  df <- format_odk_data(df)

  # Combine exact and approximate options to get the age in years
  df$'a3_a3_a_3' <- ifelse(!is.na(df$'a3_a3_a_3'), df$'a3_a3_a_3', df$'a3_a3_a_2a')

  # Combine exact and approximate options to get the age in months
  df$'a3_a3_a_6' <- ifelse(!is.na(df$'a3_a3_a_6'), df$'a3_a3_a_6', ifelse(df$'a3_a3_a_5' != 98, df$'a3_a3_a_5', NA))

  # Replace the space between different answers by a semicolon in multiple select questions
  nsep <- ";"
  multi_cols = c("visit_reason_a3_c_1",
                 "crfs_t05a_c1_a_11",
                 "crfs_t04a_b1_2",
                 "crfs_t04a_b1_2a",
                 "crfs_t04a_b1_2b",
                 "crfs_t04a_b1_4",
                 "crfs_t03_m1_3",
                 "crfs_t09a1_injection_types",
                 "crfs_t09a1_h2_2",
                 "crfs_t09a2_g3_1",
                 "crfs_t09a2_h2_2a",
                 "crfs_t08a_f2_1",
                 "crfs_t05b_c3_6")
  for (i in multi_cols) {
    df[[i]] <- stringr::str_replace_all(df[[i]], " ", nsep)
  }

  # Merge dictionaries
  dictionary <- readxl::read_excel(system.file(file.path('extdata', "facility_dict.xlsx"), package = 'timci'))
  df <- extract_match_from_dict(df, dictionary)

  # Return the processed facility data
  df

}

#' Extract enrolled participants (TIMCI-specific function)
#'
#' @param df dataframe containing the processed facility data
#' @return This function returns a dataframe containing data of enrolled participants only
#' @export
#' @import dplyr magrittr

extract_enrolled_participants <- function(df) {

  df %>% dplyr::filter(df$enrolled == 1)

}

#' Extract repeat visits (TIMCI-specific function)
#'
#' @param df dataframe containing the processed facility data
#' @return This function returns a dataframe containing data of repeat visits only
#' @export
#' @import dplyr magrittr

extract_repeat_visits <- function(df) {

  df %>% dplyr::filter(df$repeat_consult == 1)

}

#' Extract referrals (TIMCI-specific function)
#'
#' @param df dataframe containing the processed facility data
#' @return This function returns a dataframe containing data of children who were referred at Day 0 only
#' @export
#' @import dplyr magrittr

extract_referrals <- function(df) {

  df %>% dplyr::filter(df$referral_hf == 1)

}

#' Extract hypoxaemia (TIMCI-specific function)
#'
#' @param df Dataframe containing the processed facility data
#' @return This function returns a dataframe containing data of hypoxemic children only
#' @export
#' @import dplyr magrittr

extract_hypoxaemia <- function(df) {

  df %>% dplyr::filter(df$spo2_meas1_day0 <= 90)

}

#' Deidentify data (TIMCI-specific function)
#'
#' De-identification of the TIMCI research data
#' @param df dataframe containing the processed facility data
#' @return This function returns de-identified data.
#' @export
#' @import magrittr dplyr readxl

deidentify_data <- function(df) {

  # TO ADD: DOUBLE DE-IDENTIFICATION

  # Merge screening and rctls dictionaries
  s_dictionary <- readxl::read_excel(system.file(file.path('extdata', "screening_dict.xlsx"), package = 'timci'))
  m_dictionary <- readxl::read_excel(system.file(file.path('extdata', "rctls_dict.xlsx"), package = 'timci'))
  dictionary <- rbind(s_dictionary, m_dictionary)
  drops <- c("date_screen")
  dictionary <- dictionary[!(dictionary$new %in% drops),]

  df <- extract_match_from_dict(df, dictionary)

  # Malaria test done
  #df <- df %>% dplyr::mutate(malaria = ("1" %in% df$'dx_tests'))

}

#' Extract Personally Identifiable Information (PII) (TIMCI-specific function)
#'
#' Extraction of the personally identifiable data from the data collected
#' @param df dataframe containing the processed facility data
#' @return This function returns de-identified data.
#' @export
#' @import magrittr dplyr

extract_pii <- function(df) {

  df <- extract_match_from_xls_dict(df, "pii_dict.xlsx")

}

#' Generate follow-up log (TIMCI-specific function)
#'
#' Generate a list of participants to be called in a time window after baseline between wmin and wmax
#' @param df dataframe containing the processed facility data
#' @param fudf dataframe containing the processed follow-up data
#' @param wmin numerical, start of the follow-up period (in days)
#' @param wmax numerical, end of the follow-up period (in days)
#' @return This function returns a dataframe.
#' @export
#' @import magrittr dplyr

generate_fu_log <- function(df,
                            fudf,
                            wmin,
                            wmax) {

  fu_log <- extract_pii(df)
  fu_log$min_date <- as.Date(fu_log$date_visit) + wmin
  fu_log$max_date <- as.Date(fu_log$date_visit) + wmax
  fu_log$label <- paste(fu_log$fs_name,fu_log$ls_name)
  fu_log$caregiver <- paste(fu_log$cg_fs_name,fu_log$cg_ls_name)
  fu_log$mother <- paste(fu_log$mother_fs_name,fu_log$mother_ls_name)
  fu_log$sex <- ifelse(fu_log$sex == 1, "male", ifelse(fu_log$sex == 2, "female", "other"))
  fu_log <- fu_log[!(fu_log$child_id %in% fudf$a1_pid),]

  col_order <- c('child_id',
                 'label',
                 'sex',
                 'date_visit',
                 'caregiver',
                 'main_cg_lbl',
                 'mother',
                 'location_name',
                 'phone_nb')
  fu_log <- fu_log[, col_order]

  fu_log %>% dplyr::rename('name' = 'child_id',
                           'enroldate' = 'date_visit',
                           'relationship' = 'main_cg_lbl',
                           'location' = 'location_name',
                           'phonenb' = 'phone_nb')

}

#' Generate caregiver recruitment log (TIMCI-specific function)
#'
#' Generate a list of caregiver to be called for the qualitative studies
#' @param df dataframe containing the processed facility data
#' @return This function returns a dataframe.
#' @export
#' @import magrittr dplyr

generate_cg_log <- function(df) {

  cp <- extract_pii(df)

  drops <- c("date_visit", "first_name", "last_name", "mother_name")
  cp[, !(names(cp) %in% drops)]

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

  n_incl1 <- sum(cp$'age_incl' == 0)
  cp <- cp %>%
    dplyr::filter(cp$'age_incl' == 1)
  n_excl1 <- sum(cp$'age_excl'  == 1)
  cp <- cp %>%
    dplyr::filter(cp$'age_excl' == 0)
  n_excl3 <- sum(cp$'inpatient' == 1)
  cp <- cp %>%
    dplyr::filter(cp$'inpatient' == 0)
  n_incl2 <- sum(cp$'sickness' == 0)
  cp <- cp %>%
    dplyr::filter(cp$'sickness' == 1)
  n_rep <- sum(cp$'repeat_consult' == 1)
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
                       n_incl2,
                       n_excl3,
                       n_rep,
                       n_con))

}
