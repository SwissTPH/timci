#' Format ODK data
#'
#' Description of format_odk_data
#' @param df dataframe containing the non de-identified (raw) ODK data
#' @return This function returns a formatted dataframe for future display and analysis.
#' @export
#' @import magrittr dplyr

format_odk_data <- function(df) {

  df$today <- strftime(df$today,"%Y-%m-%d")
  df$duration <- as.integer(round(df$end - df$start, digits = 0))
  df$start <- strftime(df$start,"%T")
  df$end <- strftime(df$end,"%T")

  df %>% dplyr::rename('date' = 'today')

}

#' Process facility data
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

  # Replace space beteween different answers in multiple select questions by semicolon
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

  # Return the processed facility data
  df

}

#' Extract enrolled participants
#'
#' @param df dataframe containing the non de-identified (raw) ODK data
#' @return This function returns a dataframe containing data of enrolled participants only
#' @export
#' @import dplyr magrittr

extract_enrolled_participants <- function(df) {

  df %>% dplyr::filter(df$consent_enrolment == 1)

}

#' Extract referrals
#'
#' @param df dataframe containing the non deidentified facility data
#' @return This function returns a dataframe containing data of children who were referred at Day 0 only
#' @export
#' @import dplyr magrittr

extract_referrals <- function(df) {

  df %>% dplyr::filter(df$referral_hf_day0 == 1)

}

#' Extract hypoxaemia
#'
#' @param df Dataframe containing the non deidentified facility data
#' @return This function returns a dataframe containing data of hypoxemic children only
#' @export
#' @import dplyr magrittr

extract_hypoxaemia <- function(df) {

  df %>% dplyr::filter(df$spo2_meas1_day0 <= 90)

}

#' Extract and match variable names using a dictionary
#'
#' @param df Input dataframe
#' @param dictionary Dataframe containing 2 columns ('old' and 'new') that map the names of the variables in the input dataframe and the names of the variables in the output dataframe
#' @return This function returns a dataframe.
#' @export
#' @import magrittr dplyr

extract_match_from_dict <- function(df, dictionary) {

  # Add column if it does not exit
  df[setdiff(dictionary$old,names(df))] <- NA

  # Rename column names based on the dictionary
  names(df)[match(dictionary$old, names(df))] <- dictionary$new
  df %>% dplyr::select(dictionary$new)

}

#' Extract and match variable names using an external Excel dictionary
#'
#' @param df Input dataframe
#' @param xls_dict Excel file containing 2 columns ('old' and 'new') that map the names of the variables in the input dataframe and the names of the variables in the output dataframe
#' @return This function returns a dataframe.
#' @export
#' @import magrittr dplyr

extract_match_from_xls_dict <- function(df, xls_dict) {

  dictionary <- readxl::read_excel(system.file(file.path('extdata', xls_dict), package = 'timci'))
  df <- extract_match_from_dict(df, dictionary)

}

#' Deidentify data
#'
#' De-identification of the TIMCI research data
#' @param df dataframe containing the non de-identified (raw) ODK data
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
  #df <- df %>% dplyr::mutate(malaria_day0 = ("1" %in% df$'dx_tests_day0'))

}

#' Extract Personally Identifiable Information (PII)
#'
#' Extraction of the personally identifiable data from the data collected
#' @param df numeric, the non de-identified (raw) data vector
#' @return This function returns de-identified data.
#' @export
#' @import magrittr dplyr

extract_pii <- function(df) {

  df <- extract_match_from_xls_dict(df, "pii_dict.xlsx")

}

#' Generate follow-up log
#'
#' Generate a list of participants to be called in a time window after baseline between wmin and wmax
#' @param df dataframe
#' @param wmin numerical, start of the follow-up period (in days)
#' @param wmax numerical, end of the follow-up period (in days)
#' @return This function returns a dataframe.
#' @export
#' @import magrittr dplyr

generate_fu_log <- function(df,
                            wmin,
                            wmax) {

  cp <- extract_pii(df)
  cp$"min_date" <- as.Date(cp$date_day0) + wmin
  cp$"max_date" <- as.Date(cp$date_day0) + wmax

  drops <- c("date_day0")
  cp[, !(names(cp) %in% drops)]

}

#' Generate caregiver recruitment log
#'
#' Generate a list of caregiver to be called for the qualitative studies
#' @param df dataframe
#' @return This function returns a dataframe.
#' @export
#' @import magrittr dplyr

generate_cg_log <- function(df) {

  cp <- extract_pii(df)

  drops <- c("date_day0", "first_name", "last_name", "mother_name")
  cp[, !(names(cp) %in% drops)]

}

#' Count the occurrence of a specific value in a column
#'
#' De-identification of the TIMCI research data
#' @param df dataframe containing the non de-identified (raw) ODK data
#' @return This function returns de-identified data.
#' @export
#' @import magrittr dplyr

count_screening <- function(df) {

  cp <- df %>% dplyr::select('a3_incl1',
                             'a3_excl1',
                             'visit_reason_incl2',
                             'visit_reason_excl3',
                             'previous_enrolment_repeat_visit',
                             'consent_consent_signed')

  n_incl1 <- sum(cp$'a3_incl1' == 0)
  cp <- cp %>%
    dplyr::filter(cp$'a3_incl1' == 1)
  n_excl1 <- sum(cp$'a3_excl1'  == 1)
  cp <- cp %>%
    dplyr::filter(cp$'a3_excl1' == 0)
  n_excl3 <- sum(cp$'visit_reason_excl3' == 1)
  cp <- cp %>%
    dplyr::filter(cp$'visit_reason_excl3' == 0)
  n_incl2 <- sum(cp$'visit_reason_incl2' == 0)
  cp <- cp %>%
    dplyr::filter(cp$'visit_reason_incl2' == 1)
  n_rep <- sum(cp$'previous_enrolment_repeat_visit' == 1)
  cp <- cp %>%
    dplyr::filter(cp$'previous_enrolment_repeat_visit' == 0)
  n_con <- sum(cp$'consent_consent_signed' == 0)

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
