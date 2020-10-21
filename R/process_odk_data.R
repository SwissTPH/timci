#' Format ODK data
#'
#' Description of format_odk_data
#' @param df dataframe containing the non de-identified (raw) ODK data
#' @return This function returns a formatted dataframe for future display and analysis.
#' @export
#' @import magrittr dplyr

format_odk_data <- function(df) {

  df$today <- strftime(df$today,"%Y-%m-%d")
  df$duration <- as.integer(round(df$end - df$start, digits=0))
  df$start <- strftime(df$start,"%T")
  df$end <- strftime(df$end,"%T")

  df %>% dplyr::rename('date' = 'today')

}

#' Extract enrolled participants
#'
#' @param df dataframe containing the non de-identified (raw) ODK data
#' @return This function returns a dataframe with enrolled participants only.
#' @export
#' @import dplyr magrittr

extract_enrolled_participants <- function(df) {

  df %>% dplyr::filter(df$enrolment==1)

}

#' Deidentify data
#'
#' De-identification of the TIMCI research data
#' @param df dataframe containing the non de-identified (raw) ODK data
#' @return This function returns de-identified data.
#' @export
#' @import magrittr dplyr

deidentify_data <- function(df) {

  subcol = c('pid',
             'date',
             'duration',
             'a3_a_3',
             'a3_a_6',
             'is_young_infant',
             'crfs_t02a_a4_a_6',
             'main_cg',
             'county',
             'crfs_t06a_tt06a_d2_6',
             'crfs_t06a_tt06a_d2_6a',
             'crfs_t09a_g3_1')

  # Combine 2 columns to get the age in years
  df$'a3_a_3' <- ifelse(!is.na(df$'a3_a_3'), df$'a3_a_3', df$'a3a_a3_a_2')
  # Combine 2 columns to get the age in months
  df$'a3_a_6' <- ifelse(!is.na(df$'a3_a_6'), df$'a3_a_6', df$'a3a_a3_a_5')
  df %>%
    dplyr::select(subcol) %>%
    dplyr::rename('sick_child_id' = 'pid',
                  'age_y'         = 'a3_a_3',
                  'age_m'         = 'a3_a_6',
                  'sex'           = 'crfs_t02a_a4_a_6',
                  'rep_temp'      = 'crfs_t06a_tt06a_d2_6',
                  'meas_temp'     = 'crfs_t06a_tt06a_d2_6a',
                  'diagnoses'     = 'crfs_t09a_g3_1'
    )

}

#' Extract Personally Identifiable Information (PII)
#'
#' Extraction of the personally identifiable data from the data collected
#' @param df numeric, the non de-identified (raw) data vector
#' @return This function returns de-identified data.
#' @export
#' @import magrittr dplyr

extract_pii <- function(df) {

  subcol <- c('pid',
              'crfs_t02b_a4_c_1',
              'crfs_t02b_a4_c_2',
              'main_cg',
              'crfs_t02b_a4_a_10',
              'crfs_t02b_a4_a_11',
              'date',
              'crfs_t02a_a4_a_1',
              'crfs_t02a_a4_a_3',
              'crfs_t02a_a4_a_6',
              'crfs_t02a_a4_a_8_1')

  df %>%
    dplyr::select(subcol) %>%
    dplyr::rename('sick_child_id'    = 'pid',
                  'cg_first_name'    = 'crfs_t02b_a4_c_1',
                  'cg_last_name'     = 'crfs_t02b_a4_c_2',
                  'phone_nb'         = 'crfs_t02b_a4_a_10',
                  'phone_ownership'  = 'crfs_t02b_a4_a_11',
                  'child_first_name' = 'crfs_t02a_a4_a_1',
                  'child_last_name'  = 'crfs_t02a_a4_a_3',
                  'child_sex'        = 'crfs_t02a_a4_a_6',
                  'mother_name'      = 'crfs_t02a_a4_a_8_1')

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
  cp$"min_date" <- as.Date(cp$date) + wmin
  cp$"max_date" <- as.Date(cp$date) + wmax

  drops <- c("date")
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

  drops <- c("date", "child_first_name", "child_last_name", "mother_name")
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

  cp <- df %>%dplyr::select('incl1',
                            'excl1',
                            'incl2',
                            'excl3',
                            'repeat_visit',
                            'consent_consent_signed')

  n_incl1 <- sum(cp$'incl1' == 0)
  cp <- cp %>%
    dplyr::filter(cp$'incl1'== 1)
  n_excl1 <- sum(cp$'excl1' == 1)
  cp <- cp %>%
    dplyr::filter(cp$'excl1'== 0)
  n_excl3 <- sum(cp$'excl3' == 1)
  cp <- cp %>%
    dplyr::filter(cp$'excl3'== 0)
  n_incl2 <- sum(cp$'incl2' == 0)
  cp <- cp %>%
    dplyr::filter(cp$'incl2'== 1)
  n_rep <- sum(cp$'repeat_visit' == 1)
  cp <- cp %>%
    dplyr::filter(cp$'repeat_visit'== 0)
  n_con <- sum(cp$'consent_consent_signed' == 0)

  data.frame(group=c("Above 5 years",
                     "First day of life",
                     "Inpatient admission",
                     "No illness",
                     "Repeat visit",
                     "Consent withdrawal"),
             value= c(n_incl1,
                      n_excl1,
                      n_incl2,
                      n_excl3,
                      n_rep,
                      n_con))

}
