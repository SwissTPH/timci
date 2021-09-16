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
  # a3-a3_a_6b corresponds to the minimal age a child can have if the date of birth is not accurately known
  if ('a3-a3_a_6' %in% cols) {
    df$'a3-a3_a_6' <- ifelse(!is.na(df$'a3-a3_a_6'),
                             df$'a3-a3_a_6',
                             ifelse(!is.na(df$'a3-a3_a_6b'),
                                    df$'a3-a3_a_6b',
                                    ifelse(df$'a3-a3_a_3' > 1,
                                           12 * df$'a3-a3_a_3',
                                           ifelse(df$'a3-a3_a_5' != 98, df$'a3-a3_a_5', NA))))
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

  # Format the location
  if (Sys.getenv('TIMCI_COUNTRY') == 'Tanzania') {
    if ('crfs-t02b-a4_c_4' %in% cols) {
      df$'crfs-t02b-a4_c_4' <- ifelse(!is.na(df$'crfs-t02b-a4_c_4'),
                                       ifelse(df$'crfs-t02b-a4_c_4' != 99,
                                              ifelse(df$'crfs-t02b-a4_c_4' != 98,
                                                     ifelse(df$'crfs-t02b-a4_c_4' != 96,
                                                            df$'crfs-t02b-a4_c_4',
                                                            'Outside the region'),
                                                     ''),
                                              df$'crfs-t02b-a4_c_4_oth'),
                                       '')
    } else{
      df$'crfs-t02b-a4_c_4' <- ''
    }
    if ('crfs-t02b-a4_c_4a' %in% cols) {
      df$'crfs-t02b-a4_c_4a' <- ifelse(!is.na(df$'crfs-t02b-a4_c_4a'),
                                       ifelse(df$'crfs-t02b-a4_c_4a' != 99,
                                              ifelse(df$'crfs-t02b-a4_c_4a' != 98,
                                                     df$'crfs-t02b-a4_c_4a',
                                                     ''),
                                              df$'crfs-t02b-a4_c_4a_oth'),
                                       '')
    } else{
      df$'crfs-t02b-a4_c_4a' <- ''
    }
    if ('crfs-t02b-a4_c_4b' %in% cols) {
      df$'crfs-t02b-a4_c_4b' <- ifelse(!is.na(df$'crfs-t02b-a4_c_4b'),
                                       ifelse(df$'crfs-t02b-a4_c_4b' != 99,
                                              ifelse(df$'crfs-t02b-a4_c_4b' != 98,
                                                     df$'crfs-t02b-a4_c_4b',
                                                     ''),
                                              df$'crfs-t02b-a4_c_4b_oth'),
                                       '')
    } else{
      df$'crfs-t02b-a4_c_4b' <- ''
    }
    if ('crfs-t02b-a4_c_4c' %in% cols) {
      df$'crfs-t02b-a4_c_4c' <- ifelse(!is.na(df$'crfs-t02b-a4_c_4c'),
                                       ifelse(df$'crfs-t02b-a4_c_4c' != 99,
                                              ifelse(df$'crfs-t02b-a4_c_4c' != 98,
                                                     df$'crfs-t02b-a4_c_4c',
                                                     ''),
                                              df$'crfs-t02b-a4_c_4c_oth'),
                                       '')
    } else{
      df$'crfs-t02b-a4_c_4c' <- ''
    }
    df$'crfs-t02b-a4_c_4' <- paste0(df$'crfs-t02b-a4_c_4',
                                    ifelse(df$'crfs-t02b-a4_c_4' != '' & df$'crfs-t02b-a4_c_4a' != '', ' - ', ''),
                                    df$'crfs-t02b-a4_c_4a',
                                    ifelse(df$'crfs-t02b-a4_c_4a' != '' & df$'crfs-t02b-a4_c_4b' != '', ' - ', ''),
                                    df$'crfs-t02b-a4_c_4b',
                                    ifelse(df$'crfs-t02b-a4_c_4b' != '' & df$'crfs-t02b-a4_c_4c' != '', ' - ', ''),
                                    df$'crfs-t02b-a4_c_4c')
  }

  # Replace the space between different answers by a semicolon in multiple select questions
  sep <- ";"
  multi_cols <- c("visit_reason-a3_c_1",
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

  text_field_cols <- c('visit_reason-a3_c_1o',
                       'visit_reason-main_cg_name',
                       'crfs-t09a1-h2_2o',
                       'crfs-t09a2-h2_2ao',
                       'crfs-t02b-a4_c_1',
                       'crfs-t02b-a4_c_2',
                       'crfs-t02a-a4_a_1',
                       'crfs-t02a-a4_a_3',
                       'crfs-t02a-a4_a_8_2',
                       'crfs-t02a-a4_a_9_2',
                       'crfs-t02b-a4_c_9')
  df <- format_text_fields(df, text_field_cols)

  # Match column names with names from dictionary
  if (Sys.getenv('TIMCI_COUNTRY') == 'Senegal') {
    df <- match_from_xls_dict(df, "main_dict_senegal.xlsx")
  } else{
    df <- match_from_xls_dict(df, "main_dict.xlsx")
  }

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

  if (Sys.getenv('TIMCI_COUNTRY') == 'Senegal') {
    dictionary <- readxl::read_excel(system.file(file.path('extdata', "main_dict_senegal.xlsx"), package = 'timci'))
  } else{
    dictionary <- readxl::read_excel(system.file(file.path('extdata', "main_dict.xlsx"), package = 'timci'))
  }
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
  if (Sys.getenv('TIMCI_COUNTRY') == 'Senegal') {
    dictionary <- readxl::read_excel(system.file(file.path('extdata', "main_dict_senegal.xlsx"), package = 'timci'))
  } else{
    dictionary <- readxl::read_excel(system.file(file.path('extdata', "main_dict.xlsx"), package = 'timci'))
  }
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

  if (Sys.getenv('TIMCI_COUNTRY') == 'Senegal') {
    dictionary <- readxl::read_excel(system.file(file.path('extdata', "main_dict_senegal.xlsx"), package = 'timci'))
  } else{
    dictionary <- readxl::read_excel(system.file(file.path('extdata', "main_dict.xlsx"), package = 'timci'))
  }
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

  df %>% dplyr::filter(referral_hf == 1 | referral_cg == 1)

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

#' Get summary statistics grouped by device ID (TIMCI-specific function)
#'
#' @param df Dataframe containing the processed facility data
#' @return This function returns a dataframe containing summary statistics grouped by device IDs
#' @export
#' @import dplyr magrittr

get_summary_by_deviceid <- function(df) {

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
                       "Not willing to give consent"),
             value = c(n_incl1,
                       n_excl1,
                       n_excl3,
                       n_incl2,
                       n_rep,
                       n_con))

}
