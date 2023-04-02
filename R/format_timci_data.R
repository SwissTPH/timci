#' Match facility data using the Day 0 dictionary adapted for each country to account for differences in the data collection (TIMCI-specific function)
#'
#' @param df dataframe
#' @param is_pilot Boolean, default set to `FALSE`
#' @return This function returns a dataframe with columns that match the specified country dictionary.
#' @export

match_from_day0_xls_dict <- function(df,
                                     is_pilot = FALSE) {

  # Import dictionary
  dictionary <- timci::import_country_specific_xls_dict("main_dict.xlsx",
                                                        Sys.getenv('TIMCI_COUNTRY'))

  # Match column names with names from dictionary
  df %>%
    timci::match_from_dict(dictionary)

}

#' Load the Day 0 dictionary - adapted for each country to account for differences in the data collection (TIMCI-specific function)
#'
#' @param is_pilot Boolean, default set to `FALSE`
#' @return This function returns a dataframe that will be used as a dictionary to convert data exported from ODK Central to a statistician-friendly format.
#' @export

read_day0_xls_dict <- function(is_pilot = FALSE) {

  dictionary <- import_country_specific_xls_dict("main_dict.xlsx",
                                                 country = Sys.getenv('TIMCI_COUNTRY'))
  dictionary

}

#' Process facility data (TIMCI-specific function)
#'
#' @param df dataframe containing the non de-identified (raw) ODK data collected at the facility level
#' @param is_pilot Boolean, default set to `FALSE`
#' @return This function returns a formatted dataframe for future display and analysis.
#' @export
#' @import dplyr magrittr stringr

process_facility_data <- function(df,
                                  is_pilot = FALSE) {

  cols <- colnames(df)

  if ('a3-a3_a_7' %in% cols) {
    # Create a deidentified version of the date of birth with a month and year accuracy for export
    df <- df %>%
      dplyr::mutate(ymdob = ifelse(!is.na(df$'a3-a3_a_7'), strftime(df$'a3-a3_a_7',"%Y-%m"), ''))
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
    df$'a3-a3_a_3' <- as.numeric(df$'a3-a3_a_3')
  } else {
    df$'a3-a3_a_3' <- df$'a3-a3_a_2a'
  }

  # Combine exact and approximate options to get the age in months
  # a3-a3_a_6b corresponds to the maximal age a child can have if the date of birth is not accurately known
  # a3-a3_a_6a corresponds to the minimal age a child can have if the date of birth is not accurately known
  if ('a3-a3_a_6' %in% cols) {
    df$'a3-a3_a_6' <- ifelse(df$'a3-dobk' == 1 & !is.na(df$'a3-a3_a_6'),
                             df$'a3-a3_a_6',
                             ifelse(df$'a3-dobk' != 98 & !is.na(df$'a3-a3_a_6a'),
                                    df$'a3-a3_a_6a',
                                    ifelse(df$'a3-a3_a_3' > 1,
                                           12 * df$'a3-a3_a_3',
                                           ifelse(df$'a3-a3_a_5' != 98, df$'a3-a3_a_5', NA))))
  } else if ('a3-a3_a_6a' %in% cols) {
    df$'a3-a3_a_6' <- ifelse(df$'a3-dobk' != 98 & !is.na(df$'a3-a3_a_6a'),
                             df$'a3-a3_a_6a',
                             ifelse(df$'a3-a3_a_3' > 1,
                                    12 * df$'a3-a3_a_3',
                                    ifelse(df$'a3-a3_a_5' != 98, df$'a3-a3_a_5', NA)))
  } else if ('a3-a3_a_5' %in% cols) {
    df$'a3-a3_a_6' <- ifelse(df$'a3-a3_a_3' > 1,
                             12 * df$'a3-a3_a_3',
                             ifelse(df$'a3-a3_a_5' != 98, df$'a3-a3_a_5', NA))
  }

  # Convert to WHO age categories
  df <- df %>%
    dplyr::mutate(age_ctg = dplyr::case_when(
      # Exact date of birth known
      as.numeric(`a3-a3_a_9`) >= 1 & as.numeric(`a3-a3_a_9`) < 7                                      ~ "[1-6d]",
      as.numeric(`a3-a3_a_9`) >= 7 & as.numeric(`a3-a3_a_9`) < 28                                     ~ "[7-27d]",
      as.numeric(`a3-a3_a_9`) >= 28 & as.numeric(`a3-a3_a_9`) < 60                                    ~ "[28-59d]",
      as.numeric(`a3-a3_a_9`) >= 60 & as.numeric(`a3-a3_a_9`) < 365                                   ~ "[60-364d]",
      as.numeric(`a3-a3_a_9`) >= 365 & as.numeric(`a3-a3_a_9`) < 1827                                 ~ "[12-59m]",
      # Non-exact date of birth
      as.numeric(`a3-a3_a_9a`) >= 0 & as.numeric(!is.na(`a3-a3_a_9b`)) & as.numeric(`a3-a3_a_9a`) < 7 ~ "[1-6d]",
      as.numeric(`a3-a3_a_9a`) >= 7 & as.numeric(`a3-a3_a_9a`) < 28                                   ~ "[7-27d]",
      as.numeric(`a3-a3_a_9a`) >= 28 & as.numeric(`a3-a3_a_9a`) < 60                                  ~ "[28-59d]",
      as.numeric(`a3-a3_a_9a`) >= 60 & as.numeric(`a3-a3_a_9a`) < 365                                 ~ "[60-364d]",
      as.numeric(`a3-a3_a_9a`) >= 365 & as.numeric(`a3-a3_a_9a`) < 1827                               ~ "[12-59m]",
      # Age categories
      as.numeric(`a3-a3_a_2`) == 0 & as.numeric(`a3-a3_a_5`) == 0 & as.numeric(`a3-a3_a_8`) == 1      ~ "[1-6d]",
      as.numeric(`a3-a3_a_2`) == 0 & as.numeric(`a3-a3_a_5`) == 0 & as.numeric(`a3-a3_a_8`) == 2      ~ "[7-27d]",
      as.numeric(`a3-a3_a_2`) == 0 & as.numeric(`a3-a3_a_5`) == 1                                     ~ "[28-59d]",
      as.numeric(`a3-a3_a_2`) == 0 & as.numeric(`a3-a3_a_5`) >= 2                                     ~ "[60-364d]",
      as.numeric(`a3-a3_a_2`) > 0 & as.numeric(`a3-a3_a_2`) < 5                                       ~ "[12-59m]",
      as.numeric(`a3-incl1`) == 0 | as.numeric(`a3-excl1`) == 1                                       ~ "non-eligible age",
      .default = "")) %>%
    dplyr::select(-`a3-a3_a_8`)  %>%
    dplyr::rename(`a3-a3_a_8` = age_ctg)

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

    # Convert to characters
    df$'crfs-t02b-a4_c_4' <- as.character(df$'crfs-t02b-a4_c_4')

    df$'crfs-t02b-a4_c_4_cpy' <- df$'crfs-t02b-a4_c_4'
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

    # Convert to characters
    df$'crfs-t02b-a4_c_4a' <- as.character(df$'crfs-t02b-a4_c_4a')

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

    # Convert to characters
    df$'crfs-t02b-a4_c_4b' <- as.character(df$'crfs-t02b-a4_c_4b')

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

    # Convert to characters
    df$'crfs-t02b-a4_c_4c' <- as.character(df$'crfs-t02b-a4_c_4c')

    df$'crfs-t02b-a4_c_4' <- paste0(df$'crfs-t02b-a4_c_4',
                                    ifelse(df$'crfs-t02b-a4_c_4' != '' & df$'crfs-t02b-a4_c_4a' != '', ' - ', ''),
                                    df$'crfs-t02b-a4_c_4a',
                                    ifelse(df$'crfs-t02b-a4_c_4a' != '' & df$'crfs-t02b-a4_c_4b' != '', ' - ', ''),
                                    df$'crfs-t02b-a4_c_4b',
                                    ifelse(df$'crfs-t02b-a4_c_4b' != '' & df$'crfs-t02b-a4_c_4c' != '', ' - ', ''),
                                    df$'crfs-t02b-a4_c_4c')

    # Convert to characters
    df$'crfs-t02b-a4_c_4' <- as.character(df$'crfs-t02b-a4_c_4')
  }

  # Extract the way the child ID has been recorded (manual entry or scan)
  if ('consent-a1_a_4' %in% cols) {
    df$'consent-a1_a_4' <- ifelse(!is.na(df$'consent-a1_a_4'), 1, 0)
  }
  if ('consent-a1_a_4a' %in% cols) {
    df$'consent-a1_a_4a' <- ifelse(!is.na(df$'consent-a1_a_4a'), 1, 0)
  }
  # Convert to integer
  df$'consent-a1_a_4' <- as.integer(df$'consent-a1_a_4')

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
                  "crfs-t09a1-t09a2-g3_1",
                  "crfs-t09a2-rxothermain",
                  "crfs-t09a1-t09a2-rxothermain",
                  "crfs-t09a2-h2_2a",
                  "crfs-t08a-f2_1",
                  "crfs-t05b-c3_6",
                  "crfs-t09a1-antimalarials",
                  "crfs-t09a1-i2_1cga1", # Kenya
                  "crfs-t09a2-i2_1a1", #Kenya
                  "crfs-t09a1-i2_1a1_cg", # Tanzania
                  "crfs-t09a1-t09a2-i2_1a1", #Tanzania
                  "crfs-t09a1-imcirx",
                  "crfs-t09a2-imcirx_hf"
                  )

  df <- timci::format_multiselect_asws(df, multi_cols, sep)

  text_field_cols <- c('visit_reason-a3_c_1o',
                       'visit_reason-main_cg_name',
                       'crfs-t02a-a4_a_1',
                       'crfs-t02a-a4_a_2',
                       'crfs-t02a-a4_a_3',
                       'crfs-t02b-a4_c_1',
                       'crfs-t02b-a4_c_2',
                       'crfs-t02a-a4_c_7',
                       'crfs-t02b-a4_c_7',
                       'crfs-t02a-a4_c_8',
                       'crfs-t02b-a4_c_8',
                       'crfs-t02a-a4_a_8_2',
                       'crfs-t02a-a4_a_9_2',
                       'crfs-t02b-a4_c_9',
                       'crfs-t02b-physical_fu_guidance',
                       'crfs-t03-m3_5o',
                       'crfs-t05a-c1_a_11o',
                       'crfs-t04a-b2_2a_o',
                       'crfs-t04a-b2_2b_o',
                       'crfs-t04a-b1_4o',
                       'crfs-t02b-a4_c_4',
                       'crfs-t04a-b2_1o',
                       'crfs-t09a1-h2_2o',
                       'crfs-t09a2-h2_2ao')
  df <- format_text_fields(df, text_field_cols)

  # Match column names with names from dictionary
  df <- timci::match_from_day0_xls_dict(df, is_pilot)
  cols <- colnames(df)

  # Format dates
  df$date_prev <- strftime(df$date_prev,"%Y-%m-%d")
  if ( 'screening_start' %in% cols ) {
    df$screening_start <- strftime(strptime(x = df$screening_start, format = "%Y-%m-%dT%T"))
  }
  if ( 'contact_start' %in% cols ) {
    df$contact_start <- strftime(strptime(x = df$contact_start, format = "%Y-%m-%dT%T"))
  }
  if ( 'consent_end' %in% cols ) {
    df$consent_end <- strftime(strptime(x = df$consent_end, format = "%Y-%m-%dT%T"))
  }
  if ( 'sd_start' %in% cols ) {
    df$sd_start <- strftime(strptime(x = df$sd_start, format = "%Y-%m-%dT%T"))
  }

  df

}

#' Process Tanzania facility data (TIMCI-specific function)
#'
#' @param df dataframe containing the non de-identified (raw) ODK data collected at the facility level
#' @param is_pilot Boolean, default set to `FALSE`
#' @return This function returns a formatted dataframe for future display and analysis.
#' @export
#' @import dplyr magrittr stringr

process_tanzania_facility_data <- function(df,
                                           is_pilot = FALSE) {

  cols <- colnames(df)

  c1 <- c('crfs-t09a2-g3_1o',
          'crfs-t09a2-g3_1',
          'crfs-t09a2-i2_1',
          'crfs-t09a2-i2_1a',
          'crfs-t09a2-i2_1b',
          'crfs-t09a2-i2_1o',
          'crfs-t09a2-j2_1',
          'crfs-t09a2-j2_1c',
          'crfs-t07a-tt07a-e2_1',
          'crfs-t07a-tt07a-e2_1a',
          'crfs-t07a-tt07a-e2_2',
          'crfs-t07a-tt07a-e2_2a',
          'crfs-t06a-tt06a-d2_6',
          'crfs-t06a-d2_6b',
          'crfs-t06a-tt06a-d2_1',
          'crfs-t06a-d2_1a',
          'crfs-t06a-tt06a-d2_4',
          'crfs-t06a-d2_4a',
          'crfs-t06a-tt06a-d2_5',
          'crfs-t06a-d2_5a',
          'crfs-t06a-tt06a-d2_2',
          'crfs-t06a-d2_2b',
          'crfs-t06a-tt06a-d2_3',
          'crfs-t06a-d2_3b',
          'crfs-t08a-f2_1',
          'crfs-t08a-f2_1o',
          'crfs-t08a-f2_2',
          'crfs-t08a-f2_3',
          'crfs-t08a-f2_4',
          'crfs-t08a-f2_5',
          'crfs-t08a-f2_6',
          'crfs-t08a-f2_7',
          'crfs-t08a-f2_8',
          'crfs-t08a-f2_9',
          'crfs-t08a-f2_10a',
          'crfs-t05b-c3_1',
          'crfs-t05b-c3_2',
          'crfs-t05b-c3_3',
          'crfs-t05b-c3_3a',
          'crfs-t05b-c3_4',
          'crfs-t05b-c3_6a',
          'crfs-t05b-c3_6',
          'crfs-t05b-c3_6o')
  c2 <- c('crfs-t09a1-t09a2-g3_1o',
          'crfs-t09a1-t09a2-g3_1',
          'crfs-t09a1-t09a2-i2_1',
          'crfs-t09a1-t09a2-i2_1a',
          'crfs-t09a1-t09a2-i2_1b',
          'crfs-t09a1-t09a2-i2_1o',
          'crfs-t09a1-t09a2-j2_1',
          'crfs-t09a1-t09a2-j2_1c',
          'crfs-t09a1-t07a-tt07a-e2_1',
          'crfs-t09a1-t07a-tt07a-e2_1a',
          'crfs-t09a1-t07a-tt07a-e2_2',
          'crfs-t09a1-t07a-tt07a-e2_2a',
          'crfs-t09a1-t06a-tt06a-d2_6',
          'crfs-t09a1-t06a-d2_6b',
          'crfs-t09a1-t06a-tt06a-d2_1',
          'crfs-t09a1-t06a-d2_1a',
          'crfs-t09a1-t06a-tt06a-d2_4',
          'crfs-t09a1-t06a-d2_4a',
          'crfs-t09a1-t06a-tt06a-d2_5',
          'crfs-t09a1-t06a-d2_5a',
          'crfs-t09a1-t06a-tt06a-d2_2',
          'crfs-t09a1-t06a-d2_2b',
          'crfs-t09a1-t06a-tt06a-d2_3',
          'crfs-t09a1-t06a-d2_3b',
          'crfs-t09a1-t08a-f2_1',
          'crfs-t09a1-t08a-f2_1o',
          'crfs-t09a1-t08a-f2_2',
          'crfs-t09a1-t08a-f2_3',
          'crfs-t09a1-t08a-f2_4',
          'crfs-t09a1-t08a-f2_5',
          'crfs-t09a1-t08a-f2_6',
          'crfs-t09a1-t08a-f2_7',
          'crfs-t09a1-t08a-f2_8',
          'crfs-t09a1-t08a-f2_9',
          'crfs-t09a1-t08a-f2_10a',
          'crfs-t09a1-t05b-c3_1',
          'crfs-t09a1-t05b-c3_2',
          'crfs-t09a1-t05b-c3_3',
          'crfs-t09a1-t05b-c3_3a',
          'crfs-t09a1-t05b-c3_4',
          'crfs-t09a1-t05b-c3_6a',
          'crfs-t09a1-t05b-c3_6',
          'crfs-t09a1-t05b-c3_6o')
  df <- combine_columns(df, c1, c2)
  process_facility_data(df, is_pilot)

}

#' Extract screening data (TIMCI-specific function)
#'
#' @param df dataframe containing the processed facility data
#' @param is_pilot Boolean, default set to `FALSE`
#' @return This function returns a dataframe containing screening data only
#' @export
#' @import dplyr magrittr

extract_screening_data <- function(df,
                                   is_pilot = FALSE) {

  dictionary <- timci::read_day0_xls_dict(is_pilot)
  # Filter dictionary to only keep deidentified variables
  dictionary <- dictionary %>%
    dplyr::filter(screening == 1)

  if ("fid_from_device" %in% colnames(df)){
    cols <- c(dictionary$new,
              "fid_from_device")
  } else{
    cols <- dictionary$new
  }


  df[cols]

}

#' Extract enrolled participants (TIMCI-specific function)
#'
#' @param df dataframe containing the processed facility data
#' @param is_pilot Boolean, default set to `FALSE`
#' @return This function returns a dataframe containing data of enrolled participants only
#' @export
#' @import dplyr magrittr

extract_enrolled_participants <- function(df,
                                          is_pilot = FALSE) {

  df %>%
    dplyr::filter(enrolled == 1) %>%
    extract_pii(is_pilot)

}

#' Extract non-enrolled participants (TIMCI-specific function)
#'
#' @param df dataframe containing the processed facility data
#' @return This function returns a dataframe containing data of enrolled participants only
#' @export
#' @import dplyr magrittr

extract_noneligible <- function(df) {

  df %>%
    dplyr::filter((is.na(enrolled) & is.na(repeat_consult)) | enrolled == 0)

}

#' Extract personally identifiable information (TIMCI-specific function)
#'
#' @param df dataframe containing the processed facility data
#' @param is_pilot Boolean, default set to `FALSE`
#' @return This function returns a list of 2 dataframes: 1 dataframe with pii and 1 dataframe with deidentified demographic data
#' @export
#' @import dplyr magrittr

extract_pii <- function(df,
                        is_pilot = FALSE) {

  # Merge dictionaries
  dictionary <- timci::read_day0_xls_dict(is_pilot)

  # Extract de-identified baseline data
  sub <- subset(dictionary, day0 == 1)
  demog <- df %>%
    dplyr::select(dplyr::any_of(sub$new))

  # Extract personally identifiable information
  sub <- subset(dictionary, contact == 1)
  pii <- df %>%
    dplyr::select(dplyr::any_of(sub$new))

  # Return a list
  list(demog, pii)

}

#' Extract visits (TIMCI-specific function)
#'
#' @param df dataframe containing the processed facility data
#' @param is_pilot Boolean, default set to `FALSE`
#' @return This function returns a dataframe containing data of all baseline and repeat visits
#' @export
#' @import dplyr magrittr

extract_all_visits <- function(df,
                               is_pilot = FALSE) {

  dictionary <- timci::read_day0_xls_dict(is_pilot)
  sub <- subset(dictionary, visits == 1)
  df <- df %>%
    dplyr::select(dplyr::any_of(sub$new)) %>%
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

  df %>% dplyr::filter(spo2_meas1 <= 90)

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

#' Get summary statistics grouped by facility ID (TIMCI-specific function)
#'
#' @param df Dataframe containing the processed facility data
#' @return This function returns a dataframe containing summary statistics grouped by facility IDs
#' @export
#' @import dplyr magrittr

get_summary_by_fid <- function(df) {

  df1 <- df %>%
    dplyr::group_by(fid_from_device) %>%
    dplyr::summarise(recruitment_start = min(date_visit),
                     recruitment_last = max(date_visit),
                     n = dplyr::n())

  enrolled <- df %>%
    dplyr::filter(enrolled == 1)
  df2 <- enrolled %>%
    dplyr::count(fid_from_device)

  df3 <- enrolled %>%
    dplyr::filter(sex == 2) %>%
    dplyr::count(fid_from_device)

  df4 <- enrolled %>%
    dplyr::filter(yg_infant == 1) %>%
    dplyr::count(fid_from_device)

  df5 <- enrolled %>%
    dplyr::filter((yg_infant == 1) & (sex == 2)) %>%
    dplyr::count(fid_from_device)

  # Merge and rename
  res <- merge(x = df1,
               y = df2,
               by = 'fid_from_device',
               all.x = TRUE)
  res <- res %>% dplyr::rename('screened' = 'n.x',
                               'children' = 'n.y')
  res <- merge(x = res,
               y = df3,
               by = 'fid_from_device',
               all.x = TRUE)
  res <- res %>% dplyr::rename('female' = 'n')

  res <- merge(x = res,
               y = df4,
               by = 'fid_from_device',
               all.x = TRUE)
  res <- res %>% dplyr::rename('yg_infant' = 'n')

  res <- merge(x = res,
               y = df5,
               by = 'fid_from_device',
               all.x = TRUE)
  res %>% dplyr::rename('yg_female' = 'n')

}

#' Count the occurrence of non-enrolment causes during screening (TIMCI-specific function)
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
  n_incl1 <- sum(cp$'age_incl' == 0, na.rm = TRUE)

  # First day of life
  cp <- cp %>%
    dplyr::filter(cp$'age_incl' == 1)
  n_excl1 <- sum(cp$'age_excl'  == 1, na.rm = TRUE)

  # Inpatient admission
  cp <- cp %>%
    dplyr::filter(cp$'age_excl' == 0)
  n_excl3 <- sum(cp$'inpatient' == 1, na.rm = TRUE)

  # No illness
  cp <- cp %>%
    dplyr::filter(cp$'inpatient' == 0)
  n_incl2 <- sum(cp$'sickness' == 0, na.rm = TRUE)

  # Repeat visit
  cp <- cp %>%
    dplyr::filter(cp$'sickness' == 1)
  n_rep <- sum(cp$'repeat_consult' == 1, na.rm = TRUE)

  # Consent withdrawal
  cp <- cp %>%
    dplyr::filter(cp$'repeat_consult' == 0)
  n_con <- sum(cp$'consent' == 0, na.rm = TRUE)

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

#' Count the occurrence of non-eligibility reasons (TIMCI-specific function)
#'
#' @param df dataframe containing the processed facility data
#' @return This function returns de-identified data.
#' @export
#' @import magrittr dplyr

count_noneligibility <- function(df) {

  cp <- df %>% dplyr::select('age_incl',
                             'age_excl',
                             'sickness',
                             'inpatient',
                             'repeat_consult',
                             'consent')

  # Above 5 years
  n_incl1 <- sum(cp$'age_incl' == 0, na.rm = TRUE)

  # First day of life
  cp <- cp %>%
    dplyr::filter(cp$'age_incl' == 1)
  n_excl1 <- sum(cp$'age_excl'  == 1, na.rm = TRUE)

  # Inpatient admission
  cp <- cp %>%
    dplyr::filter(cp$'age_excl' == 0)
  n_excl3 <- sum(cp$'inpatient' == 1, na.rm = TRUE)

  # No illness
  cp <- cp %>%
    dplyr::filter(cp$'inpatient' == 0)
  n_incl2 <- sum(cp$'sickness' == 0, na.rm = TRUE)

  # Repeat visit
  cp <- cp %>%
    dplyr::filter(cp$'sickness' == 1)
  n_rep <- sum(cp$'repeat_consult' == 1, na.rm = TRUE)

  # Consent withdrawal
  cp <- cp %>%
    dplyr::filter(cp$'repeat_consult' == 0)
  n_con <- sum(cp$'consent' == 0, na.rm = TRUE)

  data.frame(group = c("Above 5 years",
                       "First day of life",
                       "Inpatient admission",
                       "No illness",
                       "Not willing to give consent"),
             value = c(n_incl1,
                       n_excl1,
                       n_excl3,
                       n_incl2,
                       n_con))

}

#' Count the occurrence of baseline versus repeat visits (TIMCI-specific function)
#'
#' @param df dataframe containing the processed facility data
#' @return This function returns de-identified data.
#' @export
#' @import magrittr dplyr

count_baseline_vs_repeat <- function(df) {

  cp <- df %>% dplyr::select('age_incl',
                             'age_excl',
                             'sickness',
                             'inpatient',
                             'repeat_consult',
                             'consent')

  # Above 5 years
  n_incl1 <- sum(cp$'age_incl' == 0, na.rm = TRUE)

  # First day of life
  cp <- cp %>%
    dplyr::filter(cp$'age_incl' == 1)
  n_excl1 <- sum(cp$'age_excl'  == 1, na.rm = TRUE)

  # Inpatient admission
  cp <- cp %>%
    dplyr::filter(cp$'age_excl' == 0)
  n_excl3 <- sum(cp$'inpatient' == 1, na.rm = TRUE)

  # No illness
  cp <- cp %>%
    dplyr::filter(cp$'inpatient' == 0)
  n_incl2 <- sum(cp$'sickness' == 0, na.rm = TRUE)

  # Repeat visit
  cp <- cp %>%
    dplyr::filter(cp$'sickness' == 1)
  n_rep <- sum(cp$'repeat_consult' == 1, na.rm = TRUE)

  # Consent withdrawal
  cp <- cp %>%
    dplyr::filter(cp$'repeat_consult' == 0)
  n_con <- sum(cp$'consent' == 0, na.rm = TRUE)

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

#' Convert age in days to age categories (TIMCI-specific function)
#'
#' @param yi_ctg character containing the category of young infant
#' @param yr_ctg scalar value
#' @param val scalar value containing the age of the child in days
#' @param incl boolean value
#' @param excl boolean
#' @return This function returns a formatted dataframe for future display and analysis.
#' @export

convert_age2ctg <- function(yi_ctg,
                            yr_ctg,
                            val,
                            incl,
                            excl) {

  val <- as.integer(val)
  yr_ctg <- as.integer(yr_ctg)

  out <- case_when( ( val >= 1 & val < 7 ) ~ "[1-6d]",
                    ( val >= 7 & val < 28 ) ~ "[7-27d]",
                    ( val >= 28 & val < 60 ) ~ "[28-59d]",
                    ( val >= 60 & val < 365 ) ~ "[60-364d]",
                    yr_ctg == 0 ~ "[60-364d]",
                    yr_ctg > 0 ~ "[12-59m]",
                    ( incl == 1 & excl != 1 ) ~ "[12-59m]",
                    .default = "")
}

#' Convert age in days to age categories (TIMCI-specific function)
#'
#' @param val scalar value containing the age of the child in days
#' @return This function returns a formatted dataframe for future display and analysis.
#' @export

convert_yi_age2ctg <- function(val) {
  res <- ""
  val <- as.integer(val)
  if (!is.na(val)) {
    if (val >= 1 & val < 7) {
      res <- "[1-6d]"
    } else if (val >= 7 & val < 28) {
      res <- "[7-27d]"
    } else if (val >= 28 & val < 60) {
      res <- "[28-59d]"
    }
  }
  res
}

#' Match facility data using the Drug dictionary adapted for each country to account for differences in the data collection (TIMCI-specific function)
#'
#' @param df dataframe
#' @return This function returns a dataframe with columns that match the specified country dictionary.
#' @export

match_from_drug_xls_dict <- function(df) {

  # Import dictionary
  dictionary <- timci::import_country_specific_xls_dict("drug_dict.xlsx",
                                                        Sys.getenv('TIMCI_COUNTRY'))

  # Match column names with names from dictionary
  df <- df %>%
    timci::match_from_dict(dictionary)

  # Replace the space between different answers by a semicolon in multiple select questions
  sep <- ";"
  multi_cols <- c("rx_antimicrobials",
                  "rx_antimalarials",
                  "rx_imci",
                  "rx_creams",
                  "rx_consumables",
                  "rx_type",
                  "rx_antimicrobials_hf",
                  "rx_antimalarials_hf",
                  "rx_imci_hf",
                  "rx_creams_hf",
                  "rx_consumables_hf",
                  "rx_type_hf")

  df <- timci::format_multiselect_asws(df, multi_cols, sep)
  df

}
