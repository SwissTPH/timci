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
                 "n1-o3_1a")
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
  if (Sys.getenv("TIMCI_COUNTRY") == 'India'){
    day7_df$date_day0 <- ifelse(day7_df$date_day0 == "ENROLMENT DATE",
                                NA,
                                day7_df$date_day0)
  }
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

  # Set the dictionary to be used
  day28_dict <- "day28_dict.xlsx"
  if(Sys.getenv('TIMCI_COUNTRY') == 'Tanzania'){
    day28_dict <- "day28_dict_tanzania.xlsx"
  }

  # Replace the space between different answers by a semicolon in multiple select questions
  sep <- ";"
  multi_cols = c("n1_o3_1a",
                 "n1_o3_1b",
                 "n1_o3_2b",
                 "n1-o3_1a")
  df <- format_multiselect_asws(df, multi_cols, sep)

  # Separate submissions that relate to complete Day 28 follow-up and unsuccessful attempts
  successful_day28_df <- df[df$proceed == 1,]
  fail_day28_df <- df[df$proceed == 0,]

  # Match column names with names from dictionary
  dictionary <- readxl::read_excel(system.file(file.path('extdata', day28_dict), package = 'timci'))
  sub <- subset(dictionary, deidentified == 1)

  day28_df <- match_from_xls_dict(df, day28_dict)
  day28_df <- day28_df[sub$new]

  # Format dates
  day28_df$date_death_day28 <- strftime(day28_df$date_death_day28,"%Y-%m-%d")

  day28_df <- day28_df %>%
    dplyr::mutate(days = as.Date(date_call) - as.Date(date_day0), na.rm = TRUE)

  successful_day28_df <- match_from_xls_dict(successful_day28_df, day28_dict)
  successful_day28_df <- successful_day28_df[sub$new]
  successful_day28_df <- successful_day28_df %>%
    dplyr::mutate(days = as.Date(date_call) - as.Date(date_day0), na.rm = TRUE)

  fail_day28_df <- match_from_xls_dict(fail_day28_df, day28_dict)

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
