#' Process day 7 follow-up data (TIMCI-specific function)
#'
#' @param df dataframe containing the non de-identified (raw) ODK data collected during the Day 7 follow-up call
#' @param is_deidentified Boolean, flag to not export personally identifiable variable (default set to `TRUE`)
#' @return This function returns a formatted dataframe for future display and analysis.
#' @export
#' @import dplyr lubridate anytime

format_day7_data <- function(df,
                             is_deidentified = TRUE) {

  # Replace the space between different answers by a semicolon in multiple select questions
  sep <- ";"
  multi_cols = c("n1-o3_1a",
                 "n1-o3_1b",
                 "n1-o3_2b",
                 "n1-o3_3")
  df <- timci::format_multiselect_asws(df, multi_cols, sep)

  # Match column names with names from dictionary
  day7_df <- timci::match_from_filtered_xls_dict(df,
                                                 "day7_dict.xlsx",
                                                 is_deidentified = is_deidentified,
                                                 country = Sys.getenv('TIMCI_COUNTRY'))

  # Format day 0 date
  day7_df$date_day0 <- anytime::anydate(day7_df$date_day0)
  day7_df$date_day0 <- strftime(day7_df$date_day0,"%Y-%m-%d")

  # Calculate number of days between enrollment and follow-up
  day7_df <- day7_df %>%
    dplyr::mutate(days = as.Date(date_call) - as.Date(date_day0), na.rm = TRUE)

  # Format death date
  day7_df$date_death_day7 <- strftime(day7_df$date_death_day7,"%Y-%m-%d")

  # Separate submissions that relate to complete Day 7 follow-up and unsuccessful attempts
  successful_day7_df <- day7_df[day7_df$proceed_day7 == 1,]
  fail_day7_df <- day7_df[day7_df$proceed_day7 == 0,]

  list(successful_day7_df, fail_day7_df, day7_df)

}

#' Process day 28 follow-up data (TIMCI-specific function)
#'
#' @param df dataframe containing the non de-identified (raw) ODK data collected during the Day 28 follow-up call
#' @param is_deidentified Boolean, flag to not export personally identifiable variable (default set to `TRUE`)
#' @return This function returns a formatted dataframe for future display and analysis.
#' @export
#' @import dplyr

format_day28_data <- function(df,
                              is_deidentified = TRUE) {

  # Set the dictionary to be used
  day28_dict <- "day28_dict.xlsx"
  if(Sys.getenv('TIMCI_COUNTRY') == 'Tanzania'){
    day28_dict <- "day28_dict_tanzania.xlsx"
  }

  # Replace the space between different answers by a semicolon in multiple select questions
  sep <- ";"
  multi_cols = c("n1-o3_1a")
  df <- format_multiselect_asws(df, multi_cols, sep)

  # Match column names with names from dictionary
  day28_df <- timci::match_from_filtered_xls_dict(df,
                                                  "day28_dict.xlsx",
                                                  is_deidentified = is_deidentified,
                                                  country = Sys.getenv('TIMCI_COUNTRY'))

  # Format death date
  day28_df$date_death_day28 <- strftime(day28_df$date_death_day28, "%Y-%m-%d")

  # Format day 0 date
  day28_df$date_day0 <- anytime::anydate(day28_df$date_day0)
  day28_df$date_day0 <- strftime(day28_df$date_day0,"%Y-%m-%d")
  day28_df <- day28_df %>%
    dplyr::mutate(days = as.Date(date_call) - as.Date(date_day0), na.rm = TRUE)

  # Separate submissions that relate to complete Day 28 follow-up and unsuccessful attempts
  successful_day28_df <- day28_df[day28_df$proceed_day28 == 1,]
  fail_day28_df <- day28_df[day28_df$proceed_day28 == 0,]

  list(successful_day28_df, fail_day28_df, day28_df)

}

#' Clean follow-up data for estimating rough follow-up rate (TIMCI-specific function)
#'
#' @param day0_df dataframe containing the non de-identified (raw) ODK data collected at Day 0
#' @param fu_df dataframe containing follow-up data that correspond to successful (either Day 7 Day 28) calls
#' @return out
#' @import dplyr
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
