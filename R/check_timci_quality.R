#' Detect duplicate participants (TIMCI-specific function)
#'
#' @param df dataframe containing the processed facility data
#' @return This function returns a dataframe containing data of possible duplicate participants only
#' @export
#' @import dplyr magrittr

detect_duplicate_participants <- function(df) {

  # To complete
  df[duplicated(df)]

}

#' Detect inconsistent dates (TIMCI-specific function)
#'
#' @param df dataframe containing the processed facility data
#' @return This function returns a dataframe containing data of possible duplicate participants only
#' @export
#' @import dplyr magrittr

detect_inconsistent_dates <- function(df) {

  # To complete

}

# Same patient ID from different facilities
# First name is not consistent
# First name match is below … %
# First name match is below … %, while taking into account local phonetic variability
# Last name is not consistent
# First and last names are switched
# Middle name is not consistent
# Sex is not consistent
# Year of birth is not consistent
# Month of birth is not consistent
# Day of birth is not consistent
# Month and day of birth are switched
# Identity score is below … %

#Baseline visit identification
#New visit anterior to last visit received
