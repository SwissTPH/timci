#' Detect ID duplicates (TIMCI-specific function)
#'
#' @param df dataframe containing the processed facility data
#' @return This function returns a vector containing IDs of duplicate IDs
#' @export
#' @import dplyr magrittr

detect_id_duplicates <- function(df) {

  vec <- df$child_id
  vec[duplicated(vec)]

}

#' Detect name duplicates (TIMCI-specific function)
#'
#' @param df dataframe containing the processed facility data
#' @return This function returns a vector containing IDs and names of duplicate names
#' @export
#' @import dplyr magrittr

detect_name_duplicates <- function(df) {

  # Exact (case-insensitive) duplicates
  df <- dplyr::mutate(df,
                      full_name = paste(fs_name, ls_name, sep = ' '))
  df1 <- df[c("child_id", "full_name")]
  qc1 <- df1[duplicated(tolower(df1$full_name)),]

  # Switched (case-insensitive) names
  df2 <- dplyr::mutate(df,
                      full_name = paste(ls_name, fs_name, sep = ' '))
  df2 <- df2[c("child_id", "full_name")]
  df2 <- rbind(df1, df2)
  qc2 <- df2[duplicated(tolower(df2$full_name)),]

  list(qc1, qc2)

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
