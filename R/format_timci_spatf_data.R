#' Process healthcare provider interview data (TIMCI-specific function)
#'
#' @param df dataframe containing the (raw) ODK data collected in the SPA healthcare provider interview form
#' @return This function returns a formatted dataframe for future display and analysis.
#' @export

process_spa_hcpi_data <- function(df) {

  # Match column names with names from dictionary
  df <- match_from_xls_dict(df, "spa_hcpi_dict.xlsx")

}

#' Process sick child observation data (TIMCI-specific function)
#'
#' @param df dataframe containing the (raw) ODK data collected in the SPA sick child interview observation protocol form
#' @return This function returns a formatted dataframe for future display and analysis.
#' @export

process_spa_sco_data <- function(df) {

  print("test")

}

#' Load the Time-Flow Dictionary Adapted for Each Country (TIMCI-specific function)
#'
#' This function imports a country-specific Excel dictionary, which is adapted for each
#' country to account for differences in data collection processes. The dictionary is
#' intended to be used for converting data exported from ODK Central into a format that is
#' more suitable for statistical analysis
#'
#' @return This function returns a dataframe that will be used as a dictionary to convert data exported from ODK Central to a statistician-friendly format.
#' @export

read_tf_xls_dict <- function() {

  dictionary <- import_country_specific_xls_dict("tf_dict.xlsx",
                                                 country = Sys.getenv('TIMCI_COUNTRY'))
  dictionary

}

#' Extract Time-Flow Data (TIMCI-specific function)
#'
#' This function takes a data frame containing raw time-flow data and extracts the relevant
#' columns according to the Time-Flow Dictionary. The Time-Flow Dictionary is obtained using
#' the \code{\link{read_tf_xls_dict}} function.
#'
#' @param df data frame containing the raw time-flow data
#' @return A data frame containing time-flow data, as specified by the Time-Flow Dictionary.
#' @export
#' @import dplyr magrittr

extract_tf_data <- function(df) {

  dictionary <- timci::read_tf_xls_dict()

  if ("matched" %in% colnames(df)) {
    cols <- c(dictionary$new,
              "matched")
  }

  df[cols]

}

#' Match facility data using the Time-Flow dictionary adapted for each country to account for differences in the data collection (TIMCI-specific function)
#'
#' @param df dataframe
#' @return This function returns a dataframe with columns that match the specified country dictionary.
#' @export

match_from_tf_xls_dict <- function(df) {

  # Import dictionary
  dictionary <- timci::import_country_specific_xls_dict("tf_dict.xlsx",
                                                        Sys.getenv('TIMCI_COUNTRY'))

  # Match column names with names from dictionary
  df %>%
    timci::match_from_dict(dictionary)

}
