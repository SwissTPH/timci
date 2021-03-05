#' Format ODK data
#'
#' @param df dataframe containing the non de-identified (raw) ODK data, assuming standard metadata fields (`today`, `start`, `end`) are present.
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
