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
