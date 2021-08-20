#' Process healthcare provider interview data (TIMCI-specific function)
#'
#' @param df dataframe containing the (raw) ODK data collected in the healthcare provider interview
#' @return This function returns a formatted dataframe for future display and analysis.
#' @export

process_spa_hcpi_data <- function(df) {

  # Match column names with names from dictionary
  df <- match_from_xls_dict(df, "spa_hcpi_dict.xlsx")

}
