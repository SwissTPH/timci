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

#' Extract data from ODK zip
#'
#' @param odk_zip The absolute path to the zip file named "`fid`.zip" containing ODK submissions as CSV, plus separate CSVs for any repeating groups, plus any attachments in a subfolder `media`
#' @return This function returns a dataframe.
#' @export
#' @import magrittr dplyr

extract_data_from_odk_zip <- function(odk_zip) {

  # Add function body

}
