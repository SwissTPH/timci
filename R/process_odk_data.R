#' Format ODK data
#'
#' Description of format_odk_data
#' @param data dataframe containing the current ODK data
#' @return This function returns a formatted dataframe for future display and analysis.
#' @export
#' @import magrittr dplyr

format_odk_data <- function(data) {

  data$today <- strftime(data$today,"%Y-%m-%d")
  data$duration <- as.integer(round(data$end - data$start, digits=0))
  data$start <- strftime(data$start,"%T")
  data$end <- strftime(data$end,"%T")

  cp <- data %>%
    dplyr::rename('date' = 'today')

}
