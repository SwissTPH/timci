#' Format ODK data
#'
#' Description of format_odk_data
#' @param data ODK data
#' @return This function formats ODK data.
#' @export

format_odk_data <- function(data) {

  data$today <- strftime(data$today,"%Y-%m-%d")
  data$duration <- as.integer(round(data$end - data$start, digits=0))
  data$start <- strftime(data$start,"%T")
  data$end <- strftime(data$end,"%T")

  return(data)

}
