#' Process data from the ODK Central server
#'
#' This function processes ODK data.
#'
#' Description of get ODK data
#' @param data ODK data
#' @return This function processes ODK data.
#' @export

process_odk_data <- function(data) {

  data$today <- strftime(data$today,"%Y-%m-%d")
  data$duration <- as.integer(round(data$end - data$start, digits=0))
  data$start <- strftime(data$start,"%T")
  data$end <- strftime(data$end,"%T")
  return(data)

}
