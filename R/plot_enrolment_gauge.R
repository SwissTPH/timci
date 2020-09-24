#' Plot enrolment gauge
#'
#' This function plots a gauge representing enrolment.
#'
#' Description of plot_enrolment_gauge
#' @param m Maximal gauge value
#' @param s Sucess
#' @param w Warning
#' @param d Danger
#' @return This function plots a gauge representing enrolment.
#' @export

plot_enrolment_gauge <- function(m, s, w, d){

  gauge(sum(data$x), min = 0, max = m, sectors = gaugeSectors(success = s, warning = w, danger = d))

}
