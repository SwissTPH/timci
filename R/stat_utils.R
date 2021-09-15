#' Process weekly facility assessment data (TIMCI-specific function)
#'
#' @param all dataframe
#' @param varname variable name
#' @param gname group name
#' @return This function returns a summary table with.
#' @export

summary_table_disp <- function(all, varname, gname) {

  s_all <- qwraps2::summary_table(all, var)
  s_group <- qwraps2::summary_table(dplyr::group_by(all, gname), varname)
  print(t(cbind(s_all, s_group)))

}
