#' Format summary table to have a global descriptive statistics on the first row and a descriptive statistics by group in subsequent rows
#'
#' @param all Dataframe
#' @param stat_list List of lists of formulea for summarizing the dataset `all`
#' @param gvar Grouping variable
#' @return This function returns a statistical summary table ready to be printed
#' @export

format_summary_table <- function(all, stat_list, gvar = NULL){

  stat_all <- qwraps2::summary_table(x = all,
                                     summaries = stat_list)
  out <- stat_all
  # Handling NULL argument `gvar`
  if (!missing(gvar)) {
    # Quote argument `gvar`
    gvar <- dplyr::enquo(gvar)
    # Change quosure `gvar` into a string for checking if the column exists
    if (dplyr::quo_name(gvar) %in% colnames(all)) {
      # Unquote the grouping variable `gvar` to pass to `group_by`
      stat_group <- qwraps2::summary_table(x = dplyr::group_by(all, !!gvar),
                                           summaries = stat_list)
      out <- t(cbind(stat_all, stat_group))
    }
  }
  out

}
