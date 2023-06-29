#' Detect specific string in free text entries where structured entry is missing (TIMCI-specific function)
#'
#' @param df dataframe
#' @param col_val column name
#' @param col_text column name
#' @param vec vector of text to test
#' @return This function returns a dataframe containing only participants with no information in column `col`
#' @export
#' @import dplyr rlang

detect_drug <- function(df,
                        col_val,
                        col_text,
                        vec) {

  out <- NULL
  col_val <- rlang::sym(col_val)
  col_text <- rlang::sym(col_text)

  if ( timci::is_not_empty(df) ) {

    out <- df %>%
      dplyr::filter(any(vec %in% strsplit(tolower(col_text), " ")[[1]]),
                    col_val == 0)

  }

  out

}
