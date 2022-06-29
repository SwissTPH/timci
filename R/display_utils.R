#' Format string to header of level H1 for printing in console
#'
#' This function formats an input string `s` for printing as a header of level H1 in an output console
#'
#' @param s String to format
#' @export

formats2h1 <- function(s) {

  paste0("\n============================================================================================\n",
         s,
         "\n============================================================================================\n")

}

#' Format string to header of level H2 for printing in console
#'
#' This function formats an input string `s` for printing as a header of level H2 in an output console
#'
#' @param s String to format
#' @export

formats2h2 <- function(s) {

  paste0("\n--------------------------------------------------------------------------------------------\n",
         s,
         "\n--------------------------------------------------------------------------------------------\n")

}

#' Format string to header of level H3 for printing in console
#'
#' This function formats an input string `s` for printing as a header of level H2 in an output console
#'
#' @param s String to format
#' @export

formats2h3 <- function(s) {

  paste0(s,
         "\n--------------------------------------------")

}

#' Format display number of rows
#'
#' @param s_before String displayed before the number of rows
#' @param df Dataframe
#' @param s_after String displayed after the number of rows
#' @export

format_nrow <- function(s_before,
                        df,
                        s_after) {

  if(!is.null(df)){
    cat(paste0(s_before, " **", nrow(df), "** ", s_after))
  }

}
