#' Deidentify data (TIMCI-specific function)
#'
#' De-identification of the TIMCI research data
#' @param df dataframe containing the processed facility data
#' @param mapping dataframe containing the mapping of the original / hashed IDs
#' @return This function returns de-identified data.
#' @export
#' @import magrittr dplyr readxl

deidentify_data <- function(df, mapping = NULL) {

  if (is.null(mapping)) {
    mapping <- data.frame(old = df$child_id)

    # De-identification
    df$country_id <- ifelse(df$child_id != '',
                            substr(df$child_id, 1, 1),
                            substr(df$prev_id, 1, 1))
    df$hf_id <- ifelse(df$child_id != '',
                       substr(df$child_id, 3, 7),
                       substr(df$prev_id, 3, 7))
    df$child_id <- ifelse(df$child_id != '',
                          anonymise_dataframe(df, 'child_id'),
                          '')

    mapping$new <- df$child_id
    fn <- timci::export_df2xlsx(mapping, getwd(), "mapping")
  } else {
    mapping <- data.frame(old = df$child_id)

    # De-identification
    df$country_id <- ifelse(df$child_id != '',
                            substr(df$child_id, 1, 1),
                            substr(df$prev_id, 1, 1))
    df$hf_id <- ifelse(df$child_id != '',
                       substr(df$child_id, 3, 7),
                       substr(df$prev_id, 3, 7))
    df$child_id <- ifelse(df$child_id != '',
                          anonymise_dataframe(df, 'child_id'),
                          '')

    mapping$new <- df$child_id
    fn <- timci::export_df2xlsx(mapping, getwd(), "mapping")
  }

  if ("prev_id" %in% colnames(df)) {

    df$prev_id <- ifelse(df$prev_id != '',
                         anonymise_dataframe(df, 'prev_id'),
                         '')

  }

  df <- dplyr::relocate(df, 'country_id')
  df <- dplyr::relocate(df, 'hf_id', .after = 'country_id')
  df <- dplyr::relocate(df, 'child_id', .after = 'hf_id')

}

#' Deidentify SPA data (TIMCI-specific function)
#'
#' De-identification of the TIMCI research data
#' @param df dataframe containing the processed facility data
#' @param mapping dataframe containing the mapping of the original / hashed IDs
#' @return This function returns de-identified data.
#' @export
#' @import magrittr dplyr readxl

deidentify_spa_data <- function(df, mapping = NULL) {

  # De-identification
  df$child_identification_pid <- ifelse(df$child_identification_pid != '',
                                        anonymise_dataframe(df, 'child_identification_pid'),
                                        '')

  df <- dplyr::relocate(df, 'child_identification_pid')

}
