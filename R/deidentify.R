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
    df$fid <- ifelse(df$child_id != '',
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
    df$fid <- ifelse(df$child_id != '',
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

  df <- df %>%
    dplyr::relocate('country_id') %>%
    dplyr::relocate('fid', .after = 'country_id') %>%
    dplyr::relocate('child_id', .after = 'fid')

}

#' De-identify SPA data (TIMCI-specific Function)
#'
#' This function performs de-identification of the TIMCI SPA observation data.
#'
#' @param df A dataframe containing the processed facility data. This should include a column for child identification numbers that will be anonymized.
#' @param mapping Optional. A dataframe containing the mapping between original and hashed IDs. If provided, it will be used for anonymization; otherwise, IDs are anonymized without a predefined mapping.
#' @return Returns a dataframe with de-identified data. The 'child_identification_pid' column is anonymized to protect patient privacy.
#' @export
#' @import magrittr dplyr readxl

deidentify_spa_data <- function(df, mapping = NULL) {

  # De-identification
  df$`child_identification-pid` <- ifelse(df$`child_identification-pid` != '',
                                          anonymise_dataframe(df, 'child_identification-pid'),
                                          '')
  df$KEY <- ifelse(df$KEY != '',
                   anonymise_dataframe(df, 'KEY'),
                   '')

  dropped <- c("meta-instanceID",
               "deviceid",
               "hcp_identification-hcp_code_scan",
               "hcp_identification-hcp_code_manual",
               "child_identification-child_code_scan",
               "child_identification-child_code_manual",
               "meta-instanceName",
               "DeviceID")
  df <- df %>%
    dplyr::relocate(`child_identification-pid`) %>%
    dplyr::relocate('KEY', .after = `child_identification-pid`) %>%
    dplyr::select(-dropped)

}

#' De-identify SPA treatment data (TIMCI-specific Function)
#'
#' This function de-identifies treatment data within the TIMCI research dataset, focusing on anonymizing treatment identifiers to ensure privacy.
#'
#' @param df A dataframe containing the processed treatment data, which includes a 'PARENT_KEY' column for treatment identifiers that need to be anonymized.
#' @param mapping Optional. A dataframe containing the mapping between original and hashed IDs. If provided, it uses this mapping for anonymization; otherwise, the function anonymizes IDs without a predefined mapping.
#' @return Returns a dataframe with de-identified treatment data. The 'PARENT_KEY' column is anonymized to protect individual treatment records' privacy.
#' @export
#' @import magrittr dplyr readxl

deidentify_spa_rx_data <- function(df, mapping = NULL) {

  # De-identification
  df$PARENT_KEY <- ifelse(df$PARENT_KEY != '',
                          anonymise_dataframe(df, 'PARENT_KEY'),
                          '')

  df <- df %>%
    dplyr::relocate('PARENT_KEY')

}
