#' Edit non-valid facilities in Day 0 data entries (TIMCI-specific function)
#'
#' @param df dataframe
#' @return This function returns a list that contains a dataframe with corrections and the list of edits
#' @import dplyr
#' @export

correct_day0_non_valid_facilities <- function(df) {

  csv_filename <- case_when(Sys.getenv('TIMCI_COUNTRY') == 'Tanzania' ~ "day0_non_valid_facility_correction_tanzania.csv",
                            Sys.getenv('TIMCI_COUNTRY') == 'Kenya' ~ "day0_non_valid_facility_correction_kenya.csv",
                            TRUE ~ "")

  out <- list(df,NULL)
  if ( csv_filename != "" ) {
    csv_pathname <- system.file(file.path('extdata', 'cleaning', csv_filename), package = 'timci')
    edits <- readr::read_csv(csv_pathname, show_col_types = FALSE)
    df <- df %>%
      merge(edits[, c("old_child_id", "uuid", "new_child_id")],
            by.x = c("child_id", "uuid"),
            by.y = c("old_child_id", "uuid"),
            all.x = TRUE)
    df$child_id <- ifelse(is.na(df$new_child_id), df$child_id, df$new_child_id)
    df$fid <- ifelse(is.na(df$new_child_id), df$fid, substr(df$new_child_id, 3,7))
    if("fid_from_device" %in% colnames(df))
    {
      df$fid_from_device <- ifelse(is.na(df$new_child_id), df$fid_from_device, substr(df$new_child_id, 3,7))
    }

    # Remove the column new_child_id from the dataframe
    drop <- c("new_child_id")
    df <- df[,!(names(df) %in% drop)]

    out <- list(df, edits)
  }
  out

}

#' Edit incorrect child IDs in Day 0 data entries (TIMCI-specific function)
#' This function can be used to correct documented child ID duplicates, incorrect facility codes or typos
#'
#' @param df dataframe
#' @return This function returns a list that contains an edited dataframe and the list of edits
#' @import dplyr
#' @export

edit_day0_child_ids <- function(df) {

  csv_filename <- case_when(Sys.getenv('TIMCI_COUNTRY') == 'Tanzania' ~ "day0_duplicate_correction_tanzania.csv",
                            Sys.getenv('TIMCI_COUNTRY') == 'Kenya' ~ "day0_duplicate_correction_kenya.csv",
                            Sys.getenv('TIMCI_COUNTRY') == 'Senegal' ~ "day0_duplicate_correction_senegal.csv",
                            Sys.getenv('TIMCI_COUNTRY') == 'India' ~ "day0_duplicate_correction_india.csv",
                            TRUE ~ "")

  out <- list(df, NULL)
  if ( csv_filename != "" ) {

    csv_pathname <- system.file(file.path('extdata', 'cleaning', csv_filename), package = 'timci')
    edits <- readr::read_csv(csv_pathname, show_col_types = FALSE)

    found_edits <- edits[, c("old_child_id", "uuid", "new_child_id")] %>%
      merge(df[, c("child_id", "uuid")],
            by.x = c("old_child_id", "uuid"),
            by.y = c("child_id", "uuid"),
            all.x = FALSE,
            all.y = FALSE)

    df <- df %>%
      merge(edits[, c("old_child_id", "uuid", "new_child_id")],
            by.x=c("child_id", "uuid"),
            by.y=c("old_child_id", "uuid"),
            all.x=TRUE)
    df$child_id <- ifelse(is.na(df$new_child_id),
                          df$child_id,
                          df$new_child_id)
    df$child_id <- as.character(df$child_id)
    df$fid <- ifelse(is.na(df$new_child_id), df$fid, substr(df$new_child_id, 3,7))
    if("fid_from_device" %in% colnames(df))
    {
      df$fid_from_device <- ifelse(is.na(df$new_child_id), df$fid_from_device, substr(df$new_child_id, 3,7))
    }

    # Remove the column new_child_id from the dataframe
    drop <- c("new_child_id")
    df <- df[,!(names(df) %in% drop)]

    out <- list(df, found_edits)
  }
  out

}

#' Edit incorrect child IDs in Day 0 data entries (TIMCI-specific function)
#' This function can be used to correct documented child ID duplicates, incorrect facility codes or typos
#'
#' @param df dataframe
#' @return This function returns a list that contains an edited dataframe and the list of edits
#' @import dplyr
#' @export

edit_day0_to_repeat <- function(df) {

  csv_filename <- case_when(Sys.getenv('TIMCI_COUNTRY') == 'Tanzania' ~ "day0_repeat_correction_tanzania.csv",
                            TRUE ~ "")

  out <- list(df, NULL)
  if ( csv_filename != "" ) {

    csv_pathname <- system.file(file.path('extdata', 'cleaning', csv_filename), package = 'timci')
    edits <- readr::read_csv(csv_pathname, show_col_types = FALSE)

    found_edits <- edits[, c("old_child_id", "uuid")] %>%
      merge(df[, c("child_id", "uuid")],
            by.x = c("old_child_id", "uuid"),
            by.y = c("child_id", "uuid"),
            all.x = FALSE,
            all.y = FALSE)

    df <- df %>%
      merge(edits[, c("old_child_id", "uuid", "new_child_id")],
            by.x=c("child_id", "uuid"),
            by.y=c("old_child_id", "uuid"),
            all.x=TRUE)
    df$prev_enrl <- ifelse(is.na(df$new_child_id),
                           df$prev_enrl,
                           1)
    df$prev_id <- ifelse(is.na(df$new_child_id),
                         df$prev_id,
                         df$child_id)
    df$prev_hf_name_card <- ifelse(is.na(df$new_child_id),
                                   df$prev_hf_name_card,
                                   df$facility)
    df$repeat_consult <- as.integer( ifelse(is.na(df$new_child_id),
                                            df$repeat_consult,
                                            1) )
    df$consent <- ifelse(is.na(df$new_child_id),
                         df$consent,
                         NA)
    df$enrolled <- ifelse(is.na(df$new_child_id),
                          df$enrolled,
                          NA)
    df$child_id_scan <- as.integer( ifelse(is.na(df$new_child_id),
                                           df$child_id_scan,
                                           0) )
    df$child_id_manual <- as.integer( ifelse(is.na(df$new_child_id),
                                             df$child_id_manual,
                                             0) )

    # Remove the column new_child_id from the dataframe
    drop <- c("new_child_id")
    df <- df[,!(names(df) %in% drop)]

    out <- list(df, found_edits)
  }
  out

}

#' Drop incorrect child IDs in Day 0 data entries (TIMCI-specific function)
#' This function can be used to drop documented child IDs
#'
#' @param df dataframe
#' @return This function returns a list that contains a cleaned dataframe and the list of dropped records
#' @import dplyr
#' @export

delete_day0_records <- function(df) {

  csv_filename <- case_when(Sys.getenv('TIMCI_COUNTRY') == 'Tanzania' ~ "day0_drop_dummy_tanzania.csv",
                            TRUE ~ "")

  out <- list(df, NULL)
  if ( csv_filename != "" ) {

    csv_pathname <- system.file(file.path('extdata', 'cleaning', csv_filename), package = 'timci')
    records_to_drop <- readr::read_csv(csv_pathname, show_col_types = FALSE)

    found_records <- records_to_drop %>%
      merge(df[, c("child_id", "uuid")],
            by.x = c("old_child_id", "uuid"),
            by.y = c("child_id", "uuid"),
            all.x = FALSE,
            all.y = FALSE)
    df <- df[!(df$uuid %in% records_to_drop$uuid), ]

    out <- list(df, found_records)
  }
  out

}

#' Edit Day 0 data for all errors that were detected by quality checks (TIMCI-specific function)
#'
#' @param df dataframe
#' @return This function returns an edited dataframe with corrections
#' @import dplyr
#' @export

correct_day0_all <- function(df) {

  # Correct incorrect facility of enrolment
  df <- timci::correct_day0_non_valid_facilities(df)[[1]]
  # Delete dummy/test data
  df <- timci::delete_day0_records(df)[[1]]
  # Correct duplicated child IDs
  df <- timci::edit_day0_child_ids(df)[[1]]

  if (Sys.getenv("TIMCI_COUNTRY") == "Kenya") {
    out <- timci::detect_inconsistent_dates(df,
                                            "submission_date",
                                            "start",
                                            cleaning = "replace_by_start_date")
    df <- out[[2]]
  }

  df

}

#' Correct Day 7 duplicates (TIMCI-specific function)
#'
#' @param df dataframe
#' @return This function returns a list that contains a dataframe with corrections and the list of edits
#' @import dplyr
#' @export

correct_day7_duplicates <- function(df) {

  csv_filename <- NULL
  if (Sys.getenv('TIMCI_COUNTRY') == 'Kenya') {
    csv_filename <- "day7_duplicate_correction_kenya.csv"
  }

  out <- list(df,NULL)
  if (!is.null(csv_filename)) {
    csv_pathname <- system.file(file.path('extdata', 'cleaning', csv_filename), package = 'timci')
    edits <- readr::read_csv(csv_pathname)
    if ("a1-pid" %in% colnames(df))
    {
      df <- df %>%
        merge(edits[, c("old_child_id", "uuid", "new_child_id")],
              by.x = c("a1-pid", "meta-instanceID"),
              by.y = c("old_child_id", "uuid"),
              all.x=TRUE)
      df$"a1-pid" <- ifelse(is.na(df$new_child_id), df$"a1-pid", df$new_child_id)
      df$"a1-fid" <- ifelse(is.na(df$new_child_id), df$"a1-fid", substr(df$new_child_id, 3,7))
    } else if ("child_id" %in% colnames(df))
    {
      df <- df %>%
        merge(edits[, c("old_child_id", "uuid", "new_child_id")],
              by.x = c("child_id", "uuid"),
              by.y = c("old_child_id", "uuid"),
              all.x=TRUE)
      df$child_id <- ifelse(is.na(df$new_child_id), df$child_id, df$new_child_id)
      df$fid <- ifelse(is.na(df$new_child_id), df$fid, substr(df$new_child_id, 3,7))
    }

    # Remove the column new_child_id from the dataframe
    drop <- c("new_child_id")
    df <- df[,!(names(df) %in% drop)]

    out <- list(df, edits)
  }
  out

}

#' Edit Day 7 follow-up data for all errors that were detected by quality checks (TIMCI-specific function)
#'
#' @param df dataframe
#' @return This function returns an edited dataframe with corrections
#' @import dplyr
#' @export

correct_day7_all <- function(df) {

  # Correct duplicated child IDs
  df <- timci::correct_day7_duplicates(df)[[1]]

}

#' Edit incorrect healthcare provider (HCP) IDs in SPA sick child observation entries (TIMCI-specific function)
#' This function can be used to correct documented HCP IDs
#'
#' @param df dataframe
#' @return This function returns a list that contains a dataframe with corrections and the list of edits
#' @import dplyr
#' @export

correct_spa_sco_hcp_ids <- function(df) {

  csv_filename <- case_when(Sys.getenv('TIMCI_COUNTRY') == 'Kenya' ~ "spa_sco_hcp_correction_kenya.csv",
                            TRUE ~ "")

  out <- list(df, NULL)
  if ( csv_filename != "" ) {
    csv_pathname <- system.file(file.path('extdata', 'cleaning', csv_filename), package = 'timci')
    edits <- readr::read_csv(csv_pathname)
    df <- df %>%
      merge(edits[, c("old_hcp_id", "uuid", "new_hcp_id")],
            by.x = c("hcp_identification-hcpid", "meta-instanceID"),
            by.y = c("old_hcp_id", "uuid"),
            all.x = TRUE)
    df$"hcp_identification-hcpid" <- ifelse(is.na(df$new_hcp_id), df$"hcp_identification-hcpid", df$new_hcp_id)

    # Remove the column new_child_id from the dataframe
    drop <- c("new_hcp_id")
    df <- df[,!(names(df) %in% drop)]

    out <- list(df, edits)
  }
  out

}

#' Edit incorrect facility IDs in SPA sick child observation entries (TIMCI-specific function)
#' This function can be used to correct documented HCP IDs
#'
#' @param df dataframe
#' @return This function returns a list that contains a dataframe with corrections and the list of edits
#' @import dplyr
#' @export

correct_spa_sco_fids <- function(df) {

  csv_filename <- case_when(Sys.getenv('TIMCI_COUNTRY') == 'Kenya' ~ "spa_sco_facility_correction_kenya.csv",
                            TRUE ~ "")

  out <- list(df, NULL)
  if ( csv_filename != "" ) {
    csv_pathname <- system.file(file.path('extdata', 'cleaning', csv_filename), package = 'timci')
    edits <- readr::read_csv(csv_pathname)
    df <- df %>%
      merge(edits[, c("old_fid", "uuid", "new_fid")],
            by.x = c("facility_identification-fcode", "meta-instanceID"),
            by.y = c("old_fid", "uuid"),
            all.x = TRUE)
    df$"facility_identification-fcode" <- ifelse(is.na(df$new_fid),
                                                 df$"facility_identification-fcode",
                                                 df$new_fid)

    # Remove the column new_child_id from the dataframe
    drop <- c("new_fid")
    df <- df[,!(names(df) %in% drop)]

    out <- list(df, edits)
  }
  out

}

#' Edit SPA sick child observation data for all errors that were detected by quality checks (TIMCI-specific function)
#'
#' @param df dataframe
#' @return This function returns an edited dataframe with corrections
#' @import dplyr
#' @export

correct_spa_sco_all <- function(df) {

  # Edit incorrect HCP IDs
  df <- timci::correct_spa_sco_hcp_ids(df)[[1]]

  # Edit incorrect facility IDs
  df <- timci::correct_spa_sco_fids(df)[[1]]

  df

}

#' Edit drug data in Day 0 data entries (TIMCI-specific function - Kenya and Senegal only)
#'
#' @param day0_df dataframe that contains Day 0 data and needs to be corrected
#' @param drug_df dataframe that contains corrected (structured) drug data to edit in Day 0 data
#' @return This function returns a list that contains a dataframe with corrections and the list of edits
#' @import dplyr
#' @export

correct_day0_drug_data <- function(day0_df,
                                   drug_df) {

  # Remove columns from drug_df for processing
  drop <- c("start",
            "end",
            "free_text1",
            "free_text2",
            "rx_type2",
            "rx_othtype2",
            "rx_type_hf2",
            "rx_othtype_hf2",
            "child_id")
  drug_df1 <- drug_df[,!(names(drug_df) %in% drop)]

  cols <- colnames(day0_df)
  if ("rx_antibio_oth" %in% cols) {
    day0_df$rx_antibio_oth <- as.character(day0_df$rx_antibio_oth)
  }
  if ("rx_antibio_oth_hf" %in% cols) {
    day0_df$rx_antibio_oth_hf <- as.character(day0_df$rx_antibio_oth_hf)
  }
  day0_df$rx_antimalarials <- as.character(day0_df$rx_antimalarials)
  day0_df$rx_antimalarials_hf <- as.character(day0_df$rx_antimalarials_hf)
  if ("rx_consumables" %in% cols) {
    day0_df$rx_consumables <- as.character(day0_df$rx_consumables)
  }
  if ("rx_consumables_hf" %in% cols) {
    day0_df$rx_consumables_hf <- as.character(day0_df$rx_consumables_hf)
  }

  # Update blank (NA) drug values in df with values entered in the drug dataframe
  df <- day0_df %>%
    dplyr::rows_patch(drug_df1,
                      by = 'uuid',
                      unmatched = "ignore")

  # Replace 0 values in df if values entered in the drug dataframe is equal to 1
  # cols <- colnames(df)
  # colnames(drug_df1) <- paste0(colnames(drug_df1),"1")
  # df <- df %>%
  #   merge(drug_df1,
  #         by.x = "uuid",
  #         by.y = "uuid1",
  #         all.x = TRUE) %>%
  #   dplyr::mutate(rx_amoxicillin = ifelse(rx_amoxicillin == 0 & rx_amoxicillin1 == 1, 1, rx_amoxicillin)) %>%
  #   dplyr::mutate(rx_amoxicillin_hf = ifelse(rx_amoxicillin_hf == 0 & rx_amoxicillin_hf1 == 1, 1, rx_amoxicillin_hf)) %>%
  #   dplyr::mutate(rx_penicillinG = ifelse(rx_penicillinG == 0 & rx_penicillinG1 == 1, 1, rx_penicillinG)) %>%
  #   dplyr::mutate(rx_penicillinG_hf = ifelse(rx_penicillinG_hf == 0 & rx_penicillinG_hf1 == 1, 1, rx_penicillinG_hf)) %>%
  #   dplyr::mutate(rx_ceftriaxone = ifelse(rx_ceftriaxone == 0 & rx_ceftriaxone1 == 1, 1, rx_ceftriaxone)) %>%
  #   dplyr::mutate(rx_ceftriaxone_hf = ifelse(rx_ceftriaxone_hf == 0 & rx_ceftriaxone_hf1 == 1, 1, rx_ceftriaxone_hf)) %>%
  #   dplyr::mutate(rx_cef_antibiotics = ifelse(rx_cef_antibiotics == 0 & rx_cef_antibiotics1 == 1, 1, rx_cef_antibiotics)) %>%
  #   dplyr::mutate(rx_cef_antibiotics_hf = ifelse(rx_cef_antibiotics_hf == 0 & rx_cef_antibiotics_hf1 == 1, 1, rx_cef_antibiotics_hf)) %>%
  #   dplyr::mutate(rx_ciprofloxacin = ifelse(rx_ciprofloxacin == 0 & rx_ciprofloxacin1 == 1, 1, rx_ciprofloxacin)) %>%
  #   dplyr::mutate(rx_ciprofloxacin_hf = ifelse(rx_ciprofloxacin_hf == 0 & rx_ciprofloxacin_hf1 == 1, 1, rx_ciprofloxacin_hf)) %>%
  #   dplyr::mutate(rx_gentamicin = ifelse(rx_gentamicin == 0 & rx_gentamicin1 == 1, 1, rx_gentamicin)) %>%
  #   dplyr::mutate(rx_gentamicin_hf = ifelse(rx_gentamicin_hf == 0 & rx_gentamicin_hf1 == 1, 1, rx_gentamicin_hf)) %>%
  #   dplyr::mutate(rx_metronidazol = ifelse(rx_metronidazol == 0 & rx_metronidazol1 == 1, 1, rx_metronidazol)) %>%
  #   dplyr::mutate(rx_metronidazol_hf = ifelse(rx_metronidazol_hf == 0 & rx_metronidazol_hf1 == 1, 1, rx_metronidazol_hf)) %>%
  #   dplyr::mutate(rx_ampicillin = ifelse(rx_ampicillin == 0 & rx_ampicillin1 == 1, 1, rx_ampicillin)) %>%
  #   dplyr::mutate(rx_ampicillin_hf = ifelse(rx_ampicillin_hf == 0 & rx_ampicillin_hf1 == 1, 1, rx_ampicillin_hf)) %>%
  #   dplyr::mutate(rx_azithromycin = ifelse(rx_azithromycin == 0 & rx_azithromycin1 == 1, 1, rx_azithromycin)) %>%
  #   dplyr::mutate(rx_azithromycin_hf = ifelse(rx_azithromycin_hf == 0 & rx_azithromycin_hf1 == 1, 1, rx_azithromycin_hf)) %>%
  #   dplyr::mutate(rx_benzathinepeniG = ifelse(rx_benzathinepeniG == 0 & rx_benzathinepeniG1 == 1, 1, rx_benzathinepeniG)) %>%
  #   dplyr::mutate(rx_benzathinepeniG_hf = ifelse(rx_benzathinepeniG_hf == 0 & rx_benzathinepeniG_hf1 == 1, 1, rx_benzathinepeniG_hf)) %>%
  #   dplyr::mutate(rx_aclav = ifelse(rx_aclav == 0 & rx_aclav1 == 1, 1, rx_aclav)) %>%
  #   dplyr::mutate(rx_aclav_hf = ifelse(rx_aclav_hf == 0 & rx_aclav_hf1 == 1, 1, rx_aclav_hf)) %>%
  #   dplyr::mutate(rx_cotrimoxazole = ifelse(rx_cotrimoxazole == 0 & rx_cotrimoxazole1 == 1, 1, rx_cotrimoxazole)) %>%
  #   dplyr::mutate(rx_cotrimoxazole_hf = ifelse(rx_cotrimoxazole_hf == 0 & rx_cotrimoxazole_hf1 == 1, 1, rx_cotrimoxazole_hf)) %>%
  #   dplyr::mutate(rx_antibio_oth = ifelse(rx_antibio_oth != rx_antibio_oth1, paste(rx_antibio_oth,rx_antibio_oth1,";"), rx_antibio_oth)) %>%
  #   dplyr::mutate(rx_antimalarials = ifelse(rx_antimalarials != rx_antimalarials1, paste(rx_antimalarials,rx_antimalarials1,";"), rx_antimalarials)) %>%
  #   dplyr::mutate(rx_imci = ifelse(rx_imci != rx_imci1, paste(rx_imci,rx_imci1,";"), rx_imci)) %>%
  #   dplyr::mutate(rx_creams = ifelse(rx_creams != rx_creams1, paste(rx_creams,rx_creams1,";"), rx_creams)) %>%
  #   dplyr::mutate(rx_consumables = ifelse(rx_consumables != rx_consumables1, paste(rx_consumables,rx_consumables1,";"), rx_consumables)) %>%
  #   dplyr::select(cols)

  out <- list(df, drug_df)
  out

}
