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
    edits <- readr::with_edition(1, readr::read_csv(csv_pathname))
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
                            TRUE ~ "")

  out <- list(df, NULL)
  if ( csv_filename != "" ) {

    csv_pathname <- system.file(file.path('extdata', 'cleaning', csv_filename), package = 'timci')
    edits <- readr::with_edition(1, readr::read_csv(csv_pathname))

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
    edits <- readr::with_edition(1, readr::read_csv(csv_pathname))

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
    records_to_drop <- readr::with_edition(1, readr::read_csv(csv_pathname))

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
    edits <- readr::with_edition(1, readr::read_csv(csv_pathname))
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
#' This function can be used to correct documented HCP ID duplicates, incorrect facility codes or typos
#'
#' @param df dataframe
#' @return This function returns a list that contains a dataframe with corrections and the list of edits
#' @import dplyr
#' @export

correct_spa_sco_hcp_ids <- function(df) {

  csv_filename <- NULL
  if (Sys.getenv('TIMCI_COUNTRY') == 'Kenya') {
    csv_filename <- "spa_sco_hcp_correction_kenya.csv"
  }

  out <- list(df, NULL)
  if (!is.null(csv_filename)) {
    csv_pathname <- system.file(file.path('extdata', 'cleaning', csv_filename), package = 'timci')
    edits <- readr::with_edition(1, readr::read_csv(csv_pathname))
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
