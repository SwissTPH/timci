#' Write dataframe to an Excel file, with a filename consisting of a prefix and a date stamp
#'
#' @param df quality check dataframe.
#' @param idx directory where the Excel file will be created.
#' @param label filename prefix
#' @param cdir Row names
#' @param description Row names
#' @return creation timestamp of the Excel file
#' @export

quality_check_export <- function(df,
                                 idx,
                                 label,
                                 cdir,
                                 description) {

  msg <- paste0("The table of record(s) for which ",
                description,
                " is empty and therefore has not been exported.")

  if (!is.null(df)) {

    filename <- paste(idx, label, sep = "_")
    timestamp <- timci::export_df2xlsx(df, cdir, filename)
    msg <- paste0("The table of record(s) for which ",
                  description,
                  " has been exported to **",
                  filename,
                  ".xlsx** (**",
                  timestamp,
                  "**) in the **",
                  basename(cdir),
                  "** folder.")

  }

  cat(msg)

}

#' Detect non-timely submissions, i.e. submissions not sent to the server on the day they were finalised (ODK function)
#'
#' @param df dataframe containing any ODK data, assuming standard metadata fields (`start`, `end`) are present.
#' @return This function returns a dataframe containing the duration between start and end
#' @export
#' @import dplyr magrittr

detect_non_timely_submission <- function(df) {

  qc <- NULL

  # Duration from form completion to transfer on the server
  df$diff <- as.Date(as.character(df$submission_date), format="%Y-%m-%d %T") - as.Date(as.character(df$end), format="%Y-%m-%d %T")
  df$start <- as.Date(as.character(df$start), format="%Y-%m-%d")
  df$end <- as.Date(as.character(df$end), format="%Y-%m-%d")
  df$submission_date <- as.Date(as.character(df$submission_date), format="%Y-%m-%d")
  qc <- df[c("fid", "child_id", "start", "end", "submission_date", "diff", "uuid")] %>%
    dplyr::arrange(diff, fid)

}

#' Detect non-timely completion of a submission, i.e. submissions not finalised on the day they were started (ODK function)
#'
#' @param df dataframe containing any ODK data, assuming standard metadata fields (`start`, `end`) are present.
#' @return This function returns a dataframe containing the duration between start and end
#' @export
#' @import dplyr magrittr

detect_non_timely_completion <- function(df) {

  qc <- NULL

  # Duration from form completion to transfer on the server
  df$diff <- as.Date(as.character(df$end), format="%Y-%m-%d %T") - as.Date(as.character(df$start), format="%Y-%m-%d %T")
  df$start <- as.Date(as.character(df$start), format="%Y-%m-%d")
  df$end <- as.Date(as.character(df$end), format="%Y-%m-%d")
  qc <- df[c("fid", "child_id", "start", "end", "diff", "uuid")] %>%
    dplyr::arrange(diff, fid)

}

#' Detect inconsistent dates (TIMCI-specific function)
#'
#' @param df dataframe containing the processed facility data
#' @param col_date_start column
#' @param col_date_end column
#' @param list_of_cols list of columns to be exported if needed (default is set to an empty vector)
#' @param cleaning type of cleaning to be performed on inconsistent dates, by default set to "none" (i.e., no cleaning following the identification of inconsistent dates)
#' @param date_format format of the date
#' @return This function returns a dataframe containing data of possible duplicate participants only
#' @export
#' @import dplyr lubridate

detect_inconsistent_dates <- function(df,
                                      col_date_start,
                                      col_date_end,
                                      cleaning = "none",
                                      list_of_cols = c(),
                                      date_format = "%Y-%m-%d %T") {

  qc_df <- df
  cleaned_df <- NULL
  cols <- colnames(df)

  qc_df[[col_date_start]] <- as.Date(qc_df[[col_date_start]])
  qc_df[[col_date_end]] <- as.Date(qc_df[[col_date_end]])
  qc_df$diff <- floor(difftime(qc_df[[col_date_end]], qc_df[[col_date_start]], units = "days"))
  qc_df[[paste0("day_", col_date_end)]] <- lubridate::wday(qc_df[[col_date_end]], label = TRUE)
  qc_df[[paste0("day_", col_date_start)]] <- lubridate::wday(qc_df[[col_date_start]], label = TRUE)

  cols <- colnames(qc_df)
  kcols <- c()
  if ( 'fid' %in% cols ) {
    kcols <- c(kcols,"fid")
  }
  if ( 'fid_from_device' %in% cols ) {
    kcols <- c(kcols, "fid_from_device")
  }
  if ( 'child_id' %in% cols ) {
    kcols <- c(kcols, "child_id")
  } else if ( 'prev_id' %in% cols ) {
    kcols <- c(kcols, "prev_id")
  }
  kcols <- c(kcols,
             col_date_start,
             col_date_end,
             paste0("day_", col_date_start),
             paste0("day_", col_date_end))
  if ( col_date_start != "start" & col_date_end != "start" ) {
    kcols <- c(kcols, "start")
  }
  if ( col_date_start != "end" & col_date_end != "end" ) {
    kcols <- c(kcols, "end")
  }
  if ( col_date_start != "submission_date" & col_date_end != "submission_date" & "submission_date" %in% cols ) {
    kcols <- c(kcols, "submission_date")
  }
  if ( length(list_of_cols) > 0 ) {
    kcols <- c(kcols, list_of_cols)
  }
  kcols <- c(kcols, "diff", "uuid")

  qc_df <- qc_df %>%
    dplyr::select(kcols) %>%
    dplyr::filter(diff > 0)

  if (timci::is_not_empty(qc_df)) {

    if ( 'fid_from_device' %in% cols ) {
      qc_df <- qc_df %>%
        dplyr::arrange(diff, fid_from_device)
    } else if ( 'fid' %in% cols ) {
      qc_df <- qc_df %>%
        dplyr::arrange(diff, fid)
    } else{
      qc_df <- qc_df %>%
        dplyr::arrange(diff)
    }

  }

  if ( !is.null(qc_df) & cleaning == "replace_by_end_date" ) {
    cleaned_df <- df
    cleaned_df$start <- ifelse(cleaned_df$uuid %in% qc_df$uuid,
                               cleaned_df[[col_date_end]],
                               cleaned_df$start)
  }

  if ( !is.null(qc_df) & cleaning == "replace_by_start_date" ) {
    cleaned_df <- df
    cleaned_df$start <- ifelse(cleaned_df$uuid %in% qc_df$uuid,
                               cleaned_df[[col_date_start]],
                               cleaned_df$start)
    cleaned_df$end <- ifelse(cleaned_df$uuid %in% qc_df$uuid,
                             cleaned_df[[col_date_start]],
                             cleaned_df$end)
    cleaned_df$date_visit <- ifelse(cleaned_df$uuid %in% qc_df$uuid,
                                    strftime(cleaned_df[[col_date_start]], "%Y-%m-%d"),
                                    cleaned_df$date_visit)
  }

  list(qc_df, cleaned_df)

}

#' Identify non-valid IDs in a dataframe based on IDs in another dataframe (TIMCI-specific function)
#'
#' @param df1 dataframe containing the data to check
#' @param col_id1 column name containing IDs in `df1`
#' @param df2 reference dataframe
#' @param col_id2 column name containing IDs in `df2`
#' @return This function returns a dataframe containing IDs and dates at which the ID has been allocated in different columns.
#' @import dplyr
#' @export

identify_nonvalid_ids <- function(df1,
                                  col_id1,
                                  df2,
                                  col_id2) {

  df1_empty <- df1 %>%
    dplyr::filter(!!rlang::sym(col_id1) == "" | is.na(!!rlang::sym(col_id1)))

  df1 <- df1 %>%
    dplyr::filter(!!rlang::sym(col_id1) != "" & !is.na(!!rlang::sym(col_id1)))
  qc_df <- df1[!df1[[col_id1]] %in% df2[[col_id2]], ]

  cleaned_df <- df1[df1[[col_id1]] %in% df2[[col_id2]], ] %>%
    dplyr::bind_rows(df1_empty)

  cols <- colnames(qc_df)
  kcols <- c()
  if ( "date_visit" %in% cols ) {
    kcols <- c(kcols, "date_visit")
  } else if ( "date_call" %in% cols ) {
    kcols <- c(kcols, "date_call")
  } else if ( "date" %in% cols ) {
    kcols <- c(kcols, "date")
  }
  if ( "fid" %in% cols ) {
    kcols <- c(kcols, "fid")
  }
  if ( "fid_ra" %in% cols ) {
    kcols <- c(kcols, "fid_ra")
  }
  if ( "fid_from_device" %in% cols ) {
    kcols <- c(kcols, "fid_from_device")
  }
  if ( "fid_from_main_device" %in% cols ) {
    kcols <- c(kcols, "fid_from_main_device")
  }
  if ( "child_id" %in% cols ) {
    kcols <- c(kcols, "child_id")
  }
  if ( "child_id" %in% cols ) {
    kcols <- c(kcols, "child_id")
  } else if ( "prev_id" %in% cols ) {
    kcols <- c(kcols, "prev_id")
  } else if ( "child_identification-pid" %in% cols ) {
    kcols <- c(kcols, "child_identification-pid")
  }
  if ( "child_name" %in% cols ) {
    kcols <- c(kcols, "child_name")
  } else if ( "name" %in% cols ) {
    kcols <- c(kcols, "name")
  }
  if ( "deviceid" %in% cols ) {
    kcols <- c(kcols, "deviceid")
  }
  if ( "uuid" %in% cols ) {
    kcols <- c(kcols, "uuid")
  } else if ( "meta-instanceID" %in% cols ) {
    kcols <- c(kcols, "meta-instanceID")
  } else if ( "PARENT_KEY" %in% cols ) {
    kcols <- c(kcols, "KEY", "PARENT_KEY")
  }
  qc_df <- qc_df %>%
    dplyr::select(kcols)

  list(qc_df, cleaned_df)

}

#' Identify non-valid IDs in a dataframe based on IDs in another dataframe (TIMCI-specific function)
#'
#' @param df1 dataframe containing the data to check
#' @param col_id1 column name containing IDs in `df1`
#' @param df2 reference dataframe
#' @param col_id2 column name containing IDs in `df2`
#' @return This function returns a dataframe containing IDs and dates at which the ID has been allocated in different columns.
#' @import dplyr
#' @export

identify_nonvalid_ids2 <- function(df1,
                                   col_id1,
                                   df2,
                                   col_id2) {

  qc_df <- df1[df1[[col_id1]] %in% df2[[col_id2]], ]
  cleaned_df <- df1[!df1[[col_id1]] %in% qc_df[[col_id2]], ]

  cols <- colnames(qc_df)
  kcols <- c()
  if ( "date_visit" %in% cols ) {
    kcols <- c(kcols, "date_visit")
  } else if ( "date_call" %in% cols ) {
    kcols <- c(kcols, "date_call")
  }
  if ( "child_id" %in% cols ) {
    kcols <- c(kcols, "child_id")
  } else if ( "prev_id" %in% cols ) {
    kcols <- c(kcols, "prev_id")
  }
  if ( "uuid" %in% cols ) {
    kcols <- c(kcols, "uuid")
  }
  if ( "deviceid" %in% cols ) {
    kcols <- c(kcols, "deviceid")
  }
  qc_df <- qc_df %>%
    dplyr::select(kcols)

  list(qc_df, cleaned_df)

}

#' Identify Non-Valid IDs in a Dataframe Based on Reference IDs (TIMCI-specific)
#'
#' This function identifies non-valid IDs in a dataframe by comparing them with reference IDs from another dataframe. It is specifically designed for TIMCI data quality checks.
#'
#' @param df1 A dataframe containing the data to be checked.
#' @param col_id1 The name of the column containing IDs in the \code{df1} dataframe.
#' @param df2 A reference dataframe used for comparison.
#' @param col_id2 The name of the column containing reference IDs in the \code{df2} dataframe.
#' @return A list containing two dataframes:
#'   \item{Non-Valid IDs Dataframe}{A dataframe containing rows with non-valid IDs and related dates, grouped in relevant columns.}
#'   \item{Flagged Dataframe}{A dataframe similar to \code{df1} with an additional column indicating if each ID is flagged as non-valid (1) or not (0).}
#' @import dplyr
#' @export

identify_nonvalid_ids_flagged <- function(df1,
                                          col_id1,
                                          df2,
                                          col_id2) {

  df1_empty <- df1 %>%
    dplyr::filter(!!rlang::sym(col_id1) == "" | is.na(!!rlang::sym(col_id1)))

  df1 <- df1 %>%
    dplyr::filter(!!rlang::sym(col_id1) != "" & !is.na(!!rlang::sym(col_id1)))
  qc_df <- df1[!df1[[col_id1]] %in% df2[[col_id2]], ]

  flagged_df <- df1 %>%
    dplyr::mutate(matched = ifelse(!!rlang::sym(col_id1) %in% df2[[col_id2]], 1, 0))

  cols <- colnames(qc_df)
  kcols <- c()
  if ( "date_visit" %in% cols ) {
    kcols <- c(kcols, "date_visit")
  } else if ( "date_call" %in% cols ) {
    kcols <- c(kcols, "date_call")
  } else if ( "date" %in% cols ) {
    kcols <- c(kcols, "date")
  }
  if ( "fid" %in% cols ) {
    kcols <- c(kcols, "fid")
  }
  if ( "fid_ra" %in% cols ) {
    kcols <- c(kcols, "fid_ra")
  }
  if ( "fid_from_device" %in% cols ) {
    kcols <- c(kcols, "fid_from_device")
  }
  if ( "fid_from_main_device" %in% cols ) {
    kcols <- c(kcols, "fid_from_main_device")
  }
  if ( "child_id" %in% cols ) {
    kcols <- c(kcols, "child_id")
  }
  if ( "child_id" %in% cols ) {
    kcols <- c(kcols, "child_id")
  } else if ( "prev_id" %in% cols ) {
    kcols <- c(kcols, "prev_id")
  } else if ( "child_identification-pid" %in% cols ) {
    kcols <- c(kcols, "child_identification-pid")
  }
  if ( "child_name" %in% cols ) {
    kcols <- c(kcols, "child_name")
  } else if ( "name" %in% cols ) {
    kcols <- c(kcols, "name")
  }
  if ( "deviceid" %in% cols ) {
    kcols <- c(kcols, "deviceid")
  }
  if ( "uuid" %in% cols ) {
    kcols <- c(kcols, "uuid")
  } else if ( "meta-instanceID" %in% cols ) {
    kcols <- c(kcols, "meta-instanceID")
  }
  qc_df <- qc_df %>%
    dplyr::select(kcols)

  list(qc_df, flagged_df)

}

#' Identify non-valid IDs in a dataframe based on IDs in another dataframe (TIMCI-specific function)
#'
#' This function takes in two data frames and two column names, and identifies the non-valid IDs in the first data frame based on the IDs in the second data frame. It returns a list of two data frames, one containing the IDs and dates at which the ID has been allocated, and the other containing the cleaned data.
#'
#' @param df1 A dataframe containing the data to check for non-valid IDs.
#' @param col_id1 The column name containing IDs in \code{df1}.
#' @param df2 A reference dataframe containing the valid IDs to compare with.
#' @param col_id2 The column name containing IDs in \code{df2}.
#' @param col_date1 The name of the column containing the date in the \code{df1} dataframe.
#' @param ldate_diff Lower date difference (default is same day), negative numbers indicate a difference in the past, positive numbers indicate a difference in the future.
#' @param udate_diff Upper date difference (default is same day), negative numbers indicate a difference in the past, positive numbers indicate a difference in the future.
#' @param matched_names Boolean indicating whether to perform matching based on names.
#' @param cleaning The cleaning option, which can be "drop_all" to remove non-valid IDs from \code{df1}.
#'
#' @return A list containing two data frames. The first data frame contains the IDs and dates at which the ID has been allocated in different columns. The second data frame contains the cleaned data.
#
#' @export

identify_nonvalid_ids_with_matched_names <- function(df1,
                                                     col_id1,
                                                     df2,
                                                     col_id2,
                                                     col_date1,
                                                     ldate_diff,
                                                     udate_diff,
                                                     matched_names = FALSE,
                                                     cleaning = "none") {

  cleaned_df <- NULL

  # Detect IDs in df1 that are not found in df2
  qc_df <- df1[!df1[[col_id1]] %in% df2[[col_id2]], ]

  if (cleaning == "drop_all") {
    cleaned_df <- df1[df1[[col_id1]] %in% df2[[col_id2]], ]
  }

  cols <- colnames(qc_df)
  kcols <- c()
  if ( "date_visit" %in% cols ) {
    kcols <- c(kcols, "date_visit")
  } else if ( "date_call" %in% cols ) {
    kcols <- c(kcols, "date_call")
  } else if ( "date_repeat" %in% cols ) {
    kcols <- c(kcols, "date_repeat")
  }
  if ( "district" %in% cols ) {
    kcols <- c(kcols, "district")
  }
  if ( "child_id" %in% cols ) {
    kcols <- c(kcols, "child_id")
  } else if ( "prev_id" %in% cols ) {
    kcols <- c(kcols, "prev_id")
  }
  if ( "date_day0" %in% cols ) {
    kcols <- c(kcols, "date_day0")
  }
  if ( "name" %in% cols ) {
    kcols <- c(kcols, "name")
  } else if ( "child_name" %in% cols ) {
    kcols <- c(kcols, "child_name")
  }
  if ( "cg_name" %in% cols ) {
    kcols <- c(kcols, "cg_name")
  }
  if ( "deviceid" %in% cols ) {
    kcols <- c(kcols, "deviceid")
  }
  if ( "uuid" %in% cols ) {
    kcols <- c(kcols, "uuid")
  }
  qc_df <- qc_df %>%
    dplyr::select(kcols)

  if (matched_names) {
    if ( "name" %in% kcols ) {
      out <- timci::find_best_matched_names_between_fu_and_day0(df = qc_df %>%
                                                                  dplyr::mutate(name = tolower(name)),
                                                                day0_df = df2,
                                                                col_date = col_date1,
                                                                col_name = "name",
                                                                ldate_diff = ldate_diff,
                                                                udate_diff = udate_diff)
    } else if ( "child_name" %in% kcols ) {
      out <- timci::find_best_matched_names_between_fu_and_day0(df = qc_df %>%
                                                                  dplyr::mutate(child_name = tolower(child_name)),
                                                                day0_df = df2,
                                                                col_date = col_date1,
                                                                col_name = "child_name",
                                                                ldate_diff = ldate_diff,
                                                                udate_diff = udate_diff)
    }
    if ( !is.null(out[[1]]) ) {
      qc_df <- out[[1]]
    }
  }

  list(qc_df, cleaned_df)

}

#' Identify non-valid IDs in a dataframe based on IDs in another dataframe (TIMCI-specific function)
#'
#' @param df dataframe containing the data to check
#' @param col_id column name containing IDs in `df1`
#' @param day0_df Day 0 dataframe
#' @param start_date start date
#' @param end_date end date
#' @return This function returns a dataframe containing IDs and dates at which the ID has been allocated in different columns.
#' @export

identify_ids_outside_lock_range <- function(df,
                                            col_id,
                                            day0_df,
                                            start_date,
                                            end_date) {

  # Filter Day 0 data so as to only keep enrolment data outside the lock range
  day0_df <- day0_df %>%
    dplyr::filter(date_visit < as.Date(start_date, "%Y-%m-%d") | date_visit > as.Date(lock_date, "%Y-%m-%d"))
  # Identify valid follow-ups outside the lock range
  qc_df <- df[df[[col_id]] %in% day0_df$child_id, ]
  # Remove valid follow-ups outside lock range from the cleaned dataframe
  cleaned_df <- df[!df[[col_id]] %in% day0_df$child_id, ]

  list(qc_df, cleaned_df)

}

# Same patient ID from different facilities
# First name is not consistent
# First name match is below … %
# First name match is below … %, while taking into account local phonetic variability
# Last name is not consistent
# First and last names are switched
# Middle name is not consistent
# Sex is not consistent
# Year of birth is not consistent
# Month of birth is not consistent
# Day of birth is not consistent
# Month and day of birth are switched
# Identity score is below … %

#Baseline visit identification
#New visit anterior to last visit received
