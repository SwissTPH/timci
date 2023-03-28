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

#' Export datasets
#'
#' @param df quality check dataframe.
#' @param idx directory where the Excel file will be created.
#' @param label filename prefix
#' @param cdir Row names
#' @param description Row names
#' @return creation timestamp of the Excel file
#' @export

dataset_export <- function(df,
                           idx,
                           label,
                           cdir,
                           description) {

  msg <- paste0("**",
                description,
                "** is a NULL object and cannot be exported.")

  if (!is.null(df)) {

    filename <- paste(idx, label, sep = "_")
    timestamps <- timci::export_df2csvxlsx(df, cdir, filename)
    msg <- paste0(description,
                  " have been exported to **",
                  filename,
                  ".xslx** (**",
                  timestamps[[1]],
                  "**) and to **",
                  filename,
                  ".csv** (**",
                  timestamps[[2]],
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
#' @param cleaning type of cleaning to be performed on inconsistent dates, by default set to "none" (i.e., no cleaning following the identification of inconsistent dates)
#' @param date_format format of the date
#' @return This function returns a dataframe containing data of possible duplicate participants only
#' @export
#' @import dplyr

detect_inconsistent_dates <- function(df,
                                      col_date_start,
                                      col_date_end,
                                      cleaning = "none",
                                      date_format = "%Y-%m-%d %T") {

  qc_df <- NULL
  cleaned_df <- NULL
  cols <- colnames(df)

  df$diff <- floor(difftime(df[[col_date_end]], df[[col_date_start]], units = "days"))
  if ( 'fid_from_device' %in% cols ) {
    kcols <- c("fid_from_device", "child_id", col_date_start, col_date_end, "diff", "uuid")
  } else if ( 'fid' %in% cols ) {
    kcols <- c("fid", "child_id", col_date_start, col_date_end, "diff", "uuid")
  } else if ( 'hf_id' %in% cols ) {
    kcols <- c("hf_id", "child_id", col_date_start, col_date_end, "diff", "uuid")
  } else {
    kcols <- c("child_id", col_date_start, col_date_end, "diff", "uuid")
  }

  qc_df <- df %>%
    dplyr::select(kcols) %>%
    dplyr::filter(diff > 0)

  if (timci::is_not_empty(qc_df)) {

    if ( 'fid_from_device' %in% cols ) {
      qc_df <- qc_df %>%
        dplyr::arrange(diff, fid_from_device)
    } else if ( 'fid' %in% cols ) {
      qc_df <- qc_df %>%
        dplyr::arrange(diff, fid)
    } else if ( 'hf_id' %in% cols ) {
      qc_df <- qc_df %>%
        dplyr::arrange(diff, hf_id)
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
#' @export

identify_nonvalid_ids <- function(df1,
                                  col_id1,
                                  df2,
                                  col_id2) {

  qc_df <- df1[!df1[[col_id1]] %in% df2[[col_id2]], ]
  cleaned_df <- df1[df1[[col_id1]] %in% df2[[col_id2]], ]

  list(qc_df, cleaned_df)

}

#' Identify non-valid IDs in a dataframe based on IDs in another dataframe (TIMCI-specific function)
#'
#' @param df1 dataframe containing the data to check
#' @param col_id1 column name containing IDs in `df1`
#' @param df2 reference dataframe
#' @param col_id2 column name containing IDs in `df2`
#' @param col_date column name containing dates in `df1`
#' @param lock_date lock date
#' @return This function returns a dataframe containing IDs and dates at which the ID has been allocated in different columns.
#' @export

identify_nonvalid_ids2 <- function(df1,
                                   col_id1,
                                   df2,
                                   col_id2,
                                   col_date,
                                   lock_date) {

  qc_df <- df1[df1[[col_id1]] %in% df2[[col_id2]], ]
  qc_df <- qc_df[qc_df[[col_date]] > as.Date(lock_date, "%Y-%m-%d"), ]
  cleaned_df <- df1[!df1[[col_id1]] %in% qc_df[[col_id2]], ]

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
