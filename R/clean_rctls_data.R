#' Correct Day 0 facilities (TIMCI-specific function)
#'
#' @param df dataframe
#' @return This function returns a list that contains a dataframe with corrections and the list of edits
#' @import dplyr
#' @export

correct_day0_facilities <- function(df) {

  csv_filename <- NULL
  if (Sys.getenv('TIMCI_COUNTRY') == 'Kenya') {
    csv_filename <- "day0_facility_correction_kenya.csv"
  }

  out <- list(df,NULL)
  if (!is.null(csv_filename)) {
    csv_pathname <- system.file(file.path('extdata', 'cleaning', csv_filename), package = 'timci')
    edits <- readr::with_edition(1, readr::read_csv(csv_pathname))
    df <- df %>%
      merge(edits,
            by.x = c("child_id", "uuid"),
            by.y = c("old_child_id", "uuid"),
            all.x = TRUE)
    df$child_id <- ifelse(is.na(df$new_child_id), df$child_id, df$new_child_id)
    df$fid <- ifelse(is.na(df$new_child_id), df$fid, substr(df$new_child_id, 3,7))
    out <- list(df, edits)
  }
  out

}

#' Correct Day 0 duplicates (TIMCI-specific function)
#'
#' @param df dataframe
#' @return This function returns a list that contains a dataframe with corrections and the list of edits
#' @import dplyr
#' @export

correct_day0_duplicates <- function(df) {

  csv_filename <- NULL
  if (Sys.getenv('TIMCI_COUNTRY') == 'Tanzania') {
    csv_filename <- "day0_duplicate_correction_tanzania.csv"
  }
  # else if (Sys.getenv('TIMCI_COUNTRY') == 'Kenya') {
  #   csv_filename <- "day0_duplicate_correction_kenya.csv"
  # }

  out <- list(df,NULL)
  if (!is.null(csv_filename)) {
    csv_pathname <- system.file(file.path('extdata', 'cleaning', csv_filename), package = 'timci')
    edits <- readr::with_edition(1, readr::read_csv(csv_pathname))
    df <- df %>%
      merge(edits,
            by.x=c("child_id", "uuid"),
            by.y=c("old_child_id", "uuid"),
            all.x=TRUE)
    df$child_id <- ifelse(is.na(df$new_child_id), df$child_id, df$new_child_id)
    df$fid <- ifelse(is.na(df$new_child_id), df$fid, substr(df$new_child_id, 3,7))
    if("fid_from_device" %in% colnames(df))
    {
      df$fid_from_device <- ifelse(is.na(df$new_child_id), df$fid_from_device, substr(df$new_child_id, 3,7))
    }
    out <- list(df, edits)
  }
  out

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
    df <- df %>%
      merge(edits,
            by.x=c("child_id", "uuid"),
            by.y=c("old_child_id", "uuid"),
            all.x=TRUE)
    df$child_id <- ifelse(is.na(df$new_child_id), df$child_id, df$new_child_id)
    df$fid <- ifelse(is.na(df$new_child_id), df$fid, substr(df$new_child_id, 3,7))
    out <- list(df, edits)
  }
  out

}
