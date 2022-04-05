#' Correct duplicates (TIMCI-specific function)
#'
#' @param df dataframe
#' @return This function returns a list that contains a dataframe with corrections and the list of edits
#' @import dplyr
#' @export

correct_duplicates <- function(df) {

  csv_filename <- NULL
  if (Sys.getenv('TIMCI_COUNTRY') == 'Tanzania') {
    csv_filename <- "day0_duplicate_correction_tanzania.csv"
  }

  out <- list(NULL,NULL)
  if (!is.null(csv_filename)) {
    csv_pathname <- system.file(file.path('extdata', 'cleaning', csv_filename), package = 'timci')
    duplicate_edits <- readr::with_edition(1, readr::read_csv(csv_pathname))
    df <- df %>%
      merge(duplicate_edits, by.x=c("child_id", "uuid"), by.y=c("old_child_id", "uuid"), all.x=TRUE)
    df$child_id <- ifelse(is.na(df$new_child_id), df$child_id, df$new_child_id)
    df$fid <- ifelse(is.na(df$new_child_id), df$fid, substr(df$new_child_id, 3,7))
    if("fid_from_device" %in% colnames(df))
    {
      df$fid_from_device <- ifelse(is.na(df$new_child_id), df$fid_from_device, substr(df$new_child_id, 3,7))
    }
    out <- list(df, duplicate_edits)
  }
  out

}
