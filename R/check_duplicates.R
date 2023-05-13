#' Detect ID duplicates (TIMCI-specific function)
#'
#' @param df dataframe containing the processed facility data
#' @param col vector containing a column of the processed facility data
#' @return This function returns a dataframe containing IDs and their frequencies (a frequency strictly superior to 1 indicates a duplicate).
#' @export
#' @import dplyr

detect_id_duplicates <- function(df, col = child_id) {

  # Quote the arguments that refer to data frame columns
  col <- dplyr::enquo(col)

  res <- df %>%
    group_by(!!col) %>%
    count
  res <- res %>%
    dplyr::rename(id_fq = n)

}

#' Identify ID duplicates by dates (TIMCI-specific function)
#'
#' @param df dataframe containing the processed facility data
#' @param col_date name of the column containing dates in `df`
#' @param col_id name of the column containing IDs in `df`
#' @param cleaning type of cleaning to be performed on duplicates, by default set to "none" (i.e., no cleaning following the identification of duplicates)
#' @return This function returns a dataframe containing IDs and dates at which the ID has been allocated in different columns.
#' @export
#' @import dplyr

identify_duplicates_by_dates <- function(df,
                                         col_id,
                                         col_date,
                                         cleaning = "none") {

  qc_df <- NULL
  qc_df2 <- NULL
  cleaned_df <- NULL

  if ( timci::is_not_empty(df) ) {

    qc_df <- df %>%
      dplyr::rename(date_value1 = !!dplyr::enquo(col_date)) %>%
      dplyr::rename(id = !!dplyr::enquo(col_id)) %>%
      dplyr::group_by(id) %>%
      dplyr::mutate(date_value2 = sort(date_value1,
                                       decreasing = TRUE,
                                       na.last = TRUE)) %>% # order by descending dates
      dplyr::mutate(row_n = row_number()) %>%
      tidyr::pivot_wider(id,
                         names_from = row_n,
                         values_from = c(date_value2),
                         names_prefix = "date_")

    if ( "date_2" %in% colnames(qc_df) ) {
      qc_df <- qc_df %>%
        dplyr::filter(!is.na(date_2))
      qc_df2 <- df[df[[col_id]] %in% qc_df$id, ]
    } else {
      qc_df <- NULL
    }

    if ( !is.null(qc_df) & cleaning == "drop_all" ) {
      cleaned_df <- df[!df[[col_id]] %in% qc_df$id, ]
    }
    if ( !is.null(qc_df) & cleaning == "keep_latest" ) {
      # Order data by descending dates
      df <- df %>%
        dplyr::arrange(desc(!!dplyr::enquo(col_date)))
      # Duplicated() determines which elements of a vector or data frame are duplicates of elements with smaller subscripts
      # (i.e. in the present situation: duplicates of elements with a later record available)
      # Extract unique elements by selecting only those elements that are not duplicates of elements detected earlier
      cleaned_df <- df[!duplicated(df[[col_id]]), ]
    }

  }

  list(qc_df, cleaned_df, qc_df2)

}

#' Identify ID duplicates with names (TIMCI-specific function)
#'
#' @param df dataframe containing the processed facility data
#' @param col_date name of the column containing dates in `df`
#' @param col_id name of the column containing IDs in `df`
#' @param cleaning type of cleaning to be performed on duplicates, by default set to "none" (i.e., no cleaning following the identification of duplicates)
#' @return This function returns a dataframe containing IDs and dates at which the ID has been allocated in different columns.
#' @export
#' @import dplyr

identify_duplicates_with_names <- function(df,
                                           col_id,
                                           col_date,
                                           cleaning = "none") {

  qc_df <- NULL
  qc_df2 <- NULL
  cleaned_df <- NULL

  if ( timci::is_not_empty(df) ) {

    qc_df <- df %>%
      dplyr::rename(date_value = !!dplyr::enquo(col_date)) %>%
      dplyr::rename(id = !!dplyr::enquo(col_id)) %>%
      dplyr::group_by(id)

    if ( "fs_name" %in% colnames(qc_df) ) {
      qc_df <- qc_df %>%
        dplyr::mutate(name = paste(fs_name, ls_name, sep = " "))
    }

    qc_df <- qc_df %>%
      dplyr::mutate(date = sort(date_value,
                                decreasing = TRUE,
                                na.last = TRUE)) %>% # order by descending dates
      dplyr::mutate(row_n = row_number()) %>%
      tidyr::pivot_wider(id,
                         names_from = row_n,
                         values_from = c(date, name, uuid))

    if ( "date_2" %in% colnames(qc_df) ) {
      qc_df <- qc_df %>%
        dplyr::filter(!is.na(date_2))
      qc_df2 <- df[df[[col_id]] %in% qc_df$id, ]
    } else {
      qc_df <- NULL
    }

    if ( !is.null(qc_df) & cleaning == "drop_all" ) {
      cleaned_df <- df[!df[[col_id]] %in% qc_df$id, ]
    }
    if ( !is.null(qc_df) & cleaning == "keep_latest" ) {
      # Order data by descending dates
      df <- df %>%
        dplyr::arrange(desc(!!dplyr::enquo(col_date)))
      # Duplicated() determines which elements of a vector or data frame are duplicates of elements with smaller subscripts
      # (i.e. in the present situation: duplicates of elements with a later record available)
      # Extract unique elements by selecting only those elements that are not duplicates of elements detected earlier
      cleaned_df <- df[!duplicated(df[[col_id]]), ]
    }

  }

  list(qc_df, cleaned_df, qc_df2)

}

#' Identify day 0 ID duplicates wit names and corresponding Day 7 follow-ups (TIMCI-specific function)
#'
#' @param df dataframe containing the processed facility data
#' @param day7fu_df dataframe containing Day 7 follow-up data
#' @param cleaning type of cleaning to be performed on duplicates, by default set to "none" (i.e., no cleaning following the identification of duplicates)
#' @return This function returns a dataframe containing IDs and dates at which the ID has been allocated in different columns.
#' @export
#' @import dplyr

identify_day0_duplicates_and_fu <- function(df,
                                            day7fu_df = NULL,
                                            cleaning = "none") {

  qc_df <- NULL
  qc_df2 <- NULL
  cleaned_df <- NULL

  if ( timci::is_not_empty(df) ) {

    qc_df <- df %>%
      dplyr::rename(id = child_id) %>%
      dplyr::group_by(id) %>%
        dplyr::mutate(name = paste(fs_name, ls_name, sep = " ")) %>%
      dplyr::mutate(date = sort(date_visit,
                                decreasing = TRUE,
                                na.last = TRUE)) %>% # order by descending dates
      dplyr::mutate(row_n = row_number()) %>%
      tidyr::pivot_wider(id,
                         names_from = row_n,
                         values_from = c(date, name, uuid))

    if ( "date_2" %in% colnames(qc_df) ) {
      qc_df <- qc_df %>%
        dplyr::filter(!is.na(date_2))
      qc_df2 <- df[df$child_id %in% qc_df$id, ]
    } else {
      qc_df <- NULL
    }

    if ( timci::is_not_empty(day7fu_df) ) {

      day7fu_df <- day7fu_df %>%
        dplyr::rename(id = child_id) %>%
        dplyr::rename(day7_name = name) %>%
        dplyr::rename(day7_uuid = uuid) %>%
        dplyr::group_by(id) %>%
        dplyr::mutate(day7_date = sort(date_call,
                                       decreasing = TRUE,
                                       na.last = TRUE)) %>% # order by descending dates
        dplyr::mutate(row_n = row_number()) %>%
        tidyr::pivot_wider(id,
                           names_from = row_n,
                           values_from = c(day7_date, day7_name, day7_uuid))
    }

    if ( timci::is_not_empty(qc_df) & timci::is_not_empty(day7fu_df) ) {
      qc_df <- qc_df %>%
        merge(day7fu_df,
              by = "id",
              all.x = TRUE)
    }

    if ( !is.null(qc_df) & cleaning == "drop_all" ) {
      cleaned_df <- df[!df$child_id %in% qc_df$id, ]
    }
    if ( !is.null(qc_df) & cleaning == "keep_latest" ) {
      # Order data by descending dates
      df <- df %>%
        dplyr::arrange(desc(date_visit))
      # Duplicated() determines which elements of a vector or data frame are duplicates of elements with smaller subscripts
      # (i.e. in the present situation: duplicates of elements with a later record available)
      # Extract unique elements by selecting only those elements that are not duplicates of elements detected earlier
      cleaned_df <- df[!duplicated(df$child_id), ]
    }

  }

  list(qc_df, cleaned_df, qc_df2)

}

#' Detect name duplicates (TIMCI-specific function)
#' Search for exact matches and switches
#'
#' @param df dataframe containing the processed facility data
#' @return This function returns a dataframe containing IDs and names of duplicate names
#' @export
#' @import dplyr

detect_name_duplicates <- function(df) {

  qc <- NULL

  # Exact (case-insensitive) duplicates
  df <- dplyr::mutate(df, full_name = tolower(paste(fs_name, ls_name, sep = ' ')))
  df1 <- df[c("child_id", "full_name")]
  qc1 <- data.frame(table(df1$full_name))
  qc1 <- qc1 %>%
    dplyr::rename(full_name = Var1,
                  ex_name_fq = Freq)
  qc <- merge(df1, qc1, by = 'full_name')

  # Switched (case-insensitive) names
  df <- dplyr::mutate(df, switched_name = tolower(paste(ls_name, fs_name, sep = ' ')))
  df2 <- df[c("child_id", "full_name", "switched_name")]
  df2a <- df2[c("child_id", "full_name")] %>%
    dplyr::rename(name = full_name)
  df2b <- df2[c("child_id", "switched_name")] %>%
    dplyr::rename(name = switched_name)
  df2 <- rbind(df2a, df2b)
  qc2 <- data.frame(table(df2$name))
  qc2 <- qc2 %>%
    dplyr::rename(switched_name = Var1,
                  sw_name_fq = Freq)
  qc <- merge(qc, qc2, by.x = 'full_name', by.y = 'switched_name')

  # Approximate String Matching (Fuzzy Matching)
  #df3 <- df[c("child_id", "full_name")]
  #qc3 <- df3[lapply(car.vins, agrep, x = vin.vins, max.distance = c(cost=2, all=2), value = TRUE)
  #, .(NumTimesFound = .N)
  #, by = df1$full_name]
  #qc <- merge(qc, qc3, by.x = 'full_name', by.y = 'switched_name')

  qc %>% dplyr::select(child_id, ex_name_fq, sw_name_fq)

}

#' Detect participant duplicates (TIMCI-specific function)
#' Detection based on name, sex and date of birth
#'
#' @param df dataframe containing the processed facility data
#' @return This function returns a vector containing IDs of duplicate name
#' @export
#' @import dplyr magrittr

detect_participant_duplicates <- function(df) {

  qc <- NULL

  # Exact (case-insensitive) duplicates
  df <- dplyr::mutate(df, full_name = tolower(paste(fs_name, ls_name, sep = ' ')))
  df1 <- df[c("child_id", "full_name", "dob", "sex")]
  qc1 <- df1 %>%
    group_by(full_name, dob, sex) %>%
    count
  qc1 <- qc1 %>%
    dplyr::rename(ex_name_fq = n)
  qc <- merge(df1, qc1, by = c('full_name', 'dob', "sex"))

  # Switched (case-insensitive) names
  df <- dplyr::mutate(df, switched_name = tolower(paste(ls_name, fs_name, sep = ' ')))
  df2 <- df[c("child_id", "full_name", "switched_name", "dob", "sex")]
  df2a <- df2[c("child_id", "full_name", "dob", "sex")] %>%
    dplyr::rename(name = full_name)
  df2b <- df2[c("child_id", "switched_name", "dob", "sex")] %>%
    dplyr::rename(name = switched_name)
  df2 <- rbind(df2a, df2b)
  qc2 <- df2 %>%
    group_by(name, dob, sex) %>%
    count
  qc2 <- qc2 %>%
    dplyr::rename(sw_name_fq = n,
                  switched_name = name)
  qc <- merge(qc, qc2, by.x = c('full_name', 'dob', 'sex'), by.y = c('switched_name', 'dob', 'sex'))

  # Approximate String Matching (Fuzzy Matching)
  #df3 <- df[c("child_id", "full_name")]
  #qc3 <- df3[lapply(car.vins, agrep, x = vin.vins, max.distance = c(cost=2, all=2), value = TRUE)
  #, .(NumTimesFound = .N)
  #, by = df1$full_name]
  #qc <- merge(qc, qc3, by.x = 'full_name', by.y = 'switched_name')

  qc %>% dplyr::select(child_id, ex_name_fq, sw_name_fq)

}

#' Detect participant duplicates (TIMCI-specific function)
#' Detection based on name and date of birth
#'
#' @param df dataframe containing the processed facility data
#' @return This function returns a vector containing IDs of duplicate name
#' @export
#' @import dplyr magrittr

detect_namedob_duplicates <- function(df) {

  qc <- NULL

  # Exact (case-insensitive) duplicates
  df <- dplyr::mutate(df, full_name = tolower(paste(fs_name, ls_name, sep = ' ')))
  df1 <- df[c("child_id", "full_name", "dob")]
  qc1 <- df1 %>%
    group_by(full_name, dob) %>%
    count
  qc1 <- qc1 %>%
    dplyr::rename(ex_name2_fq = n)
  qc <- merge(df1, qc1, by = c('full_name', 'dob'))

  # Switched (case-insensitive) names
  df <- dplyr::mutate(df, switched_name = tolower(paste(ls_name, fs_name, sep = ' ')))
  df2 <- df[c("child_id", "full_name", "switched_name", "dob")]
  df2a <- df2[c("child_id", "full_name", "dob")] %>%
    dplyr::rename(name = full_name)
  df2b <- df2[c("child_id", "switched_name", "dob")] %>%
    dplyr::rename(name = switched_name)
  df2 <- rbind(df2a, df2b)
  qc2 <- df2 %>%
    group_by(name, dob) %>%
    count
  qc2 <- qc2 %>%
    dplyr::rename(sw_name2_fq = n,
                  switched_name = name)
  qc <- merge(qc, qc2, by.x = c('full_name', 'dob'), by.y = c('switched_name', 'dob'))

  # Approximate String Matching (Fuzzy Matching)
  #df3 <- df[c("child_id", "full_name")]
  #qc3 <- df3[lapply(car.vins, agrep, x = vin.vins, max.distance = c(cost=2, all=2), value = TRUE)
  #, .(NumTimesFound = .N)
  #, by = df1$full_name]
  #qc <- merge(qc, qc3, by.x = 'full_name', by.y = 'switched_name')

  qc %>% dplyr::select(child_id, ex_name2_fq, sw_name2_fq)

}

#' Check name consistency (TIMCI-specific function)
#'
#' @param df dataframe
#' @param col_name1 name of the column containing names in `df`
#' @param col_name2 name of the column containing names in `df`
#' @return This function returns a dataframe containing IDs and dates at which the ID has been allocated in different columns.
#' @export
#' @import dplyr

check_name_consistency <- function(df,
                                   col_name1,
                                   col_name2) {

  # Threshold to be determined exactly
  thres <- 75

  qc_df <- NULL
  cleaned_df <- NULL

  if ( timci::is_not_empty(df) ) {

    qc_df <- df %>%
      dplyr::mutate(lvr = timci::normalised_levenshtein_ratio(!!dplyr::enquo(col_name1),
                                                              !!dplyr::enquo(col_name2)))
    qc_df <- qc_df %>% dplyr::select(child_id,
                                     uuid,
                                     !!dplyr::enquo(col_name1),
                                     !!dplyr::enquo(col_name2),
                                     lvr)#[qc_df$lvr <= thres, c("child_id", "uuid", "lvr")]

  }

  list(qc_df, cleaned_df)

}

#' Concatenate names for fuzzy-matching scoring (TIMCI-specific function)
#'
#' @param df dataframe
#' @return This function returns a dataframe with an additional column `name` that contains the concatenated name.
#' @export
#' @import dplyr

concatenate_names <- function(df) {

  # Concatenate mother's name
  out <- df %>%
    dplyr::mutate(mother_name = dplyr::case_when(
      main_cg == 1 ~ tolower(paste(cg_fs_name, cg_ls_name, sep = ' ')),
      .default     = tolower(paste(mother_fs_name, mother_ls_name, sep = ' ')))) %>%
    dplyr::mutate(mother_name = gsub('[0-9]+', '', mother_name))

  # Concatenate child's name depending on the country
  if ( Sys.getenv("TIMCI_COUNTRY") == "Tanzania" ) {
    out <- out %>%
      dplyr::mutate(name = gsub('[0-9]+', '', tolower(paste(fs_name, ms_name, ls_name, sep = ' ')))) %>%
      dplyr::mutate(name_root = gsub('[0-9]+', '', tolower(paste(fs_name, ms_name, sep = ' ')))) %>%
      dplyr::mutate(name_switch = gsub('[0-9]+', '', tolower(paste(fs_name, ls_name, ms_name, sep = ' ')))) %>%
      dplyr::mutate(mother_name_first = dplyr::case_when(
        main_cg == 1 ~ tolower(cg_fs_name),
        .default     = tolower(mother_fs_name))) %>%
      dplyr::mutate(mother_name_first = gsub('[0-9]+', '', mother_name_first)) %>%
      dplyr::mutate(mother_name_last = dplyr::case_when(
        main_cg == 1 ~ tolower(cg_ls_name),
        .default     = tolower(mother_ls_name))) %>%
      dplyr::mutate(mother_name_last = gsub('[0-9]+', '', mother_name_last))
  } else{
    out <- out %>%
      dplyr::mutate(name = gsub('[0-9]+', '', tolower(paste(fs_name, ls_name, sep = ' '))))
  }

  out

}

#' Identify repeat ID duplicates by dates (TIMCI-specific function)
#'
#' @param df dataframe containing the processed facility data
#' @param col_date name of the column containing dates in `df`
#' @param col_id name of the column containing IDs in `df`
#' @return This function returns a dataframe containing IDs and dates at which the ID has been allocated in different columns.
#' @export
#' @import dplyr

identify_day0_duplicates <- function(df,
                                     col_id,
                                     col_date) {

  # Threshold for fuzzy matching
  thres1 <- 50 # threshold to be applied when matching child names
  thres2 <- 40 # threshold to be applied when matching mother names
  thres3 <- 60 # threshold to be applied when matching child name roots (2 names only so that threshold should be more restrictive)
  thres4 <- 70 # threshold to be applied when matching mother name roots (1 name only so that threshold should be more restrictive)

  qc_df <- NULL

  if ( timci::is_not_empty(df) ) {

    qc_df <- df %>%
      dplyr::filter(enrolled == 1) %>%
      dplyr::arrange(!!dplyr::enquo(col_date)) %>% # Sort data by dates
      timci::concatenate_names() %>% # Concatenate names
      dplyr::rename(id = !!dplyr::enquo(col_id),
                    date = !!dplyr::enquo(col_date)) %>%
      dplyr::group_by(id) %>%
      dplyr::mutate(row_n = row_number())

    if ( Sys.getenv("TIMCI_COUNTRY") == "Tanzania" ) {
      qc_df <- qc_df %>%
        tidyr::pivot_wider(id,
                           names_from = row_n,
                           values_from = c("date",
                                           "name",
                                           "name_root",
                                           "name_switch",
                                           "mother_name",
                                           "mother_name_first",
                                           "mother_name_last",
                                           "uuid"))
    } else{
      qc_df <- qc_df %>%
        tidyr::pivot_wider(id,
                           names_from = row_n,
                           values_from = c("date",
                                           "name",
                                           "mother_name",
                                           "uuid"))
    }

    if ( "date_2" %in% colnames(qc_df) ) {
      qc_df <- qc_df %>%
        dplyr::filter(!is.na(name_2))
      if ( Sys.getenv("TIMCI_COUNTRY") == "Tanzania" ) {
        qc_df <- qc_df %>%
          dplyr::mutate(lvr1 = timci::normalised_levenshtein_ratio(name_1, name_2)) %>%
          dplyr::mutate(lvr2 = timci::normalised_levenshtein_ratio(name_1, name_switch_2)) %>%
          dplyr::mutate(lvr3 = timci::normalised_levenshtein_ratio(name_root_1, name_root_2)) %>%
          dplyr::mutate(lvr4 = timci::normalised_levenshtein_ratio(mother_name_1, mother_name_2)) %>%
          dplyr::mutate(lvr5 = timci::normalised_levenshtein_ratio(mother_name_first_1, mother_name_first_2)) %>%
          dplyr::mutate(lvr6 = timci::normalised_levenshtein_ratio(mother_name_last_1, mother_name_last_2)) %>%
          dplyr::filter( ((lvr1 > thres1) | (lvr2 > thres1)) & (lvr3  > thres3) & ((lvr4 > thres2) | (lvr5 > thres4) | (lvr6 > thres4)) )
      } else{
        qc_df <- qc_df %>%
          dplyr::mutate(lvr1 = timci::normalised_levenshtein_ratio(name_1, name_2)) %>%
          dplyr::mutate(lvr2 = timci::normalised_levenshtein_ratio(mother_name_1, mother_name_2)) %>%
          dplyr::filter((lvr1 > thres1) & (lvr2 > thres2))
      }
    } else {
      qc_df <- NULL
    }

  }

  qc_df

}

#' Identify repeat ID duplicates by dates (TIMCI-specific function)
#'
#' @param df dataframe containing the processed facility data
#' @param col_date name of the column containing dates in `df`
#' @param col_id name of the column containing IDs in `df`
#' @param cleaning type of cleaning to be performed on duplicates, by default set to "none" (i.e., no cleaning following the identification of duplicates)
#' @return This function returns a dataframe containing IDs and dates at which the ID has been allocated in different columns.
#' @export
#' @import dplyr

identify_repeat_duplicate <- function(df,
                                      col_id,
                                      col_date,
                                      cleaning = "none") {

  cleaned_df <- NULL

  qc_df <- identify_day0_duplicates(df, col_id, col_date)

  # Filter so that keep only repeat visits (between Day 1 and Day 28)
  if ( timci::is_not_empty(qc_df) ) {
    qc_df$diff <- as.Date(as.character(qc_df$date_2), format = "%Y-%m-%d") - as.Date(as.character(qc_df$date_1), format = "%Y-%m-%d")
    qc_df <- qc_df %>%
      dplyr::filter(diff > 0 & diff < 28) %>%
      dplyr::select_if(~!(all(is.na(.)) | all(. == "")))
  } else {
    qc_df <- NULL
  }

  list(qc_df, cleaned_df)

}

#' Identify true ID duplicates by dates (TIMCI-specific function)
#'
#' @param df dataframe containing the processed facility data
#' @param col_date name of the column containing dates in `df`
#' @param col_id name of the column containing IDs in `df`
#' @param cleaning type of cleaning to be performed on duplicates, by default set to "none" (i.e., no cleaning following the identification of duplicates)
#' @return This function returns a dataframe containing IDs and dates at which the ID has been allocated in different columns.
#' @export
#' @import dplyr

identify_true_duplicate <- function(df,
                                    col_id,
                                    col_date,
                                    cleaning = "none") {

  cleaned_df <- NULL

  qc_df <- identify_day0_duplicates(df, col_id, col_date)

  # Filter so that keep only duplicates that happened on the same day
  if ( timci::is_not_empty(qc_df) ) {
    qc_df$diff <- as.Date(as.character(qc_df$date_2), format="%Y-%m-%d") - as.Date(as.character(qc_df$date_1), format="%Y-%m-%d")
    qc_df <- qc_df %>%
      dplyr::filter(diff == 0) %>%
      dplyr::select_if(~!(all(is.na(.)) | all(. == "")))
  } else {
    qc_df <- NULL
  }

  list(qc_df, cleaned_df)

}

#' Identify multiple 28-disease episodes for the same children (TIMCI-specific function)
#'
#' @param df dataframe containing the processed facility data
#' @return This function returns a dataframe.
#' @export
#' @import dplyr

identify_multiple_enrolments <- function(df) {

  qc_df <- NULL
  cleaned_df <- NULL

  # Threshold to be determined exactly
  thres <- 75

  if ( timci::is_not_empty(df) ) {

    # Generate plausible combinations of names to be compared
    if ( Sys.getenv("TIMCI_COUNTRY") == "Tanzania" ) {
      df <- df %>%
        dplyr::mutate(refname = tolower(paste(fs_name, ms_name, ls_name, sep = ' '))) %>%
        dplyr::mutate(refname2 = tolower(paste(fs_name, ms_name, sep = ' '))) %>%
        dplyr::mutate(refname3 = tolower(paste(fs_name, ls_name, sep = ' ')))
    } else{
      df <- df %>%
        dplyr::mutate(refname = tolower(paste(fs_name, ls_name, sep = ' '))) %>%
        dplyr::mutate(refname2 = tolower(paste(ls_name, fs_name, sep = ' '))) %>%
        dplyr::mutate(refname3 = tolower(paste(fs_name, sep = ' ')))
    }

    # To be continued...

  }

  list(qc_df, cleaned_df)

}

#' Detect inconsistent participant names between follow-up and baseline visits (TIMCI-specific function)
#'
#' @param refdf reference dataframe
#' @param fudf dataframe containing the follow-up data to check
#' @return This function returns a dataframe.
#' @export
#' @import dplyr

detect_inconsistent_names_between_visits <- function(refdf,
                                                     fudf) {

  qc_df <- NULL
  cleaned_df <- NULL

  # Threshold to be determined exactly
  thres <- 75

  if ( timci::is_not_empty(refdf) & timci::is_not_empty(fudf)) {

    # Generate plausible combinations of names to be compared
    if ( Sys.getenv("TIMCI_COUNTRY") == "Tanzania" ) {
      refdf <- refdf %>%
        dplyr::mutate(refname = tolower(paste(fs_name, ms_name, ls_name, sep = ' '))) %>%
        dplyr::mutate(refname2 = tolower(paste(fs_name, ms_name, sep = ' '))) %>%
        dplyr::mutate(refname3 = tolower(paste(fs_name, ls_name, sep = ' ')))
    } else{
      refdf <- refdf %>%
        dplyr::mutate(refname = tolower(paste(fs_name, ls_name, sep = ' '))) %>%
        dplyr::mutate(refname2 = tolower(paste(ls_name, fs_name, sep = ' '))) %>%
        dplyr::mutate(refname3 = tolower(paste(fs_name, sep = ' ')))
    }

    cols <- colnames(fudf)

    if ( "name" %in% cols ) {
      fudf <- fudf %>%
        dplyr::mutate(name = tolower(name))
    } else if ( "fs_name_check" %in% cols ) {
      fudf <- fudf %>%
        dplyr::mutate(name = tolower(paste(fs_name_check, ls_name_check, sep = ' ')))
    }

    qc_df <- refdf %>%
      dplyr::select(refname,
                    refname2,
                    refname3,
                    child_id)
    if ( "child_id" %in% cols ) {
      qc_df <- qc_df %>%
        merge(fudf,
              by = 'child_id',
              all = TRUE)
    } else if ( "prev_id" %in% cols ) {
      qc_df <- qc_df %>%
        merge(fudf,
              by.x = 'child_id',
              by.y = 'prev_id',
              all = TRUE)
    }

    if ( "name" %in% colnames(qc_df)) {
      qc_df <- qc_df %>%
        dplyr::rowwise() %>%
        dplyr::mutate(lvr1 = timci::normalised_levenshtein_ratio(refname, name)) %>%
        dplyr::mutate(lvr2 = timci::normalised_levenshtein_ratio(refname2, name)) %>%
        dplyr::mutate(lvr3 = timci::normalised_levenshtein_ratio(refname3, name)) %>%
        dplyr::mutate(lvr = max(lvr1, lvr2, lvr3)) %>%
        dplyr::select(child_id,
                      uuid,
                      name,
                      refname,
                      lvr) %>%
        dplyr::filter(lvr < thres)
    } else {
      qc_df <- NULL
    }

  }

  list(qc_df, cleaned_df)

}


#' Detects matched names between follow-up and day 0 dataframes (TIMCI-specific function).
#'
#' This function searches for matched names between a follow-up dataframe and a day 0 dataframe. It returns a list containing two dataframes: one with quality control information, and another one with cleaned data.
#'
#' @param df Follow-up dataframe to search for matched names.
#' @param day0_df Day 0 dataframe to search for matched names.
#' @param col_date Name of the column containing the date in the `df` dataframe.
#' @param col_name Name of the column containing the name in the `df` dataframe.
#' @param ldate_diff Lower date difference (default is same day), negative numbers indicate a difference in the past, positive numbers indicate a difference in the future.
#' @param udate_diff Upper date difference (default is same day), negative numbers indicate a difference in the past, positive numbers indicate a difference in the future.
#'
#' @return A list with two dataframes: qc_df and cleaned_df.
#' @export

detect_matched_names_between_fu_and_day0 <- function(df,
                                                     day0_df,
                                                     col_date,
                                                     col_name,
                                                     ldate_diff = 0,
                                                     udate_diff = 0) {

  qc_df <- NULL
  cleaned_df <- NULL

  thres <- 75 # Threshold for Tanzania, check for other countries

  if (timci::is_not_empty(df) & timci::is_not_empty(day0_df)) {

    # Add derived column names that take into account country cultural specificity
    day0_df <- day0_df %>%
      timci::concatenate_names() %>%
      dplyr::rename(uuid_day0 = uuid) %>%
      dplyr::select(date_visit,
                    district,
                    fid,
                    child_id,
                    name,
                    mother_name,
                    uuid_day0)

    columns <- colnames(day0_df)
    out <- data.frame(matrix(nrow = 0, ncol = length(columns)))
    colnames(out) <- columns

    df_cols <- colnames(df)

    if (col_date %in% df_cols & col_name %in% df_cols & "uuid" %in% df_cols) {

      for (row in 1:nrow(df)) {

        # Extract constraints for restricting the search in day0_df
        cdate <- as.Date(df[row, col_date])
        min_date <- as.Date(cdate + ldate_diff)
        max_date <- as.Date(cdate + udate_diff)

        if (min_date <= max_date) {

          sub_df <- day0_df %>%
            dplyr::filter(district == df[row, "district"]) %>%
            dplyr::filter(date_visit >= min_date & date_visit <= max_date) %>%
            dplyr::mutate(matched_uuid = df[row, "uuid"]) %>%
            dplyr::mutate(child_name = df[row, col_name]) %>%
            dplyr::rowwise() %>%
            dplyr::mutate(lvr = timci::normalised_levenshtein_ratio(name, child_name)) %>%
            dplyr::ungroup() %>%
            dplyr::filter(lvr > thres) %>%
            dplyr::select(-child_name)

          if ( nrow(sub_df) > 0 ) {
            out <- rbind(out, sub_df)
          }

        }

      }

    }

    if ( nrow(out) > 0 ) {
      qc_df <- df %>%
        merge(out,
              by.x = "uuid",
              by.y = "matched_uuid",
              all = TRUE)
    }

  }

  list(qc_df, cleaned_df)

}
