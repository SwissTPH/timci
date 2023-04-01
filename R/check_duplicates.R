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
                         values_from = date_value2,
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

#' Identify ID duplicates by dates (TIMCI-specific function)
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

  # Threshold to be determined exactly
  thres <- 75

  qc_df <- NULL
  cleaned_df <- NULL

  if ( timci::is_not_empty(df) ) {

    qc_df <- df %>%
      dplyr::filter(enrolled == 1) %>%
      dplyr::arrange(!!dplyr::enquo(col_date)) # Sort data by dates

    if ( Sys.getenv("TIMCI_COUNTRY") == "Tanzania" ) {
      qc_df <- qc_df %>%
        dplyr::mutate(name = tolower(paste(fs_name, ms_name, ls_name, sep = ' ')))
    } else{
      qc_df <- qc_df %>%
        dplyr::mutate(name = tolower(paste(fs_name, ls_name, sep = ' ')))
    }

    qc_df <- qc_df %>%
      dplyr::rename(id = !!dplyr::enquo(col_id),
                    date = !!dplyr::enquo(col_date)) %>%
      dplyr::group_by(id) %>%
      dplyr::mutate(row_n = row_number()) %>%
      tidyr::pivot_wider(id,
                         names_from = row_n,
                         values_from = c("date", "name"))

    if ( "date_2" %in% colnames(qc_df) ) {
      qc_df <- qc_df %>%
        dplyr::filter(!is.na(name_2)) %>%
        dplyr::mutate(lvr = timci::normalised_levenshtein_ratio(name_1, name_2))
      qc_df <- qc_df[qc_df$lvr > thres, c("id", "date_1", "date_2", "lvr")]

      # Filter so that keep only repeat visits (between Day 1 and Day 28)
      qc_df$diff <- as.Date(as.character(qc_df$date_2), format="%Y-%m-%d") - as.Date(as.character(qc_df$date_1), format="%Y-%m-%d")
      qc_df <- qc_df %>%
        dplyr::filter(diff >= 0 & diff <= 28)

    } else {
      qc_df <- NULL
    }

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

    fudf <- fudf %>%
      dplyr::mutate(name = tolower(name))

    qc_df <- refdf %>%
      dplyr::select(refname,
                    refname2,
                    refname3,
                    child_id) %>%
      merge(fudf,
            by = 'child_id',
            all = TRUE) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(lvr1 = timci::normalised_levenshtein_ratio(refname, name)) %>%
      dplyr::mutate(lvr2 = timci::normalised_levenshtein_ratio(refname2, name)) %>%
      dplyr::mutate(lvr3 = timci::normalised_levenshtein_ratio(refname3, name)) %>%
      dplyr::mutate(lvr = max(lvr1, lvr2, lvr3)) %>%
      dplyr::select(child_id,
                    uuid,
                    lvr) %>%
      dplyr::filter(lvr < thres)

  }

  list(qc_df, cleaned_df)

}
