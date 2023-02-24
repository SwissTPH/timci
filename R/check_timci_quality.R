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

  msg <- paste0("The table of participants for whom ",
                description,
                " is empty and therefore has not been exported.")

  if (!is.null(df)) {

    filename <- paste(idx, label, sep = "_")
    timestamp <- timci::export_df2xlsx(df, cdir, filename)
    msg <- paste0("The table of participants for whom ",
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
#' @return This function returns a dataframe containing data of possible duplicate participants only
#' @export
#' @import dplyr

detect_inconsistent_dates <- function(df,
                                      col_date_start,
                                      col_date_end,
                                      cleaning = "none") {

  qc_df <- NULL
  cleaned_df <- NULL

  df$diff <- as.Date(as.character(df[[col_date_end]]), format="%Y-%m-%d %T") - as.Date(as.character(df[[col_date_start]]), format="%Y-%m-%d %T")
  kcols <- c("fid", "child_id", col_date_start, col_date_end, "diff", "uuid")
  if ( !"start" %in% kcols ) {
    kcols <- c(kcols, "start")
  }
  if ( !"end" %in% kcols ) {
    kcols <- c(kcols, "end")
  }

  qc_df <- df %>%
    dplyr::filter(diff > 0)

  if (timci::is_not_empty(qc_df)) {
    qc_df <- qc_df %>%
      dplyr::select(kcols) %>%
      dplyr::arrange(diff, fid)
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
      dplyr::arrange(!!dplyr::enquo(col_date)) %>% # order by ascending dates
      dplyr::rename(id = !!dplyr::enquo(col_id)) %>%
      dplyr::group_by(id) %>%
      dplyr::mutate(row_n = row_number()) %>%
      tidyr::pivot_wider(id,
                         names_from = row_n,
                         values_from = c(col_date),
                         names_prefix = "date_")

    if ( "date_2" %in% colnames(qc_df) ) {
      qc_df <- qc_df %>%
        filter(!is.na(date_2))
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

  qc_df <- NULL
  cleaned_df <- NULL

  if ( timci::is_not_empty(df) ) {

    qc_df <- df %>%
      dplyr::filter(enrolled == 1) %>%
      dplyr::arrange(desc(date_visit))

    if (Sys.getenv("TIMCI_COUNTRY") == "Tanzania") {
      qc_df <- qc_df %>%
        dplyr::mutate(name = tolower(paste(fs_name, ms_name, ls_name, sep = ' ')))
    } else{
      qc_df <- qc_df %>%
        dplyr::mutate(name = tolower(paste(fs_name, ls_name, sep = ' ')))
    }

    qc_df <- qc_df %>%
      dplyr::rename(id = !!dplyr::enquo(col_id),
                    date = date_visit) %>%
      dplyr::group_by(id) %>%
      dplyr::mutate(row_n = row_number()) %>%
      tidyr::pivot_wider(id,
                         names_from = row_n,
                         values_from = c("date", "name"))

      if ( "date_2" %in% colnames(qc_df) ) {
        qc_df <- qc_df %>%
          dplyr::filter(!is.na(name_2)) %>%
          dplyr::mutate(lvr = timci::normalised_levenshtein_ratio(name_1, name_2))
        # Threshold to be determined exactly
        qc_df <- qc_df[qc_df$lvr > 75, c("id", "date_1", "date_2", "lvr")]

        # Filter so that keep only repeat visits (between Day 1 and Day 28)
        qc_df$diff <- as.Date(as.character(qc_df$date_1), format="%Y-%m-%d") - as.Date(as.character(qc_df$date_2), format="%Y-%m-%d")
        qc_df <- qc_df %>%
          dplyr::filter(diff >= 0 & diff <= 28)

      } else {
        qc_df <- NULL
      }

  }

  list(qc_df, cleaned_df)

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

#' Detect missing clinical presentation (TIMCI-specific function)
#'
#' @param facility_df dataframe containing the processed facility data
#' @return This function returns a dataframe containing only participants with no clinical presentation
#' @export
#' @import dplyr

detect_missing_clinical_presentation <- function(facility_df) {

  out <- NULL

  if ( timci::is_not_empty(facility_df) ) {

    facility_df$sx_vomit_evthing[is.na(facility_df$sx_vomit_evthing)] <- 0
    facility_df$sx_unable_feed[is.na(facility_df$sx_unable_feed)] <- 0

    out <- facility_df %>%
      dplyr::mutate(danger_signs = ifelse(sx_convulsions == 1 | sx_lethargy == 1 | sx_vomit_evthing == 1 | sx_unable_feed == 1,
                                          1,
                                          0)) %>%
      dplyr::mutate(missing_clinical_presentation = ifelse(danger_signs == 0 & (sx_vomit == 0 | sx_vomit == 98 ) & (sx_less_feed == 0 | sx_less_feed == 98) & (sx_cough == 0 | sx_cough == 98) & (sx_difficulty_breath == 0 | sx_difficulty_breath == 98) & (sx_diarrhoea == 0 | sx_diarrhoea == 98) & (sx_fever == 0 | sx_fever == 98) & sx_var == 96,
                                                           1,
                                                           0)) %>%
      filter(missing_clinical_presentation == 1)

  }

  out

}

#' Detect missing diagnosis (TIMCI-specific function)
#'
#' @param facility_df dataframe containing the processed facility data
#' @return This function returns a dataframe containing only participants with no diagnosis
#' @export
#' @import dplyr

detect_missing_diagnosis <- function(facility_df) {

  out <- NULL

  if ( timci::is_not_empty(facility_df) ) {

    out <- facility_df %>%
      dplyr::mutate(missing_diagnosis = ifelse( dx_severe == 0 & dx_pneumonia == 0 & dx_diarrhoea == 0 & dx_dehydration == 0 & dx_malaria == 0 & dx_ear_infection == 0 & dx_malnutrition == 0 &	dx_anaemia == 0	&	dx_mlist == 96 & dx_oth_yn == 0,
                                               1,
                                               0 )) %>%
      filter(missing_diagnosis == 1)

  }

  out

}

#' Detect missing referral reported by caregiver at the exit of the consultation (TIMCI-specific function)
#'
#' @param facility_df dataframe containing the processed facility data
#' @return This function returns a dataframe containing only participants with no referral information at the exit of the consultation.
#' @export
#' @import dplyr

detect_missing_referral <- function(facility_df) {

  out <- NULL

  if ( timci::is_not_empty(facility_df) ) {

    facility_df$referral_cg <- ifelse(!is.na(facility_df$referral_cg), facility_df$referral_cg, 100)

    out <- facility_df %>%
      dplyr::mutate(missing_referral_cg = ifelse(referral_cg == 98 | referral_cg == 97 | referral_cg == 100,
                                                 1,
                                                 0)) %>%
      filter(missing_referral_cg == 1)

  }

  out

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
