#' Detect submission not submitted on the day they were finalised (ODK function)
#'
#' @param df dataframe containing any ODK data, assuming standard metadata fields (`start`, `end`) are present.
#' @return This function returns a dataframe containing the duration between start and end
#' @export
#' @import dplyr magrittr

detect_submission_duration <- function(df) {

  qc <- NULL

  # Duration from form completion to transfer on the server
  df$diff <- as.Date(as.character(df$submission_date), format="%Y-%m-%d %T") - as.Date(as.character(df$end), format="%Y-%m-%d %T")
  qc <- df[c("fid", "child_id", "start", "diff")] %>%
    dplyr::arrange(fid, diff)

}

#' Detect submission not finalised on the day they were started (ODK function)
#'
#' @param df dataframe containing any ODK data, assuming standard metadata fields (`start`, `end`) are present.
#' @return This function returns a dataframe containing the duration between start and end
#' @export
#' @import dplyr magrittr

detect_submission_duration2 <- function(df) {

  qc <- NULL

  # Duration from form completion to transfer on the server
  df$diff <- as.Date(as.character(df$end), format="%Y-%m-%d %T") - as.Date(as.character(df$start), format="%Y-%m-%d %T")
  qc <- df[c("fid", "child_id", "start", "diff")] %>%
    dplyr::arrange(fid, diff)

}

#' Detect ID duplicates (TIMCI-specific function)
#'
#' @param df dataframe containing the processed facility data
#' @param col vector containing a column of the processed facility data
#' @return This function returns a dataframe containing unique IDs and their frequencies (a frequency strictly superior to 1 indicates a duplicate).
#' @export
#' @import dplyr magrittr

detect_id_duplicates <- function(df, col = child_id) {

  # Quote the arguments that refer to data frame columns
  col <- dplyr::enquo(col)

  res <- df %>%
    group_by(!!col) %>%
    count
  res <- res %>%
    dplyr::rename(id_fq = n)

}

#' Detect name duplicates (TIMCI-specific function)
#' Search for exact matches and switches
#'
#' @param df dataframe containing the processed facility data
#' @return This function returns a dataframe containing IDs and names of duplicate names
#' @export
#' @import dplyr magrittr

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

#' Detect inconsistent dates (TIMCI-specific function)
#'
#' @param df1 dataframe containing the processed facility data
#' @param df2 dataframe containing the processed facility data
#' @return This function returns a dataframe containing data of possible duplicate participants only
#' @export
#' @import dplyr magrittr

detect_inconsistent_dates <- function(df1, df2) {

  # To complete

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
