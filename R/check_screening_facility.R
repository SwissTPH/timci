#' Extract main device ID used in a facility at a given date
#'
#' @param facility_data dataframe containing the processed facility data
#' @param research_facilities dataframe containing the list of research facilities and their characteristics
#'
#' @export
#' @import dplyr

extract_daily_facility_for_deviceid <- function(facility_data,
                                                research_facilities) {

  cols <- c("facility_id", "facility_name")
  uq_facilities <- research_facilities[, cols] %>%
    distinct()

  facility_data[facility_data$fid %in% research_facilities$facility_id, ] %>%
    dplyr::select(c('date_visit', 'device_id', 'fid')) %>%
    dplyr::group_by(date_visit, device_id, fid) %>%
    dplyr::mutate(n = n()) %>%
    ungroup %>%
    merge(y = uq_facilities,
          by.x = 'fid',
          by.y = 'facility_id',
          all.x = TRUE) %>%
    dplyr::arrange(date_visit, device_id, desc(n)) %>%
    dplyr::distinct(date_visit, device_id, .keep_all = TRUE)

}

#' Extract main device ID used in a facility
#'
#' @param facility_data dataframe containing the processed facility data
#' @param research_facilities dataframe containing the list of research facilities and their characteristics
#'
#' @export
#' @import dplyr

extract_main_facility_for_deviceid <- function(facility_data,
                                                research_facilities) {

  cols <- c("facility_id", "facility_name")
  uq_facilities <- research_facilities[, cols] %>%
    distinct()

  facility_data[facility_data$fid %in% research_facilities$facility_id, ] %>%
    dplyr::select(c('device_id', 'fid')) %>%
    dplyr::group_by(device_id, fid) %>%
    dplyr::mutate(n = n()) %>%
    ungroup %>%
    merge(y = uq_facilities,
          by.x = 'fid',
          by.y = 'facility_id',
          all.x = TRUE) %>%
    dplyr::arrange(device_id, desc(n)) %>%
    dplyr::distinct(device_id, .keep_all = TRUE)

}

#' Allocate the screening facility
#'
#' Several information can be triangulated to:
#' the device ID,
#' the GPS coordinates,
#' the selection of the facility by the research assistant
#' and the child ID
#'
#' @param facility_data dataframe containing the processed facility data
#' @param research_facilities dataframe containing the list of research facilities and their characteristics
#'
#' @export
#' @import dplyr

allocate_screening_facility <- function(facility_data,
                                        research_facilities) {

  # Extract the main device ID used in a facility
  ddf1 <- extract_main_facility_for_deviceid(facility_data,
                                             research_facilities) %>%
    dplyr::rename(fid_from_main_device = fid,
                  facility_name_from_main_device = facility_name)

  # Extract the main device ID used in a facility at a given date
  ddf2 <- extract_daily_facility_for_deviceid(facility_data,
                                             research_facilities) %>%
    dplyr::rename(fid_from_daily_device = fid,
                  facility_name_from_daily_device = facility_name)

  # Remove all data that originates from non-valid device IDs
  #out <- facility_data[facility_data$device_id %in% ddf2$device_id,] # commented on 28.07.2023 for Tanzania / India hard locks
  out <- facility_data

  out <- out %>%
    merge(y = ddf1[, c("device_id", "fid_from_main_device", "facility_name_from_main_device")],
          by = c('device_id'),
          all.x = TRUE) %>%
    merge(y = ddf2[, c("date_visit", "device_id", "fid_from_daily_device", "facility_name_from_daily_device")],
          by = c('date_visit', 'device_id'),
          all.x = TRUE)
  out <- out %>%
    ungroup() %>%
    dplyr::mutate(fid_from_device = ifelse(!is.na(fid_from_daily_device), fid_from_daily_device, fid_from_main_device)) %>%
    dplyr::mutate(facility_name = ifelse(!is.na(fid_from_daily_device), facility_name_from_daily_device, facility_name_from_main_device)) %>%
    dplyr::mutate(fid_discrepancy = ifelse(fid_from_device != fid_from_main_device, 1, 0))

  drop <- c("fid_from_daily_device",
            "facility_name_from_daily_device")

  out[,!(names(out) %in% drop)]

}

#' Allocate the screening facility (copy to allow for a list output - to merge with function above when time allows for it)
#'
#' Several information can be triangulated to:
#' the device ID,
#' the GPS coordinates,
#' the selection of the facility by the research assistant
#' and the child ID
#'
#' @param facility_data dataframe containing the processed facility data
#' @param research_facilities dataframe containing the list of research facilities and their characteristics
#'
#' @export
#' @import dplyr

allocate_screening_facility2 <- function(facility_data,
                                         research_facilities) {

  # Extract the main device ID used in a facility
  ddf1 <- extract_main_facility_for_deviceid(facility_data,
                                             research_facilities) %>%
    dplyr::rename(fid_from_main_device = fid,
                  facility_name_from_main_device = facility_name)

  # Extract the main device ID used in a facility at a given date
  ddf2 <- extract_daily_facility_for_deviceid(facility_data,
                                              research_facilities) %>%
    dplyr::rename(fid_from_daily_device = fid,
                  facility_name_from_daily_device = facility_name)

  # Remove all data that originates from non-valid device IDs
  out <- facility_data[facility_data$device_id %in% ddf2$device_id,]
  qc_df <- facility_data[!facility_data$device_id %in% ddf2$device_id,]

  out <- out %>%
    merge(y = ddf1[, c("device_id", "fid_from_main_device", "facility_name_from_main_device")],
          by = c('device_id'),
          all.x = TRUE) %>%
    merge(y = ddf2[, c("date_visit", "device_id", "fid_from_daily_device", "facility_name_from_daily_device")],
          by = c('date_visit', 'device_id'),
          all.x = TRUE)
  out <- out %>%
    ungroup() %>%
    dplyr::mutate(fid_from_device = ifelse(!is.na(fid_from_daily_device),
                                           fid_from_daily_device,
                                           fid_from_main_device)) %>%
    dplyr::mutate(facility_name = ifelse(!is.na(fid_from_daily_device),
                                         facility_name_from_daily_device,
                                         facility_name_from_main_device)) %>%
    dplyr::mutate(fid_discrepancy = ifelse(fid_from_device != fid_from_main_device,
                                           1,
                                           0))

  drop <- c("fid_from_daily_device",
            "facility_name_from_daily_device")

  out <- out[,!(names(out) %in% drop)]

  list(qc_df, out)

}

#' Remove facilities in which data collection is conducted for another study that the current study (India-specific)
#'
#' @param facility_data dataframe containing the processed facility data
#' @param all_facilities dataframe containing the list of research facilities and their characteristics
#' @param excluded_facilities dataframe containing the list of research facilities to be excluded and their characteristics
#'
#' @export
#' @import dplyr

remove_facilities_for_other_studies <- function(facility_data,
                                                all_facilities,
                                                excluded_facilities) {

  # Extract the main device ID used in a facility at a given date
  ddf <- extract_daily_facility_for_deviceid(facility_data,
                                             all_facilities) %>%
    dplyr::rename(fid_from_daily_device = fid,
                  facility_name_from_daily_device = facility_name)

  out <- facility_data %>%
    merge(y = ddf[, c("date_visit", "device_id", "fid_from_daily_device")],
          by = c('date_visit', 'device_id'),
          all.x = TRUE)

  # Remove all data that originates from facilities used in other TIMCI studies but not in the current study
  qc_df <- out[out$fid_from_daily_device %in% excluded_facilities$facility_id,]
  out <- out[!out$fid_from_daily_device %in% excluded_facilities$facility_id,]

  drop <- c("fid_from_daily_device")

  out <- out[,!(names(out) %in% drop)]

  list(qc_df, out)

}
