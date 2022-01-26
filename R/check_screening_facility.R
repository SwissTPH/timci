#' Extract device IDs
#'
#' @param facility_data dataframe containing the processed facility data
#' @param research_facilities dataframe containing the list of research facilities and their characteristics
#' @return
#' @export
#' @import dplyr
#'
#' @examples

extract_daily_facility_for_deviceid <- function(facility_data,
                                                research_facilities) {

  facility_data[facility_data$fid %in% research_facilities$facility_id, ] %>%
    dplyr::select(c('date_visit', 'device_id', 'fid')) %>%
    dplyr::group_by(date_visit, device_id, fid) %>%
    dplyr::mutate(n = n()) %>%
    ungroup %>%
    merge(y = research_facilities[, c("facility_id", "facility_name")],
          by.x = 'fid',
          by.y = 'facility_id',
          all.x = TRUE) %>%
    dplyr::arrange(date_visit, device_id, desc(n)) %>%
    dplyr::distinct(date_visit, device_id, .keep_all = TRUE)

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
#' @return
#' @export
#' @import dplyr
#'
#' @examples

allocate_screening_facility <- function(facility_data,
                                        research_facilities) {

  ddf <- extract_daily_facility_for_deviceid(facility_data,
                                             research_facilities) %>%
    dplyr::rename(fid_from_device = fid)
  fn <- timci::export_df2xlsx(ddf,
                              getwd(),
                              "devices")

  facility_data[facility_data$device_id %in% ddf$device_id,] %>%
    merge(y = ddf[, c("date_visit", "device_id", "fid_from_device", "facility_name")],
          by = c('date_visit', 'device_id'),
          all.x = TRUE)

}
