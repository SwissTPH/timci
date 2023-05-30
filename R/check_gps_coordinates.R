#' Find Closest Facility
#'
#' This function finds the closest facility from a data frame to a set of facilities
#'
#' @param df A data frame containing longitude and latitude information for each point
#' @param facilities A data frame containing longitude and latitude information for each facility
#'
#' @return A data frame with the closest facilities within a distance of 100 units
#'
#' @import dplyr
#' @import geosphere
#'
#' @export

find_closest_facility <- function(df,
                                  facilities) {

  cols <- colnames(df)

  if ( "longitude" %in% cols ) {

    tmp1 <- df %>%
      dplyr::filter(longitude == "" | is.na(longitude))

    tmp2 <- df %>%
      dplyr::filter(longitude != "" & !is.na(longitude))

    facilities2 <- facilities %>%
      dplyr::rename(lon = Longitude,
                    lat = Latitude) %>%
      dplyr::slice(rep(row_number(), nrow(tmp2)))

    out <- tmp2 %>%
      dplyr::slice(rep(row_number(), nrow(facilities))) %>%
      dplyr::bind_cols(facilities2) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(dist = geosphere::distHaversine(c(as.numeric(longitude), as.numeric(latitude)),
                                                    c(as.numeric(lon), as.numeric(lat)))) %>%
      dplyr::ungroup() %>%
      dplyr::filter(dist < 200) %>%
      dplyr::filter(fid != facility_id) %>%
      dplyr::bind_rows(tmp1)

    out <- out[order(out$dist,
                     na.last = TRUE,
                     decreasing = FALSE),] %>%
      dplyr::distinct(uuid, .keep_all = TRUE)

  } else {
      out <- df
  }

  out

}
