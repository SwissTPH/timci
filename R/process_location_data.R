#' Process GPS coordinates (TIMCI-specific function)
#'
#' @param df dataframe
#' @return This function returns cleaned GPS coordinates.
#' @export

process_gps_coordinates <- function(df) {

  gps_coordinates <- NULL
  gps_vec <- c("latitude", "longitude", "gps_accuracy")

  if (!is.null(df)) {

    cols <- colnames(df)

    if (("latitude" %in% cols) & ("longitude" %in% cols)) {

      gps_coordinates <- df[, gps_vec]
      gps_coordinates <- na.omit(gps_coordinates) %>%
        dplyr::filter(gps_accuracy <= 150)# gps_coordinates[!apply(gps_coordinates == "", 1, all),]

    }

  }

  gps_coordinates

}
