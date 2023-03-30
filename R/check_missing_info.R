#' Detect missing (NA) value (TIMCI-specific function)
#'
#' @param df dataframe
#' @param col column name to check for missingness
#' @return This function returns a dataframe containing only participants with no information in column `col`
#' @export
#' @import dplyr rlang

detect_missing_var <- function(df, col) {

  out <- NULL
  col <- rlang::sym(col)

  if ( timci::is_not_empty(df) ) {

    out <- df %>%
      dplyr::mutate(missing = ifelse(is.na(!!col),
                                     1,
                                     0)) %>%
      filter(missing == 1)

  }

  out

}

#' Detect blank ("") value (TIMCI-specific function)
#'
#' @param df dataframe
#' @param col column name to check for missingness
#' @return This function returns a dataframe containing only participants with no information in column `col`
#' @export
#' @import dplyr rlang

detect_blank_var <- function(df, col) {

  out <- NULL
  col <- rlang::sym(col)

  if ( timci::is_not_empty(df) ) {

    out <- df %>%
      dplyr::mutate(missing = ifelse(!!col == "",
                                     1,
                                     0)) %>%
      filter(missing == 1)

  }

  out

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

#' Detect missing treatment (TIMCI-specific function)
#'
#' @param facility_df dataframe containing the processed facility data
#' @return This function returns a dataframe containing only participants with no treatment
#' @export
#' @import dplyr

detect_missing_treatment <- function(facility_df) {

  out <- NULL

  if ( timci::is_not_empty(facility_df) ) {

    out <- facility_df %>%
      dplyr::mutate(missing = ifelse(is.na(rx_amoxicillin),
                                     1,
                                     0)) %>%
      filter(missing == 1)

  }

  out

}
