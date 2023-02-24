#' Generate caregiver recruitment log (TIMCI-specific function)
#'
#' Generate a list of caregiver to be called for the qualitative studies
#' @param main_data dataframe containing day 0 data (either personally deidentifiable data or deidentified facility data)
#' @param day7fu dataframe containing day7 follow-up data
#' @return This function returns a dataframe.
#' @export
#' @import magrittr dplyr

generate_cg_log <- function(main_data, day7fu) {

  # Select only caregivers who express willingness to participate at Day 7
  cg_selection <- day7fu %>%
    dplyr::filter(qual_ok == 1) %>%
    dplyr::select(c("qual_ok", "child_id"))

  # Merge cg_selection with participant contact information
  cg_selection <- merge(cg_selection,
                        main_data,
                        by = "child_id",
                        no.dups = TRUE)

  # Merge dictionaries
  dictionary <- timci::read_day0_xls_dict(is_pilot = FALSE)
  sub <- subset(dictionary, qual == 1)

  # Extract data for caregiver IDIs
  cg_subselection <- cg_selection %>%
    dplyr::select(dplyr::any_of(sub$new)) %>%
    dplyr::mutate(referral_comb = ifelse(referral_cg == 1 | referral_hf == 1, 1, 0)) %>%
    dplyr::mutate(
      month_category = dplyr::case_when(
        age_mo < 2 ~ "0-1m",
        age_mo < 12 ~ "2-11m",
        age_mo < 24 ~ "12-23m",
        age_mo >= 24 & age_mo < 60 ~ "24-59m"
      )
    )

}
