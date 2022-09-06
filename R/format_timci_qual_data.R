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
  cg_selection <- day7fu[day7fu$qual_ok == 1,]
  cg_selection <- cg_selection %>%
    dplyr::select(c("qual_ok", "child_id"))

  # Merge cg_selection with participant contact information
  cg_selection <- merge(cg_selection, main_data, by = "child_id", no.dups = TRUE)

  # Merge dictionaries
  if (Sys.getenv('TIMCI_COUNTRY') == 'Senegal') {
    dictionary <- readxl::read_excel(system.file(file.path('extdata', "main_dict_senegal.xlsx"), package = 'timci'))
  } else if (Sys.getenv('TIMCI_COUNTRY') == 'Kenya') {
    dictionary <- readxl::read_excel(system.file(file.path('extdata', "main_dict_kenya.xlsx"), package = 'timci'))
  } else if (Sys.getenv('TIMCI_COUNTRY') == 'India') {
    dictionary <- readxl::read_excel(system.file(file.path('extdata', "main_dict_india.xlsx"), package = 'timci'))
  } else {
    dictionary <- readxl::read_excel(system.file(file.path('extdata', "main_dict_tanzania.xlsx"), package = 'timci'))
  }

  # Extract data for caregiver IDIs
  sub <- subset(dictionary, qual == 1)
  cg_subselection <- cg_selection %>%
    dplyr::select(dplyr::any_of(sub$new)) %>%
    dplyr::mutate(referral_comb = ifelse(referral_cg == 1 | referral_hf == 1, 1, 0)) %>%
    dplyr::mutate(month_category = ifelse(age_mo < 2, "0-1m", ifelse(age_mo < 12, "2-11m", ifelse(age_mo < 24, "12-23m", "24-59m"))))

}
