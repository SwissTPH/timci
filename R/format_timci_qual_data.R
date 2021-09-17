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
  } else{
    dictionary <- readxl::read_excel(system.file(file.path('extdata', "main_dict.xlsx"), package = 'timci'))
  }


  # Extract data for caregiver IDIs
  sub <- subset(dictionary, qual == 1)
  cg_subselection <- cg_selection[sub$new]
  cg_subselection

}
