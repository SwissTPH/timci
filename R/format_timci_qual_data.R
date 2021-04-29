#' Generate caregiver recruitment log (TIMCI-specific function)
#'
#' Generate a list of caregiver to be called for the qualitative studies
#' @param pii dataframe containing personally identifiable data
#' @param day7fu dataframe containing day7 follow-up data
#' @return This function returns a dataframe.
#' @export
#' @import magrittr dplyr

generate_cg_log <- function(pii, day7fu) {

  # Select only caregivers who express willingness to participate at Day 7
  cg_selection <- day7fu[day7fu$qual_ok,]

  # Merge cg_selection with participant contact information
  cg_selection <- merge(cg_selection, pii, by = "child_id", no.dups = TRUE)

  drops <- c("date_visit", "first_name", "last_name", "mother_name")
  pii[, !(names(pii) %in% drops)]

  cg_selection

}
