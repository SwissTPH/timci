#' Count submissions (TIMCI-specific)
#'
#' @param facility_data Dataframe that contains TIMCI Day 0 facility data.
#' @param raw_day7fu_data Dataframe that contains TIMCI Day 7 follow-up data.
#' @param raw_hospit_data Dataframe that contains TIMCI hospital follow-up data.
#' @param raw_day28fu_data Dataframe that contains TIMCI Day 28 follow-up data.
#'
#' @export

count_submissions <- function(facility_data,
                              raw_day7fu_data,
                              raw_hospit_data,
                              raw_day28fu_data){

  n_total <- 0

  # Screening data
  n_screened <- nrow(facility_data)
  screening_data <- timci::extract_screening_data(facility_data)
  visit_names <- c(screening_str)
  submissions <- c(n_screened)
  n_total <- n_total + n_screened

  study_data <- timci::extract_all_visits(facility_data)
  res <- timci::extract_enrolled_participants(facility_data)
  noneligible <- timci::extract_noneligible(facility_data)

  # Baseline data
  baseline_data <- timci::extract_baseline_visits(study_data)
  baseline_data <- timci::allocate_screening_facility(baseline_data,
                                                      params$research_facilities)

  demog_data <- res[[1]]
  n_enrolled <- nrow(demog_data)
  n_enrolled7 <- nrow(dplyr::filter(demog_data, date_visit >= as.Date(end_date - 6)))
  n_enrolled28 <- nrow(dplyr::filter(demog_data, date_visit >= as.Date(end_date - 27)))
  visit_names <- c(visit_names, paste0(baseline_str, kableExtra::footnote_marker_number(1)))
  submissions <- c(submissions, n_enrolled)

  # Count facility submissions corresponding to repeat visits
  repeat_data <- timci::extract_repeat_visits(study_data)
  n_repeat <- nrow(repeat_data)
  visit_names <- c(visit_names, paste0(repeat_str, kableExtra::footnote_marker_number(2)))
  submissions <- c(submissions, n_repeat)

  # Count Day 7 phone call submissions
  if (!is.null(raw_day7fu_data)) {
    n_day7fu <- nrow(raw_day7fu_data)
    visit_names <- c(visit_names, day7_phone_call_str)
    submissions <- c(submissions, n_day7fu)
    n_total <- n_total + n_day7fu
  }

  if (Sys.getenv('TIMCI_COUNTRY') == "Tanzania" || Sys.getenv('TIMCI_COUNTRY') == "India") {
    if (!is.null(raw_day28fu_data)) {
      n_day28fu <- nrow(raw_day28fu_data)
      visit_names <- c(visit_names, "Day 28 phone call")
      submissions <- c(submissions, n_day28fu)
      n_total <- n_total + n_day28fu
    }
  }

  # Count hospital visit submissions
  if (!is.null(raw_hospit_data)) {
    n_hospit <- nrow(raw_hospit_data)
    visit_names <- c(visit_names, hospital_submission_str)
    submissions <- c(submissions, n_hospit)
    n_total <- n_total + n_hospit
  }

  # Count withdrawal submissions
  if (!is.null(raw_withdrawal_data)) {
    n_withdrawal <- nrow(raw_withdrawal_data)
    visit_names <- c(visit_names, withdrawal_str)
    submissions <- c(submissions, n_withdrawal)
    n_total <- n_total + n_withdrawal
  }

}
