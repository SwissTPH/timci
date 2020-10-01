#' Deidentify data
#'
#' De-identification of the TIMCI research data
#' @param data numeric, the non de-identified (raw) data vector
#' @return This function returns de-identified data.
#' @export
#' @import dplyr magrittr

deidentify_data <- function(data) {

  cp <- data %>%
    dplyr::filter(data$screening_age_eligibility_visit_reason_previous_enrolment_consent_enrolment==1)
  cp <- cp %>%
    dplyr::select('id',
                  'today',
                  'duration',
                  'screening_age_eligibility_visit_reason_previous_enrolment_consent_a1_a_4',
                  'crfs_timci_02b_a4_c_5',
                  'crfs_timci_06a_d2_6a',
                  'crfs_timci_09a_g3_1'
    )
  cp <- cp %>%
    dplyr::rename('date' = 'today',
                  'sick_child_id' = 'screening_age_eligibility_visit_reason_previous_enrolment_consent_a1_a_4',
                  'residence_type' = 'crfs_timci_02b_a4_c_5',
                  'temperature' = 'crfs_timci_06a_d2_6a',
                  'diagnoses' = 'crfs_timci_09a_g3_1'
    )

  return(cp)

}
