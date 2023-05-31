#' Create cleaning flowchart for screening data (TIMCI-specific)
#'
#' @param n_raw_screening_records Initial number of screening records
#' @param n_nonvalid_deviceid_records Number of screening records with a non-valid device ID
#' @param n_other_fid_records Number of screening records corresponding to facility involved in another TIMCI study (India-specific)
#' @param n_before_startdate_records Number of screening records with an entry date anterior to the start date
#' @param n_after_lockdate_records Number of screening records with an entry date posterior to the lock date
#' @param n_ineligible_cg_records Number of screening records with an ineligible caregiver
#' @param n_edited_repeat_visit_records Number of screening records that were edited manually
#' @param n_incorrect_date_setup_records Number of screening records with an incorrect date that had to be edited manually
#' @param n_late_submissions Number of screening records with late submission
#' @param n_late_completions Number of screening records with late completion
#' @param n_inconsistent_age_info Number of screening records with inconsistent age information
#' @param n_cleaned_screening_records Number of cleaned screening records
#' @return This function returns a graph object
#' @export
#' @import DiagrammeR

create_screening_qc_flowchart <- function(n_raw_screening_records,
                                          n_nonvalid_deviceid_records,
                                          n_other_fid_records,
                                          n_before_startdate_records,
                                          n_after_lockdate_records,
                                          n_ineligible_cg_records,
                                          n_edited_repeat_visit_records,
                                          n_incorrect_date_setup_records,
                                          n_late_submissions,
                                          n_late_completions,
                                          n_inconsistent_age_info,
                                          n_cleaned_screening_records) {

  n_excluded <- n_nonvalid_deviceid_records + n_other_fid_records + n_before_startdate_records + n_after_lockdate_records
  n_edited <- n_incorrect_date_setup_records + n_ineligible_cg_records + n_edited_repeat_visit_records
  n_informed <- n_late_submissions + n_late_completions + n_inconsistent_age_info

  gr <- sprintf("digraph flowchart {
                  # node definitions with substituted label text
                  node [fontname = Helvetica, shape = rectangle, fixedsize = false, width = 1]

                  1 [label = 'Raw screening records\nN = %s', shape = folder, style = filled, fillcolor = '#f79679']
                  m1 [label = 'Excluded (N = %s)\n%s record(s) with non-valid device IDs\n%s record(s) collected in a facility from other TIMCI studies\n%s record(s) anterior to the study start date\n%s record(s) posterior to the lock date']
                  m2 [label = 'Automatically edited (N = %s)\n%s record(s) corrected for the creation date\n%s record(s) with ineligible (underaged) caregiver']
                  m3 [label = 'Manually edited (N = XX)\nXX record(s) corrected for non-valid facility IDs (out of XX detected)\nXX record(s) corrected for incorrect facility ID (out of XX detected)\nXX record(s) corrected for incorrect facility ID (out of XX detected)\nXX record(s) corrected for incorrect facility ID (out of XX detected)\n%s record(s) modified from new enrolment to repeat visit (out of XX detected)']
                  m4 [label = 'Other checks triggered (N = %s)\n%s record(s) with late submission\n%s record(s) with late completion\n%s record(s) with inconsistent age information']
                  2 [label = 'Cleaned screening records\nN = %s', shape = folder, style = filled, fillcolor = '#f79679']

                  node [shape=none, width=0, height=0, label='']
                  p4 -> 2 [arrowhead='none']
                  {rank=same; p4 -> m4}

                  node [shape=none, width=0, height=0, label='']
                  p3 -> p4 [arrowhead='none']
                  {rank=same; p3 -> m3}

                  node [shape=none, width=0, height=0, label='']
                  p2 -> p3 [arrowhead='none']
                  {rank=same; p2 -> m2}

                  node [shape=none, width=0, height=0, label='']
                  p1 -> p2 [arrowhead='none']
                  {rank=same; p1 -> m1}

                  edge [dir=none]
                  1 -> p1
                }",
                n_raw_screening_records,
                n_excluded,
                n_nonvalid_deviceid_records,
                n_other_fid_records,
                n_before_startdate_records,
                n_after_lockdate_records,
                n_edited,
                n_incorrect_date_setup_records,
                n_ineligible_cg_records,
                n_edited_repeat_visit_records,
                n_informed,
                n_late_submissions,
                n_late_completions,
                n_inconsistent_age_info,
                n_cleaned_screening_records)

  DiagrammeR::grViz(gr)

}

#' Create a flowchart summarizing the cleaning of drug re-entry data (TIMCI-specific)
#'
#' This function generates a flowchart summarizing the cleaning of drug re-entry data
#' specific to the TIMCI project. The flowchart displays the initial number of drug
#' records, the number of records excluded due to duplicate IDs, and the number of records
#' that passed other cleaning checks for specific drugs. The function returns a graph object
#' that can be plotted using the DiagrammeR package.
#'
#' @param n_raw_drug_records The initial number of drug records.
#' @param n_dropped_duplicate_records The number of drug records dropped due to duplicate IDs.
#' @param n_amox_records The number of records that passed the cleaning check for amoxicillin.
#' @param n_aclav_records The number of records that passed the cleaning check for amoxicillin + clavulanic acid.
#' @param n_metro_records The number of records that passed the cleaning check for metronidazole.
#' @param n_ctx_records The number of records that passed the cleaning check for cotrimoxazole.
#' @param n_cipro_records The number of records that passed the cleaning check for ciprofloxacin.
#' @param n_genta_records The number of records that passed the cleaning check for gentamicin.
#' @param n_penig_records The number of records that passed the cleaning check for benzylpenicillin.
#' @param n_ceftriaxone_records The number of records that passed the cleaning check for ceftriaxone.
#' @param n_cefixime_records The number of records that passed the cleaning check for cefixime.
#' @param n_ampi_records The number of records that passed the cleaning check for ampicillin.
#' @param n_azithromycin_records The number of records that passed the cleaning check for azithromycin.
#' @param n_benzathinepeniG_records The number of records that passed the cleaning check for benzathine benzylpenicillin.
#' @param n_antibiotics_records The number of records that passed the cleaning check for other antibiotics.
#' @param n_antimalarials_records The number of records that passed the cleaning check for antimalarials.
#' @param n_cleaned_drug_records The total number of cleaned drug records.
#'
#' @return This function returns a graph object that can be plotted using the DiagrammeR package.
#'
#' @import DiagrammeR
#' @export

create_drug_qc_flowchart <- function(n_raw_drug_records,
                                     n_dropped_duplicate_records,
                                     n_amox_records,
                                     n_aclav_records,
                                     n_metro_records,
                                     n_ctx_records,
                                     n_cipro_records,
                                     n_genta_records,
                                     n_penig_records,
                                     n_ceftriaxone_records,
                                     n_cefixime_records,
                                     n_ampi_records,
                                     n_azithromycin_records,
                                     n_benzathinepeniG_records,
                                     n_antibiotics_records,
                                     n_antimalarials_records,
                                     n_cleaned_drug_records) {

  n_excluded <- n_dropped_duplicate_records
  n_informed <- n_amox_records +
    n_aclav_records +
    n_metro_records +
    n_ctx_records +
    n_cipro_records +
    n_genta_records +
    n_penig_records +
    n_ceftriaxone_records +
    n_cefixime_records +
    n_ampi_records +
    n_azithromycin_records +
    n_benzathinepeniG_records +
    n_antibiotics_records +
    n_antimalarials_records

  gr <- sprintf("digraph flowchart {
                  # node definitions with substituted label text
                  node [fontname = Helvetica, shape = rectangle, fixedsize = false, width = 1]

                  1 [label = 'Raw drug records\n(N = %s)', shape = folder, style = filled, fillcolor = '#f79679']
                  m1 [label = 'Excluded (N = %s)\n%s record(s) with a non-unique ID']
                  m2 [label = 'Other checks (N = %s)\n%s record(s) with Amoxicillin\n%s record(s) with Amoxicillin + Clavulanic Acid\n%s record(s) with Metronidazole\n%s record(s) with Cotrimoxazole\n%s record(s) with Ciprofloxacin\n%s record(s) with Gentamicin\n%s record(s) with Benzylpenicillin\n%s record(s) with Ceftriaxone\n%s record(s) with Cefixime\n%s record(s) with Ampicillin\n%s record(s) with Azithromycin\n%s record(s) with Benzathine Benzylpenicillin\n%s record(s) with other antibiotics\n%s record(s) with antimalarials']
                  2 [label = 'Cleaned drug records\n(N = %s)', shape = folder, style = filled, fillcolor = '#f79679']

                  node [shape=none, width=0, height=0, label='']
                  p2 -> 2 [arrowhead='none']
                  {rank=same; p2 -> m2}

                  node [shape=none, width=0, height=0, label='']
                  p1 -> p2 [arrowhead='none']
                  {rank=same; p1 -> m1}

                  edge [dir=none]
                  1 -> p1;
                }",
                n_raw_drug_records,
                n_excluded,
                n_dropped_duplicate_records,
                n_informed,
                n_amox_records,
                n_aclav_records,
                n_metro_records,
                n_ctx_records,
                n_cipro_records,
                n_genta_records,
                n_penig_records,
                n_ceftriaxone_records,
                n_cefixime_records,
                n_ampi_records,
                n_azithromycin_records,
                n_benzathinepeniG_records,
                n_antibiotics_records,
                n_antimalarials_records,
                n_cleaned_drug_records)

  DiagrammeR::grViz(gr)

}

#' Create cleaning flowchart for Day 0 data (TIMCI-specific)
#'
#' @param n_raw_day0_records Initial number of Day 0 records
#' @param n_nonvalid_fid_records Number of Day 0 records with a non-valid device ID
#' @param n_incorrect_enroldate_records Number of Day 0 records with an entry date posterior to the lock date
#' @param n_dropped_duplicate_records Number of Day 0 records that were dropped
#' @param n_edited_duplicate_records Number of Day 0 records with an entry date posterior to the lock date
#' @param n_cleaned_day0_records TBD
#' @return This function returns a graph object
#' @export
#' @import DiagrammeR

create_day0_qc_flowchart <- function(n_raw_day0_records,
                                     n_nonvalid_fid_records,
                                     n_incorrect_enroldate_records,
                                     n_dropped_duplicate_records,
                                     n_edited_duplicate_records,
                                     n_cleaned_day0_records) {

  n_excluded <- n_nonvalid_fid_records + n_dropped_duplicate_records
  n_edited <- n_incorrect_enroldate_records + n_edited_duplicate_records

  gr <- sprintf("digraph flowchart {
                  # node definitions with substituted label text
                  node [fontname = Helvetica, shape = rectangle, fixedsize = false, width = 1]

                  1 [label = 'Raw Day 0 records\n(N = %s)', shape = folder, style = filled, fillcolor = '#f79679']
                  m1 [label = 'Excluded (N = %s)\n%s record(s) with non-valid facility IDs\n%s dummy record(s) with a non-unique child ID']
                  m2 [label = 'Automatically/manually edited (N = %s)\n%s record(s) with a non-unique child ID\n%s record(s) with incorrect enrolment date\nXX record(s) with re-entered structured drug data']
                  m3 [label = 'Other checks triggered (N = XX)']
                  2 [label = 'Cleaned Day 0 records\n(N = %s)', shape = folder, style = filled, fillcolor = '#f79679']

                  node [shape=none, width=0, height=0, label='']
                  p3 -> 2 [arrowhead='none']
                  {rank=same; p3 -> m3}

                  node [shape=none, width=0, height=0, label='']
                  p2 -> p3 [arrowhead='none']
                  {rank=same; p2 -> m2}

                  node [shape=none, width=0, height=0, label='']
                  p1 -> p2 [arrowhead='none']
                  {rank=same; p1 -> m1}

                  edge [dir=none]
                  1 -> p1
                }",
                n_raw_day0_records,
                n_excluded,
                n_nonvalid_fid_records,
                n_dropped_duplicate_records,
                n_edited,
                n_edited_duplicate_records,
                n_incorrect_enroldate_records,
                n_cleaned_day0_records)

  DiagrammeR::grViz(gr)

}

#' Create cleaning flowchart for Day 7 follow-up data (TIMCI-specific)
#'
#' @param n_raw_repeat_records Number of records
#' @param n_nonvalid_pids_repeat_records Number of records with a non-valid participan ID
#' @param n_cleaned_repeat_records TBD
#' @return This function returns a graph object
#' @export
#' @import DiagrammeR

create_repeat_qc_flowchart <- function(n_raw_repeat_records,
                                       n_nonvalid_pids_repeat_records,
                                       n_cleaned_repeat_records) {

  n_excluded <- n_nonvalid_pids_repeat_records

  gr <- sprintf("digraph flowchart {
                  # node definitions with substituted label text
                  node [fontname = Helvetica, shape = rectangle, fixedsize = false, width = 1]

                  1 [label = 'Raw repeat visit records\n(N = %s)', shape = folder, style = filled, fillcolor = '#f79679']
                  m1 [label = 'Excluded (N = %s)\n%s record(s) with non-valid child IDs\n']
                  2 [label = 'Cleaned repeat visit records\n(N = %s)', shape = folder, style = filled, fillcolor = '#f79679']

                  node [shape=none, width=0, height=0, label='']
                  p1 -> 2;
                  {rank=same; p1 -> m1}

                  edge [dir=none]
                  1 -> p1;
                }",
                n_raw_repeat_records,
                n_excluded,
                n_nonvalid_pids_repeat_records,
                n_cleaned_repeat_records)

  DiagrammeR::grViz(gr)

}

#' Create cleaning flowchart for Day 7 follow-up data (TIMCI-specific)
#'
#' @param n_raw_allday7fu_records Number of records
#' @param n_after_lockdate_records TBD
#' @param n_nonvalid_pid_records Number of records with a non-valid participan ID
#' @param n_cleaned_allday7fu_records TBD
#' @return This function returns a graph object
#' @export
#' @import DiagrammeR

create_day7fu_qc_flowchart <- function(n_raw_allday7fu_records,
                                       n_after_lockdate_records,
                                       n_nonvalid_pid_records,
                                       n_cleaned_allday7fu_records) {

  n_excluded <- n_after_lockdate_records + n_nonvalid_pid_records

  gr <- sprintf("digraph flowchart {
                  # node definitions with substituted label text
                  node [fontname = Helvetica, shape = rectangle, fixedsize = false, width = 1]

                  1 [label = 'Raw Day 7 follow-up records\n(N = %s)', shape = folder, style = filled, fillcolor = '#f79679']
                  m1 [label = 'Excluded (N = %s)\n%s record(s) posterior to the lock date\n%s record(s) with non-valid child IDs\n']
                  m2 [label = 'Automatically/manually edited (N = XX)']
                  m3 [label = 'Other checks triggered (N = XX)']
                  2 [label = 'Cleaned Day 7 follow-up records\n(N = %s)', shape = folder, style = filled, fillcolor = '#f79679']

                  node [shape=none, width=0, height=0, label='']
                  p3 -> 2 [arrowhead='none']
                  {rank=same; p3 -> m3}

                  node [shape=none, width=0, height=0, label='']
                  p2 -> p3 [arrowhead='none']
                  {rank=same; p2 -> m2}

                  node [shape=none, width=0, height=0, label='']
                  p1 -> p2 [arrowhead='none']
                  {rank=same; p1 -> m1}

                  edge [dir=none]
                  1 -> p1
                }",
                n_raw_allday7fu_records,
                n_excluded,
                n_after_lockdate_records,
                n_nonvalid_pid_records,
                n_cleaned_allday7fu_records)

  DiagrammeR::grViz(gr)

}

#' Create cleaning flowchart for successful Day 7 follow-up data (TIMCI-specific)
#'
#' @param n_raw_day7fu_records Number of records
#' @param n_dropped_duplicate_records TBD
#' @param n_cleaned_day7fu_records TBD
#' @return This function returns a graph object
#' @export
#' @import DiagrammeR

create_day7fu_outcome_qc_flowchart <- function(n_raw_day7fu_records,
                                               n_dropped_duplicate_records,
                                               n_cleaned_day7fu_records) {

  n_excluded <- n_dropped_duplicate_records

  gr <- sprintf("digraph flowchart {
                  # node definitions with substituted label text
                  node [fontname = Helvetica, shape = rectangle, fixedsize = false, width = 1]

                  1 [label = 'Raw successful Day 7 follow-up records\n(N = %s)', shape = folder, style = filled, fillcolor = '#f79679']
                  m1 [label = 'Excluded (N = %s)\n%s record(s) with a more recent follow-up available\n']
                  m2 [label = 'Automatically/manually edited (N = XX)']
                  m3 [label = 'Other checks triggered (N = XX)']
                  2 [label = 'Cleaned successful Day 7 follow-up records\n(N = %s)', shape = folder, style = filled, fillcolor = '#f79679']

                  node [shape=none, width=0, height=0, label='']
                  p3 -> 2 [arrowhead='none']
                  {rank=same; p3 -> m3}

                  node [shape=none, width=0, height=0, label='']
                  p2 -> p3 [arrowhead='none']
                  {rank=same; p2 -> m2}

                  node [shape=none, width=0, height=0, label='']
                  p1 -> p2 [arrowhead='none']
                  {rank=same; p1 -> m1}

                  edge [dir=none]
                  1 -> p1
                }",
                n_raw_day7fu_records,
                n_excluded,
                n_dropped_duplicate_records,
                n_cleaned_day7fu_records)

  DiagrammeR::grViz(gr)

}

#' Create cleaning flowchart for Day 28 follow-up data (TIMCI-specific)
#'
#' @param n_raw_allday28fu_records Number of records
#' @param n_after_lockdate_records TBD
#' @param n_nonvalid_pid_records Number of records with a non-valid participan ID
#' @param n_cleaned_allday28fu_records TBD
#' @return This function returns a graph object
#' @export
#' @import DiagrammeR

create_day28fu_qc_flowchart <- function(n_raw_allday28fu_records,
                                        n_after_lockdate_records,
                                        n_nonvalid_pid_records,
                                        n_cleaned_allday28fu_records) {

  n_excluded <- n_after_lockdate_records + n_nonvalid_pid_records

  gr <- sprintf("digraph flowchart {
                  # node definitions with substituted label text
                  node [fontname = Helvetica, shape = rectangle, fixedsize = false, width = 1]

                  1 [label = 'Raw Day 28 follow-up records\n(N = %s)', shape = folder, style = filled, fillcolor = '#f79679']
                  m1 [label = 'Excluded (N = %s)\n%s record(s) posterior to the lock date\n%s record(s) with non-valid child IDs\n']
                  m2 [label = 'Automatically/manually edited (N = XX)']
                  m3 [label = 'Other checks triggered (N = XX)']
                  2 [label = 'Cleaned Day 28 follow-up records\n(N = %s)', shape = folder, style = filled, fillcolor = '#f79679']

                  node [shape=none, width=0, height=0, label='']
                  p3 -> 2 [arrowhead='none']
                  {rank=same; p3 -> m3}

                  node [shape=none, width=0, height=0, label='']
                  p2 -> p3 [arrowhead='none']
                  {rank=same; p2 -> m2}

                  node [shape=none, width=0, height=0, label='']
                  p1 -> p2 [arrowhead='none']
                  {rank=same; p1 -> m1}

                  edge [dir=none]
                  1 -> p1
                }",
                n_raw_allday28fu_records,
                n_excluded,
                n_after_lockdate_records,
                n_nonvalid_pid_records,
                n_cleaned_allday28fu_records)

  DiagrammeR::grViz(gr)

}

#' Create cleaning flowchart for successful Day 28 follow-up data (TIMCI-specific)
#'
#' @param n_raw_day28fu_records Number of records
#' @param n_dropped_duplicate_records TBD
#' @param n_cleaned_day28fu_records TBD
#' @return This function returns a graph object
#' @export
#' @import DiagrammeR

create_day28fu_outcome_qc_flowchart <- function(n_raw_day28fu_records,
                                                n_dropped_duplicate_records,
                                                n_cleaned_day28fu_records) {

  n_excluded <- n_dropped_duplicate_records

  gr <- sprintf("digraph flowchart {
                  # node definitions with substituted label text
                  node [fontname = Helvetica, shape = rectangle, fixedsize = false, width = 1]

                  1 [label = 'Raw successful Day 28 follow-up records\n(N = %s)', shape = folder, style = filled, fillcolor = '#f79679']
                  m1 [label = 'Excluded (N = %s)\n%s record(s) with a more recent follow-up available\n']
                  m2 [label = 'Automatically/manually edited (N = XX)']
                  m3 [label = 'Other checks triggered (N = XX)']
                  2 [label = 'Cleaned successful Day 28 follow-up records\n(N = %s)', shape = folder, style = filled, fillcolor = '#f79679']

                  node [shape=none, width=0, height=0, label='']
                  p3 -> 2 [arrowhead='none']
                  {rank=same; p3 -> m3}

                  node [shape=none, width=0, height=0, label='']
                  p2 -> p3 [arrowhead='none']
                  {rank=same; p2 -> m2}

                  node [shape=none, width=0, height=0, label='']
                  p1 -> p2 [arrowhead='none']
                  {rank=same; p1 -> m1}

                  edge [dir=none]
                  1 -> p1
                }",
                n_raw_day28fu_records,
                n_excluded,
                n_dropped_duplicate_records,
                n_cleaned_day28fu_records)

  DiagrammeR::grViz(gr)

}

#' Create cleaning flowchart for hospitalisation data (TIMCI-specific)
#'
#' @param n_raw_hospit_records Initial number of hospitalisation records
#' @param n_afterlock_pids_hospitfu_records Number of records outside of lock range
#' @param n_nonvalid_pid_records Number of screening records with a non-valid device ID
#' @param n_duplicated_records Number of screening records with an entry date posterior to the lock date
#' @param n_cleaned_hospit_records Number of cleaned screening records
#' @return This function returns a graph object
#' @export
#' @import DiagrammeR

create_hospit_qc_flowchart <- function(n_raw_hospit_records,
                                       n_afterlock_pids_hospitfu_records,
                                       n_nonvalid_pid_records,
                                       n_duplicated_records,
                                       n_cleaned_hospit_records) {

  n_excl1 <- n_afterlock_pids_hospitfu_records + n_nonvalid_pid_records

  gr <- sprintf("digraph flowchart {
                  # node definitions with substituted label text
                  node [fontname = Helvetica, shape = rectangle, fixedsize = false, width = 1]

                  1 [label = 'Raw hospitalisation records\n(N = %s)', shape = folder, style = filled, fillcolor = '#f79679']
                  m1 [label = 'Excluded (N = %s)\n%s record(s) posterior to the lock date\n%s record(s) with non-valid child IDs']
                  m2 [label = 'Automatically/manually edited (N = XX)']
                  m3 [label = 'Other checks triggered (N = XX)']
                  2 [label = 'Cleaned hospitalisation records\n(N = %s)', shape = folder, style = filled, fillcolor = '#f79679']

                  node [shape=none, width=0, height=0, label='']
                  p3 -> 2 [arrowhead='none']
                  {rank=same; p3 -> m3}

                  node [shape=none, width=0, height=0, label='']
                  p2 -> p3 [arrowhead='none']
                  {rank=same; p2 -> m2}

                  node [shape=none, width=0, height=0, label='']
                  p1 -> p2 [arrowhead='none']
                  {rank=same; p1 -> m1}

                  edge [dir=none]
                  1 -> p1
                }",
                n_raw_hospit_records,
                n_excl1,
                n_afterlock_pids_hospitfu_records,
                n_nonvalid_pid_records,
                n_cleaned_hospit_records)

  DiagrammeR::grViz(gr)

}
