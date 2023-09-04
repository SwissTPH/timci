' Create a Flowchart for Cleaning Screening Data (TIMCI-specific)
#'
#' This function generates a flowchart to visualize the cleaning process for screening data specific to the TIMCI project. The flowchart illustrates the steps involved in identifying and addressing different issues in the raw screening records to obtain cleaned data for further analysis.

#' @param n_raw_screening_records Initial number of screening records.
#' @param n_nonvalid_deviceid_records Number of screening records with a non-valid device ID.
#' @param n_other_fid_records Number of screening records corresponding to a facility involved in another TIMCI study (India-specific).
#' @param n_before_startdate_records Number of screening records with an entry date earlier than the study start date.
#' @param n_before_facility_startdate_records Number of screening records with an entry date earlier than the specific facility start date.
#' @param n_after_lockdate_records Number of screening records with an entry date later than the lock date.
#' @param n_ineligible_cg_records Number of screening records with an ineligible caregiver.
#' @param n_nonvalid_fid_records Number of screening records with a non-valid facility ID.
#' @param n_edited_nonvalid_fid_records Number of screening records where non-valid facility IDs were manually corrected.
#' @param n_inconsistent_fid_records Number of screening records with inconsistent facility ID information.
#' @param n_edited_inconsistent_fid_records Number of screening records where inconsistent facility IDs were manually corrected.
#' @param n_repeat_visit_records Number of screening records marked as repeat visits.
#' @param n_edited_repeat_visit_records Number of screening records that were manually edited to indicate repeat visits.
#' @param n_incorrect_date_setup_records Number of screening records with incorrect creation dates that were manually edited.
#' @param n_late_submissions Number of screening records with late submissions.
#' @param n_late_completions Number of screening records with late completions.
#' @param n_inconsistent_age_info Number of screening records with inconsistent age information.
#' @param n_cleaned_screening_records Number of cleaned screening records obtained after the data cleaning process.
#' @return This function returns a graph object representing the flowchart.
#' @export
#' @import DiagrammeR

create_screening_qc_flowchart <- function(n_raw_screening_records,
                                          n_nonvalid_deviceid_records,
                                          n_other_fid_records,
                                          n_before_startdate_records,
                                          n_before_facility_startdate_records,
                                          n_after_lockdate_records,
                                          n_ineligible_cg_records,
                                          n_nonvalid_fid_records,
                                          n_edited_nonvalid_fid_records,
                                          n_inconsistent_fid_records,
                                          n_edited_inconsistent_fid_records,
                                          n_repeat_visit_records,
                                          n_edited_repeat_visit_records,
                                          n_incorrect_date_setup_records,
                                          n_late_submissions,
                                          n_late_completions,
                                          n_inconsistent_age_info,
                                          n_cleaned_screening_records) {

  n_excluded <- n_nonvalid_deviceid_records + n_other_fid_records + n_before_startdate_records + n_before_facility_startdate_records + n_after_lockdate_records
  n_auto_edited <- n_incorrect_date_setup_records + n_ineligible_cg_records
  n_manual_edited <- n_edited_nonvalid_fid_records + n_edited_inconsistent_fid_records + n_edited_repeat_visit_records
  n_informed <- n_late_submissions + n_late_completions + n_inconsistent_age_info

  gr <- sprintf("digraph flowchart {
                  # node definitions with substituted label text
                  node [fontname = Helvetica, shape = rectangle, fixedsize = false, width = 1]

                  1 [label = 'Raw screening records\nN = %s', shape = folder, style = filled, fillcolor = '#f79679']
                  m1 [label = 'Excluded (N = %s)\n%s record(s) with a non-valid device ID\n%s record(s) with a facility from another TIMCI study\n%s record(s) anterior to the study start date\n%s record(s) anterior to the specific facility start date\n%s record(s) posterior to the lock date']
                  m2 [label = 'Automatically edited (N = %s)\n%s record(s) with an incorrect creation date\n%s record(s) with an ineligible caregiver']
                  m3 [label = 'Manually edited (N = %s)\n%s record(s) with non-valid facility IDs (out of %s detected)\n%s record(s) corrected for inconsistent facility ID (out of %s detected)\n%s record(s) modified from new enrolment to repeat visit (out of %s detected)']
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
                n_before_facility_startdate_records,
                n_after_lockdate_records,
                n_auto_edited,
                n_incorrect_date_setup_records,
                n_ineligible_cg_records,
                n_manual_edited,
                n_edited_nonvalid_fid_records,
                n_nonvalid_fid_records,
                n_edited_inconsistent_fid_records,
                n_inconsistent_fid_records,
                n_edited_repeat_visit_records,
                n_repeat_visit_records,
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
#' @param n_manual_edits
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
                                     n_manual_edits,
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
                  m2 [label = 'Manually edited (N = %s)']
                  m3 [label = 'Other checks (N = %s)\n%s record(s) with Amoxicillin\n%s record(s) with Amoxicillin + Clavulanic Acid\n%s record(s) with Metronidazole\n%s record(s) with Cotrimoxazole\n%s record(s) with Ciprofloxacin\n%s record(s) with Gentamicin\n%s record(s) with Benzylpenicillin\n%s record(s) with Ceftriaxone\n%s record(s) with Cefixime\n%s record(s) with Ampicillin\n%s record(s) with Azithromycin\n%s record(s) with Benzathine Benzylpenicillin\n%s record(s) with other antibiotics\n%s record(s) with antimalarials']
                  2 [label = 'Cleaned drug records\n(N = %s)', shape = folder, style = filled, fillcolor = '#f79679']

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
                  1 -> p1;
                }",
                n_raw_drug_records,
                n_excluded,
                n_dropped_duplicate_records,
                n_manual_edits,
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

#' Create a Flowchart Illustrating Cleaning Process for Day 0 Data (TIMCI-specific)
#'
#' This function generates a flowchart diagram illustrating the cleaning process for Day 0 data
#' specific to TIMCI. The flowchart visually represents the various stages and
#' outcomes of data processing, including exclusion, editing, and checks for data quality related to
#' Day 0 records.
#'
#' @param n_raw_day0_records Initial number of Day 0 records in the dataset.
#' @param n_nonvalid_fid_records Number of records with a non-valid facility ID.
#' @param n_incorrect_enroldate_records Number of records with an incorrect enrolment date.
#' @param n_dropped_true_duplicate_records Number of records dropped due to true duplication.
#' @param n_true_duplicate_detected Number of true duplicate records detected.
#' @param n_duplicate_records Number of records with duplicated child IDs.
#' @param n_dropped_duplicate_records Number of records dropped due to duplicated child IDs.
#' @param n_edited_duplicate_records Number of records edited due to duplicated child IDs.
#' @param n_drug_edits Number of records with re-entered structured drug data.
#' @param n_spo2_meas1_edits Number of records with corrected SpO2 measure 1 (India only).
#' @param n_spo2_meas2_edits Number of records with corrected SpO2 measure 2 (India only).
#' @param n_negative_illness_onset Number of records with a negative illness onset.
#' @param n_missing_cp Number of records with missing clinical presentation.
#' @param n_missing_diagnosis Number of records with missing diagnosis.
#' @param n_missing_referral_cg Number of records with no referral information from caregiver.
#' @param n_cleaned_day0_records Number of records after cleaning.
#' @return A graph object representing the flowchart of the Day 0 data cleaning process.
#' @export
#' @import DiagrammeR

create_day0_qc_flowchart <- function(n_raw_day0_records,
                                     n_nonvalid_fid_records,
                                     n_incorrect_enroldate_records,
                                     n_dropped_true_duplicate_records,
                                     n_true_duplicate_detected,
                                     n_duplicate_records,
                                     n_dropped_duplicate_records,
                                     n_edited_duplicate_records,
                                     n_drug_edits,
                                     n_spo2_meas1_edits,
                                     n_spo2_meas2_edits,
                                     n_negative_illness_onset,
                                     n_missing_cp,
                                     n_missing_diagnosis,
                                     n_missing_referral_cg,
                                     n_cleaned_day0_records) {

  n_excluded <- n_nonvalid_fid_records + n_dropped_true_duplicate_records + n_dropped_duplicate_records
  n_auto_edited <- n_incorrect_enroldate_records + n_negative_illness_onset
  n_manual_edited <- n_edited_duplicate_records + n_drug_edits + n_spo2_meas1_edits + n_spo2_meas2_edits
  n_informed <- n_missing_cp + n_missing_diagnosis + n_missing_referral_cg

  gr <- sprintf("digraph flowchart {
                  # node definitions with substituted label text
                  node [fontname = Helvetica, shape = rectangle, fixedsize = false, width = 1]

                  1 [label = 'Raw Day 0 records\n(N = %s)', shape = folder, style = filled, fillcolor = '#f79679']
                  m1 [label = 'Excluded (N = %s)\n%s record(s) with non-valid facility IDs\n%s record(s) with a duplicated record (out of %s detected)\n%s record(s) with a duplicated child ID (out of %s detected)']
                  m2 [label = 'Automatically edited (N = %s)\n%s record(s) with incorrect enrolment date\n%s record(s) with a negative illness onset']
                  m3 [label = 'Manually edited (N = %s)\n%s record(s) with a duplicated child ID (out of %s detected)\n%s record(s) with re-entered structured drug data\n%s record(s) with corrected SpO2 measure 1 (India only)\n%s record(s) with corrected SpO2 measure 2 (India only)']
                  m4 [label = 'Other checks triggered (N = %s)\n%s record(s) with a missing clinical presentation\n%s record(s) with a missing diagnosis\n%s record(s) with no referral info from caregiver']
                  2 [label = 'Cleaned Day 0 records\n(N = %s)', shape = folder, style = filled, fillcolor = '#f79679']

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
                n_raw_day0_records,
                n_excluded,
                n_nonvalid_fid_records,
                n_dropped_true_duplicate_records,
                n_true_duplicate_detected,
                n_dropped_duplicate_records,
                n_duplicate_records,
                n_auto_edited,
                n_incorrect_enroldate_records,
                n_negative_illness_onset,
                n_manual_edited,
                n_edited_duplicate_records,
                n_duplicate_records,
                n_drug_edits,
                n_spo2_meas1_edits,
                n_spo2_meas2_edits,
                n_informed,
                n_missing_cp,
                n_missing_diagnosis,
                n_missing_referral_cg,
                n_cleaned_day0_records)

  DiagrammeR::grViz(gr)

}

#' Create a Flowchart Illustrating Cleaning Process for Repeat Visit Data (TIMCI-specific)
#'
#' This function generates a flowchart diagram illustrating the cleaning process for repeat visit data
#' specific to TIMCI. The flowchart visually represents the various stages and
#' outcomes of data processing, including exclusion, editing, and checks for data quality related to
#' repeat visit records
#'
#' @param n_raw_repeat_records Number of records
#' @param n_nonvalid_pids_repeat_records Number of records with a non-valid participant ID
#' @param n_edit_nonvalid_pid_repeat_records
#' @param n_drop_nonvalid_pid_repeat_records
#' @param n_edit_inconsistent_names_repeat_records
#' @param n_drop_inconsistent_names_repeat_records
#' @param n_nonconsistent_names_repeat_records
#' @param n_visit_before_enrolment
#' @param n_cleaned_repeat_records TBD
#' @return This function returns a graph object
#' @export
#' @import DiagrammeR

create_repeat_qc_flowchart <- function(n_raw_repeat_records,
                                       n_nonvalid_pids_repeat_records,
                                       n_edit_nonvalid_pid_repeat_records,
                                       n_drop_nonvalid_pid_repeat_records,
                                       n_edit_inconsistent_names_repeat_records,
                                       n_drop_inconsistent_names_repeat_records,
                                       n_nonconsistent_names_repeat_records,
                                       n_visit_before_enrolment,
                                       n_cleaned_repeat_records) {

  n_excluded <- n_drop_nonvalid_pid_repeat_records + n_drop_inconsistent_names_repeat_records
  n_auto_edited <- 0
  n_manual_edited <- n_edit_nonvalid_pid_repeat_records + n_edit_inconsistent_names_repeat_records
  n_informed <- n_visit_before_enrolment

  gr <- sprintf("digraph flowchart {
                  # node definitions with substituted label text
                  node [fontname = Helvetica, shape = rectangle, fixedsize = false, width = 1]

                  1 [label = 'Raw repeat visit records\n(N = %s)', shape = folder, style = filled, fillcolor = '#f79679']
                  m1 [label = 'Excluded (N = %s)\n%s record(s) with non-valid child IDs (out of %s detected)\n%s record(s) with non consistent names (out of %s detected)']
                  m2 [label = 'Automatically edited (N = %s)']
                  m3 [label = 'Manually edited (N = %s)\n%s record(s) with non-valid child IDs (out of %s detected)\n%s record(s) with non consistent names (out of %s detected)']
                  m4 [label = 'Other checks triggered (N = %s)\n%s record(s) with a date of visit prior to enrolment']
                  2 [label = 'Cleaned repeat visit records\n(N = %s)', shape = folder, style = filled, fillcolor = '#f79679']

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
                  1 -> p1;
                }",
                n_raw_repeat_records,
                n_excluded,
                n_drop_nonvalid_pid_repeat_records,
                n_nonvalid_pids_repeat_records,
                n_drop_inconsistent_names_repeat_records,
                n_nonconsistent_names_repeat_records,
                n_auto_edited,
                n_manual_edited,
                n_edit_nonvalid_pid_repeat_records,
                n_nonvalid_pids_repeat_records,
                n_edit_inconsistent_names_repeat_records,
                n_nonconsistent_names_repeat_records,
                n_informed,
                n_visit_before_enrolment,
                n_cleaned_repeat_records)

  DiagrammeR::grViz(gr)

}

#' Create cleaning flowchart for Day 7 follow-up data (TIMCI-specific)
#'
#' @param n_raw_allday7fu_records Number of records
#' @param n_after_lockdate_records TBD
#' @param n_cdsa_pilot_day7fu_records
#' @param n_nonvalid_pid_records Number of records with a non-valid participan ID
#' @param n_edit_nonvalid_pid_records
#' @param n_drop_nonvalid_pid_records
#' @param n_inconsistent_name_records
#' @param n_edit_inconsistent_name_records
#' @param n_drop_inconsistent_names_day7fu_records
#' @param n_cleaned_allday7fu_records TBD
#' @return This function returns a graph object
#' @export
#' @import DiagrammeR

create_day7fu_qc_flowchart <- function(n_raw_allday7fu_records,
                                       n_cdsa_pilot_day7fu_records,
                                       n_after_lockdate_records,
                                       n_nonvalid_pid_records,
                                       n_edit_nonvalid_pid_records,
                                       n_drop_nonvalid_pid_records,
                                       n_inconsistent_name_records,
                                       n_edit_inconsistent_name_records,
                                       n_drop_inconsistent_names_day7fu_records,
                                       n_cleaned_allday7fu_records) {

  n_excluded <- n_cdsa_pilot_day7fu_records + n_after_lockdate_records + n_drop_nonvalid_pid_records + n_drop_inconsistent_names_day7fu_records
  n_auto_edited <- 0
  n_manual_edited <- n_edit_nonvalid_pid_records + n_edit_inconsistent_name_records
  n_informed <- 0

  gr <- sprintf("digraph flowchart {
                  # node definitions with substituted label text
                  node [fontname = Helvetica, shape = rectangle, fixedsize = false, width = 1]

                  1 [label = 'Raw Day 7 follow-up records\n(N = %s)', shape = folder, style = filled, fillcolor = '#f79679']
                  m1 [label = 'Excluded (N = %s)\n%s record(s) related to the CDSA pilot (India only)\n%s record(s) outside the lock date range\n%s record(s) with non-valid child IDs (out of %s detected)\n%s record(s) with non-consistent names (out of %s detected)']
                  m2 [label = 'Automatically edited (N = %s)']
                  m3 [label = 'Manually edited (N = %s)\n%s record(s) with non-valid child IDs (out of %s detected)\n%s record(s) with non-consistent names (out of %s detected)']
                  m4 [label = 'Other checks triggered (N = %s)']
                  2 [label = 'Cleaned Day 7 follow-up records\n(N = %s)', shape = folder, style = filled, fillcolor = '#f79679']

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
                n_raw_allday7fu_records,
                n_excluded,
                n_cdsa_pilot_day7fu_records,
                n_after_lockdate_records,
                n_drop_nonvalid_pid_records,
                n_nonvalid_pid_records,
                n_drop_inconsistent_names_day7fu_records,
                n_inconsistent_name_records,
                n_auto_edited,
                n_manual_edited,
                n_edit_nonvalid_pid_records,
                n_nonvalid_pid_records,
                n_edit_inconsistent_name_records,
                n_inconsistent_name_records,
                n_informed,
                n_cleaned_allday7fu_records)

  DiagrammeR::grViz(gr)

}

#' Create a Flowchart Illustrating Cleaning Process for Successful Day 7 Follow-up Data (TIMCI-specific)
#'
#' This function generates a flowchart diagram illustrating the cleaning process for successful Day 7
#' follow-up data specific to TIMCI. The flowchart visually represents the various
#' stages and outcomes of data processing, including exclusion, editing, and checks for data quality.
#'
#' @param n_raw_day7fu_records Number of raw Day 7 follow-up records in the dataset.
#' @param n_dropped_duplicate_records Number of records dropped due to duplication.
#' @param n_fu_prior_day0_day7fu Number of records with follow-up prior to Day 0 (enrolment).
#' @param n_death_prior_day0_day7fu Number of records with death prior to Day 0 (enrolment).
#' @param n_hospit_prior_day0_day7fu Number of records with hospitalization prior to Day 0 (enrolment).
#' @param n_death_prior_hospit_day7fu Number of records with death prior to hospitalization.
#' @param n_cleaned_day7fu_records Number of records after cleaning.
#' @return A graph object representing the flowchart of the cleaning process.
#' @export
#' @import DiagrammeR

create_day7fu_outcome_qc_flowchart <- function(n_raw_day7fu_records,
                                               n_dropped_duplicate_records,
                                               n_fu_prior_day0_day7fu,
                                               n_death_prior_day0_day7fu,
                                               n_hospit_prior_day0_day7fu,
                                               n_death_prior_hospit_day7fu,
                                               n_cleaned_day7fu_records) {

  n_excluded <- n_dropped_duplicate_records
  n_auto_edited <- 0
  n_manual_edited <- 0
  n_informed <- n_fu_prior_day0_day7fu + n_death_prior_day0_day7fu + n_hospit_prior_day0_day7fu + n_death_prior_hospit_day7fu

  gr <- sprintf("digraph flowchart {
                  # node definitions with substituted label text
                  node [fontname = Helvetica, shape = rectangle, fixedsize = false, width = 1]

                  1 [label = 'Raw successful Day 7 follow-up records\n(N = %s)', shape = folder, style = filled, fillcolor = '#f79679']
                  m1 [label = 'Excluded (N = %s)\n%s record(s) with a more recent follow-up available\n']
                  m2 [label = 'Automatically edited (N = %s)']
                  m3 [label = 'Manually edited (N = %s)']
                  m4 [label = 'Other checks triggered (N = %s)\n%s record(s) with follow-up prior to enrolment\n%s record(s) with death prior to enrolment\n%s record(s) with hospitalisation prior to enrolment\n%s record(s) with death prior to hospitalisation']
                  2 [label = 'Cleaned successful Day 7 follow-up records\n(N = %s)', shape = folder, style = filled, fillcolor = '#f79679']

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
                n_raw_day7fu_records,
                n_excluded,
                n_dropped_duplicate_records,
                n_auto_edited,
                n_manual_edited,
                n_informed,
                n_fu_prior_day0_day7fu,
                n_death_prior_day0_day7fu,
                n_hospit_prior_day0_day7fu,
                n_death_prior_hospit_day7fu,
                n_cleaned_day7fu_records)

  DiagrammeR::grViz(gr)

}

#' Create cleaning flowchart for Day 28 follow-up data (TIMCI-specific)
#'
#' @param n_raw_allday28fu_records Number of records
#' @param n_cdsa_pilot_day28fu_records
#' @param n_after_lockdate_records TBD
#' @param n_nonvalid_pid_records Number of records with a non-valid participan ID
#' @param n_edit_nonvalid_pid_records
#' @param n_drop_nonvalid_pid_records
#' @param n_inconsistent_name_records
#' @param n_edit_inconsistent_name_records
#' @param n_drop_inconsistent_name_records
#' @param n_cleaned_allday28fu_records TBD
#' @return This function returns a graph object
#' @export
#' @import DiagrammeR

create_day28fu_qc_flowchart <- function(n_raw_allday28fu_records,
                                        n_cdsa_pilot_day28fu_records,
                                        n_after_lockdate_records,
                                        n_nonvalid_pid_records,
                                        n_edit_nonvalid_pid_records,
                                        n_drop_nonvalid_pid_records,
                                        n_inconsistent_name_records,
                                        n_edit_inconsistent_name_records,
                                        n_drop_inconsistent_name_records,
                                        n_cleaned_allday28fu_records) {

  n_excluded <- n_cdsa_pilot_day28fu_records + n_after_lockdate_records + n_drop_nonvalid_pid_records + n_drop_inconsistent_name_records
  n_auto_edited <- 0
  n_manual_edited <- n_edit_nonvalid_pid_records + n_edit_inconsistent_name_records
  n_informed <- 0

  gr <- sprintf("digraph flowchart {
                  # node definitions with substituted label text
                  node [fontname = Helvetica, shape = rectangle, fixedsize = false, width = 1]

                  1 [label = 'Raw Day 28 follow-up records\n(N = %s)', shape = folder, style = filled, fillcolor = '#f79679']
                  m1 [label = 'Excluded (N = %s)\n%s record(s) related to the CDSA pilot (India only)\n%s record(s) outside the lock date range\n%s record(s) with non-valid child IDs (out of %s detected)\n%s record(s) with non-consistent names (out of %s detected)']
                  m2 [label = 'Automatically edited (N = %s)']
                  m3 [label = 'Manually edited (N = %s)\n%s record(s) with non-valid child IDs (out of %s detected)\n%s record(s) with non-consistent names (out of %s detected)']
                  m4 [label = 'Other checks triggered (N = %s)']
                  2 [label = 'Cleaned Day 28 follow-up records\n(N = %s)', shape = folder, style = filled, fillcolor = '#f79679']

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
                n_raw_allday28fu_records,
                n_excluded,
                n_cdsa_pilot_day28fu_records,
                n_after_lockdate_records,
                n_drop_nonvalid_pid_records,
                n_nonvalid_pid_records,
                n_drop_inconsistent_name_records,
                n_inconsistent_name_records,
                n_auto_edited,
                n_manual_edited,
                n_edit_nonvalid_pid_records,
                n_nonvalid_pid_records,
                n_edit_inconsistent_name_records,
                n_inconsistent_name_records,
                n_informed,
                n_cleaned_allday28fu_records)

  DiagrammeR::grViz(gr)

}

#' Create cleaning flowchart for successful Day 28 follow-up data (TIMCI-specific)
#'
#' @param n_raw_day28fu_records Number of records
#' @param n_dropped_duplicate_records TBD
#' @param n_fu_prior_day0_day28fu
#' @param n_death_prior_day0_day28fu
#' @param n_hospit_prior_day0_day28fu
#' @param n_death_prior_hospit_day28fu
#' @param n_cleaned_day28fu_records TBD
#' @return This function returns a graph object
#' @export
#' @import DiagrammeR

create_day28fu_outcome_qc_flowchart <- function(n_raw_day28fu_records,
                                               n_dropped_duplicate_records,
                                               n_fu_prior_day0_day28fu,
                                               n_death_prior_day0_day28fu,
                                               n_hospit_prior_day0_day28fu,
                                               n_death_prior_hospit_day28fu,
                                               n_cleaned_day28fu_records) {

  n_excluded <- n_dropped_duplicate_records
  n_auto_edited <- 0
  n_manual_edited <- 0
  n_informed <- 0

  gr <- sprintf("digraph flowchart {
                  # node definitions with substituted label text
                  node [fontname = Helvetica, shape = rectangle, fixedsize = false, width = 1]

                  1 [label = 'Raw successful Day 28 follow-up records\n(N = %s)', shape = folder, style = filled, fillcolor = '#f79679']
                  m1 [label = 'Excluded (N = %s)\n%s record(s) with a more recent follow-up available\n']
                  m2 [label = 'Automatically edited (N = %s)']
                  m3 [label = 'Manually edited (N = %s)']
                  m4 [label = 'Other checks triggered (N = %s)\n%s record(s) with follow-up prior to enrolment\n%s record(s) with death prior to enrolment\n%s record(s) with hospitalisation prior to enrolment\n%s record(s) with death prior to hospitalisation']
                  2 [label = 'Cleaned successful Day 28 follow-up records\n(N = %s)', shape = folder, style = filled, fillcolor = '#f79679']

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
                n_raw_day28fu_records,
                n_excluded,
                n_dropped_duplicate_records,
                n_auto_edited,
                n_manual_edited,
                n_informed,
                n_fu_prior_day0_day28fu,
                n_death_prior_day0_day28fu,
                n_hospit_prior_day0_day28fu,
                n_death_prior_hospit_day28fu,
                n_cleaned_day28fu_records)

  DiagrammeR::grViz(gr)

}

#' Create a Flowchart Illustrating Cleaning Process for Hospitalisation Data (TIMCI-specific)
#'
#' This function generates a flowchart diagram illustrating the cleaning process for hospitalisation data
#' specific to TIMCI. The flowchart visually represents the various stages and
#' outcomes of data processing, including exclusion, editing, and checks for data quality related to
#' hospitalisation records.
#'
#' @param n_raw_hospit_records Initial number of hospitalisation records in the dataset.
#' @param n_cdsa_pilot_hospitfu_records Number of records related to the CDSA pilot (India only).
#' @param n_afterlock_pids_hospitfu_records Number of records outside the specified lock date range.
#' @param n_nonvalid_pid_records Number of records with non-valid child IDs (out of a total of detected IDs).
#' @param n_edit_nonvalid_pid_hospitfu_records Number of records edited due to non-valid child IDs.
#' @param n_drop_nonvalid_pid_hospitfu_records Number of records dropped due to non-valid child IDs.
#' @param n_inconsistent_names_day7fu_records Number of records with non-consistent names (out of a total of detected inconsistencies).
#' @param n_edit_inconsistent_name_hospitfu_records Number of records edited due to non-consistent names.
#' @param n_drop_inconsistent_name_hospitfu_records Number of records dropped due to non-consistent names.
#' @param n_duplicated_records Number of records with an entry date posterior to the lock date.
#' @param n_death_prior_day0_hospitfu Number of records with death prior to enrolment.
#' @param n_hospit_prior_day0_hospitfu Number of records with hospitalisation prior to enrolment.
#' @param n_discharge_prior_day0_hospitfu Number of records with discharge prior to enrolment.
#' @param n_cleaned_hospit_records Number of records after cleaning.
#' @return A graph object representing the flowchart of the hospitalisation data cleaning process.
#' @export
#' @import DiagrammeR

create_hospit_qc_flowchart <- function(n_raw_hospit_records,
                                       n_cdsa_pilot_hospitfu_records,
                                       n_afterlock_pids_hospitfu_records,
                                       n_nonvalid_pid_records,
                                       n_edit_nonvalid_pid_hospitfu_records,
                                       n_drop_nonvalid_pid_hospitfu_records,
                                       n_inconsistent_names_day7fu_records,
                                       n_edit_inconsistent_name_hospitfu_records,
                                       n_drop_inconsistent_name_hospitfu_records,
                                       n_duplicated_records,
                                       n_death_prior_day0_hospitfu,
                                       n_hospit_prior_day0_hospitfu,
                                       n_discharge_prior_day0_hospitfu,
                                       n_cleaned_hospit_records) {

  n_excluded <- n_cdsa_pilot_hospitfu_records + n_afterlock_pids_hospitfu_records + n_drop_nonvalid_pid_hospitfu_records + n_drop_inconsistent_name_hospitfu_records
  n_auto_edited <- 0
  n_manual_edited <- n_edit_nonvalid_pid_hospitfu_records + n_edit_inconsistent_name_hospitfu_records
  n_informed <- n_death_prior_day0_hospitfu + n_hospit_prior_day0_hospitfu + n_discharge_prior_day0_hospitfu

  gr <- sprintf("digraph flowchart {
                  # node definitions with substituted label text
                  node [fontname = Helvetica, shape = rectangle, fixedsize = false, width = 1]

                  1 [label = 'Raw hospitalisation records\n(N = %s)', shape = folder, style = filled, fillcolor = '#f79679']
                  m1 [label = 'Excluded (N = %s)\n%s record(s) related to the CDSA pilot (India only)\n%s record(s) outside the lock date range\n%s record(s) with non-valid child IDs (out of %s detected)\n%s record(s) with non-consistent names (out of %s detected)']
                  m2 [label = 'Automatically edited (N = %s)']
                  m3 [label = 'Manually edited (N = %s)\n%s record(s) with non-valid child IDs (out of %s detected)\n%s record(s) with non-consistent names (out of %s detected)']
                  m4 [label = 'Other checks triggered (N = %s)\n%s record(s) with death prior to enrolment\n%s record(s) with hospitalisation prior to enrolment\n%s record(s) with discharge prior to enrolment']
                  2 [label = 'Cleaned hospitalisation records\n(N = %s)', shape = folder, style = filled, fillcolor = '#f79679']

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
                n_raw_hospit_records,
                n_excluded,
                n_cdsa_pilot_hospitfu_records,
                n_afterlock_pids_hospitfu_records,
                n_drop_nonvalid_pid_hospitfu_records,
                n_nonvalid_pid_records,
                n_drop_inconsistent_name_hospitfu_records,
                n_inconsistent_names_day7fu_records,
                n_auto_edited,
                n_manual_edited,
                n_edit_nonvalid_pid_hospitfu_records,
                n_nonvalid_pid_records,
                n_edit_inconsistent_name_hospitfu_records,
                n_inconsistent_names_day7fu_records,
                n_informed,
                n_death_prior_day0_hospitfu,
                n_hospit_prior_day0_hospitfu,
                n_discharge_prior_day0_hospitfu,
                n_cleaned_hospit_records)

  DiagrammeR::grViz(gr)

}

#' Create a Flowchart Illustrating Cleaning Process for Withdrawal Data (TIMCI-specific)
#'
#' This function generates a flowchart diagram illustrating the cleaning process for withdrawal data
#' specific to TIMCI. The flowchart visually represents the various stages and
#' outcomes of data processing, including exclusion, editing, and checks for data quality related to
#' withdrawal records.
#'
#' @param n_raw_wd_records Initial number of withdrawal records in the dataset.
#' @param n_no_doc_child_id_withdrawal Number of records with no child ID.
#' @param n_drop_nonvalid_ids Number of records dropped due to non-valid child IDs.
#' @param n_invalid_date_of_withdrawal Number of records with a date of withdrawal prior to enrolment.
#' @param n_cleaned_wd_records Number of records after cleaning.
#' @return A graph object representing the flowchart of the withdrawal data cleaning process.
#' @export
#' @import DiagrammeR

create_withdrawal_qc_flowchart <- function(n_raw_wd_records,
                                           n_no_doc_child_id_withdrawal,
                                           n_drop_nonvalid_ids,
                                           n_invalid_date_of_withdrawal,
                                           n_cleaned_wd_records) {

  n_excluded <- n_no_doc_child_id_withdrawal + n_drop_nonvalid_ids
  n_auto_edited <- 0
  n_manual_edited <- 0
  n_informed <- n_invalid_date_of_withdrawal

  gr <- sprintf("digraph flowchart {
                  # node definitions with substituted label text
                  node [fontname = Helvetica, shape = rectangle, fixedsize = false, width = 1]

                  1 [label = 'Raw withdrawal records\n(N = %s)', shape = folder, style = filled, fillcolor = '#f79679']
                  m1 [label = 'Excluded (N = %s)\n%s record(s) with no child ID\n%s record(s) with a non-valid child ID']
                  m2 [label = 'Automatically edited (N = %s)']
                  m3 [label = 'Manually edited (N = %s)']
                  m4 [label = 'Other checks triggered (N = %s)\n%s record(s) with a date of withdrawal prior to enrolment']
                  2 [label = 'Cleaned withdrawal records\n(N = %s)', shape = folder, style = filled, fillcolor = '#f79679']

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
                n_raw_wd_records,
                n_excluded,
                n_no_doc_child_id_withdrawal,
                n_drop_nonvalid_ids,
                n_auto_edited,
                n_manual_edited,
                n_informed,
                n_invalid_date_of_withdrawal,
                n_cleaned_wd_records)

  DiagrammeR::grViz(gr)

}
