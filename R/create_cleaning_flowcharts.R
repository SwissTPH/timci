#' Create cleaning flowchart for screening data (TIMCI-specific)
#'
#' @param n_raw_screening_records Initial number of screening records
#' @param n_nonvalid_deviceid_records Number of screening records with a non-valid device ID
#' @param n_after_lockdate_records Number of screening records with an entry date posterior to the lock date
#' @param n_ineligible_cg_records Number of screening records with an ineligible caregiver
#' @param n_edited_repeat_visit_records Number of screening records that were edited manually
#' @param n_cleaned_screening_records Number of cleaned screening records
#' @return This function returns a graph object
#' @export
#' @import DiagrammeR

create_screening_qc_flowchart <- function(n_raw_screening_records,
                                          n_nonvalid_deviceid_records,
                                          n_after_lockdate_records,
                                          n_ineligible_cg_records,
                                          n_edited_repeat_visit_records,
                                          n_cleaned_screening_records) {

  n_excluded <- n_nonvalid_deviceid_records + n_after_lockdate_records + n_ineligible_cg_records
  n_edited <- n_edited_repeat_visit_records

  gr <- sprintf("digraph flowchart {
                  # node definitions with substituted label text
                  node [fontname = Helvetica, shape = rectangle, fixedsize = false, width = 1]

                  1 [label = 'Raw screening records\n(N = %s)']
                  m1 [label = 'Excluded (N = %s)\n%s record(s) with non-valid device IDs\n%s record(s) with ineligible caregiver\n%s record(s) posterior to the lock date\n\nManually edited (N = %s)\n%s record(s) corrected to repeat visit']
                  2 [label = 'Cleaned screening records\n(N = %s)']

                  node [shape=none, width=0, height=0, label='']
                  p1 -> 2;
                  {rank=same; p1 -> m1}

                  edge [dir=none]
                  1 -> p1;
                }",
                n_raw_screening_records,
                n_excluded,
                n_nonvalid_deviceid_records,
                n_ineligible_cg_records,
                n_after_lockdate_records,
                n_edited,
                n_edited_repeat_visit_records,
                n_cleaned_screening_records)

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

                  1 [label = 'Raw Day 0 records\n(N = %s)']
                  m1 [label = 'Excluded (N = %s)\n%s record(s) with non-valid facility IDs\n%s dummy record(s) with a non-unique child ID\n\nManually edited (N = %s)\n%s record(s) with incorrect enrolment date\n%s record(s) with a non-unique child ID']
                  2 [label = 'Cleaned Day 0 records\n(N = %s)']

                  node [shape=none, width=0, height=0, label='']
                  p1 -> 2;
                  {rank=same; p1 -> m1}

                  edge [dir=none]
                  1 -> p1;
                }",
                n_raw_day0_records,
                n_excluded,
                n_nonvalid_fid_records,
                n_dropped_duplicate_records,
                n_edited,
                n_incorrect_enroldate_records,
                n_edited_duplicate_records,
                n_cleaned_day0_records)

  DiagrammeR::grViz(gr)

}

#' Create cleaning flowchart for Day 7 follow-up data (TIMCI-specific)
#'
#' @param n_raw_records Number of records
#' @param n_nonvalid_pids Number of records with a non-valid participan ID
#' @param n_clean_records TBD
#' @param d TBD
#' @param e TBD
#' @param f TBD
#' @param g TBD
#' @return This function returns a graph object
#' @export
#' @import DiagrammeR

create_day7fu_qc_flowchart <- function(n_raw_records,
                                       n_nonvalid_pids,
                                       n_clean_records,
                                       d,
                                       e,
                                       f,
                                       g) {
  gr <- sprintf("digraph flowchart {
                  # node definitions with substituted label text
                  node [fontname = Helvetica, shape = rectangle, fixedsize = false, width = 1]

                  1 [label = 'Raw follow-up records\n(N = %s)']
                  m1 [label = 'Excluded\nNon-valid child IDs (N = %s)']
                  2 [label = 'Clean follow-up records\n(N = %s)']
                  m2 [label = 'Excluded %s']
                  3 [label = 'Enrolment\n(N = %s)']
                  4 [label = 'Day 7 follow-up\n(N = %s)']

                  node [shape=none, width=0, height=0, label='']
                  p1 -> 2; p2 -> 3 -> 4;
                  {rank=same; p1 -> m1}
                  {rank=same; p2 -> m2}

                  edge [dir=none]
                  1 -> p1; 2 -> p2;
                }",
                n_raw_records,
                n_nonvalid_pids,
                n_clean_records,
                d,
                e,
                f,
                g)
  DiagrammeR::grViz(gr)
}

#' Create cleaning flowchart for hospitalisation data (TIMCI-specific)
#'
#' @param n_raw_hospit_records Initial number of hospitalisation records
#' @param n_nonvalid_pid_records Number of screening records with a non-valid device ID
#' @param n_duplicated_records Number of screening records with an entry date posterior to the lock date
#' @param n_cleaned_hospit_records Number of cleaned screening records
#' @return This function returns a graph object
#' @export
#' @import DiagrammeR

create_hospit_qc_flowchart <- function(n_raw_hospit_records,
                                      n_nonvalid_pid_records,
                                      n_duplicated_records,
                                      n_cleaned_hospit_records) {

  n_excl1 <- n_nonvalid_pid_records + n_duplicated_records

  gr <- sprintf("digraph flowchart {
                  # node definitions with substituted label text
                  node [fontname = Helvetica, shape = rectangle, fixedsize = false, width = 1]

                  1 [label = 'Raw screenings\n(N = %s)']
                  m1 [label = 'Excluded (N = %s)\n%s record(s) with non-valid child IDs\n%s duplicated record(s)']
                  2 [label = 'Cleaned screenings\n(N = %s)']

                  node [shape=none, width=0, height=0, label='']
                  p1 -> 2;
                  {rank=same; p1 -> m1}

                  edge [dir=none]
                  1 -> p1;
                }",
                n_raw_hospit_records,
                n_excl1,
                n_nonvalid_pid_records,
                n_duplicated_records,
                n_cleaned_hospit_records)

  DiagrammeR::grViz(gr)

}

#' Create flowchart (TIMCI-specific)
#'
#' @param n_raw_records Number of records
#' @param n_nonvalid_deviceid_records Number of records with a non-valid device ID
#' @param n_screening TBD
#' @param d TBD
#' @param e TBD
#' @param f TBD
#' @param g TBD
#' @return This function returns a graph object
#' @export
#' @import DiagrammeR

create_flowchart <- function(n_raw_records,
                             n_nonvalid_deviceid_records,
                             n_screening,
                             d,
                             e,
                             f,
                             g) {
  gr <- sprintf("digraph flowchart {
                  # node definitions with substituted label text
                  node [fontname = Helvetica, shape = rectangle, fixedsize = false, width = 1]

                  1 [label = 'Records\n(N = %s)']
                  m1 [label = 'Excluded\nNon-valid device IDs (N = %s)']
                  2 [label = 'Screenings\n(N = %s)']
                  m2 [label = 'Excluded %s']
                  3 [label = 'Enrolment\n(N = %s)']
                  4 [label = 'Day 7 follow-up\n(N = %s)']

                  node [shape=none, width=0, height=0, label='']
                  p1 -> 2; p2 -> 3 -> 4;
                  {rank=same; p1 -> m1}
                  {rank=same; p2 -> m2}

                  edge [dir=none]
                  1 -> p1; 2 -> p2;
                }",
                n_raw_records,
                n_nonvalid_deviceid_records,
                n_screening,
                d,
                e,
                f,
                g)
  DiagrammeR::grViz(gr)
}
