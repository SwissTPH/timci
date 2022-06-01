#' Generate folder structure (TIMCI-specific function)
#'
#' @param output_dir directory
#' @param study_dirname directory
#' @return This function returns a list of directories
#' @export

generate_folder_structure <- function(output_dir, study_dirname) {

  # Create the structure of the folder and subfolders that are created everyday to store the reports and exports
  subdir <- file.path(output_dir, paste0("export_", Sys.Date()))
  dir.create(subdir, showWarnings = FALSE)

  dir.create(file.path(subdir, study_dirname), showWarnings = FALSE)

  # RCT / LS raw data folder
  raw_rctls_db_dir <- file.path(subdir, study_dirname, paste0("01_", Sys.getenv('TIMCI_COUNTRY'), "_raw_rct_ls"))
  dir.create(raw_rctls_db_dir, showWarnings = FALSE)
  # Follow-up folder
  fu_dir <- file.path(subdir, study_dirname, paste0("02_", Sys.getenv('TIMCI_COUNTRY'), "_followup"))
  dir.create(fu_dir, showWarnings = FALSE)
  # SPA / time-flow / process mapping folder
  raw_spa_db_dir <- file.path(subdir, study_dirname, paste0("03_", Sys.getenv('TIMCI_COUNTRY'), "_raw_spa_tfpm"))
  dir.create(raw_spa_db_dir, showWarnings = FALSE)
  # Cost-effectiveness folder
  cost_dir <- file.path(subdir, study_dirname, paste0("04_", Sys.getenv('TIMCI_COUNTRY'), "_cost"))
  dir.create(cost_dir, showWarnings = FALSE)
  # Report folder
  report_dir <- file.path(subdir, study_dirname, paste0("07_", Sys.getenv('TIMCI_COUNTRY'), "_reports"))
  dir.create(report_dir, showWarnings = FALSE)
  # Qualitative folders
  qual_dir <- file.path(subdir, study_dirname, paste0("05_", Sys.getenv('TIMCI_COUNTRY'), "_raw_qualitative"))
  dir.create(qual_dir, showWarnings = FALSE)
  cg_idi_dir <- file.path(qual_dir, paste0("01_", Sys.getenv('TIMCI_COUNTRY'), "_caregiver_idis"))
  dir.create(cg_idi_dir, showWarnings = FALSE)
  hcp_idi_dir <- file.path(qual_dir, paste0("02_", Sys.getenv('TIMCI_COUNTRY'), "_provider_idis"))
  dir.create(hcp_idi_dir, showWarnings = FALSE)
  kii_dir <- file.path(qual_dir, paste0("03_", Sys.getenv('TIMCI_COUNTRY'), "_kiis"))
  dir.create(kii_dir, showWarnings = FALSE)
  online_survey_dir <- file.path(qual_dir, paste0("04_", Sys.getenv('TIMCI_COUNTRY'), "_online_survey"))
  dir.create(online_survey_dir, showWarnings = FALSE)
  # PATH folders
  path_dir <- file.path(subdir, study_dirname, paste0("06_", Sys.getenv('TIMCI_COUNTRY'), "_path"))
  dir.create(path_dir, showWarnings = FALSE)
  # Quality checks and cleaned folders
  qc_dir <- file.path(subdir, study_dirname, paste0("08a_", Sys.getenv('TIMCI_COUNTRY'), "_quality_checks"))
  dir.create(qc_dir, showWarnings = FALSE)
  lock_dir <- file.path(subdir, study_dirname, paste0("08b_", Sys.getenv('TIMCI_COUNTRY'), "_cleaned_datasets"))
  dir.create(lock_dir, showWarnings = FALSE)

  out <- list(subdir,
              raw_rctls_db_dir,
              fu_dir,
              raw_spa_db_dir,
              report_dir,
              qc_dir,
              lock_dir,
              path_dir,
              cost_dir,
              cg_idi_dir,
              hcp_idi_dir,
              kii_dir,
              online_survey_dir)

}
