#' Process hospital data (TIMCI-specific function)
#'
#' @param df dataframe containing the non de-identified (raw) ODK data collected at the referral level
#' @param is_deidentified Boolean, flag to not export personally identifiable variable (default set to `TRUE`)
#' @return This function returns a formatted dataframe for future display and analysis.
#' @export
#' @import dplyr

format_hospital_data <- function(df,
                                 is_deidentified = TRUE) {

  # Replace the space between different answers by a semicolon in multiple select questions
  sep <- ";"
  multi_cols = c("n4-n4_1",
                 "n4-n4_8")
  df <- format_multiselect_asws(df,
                                multi_cols, sep)

  df <- timci::match_from_filtered_xls_dict(df,
                                            "hospit_dict.xlsx",
                                            is_deidentified = is_deidentified,
                                            country = Sys.getenv('TIMCI_COUNTRY'))
  df

}

#' Generate hospital follow-up log (TIMCI-specific function)
#'
#' Generate a list of participants to be searched for at hospital level
#' @param pii dataframe containing personally identifiable data
#' @param fu7df dataframe containing day 7 follow-up data
#' @param day0df dataframe containing day 0 data
#' @param hospitdf dataframe containing hospital follow-up data
#' @param deidentify boolean, enable the generation of a format with personally identifiable information (PII) for upload on ODK Central (if set to FALSE) or of a deidentified dataframe (if set to TRUE)
#' @param is_pilot Boolean, default set to `FALSE`
#' @return This function returns a dataframe that contains the list of participants to be searched for at hospital level.
#' @export
#' @import dplyr

generate_hospital_log <- function(pii,
                                  fu7df,
                                  day0df,
                                  hospitdf,
                                  deidentify = FALSE,
                                  is_pilot = FALSE) {

  hospit_log <- NULL
  hospit_log2 <- NULL

  if((Sys.getenv('TIMCI_COUNTRY') != 'Tanzania') | (is_pilot == TRUE)){
    col_order <- c('child_id',
                   'sex',
                   'date_visit',
                   'dob',
                   'age_mo',
                   'fs_name',
                   'ls_name',
                   'facility',
                   'phone_nb',
                   'cg_fs_name',
                   'cg_ls_name')
  } else{
    col_order <- c('child_id',
                   'sex',
                   'date_visit',
                   'dob',
                   'age_mo',
                   'fs_name',
                   'ls_name',
                   'ms_name',
                   'facility',
                   'phone_nb',
                   'cg_fs_name',
                   'cg_ls_name',
                   'child_hf_id')
  }

  rpii <- pii[, col_order]

  if (!is.null(fu7df)) {
    if (nrow(fu7df) > 0) {

      ###############################################################################################################
      # Select children who visited a referral facility structure between Day 0 and Day 7 - Source: Day 7 follow-up #
      ###############################################################################################################

      day7fu_data <- timci::format_day7_data(fu7df)[[3]]

      if (Sys.getenv('TIMCI_COUNTRY') == 'India') {
        day7fu_data <- day7fu_data %>%
          dplyr::rowwise() %>%
          dplyr::mutate(hosp_visit = ifelse(13 %in% as.integer(unlist(strsplit(hf_visit_type, split = ";"))),
                                     1,
                                     0))
      } else {
        day7fu_data <- day7fu_data %>%
          dplyr::rowwise() %>%
          dplyr::mutate(hosp_visit = ifelse(1 %in% as.integer(unlist(strsplit(hf_visit_type, split = ";"))),
                                            1,
                                            0))
      }

      hospit_log <- day7fu_data %>%
        dplyr::filter( (status_day7 == 2) | (hf_visit_day7 == 1 & hosp_visit == 1) )

      hospit_log <- merge(hospit_log,
                          rpii,
                          by = "child_id")
      if (!is.null(hospit_log)) {
        if (nrow(hospit_log) > 0) {
          if((Sys.getenv('TIMCI_COUNTRY') != 'Tanzania') | (is_pilot == TRUE)) {
            hospit_log$child_name <- paste(hospit_log$fs_name, hospit_log$ls_name)
          } else {
            hospit_log$child_name <- paste(hospit_log$fs_name, hospit_log$ms_name, hospit_log$ls_name)
          }
          hospit_log$label <- paste0(hospit_log$child_name, " - ", hospit_log$rhf_id, " ", hospit_log$rhf_name, " (", hospit_log$rhf_loc_name, ") - enrolled at ", hospit_log$fid, " ", hospit_log$facility)
          hospit_log$sex <- ifelse(hospit_log$sex == 1, "male", ifelse(hospit_log$sex == 2, "female", "other"))
          hospit_log$date_day0 <- as.Date(hospit_log$date_day0, format = "%Y-%m-%d")
          hospit_log$rhf_id <- as.character(hospit_log$rhf_id)
          hospit_log$rhf_oth <- as.character(hospit_log$rhf_oth)

          # Exclude true duplicates (children with the same participant ID, the same name, the same sex and the same age)
          # Participants may appear several times in the hospital follow-up log because their Day 7 follow-up has been done several times.
          hospit_log <- hospit_log %>% dplyr::distinct_at(dplyr::vars(child_id,
                                                                      child_name,
                                                                      sex,
                                                                      age_mo),
                                                          .keep_all = TRUE)

          # Order columns
          if (deidentify) {
            col_order <- c('device_id',
                           'district',
                           'rhf_loc_id',
                           'rhf_loc_oth',
                           'rhf_loc_name',
                           'rhf_id',
                           'rhf_oth',
                           'rhf_name',
                           'date_hosp_day7',
                           'child_id',
                           'sex',
                           'age_mo',
                           'fid',
                           'facility',
                           'date_day0',
                           'date_call')
          } else{
            if ((Sys.getenv('TIMCI_COUNTRY') != 'Tanzania') | (is_pilot == TRUE)) {
              col_order <- c('device_id',
                             'district',
                             'rhf_loc_id',
                             'rhf_loc_oth',
                             'rhf_loc_name',
                             'rhf_id',
                             'rhf_oth',
                             'rhf_name',
                             'date_hosp_day7',
                             'child_id',
                             'label',
                             'child_name',
                             'sex',
                             'dob',
                             'age_mo',
                             'fid',
                             'facility',
                             'date_day0',
                             'date_call',
                             'phone_nb',
                             'cg_fs_name',
                             'cg_ls_name')
            } else {
              col_order <- c('device_id',
                             'district',
                             'rhf_loc_id',
                             'rhf_loc_oth',
                             'rhf_loc_name',
                             'rhf_id',
                             'rhf_oth',
                             'rhf_name',
                             'date_hosp_day7',
                             'child_id',
                             'label',
                             'child_name',
                             'sex',
                             'dob',
                             'age_mo',
                             'fid',
                             'facility',
                             'date_day0',
                             'date_call',
                             'child_hf_id',
                             'phone_nb',
                             'cg_fs_name',
                             'cg_ls_name')
            }
          }
          hospit_log <- hospit_log[, col_order]

          hospit_log <- hospit_log %>% dplyr::rename('name' = 'child_id',
                                                     'enroldate' = 'date_day0',
                                                     'hvisitdate' = 'date_hosp_day7',
                                                     'day7fudate' = 'date_call')
        }
      }

      ###############################################################################################################################
      # Select children who were admitted in the facility of enrolment at Day 0 or referred at Day 0 and lost to follow-up at Day 7 #
      ###############################################################################################################################

      # Lost to follow-up at Day 7
      ltfu_df <- timci::generate_ltfu_log(df = day0df,
                                          fudf = day7fu_data,
                                          end_date = 12,
                                          raw = FALSE)

      hospit_log2 <- ltfu_df %>% dplyr::filter( referral_cg == 1 | referral_hf == 1 )
      if ((Sys.getenv('TIMCI_COUNTRY') != 'Tanzania') | (is_pilot == TRUE)) {
        col_order <- c('child_id',
                       'fs_name',
                       'ls_name',
                       'dob',
                       'sex',
                       'phone_nb',
                       'cg_fs_name',
                       'cg_ls_name')
      } else {
        col_order <- c('child_id',
                       'fs_name',
                       'ms_name',
                       'ls_name',
                       'dob',
                       'sex',
                       'phone_nb',
                       'cg_fs_name',
                       'cg_ls_name')
      }
      rpii2 <- rpii[, col_order]

      hospit_log2 <- merge(hospit_log2,
                           rpii2,
                           by = "child_id")

      if (!is.null(hospit_log2)) {
        if (nrow(hospit_log2) > 0) {
          if ((Sys.getenv('TIMCI_COUNTRY') != 'Tanzania') | (is_pilot == TRUE)) {
            hospit_log2$child_name <- paste(hospit_log2$fs_name, hospit_log2$ls_name)
          } else {
            hospit_log2$child_name <- paste(hospit_log2$fs_name, hospit_log2$ms_name, hospit_log2$ls_name)
          }
          hospit_log2$label <- paste0(hospit_log2$child_name)
          hospit_log2$sex <- ifelse(hospit_log2$sex == 1, "male", ifelse(hospit_log2$sex == 2, "female", "other"))
          hospit_log2$date_visit <- as.Date(hospit_log2$date_visit, format = "%Y-%m-%d")
          hospit_log2$ref_facility <- as.character(hospit_log2$ref_facility)

          # Order columns
          if (deidentify) {
            col_order <- c('date_visit',
                           'device_id',
                           'district',
                           'referral_cg',
                           'referral_hf',
                           'urg_referral_hf',
                           'ref_facility',
                           'ref_facility_oth',
                           'child_id',
                           'sex',
                           'age_mo',
                           'fid',
                           'facility')
          } else{
            col_order <- c('date_visit',
                           'device_id',
                           'district',
                           'referral_cg',
                           'referral_hf',
                           'urg_referral_hf',
                           'ref_facility',
                           'ref_facility_oth',
                           'child_id',
                           'label',
                           'child_name',
                           'sex',
                           'dob',
                           'age_mo',
                           'fid',
                           'facility')
          }
          hospit_log2 <- hospit_log2[, col_order]

          hospit_log2 <- hospit_log2 %>% dplyr::rename('name' = 'child_id',
                                                       'enroldate' = 'date_visit',
                                                       'rhf_id' = 'ref_facility',
                                                       'rhf_oth' = 'ref_facility_oth')
        }
      }

    }
  }

  if (!is.null(hospit_log2)) {
    if (nrow(hospit_log2) > 0) {
      if (!is.null(hospit_log)) {
        if (nrow(hospit_log) > 0) {
          hospit_log <- dplyr::bind_rows(hospit_log, hospit_log2)
        } else {
          hospit_log <- hospit_log2
          hospit_log$day7fudate <- NA
        }
      } else{
        hospit_log <- hospit_log2
      }
    }
  }

  # Exclude children who already underwent hospital follow-up
  if (!is.null(hospitdf)) {
    if (nrow(hospitdf) > 0) {
      hospit_data <- timci::format_hospital_data(hospitdf)
      hospit_log <- hospit_log[!(hospit_log$name %in% hospit_data$child_id),]
    }
  }

  hospit_log

}
