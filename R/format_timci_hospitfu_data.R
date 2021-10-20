#' Process hospital data (TIMCI-specific function)
#'
#' @param df dataframe containing the non de-identified (raw) ODK data collected at the referral level
#' @return This function returns a formatted dataframe for future display and analysis.
#' @export
#' @import dplyr magrittr

format_hospital_data <- function(df) {

  # Replace the space between different answers by a semicolon in multiple select questions
  sep <- ";"
  multi_cols = c("n4_n4_1")
  df <- format_multiselect_asws(df, multi_cols, sep)

  # Match column names with names from dictionary
  dictionary <- readxl::read_excel(system.file(file.path('extdata', "hospit_dict.xlsx"), package = 'timci'))
  sub <- subset(dictionary, deidentified == 1)
  df <- match_from_xls_dict(df, "hospit_dict.xlsx")
  df <- df[sub$new]

}

#' Generate hospital follow-up log (TIMCI-specific function)
#'
#' Generate a list of participants to be searched for at hospital level
#' @param pii dataframe containing personally identifiable data
#' @param fu7df dataframe containing day 7 follow-up data
#' @param hospitdf dataframe containing hospital follow-up data
#' @param deidentify boolean, enable the generation of a format with personally identifiable information (PII) for upload on ODK Central (if set to FALSE) or of a deidentified dataframe (if set to TRUE)
#' @return This function returns a dataframe that contains the list of participants to be searched for at hospital level.
#' @export
#' @import magrittr dplyr

generate_hospital_log <- function(pii,
                                  fu7df,
                                  hospitdf,
                                  deidentify = FALSE) {

  hospit_log <- NULL
  if (!is.null(fu7df)) {
    if (nrow(fu7df) > 0) {

      day7fu_data <- timci::format_day7_data(fu7df)[[3]]
      hospit_log <- day7fu_data %>% dplyr::filter( (hf_visit_day7 == 1 & hf_visit_type == 1) | (status_day7 == 2) )

      col_order <- c('child_id',
                     'sex',
                     'date_visit',
                     'dob',
                     'age_mo',
                     'fs_name',
                     'ls_name',
                     'facility')
      rpii <- pii[, col_order]

      hospit_log <- merge(hospit_log, rpii, by = "child_id")
      if (!is.null(hospit_log)) {
        if (nrow(hospit_log) > 0) {
          hospit_log$child_name <- paste(hospit_log$fs_name, hospit_log$ls_name)
          hospit_log$label <- paste0(hospit_log$child_name, " (", hospit_log$rhf_id, " ", hospit_log$rhf_name, " - ", hospit_log$rhf_loc_name, ")")
          hospit_log$sex <- ifelse(hospit_log$sex == 1, "male", ifelse(hospit_log$sex == 2, "female", "other"))

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
                           'date_call')
          }
          hospit_log <- hospit_log[, col_order]

          hospit_log <- hospit_log %>% dplyr::rename('name' = 'child_id',
                                                     'enroldate' = 'date_day0',
                                                     'hvisitdate' = 'date_hosp_day7',
                                                     'day7fudate' = 'date_call')
        }
      }

      # Exclude children who already underwent successful hospital follow-up
      if (!is.null(hospitdf)) {
        if (nrow(hospitdf) > 0) {
          hospit_data <- timci::format_hospital_data(hospitdf)
          #hospit_data <- hospit_data %>% dplyr::filter(found == 1)
          hospit_log <- hospit_log[!(hospit_log$name %in% hospit_data$child_id),]
        }
      }

    }
  }

  "
  # Add a first generic row
  hospit_log <- rbind(data.frame('fid' = 'F0000',
                                 'device_id' = 'none',
                                 'child_id' = 'X-F0000-P0000',
                                 'child_name' = 'CHILD NAME',
                                 'sex' = 'SEX',
                                 'date_visit' = 'ENROLMENT DATE',
                                 'caregiver' = 'CAREGIVER NAME',
                                 'main_cg_lbl' = 'RELATIONSHIP',
                                 'mother' = 'MOTHER NAME',
                                 'phone_nb' = 'PHONE NB 1',
                                 'phone_nb2' = 'PHONE NB 2',
                                 'phone_nb3' = 'PHONE NB 3',
                                 'location' = 'LOCATION',
                                 'cmty_contact' = 'CONTACT',
                                 'label' = 'CHILD NAME (VALID WINDOW)'),
                      hospit_log)
  "
  hospit_log

}
