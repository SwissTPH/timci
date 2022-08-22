#' Process weekly facility assessment data (TIMCI-specific function)
#'
#' @param df dataframe containing the (raw) ODK data collected in the weekly facility assessment
#' @param is_pilot Boolean that enables to select the pilot mode for Tanzania and India (optional, default set to FALSE)
#' @return This function returns a formatted dataframe for future display and analysis.
#' @export
#' @import dplyr lubridate

process_weekly_fa_data <- function(df,
                                   is_pilot = FALSE) {

  df$"page1-pox_use" <- as.integer(df$"page1-pox_use")
  df$"page1-new_hcp" <- as.integer(df$"page1-new_hcp")
  df$"page1-hcp_leave" <- as.integer(df$"page1-hcp_leave")
  if (Sys.getenv('TIMCI_COUNTRY') != "Tanzania" | is_pilot) {
    df$"page1-drug_stock_out" <- as.integer(df$"page1-drug_stock_out")
  } else{
    df$"page1-AmoxicillinDT" <- as.integer(df$"page1-AmoxicillinDT")
    df$"page1-ORS_zinc" <- as.integer(df$"page1-ORS_zinc")
    df$"page1-ALU_Artesunate" <- as.integer(df$"page1-ALU_Artesunate")
    df$"page1-listofantibiotics" <- as.integer(df$"page1-listofantibiotics")
    df$"page1-Phenobarbitone." <- as.integer(df$"page1-Phenobarbitone.")
    df$"page1-fever_antpain" <- as.integer(df$"page1-fever_antpain")
    df$"page1-drug_stock_out" <- ifelse(df$"page1-AmoxicillinDT"==1 | df$"page1-ORS_zinc"==1 | df$"page1-ALU_Artesunate"==1 | df$"page1-listofantibiotics"==1 | df$"page1-Phenobarbitone."==1 | df$"page1-fever_antpain"==1, 1, 0)
  }

  if (Sys.getenv('TIMCI_COUNTRY') != "India") {
    df$"page1-children_seen" <- as.integer(df$"page1-children_seen")
  }

  # Match column names with names from dictionary
  df <- match_from_xls_dict(df, "wfa_dict.xlsx")

  # Extract the week date
  df <- df %>%
    dplyr::mutate(date_week2 = ifelse(!is.na(date_week), as.Date(date_week, '%d.%m.%Y'), as.Date(date, '%Y-%m-%d'))) %>%
    dplyr::mutate(date_week3 = lubridate::floor_date(as.Date(date_week2, origin = lubridate::origin),
                                                             "week",
                                                             week_start = getOption("lubridate.week.start", 5)))
  drop <- c("date_week",
            "date_week2")
  df <- df[,!(names(df) %in% drop)]
  df %>% rename(date_week = date_week3)

}

#' Extract last available facility assessment data (TIMCI-specific function)
#'
#' @param df dataframe containing the processed facility assessment data
#' @param stats dataframe containing summary statistics for each facility
#' @return This function returns a dataframe that only contains the last available assessment for each facility.
#' @export
#' @import dplyr

extract_last_fa_data <- function(df, stats) {

  # Retrieve last assessment
  res <- plyr::ddply(df, 'fcode', head, 1)
  res <- merge(x = res,
               y = stats,
               by.x = 'fcode',
               by.y = 'fid_from_device',
               all.y = TRUE)

  # Retrieve previous assessment
  prev <- plyr::ddply(df, 'fcode', head, 2)
  prev <- plyr::ddply(prev, 'fcode', tail, 1)

  # Add the date of the last assessment
  res <- merge(x = res,
               y = prev[,c('date_week','fcode')],
               by = 'fcode',
               all = TRUE)
  res %>%
    dplyr::rename('date_week' = 'date_week.x',
                  'prev_week' = 'date_week.y')

}

#' Display weekly facility assessment data per facility (TIMCI-specific function)
#'
#' @param df dataframe containing the last available assessment for each facility
#' @param all_df dataframe containing all the processed facility assessment data
#' @return This function outputs information about the weekly facility assessment data per facility.
#' @export

display_weekly_fa_data_per_facility <- function(df, all_df) {

  sub <- df[!is.na(df$facility) & df$pox_in_use == 0,]
  n <- nrow(sub)
  cat('##', n, 'facility(ies) where the pulse oximeter is not used\n\n')
  if (n > 0) {
    for (i in 1:nrow(sub)) {
      cat('* ', sub[i,'facility'], paste0('(', sub[i,'district'], ')'),'\n\n')
    }
  } else {
    cat('N/A\n\n')
  }
  sub <- df[!is.na(df$facility) & df$hcp_arrival == 1,]
  n <- nrow(sub)
  cat('##', n, 'facility(ies) where a new provider started\n\n')
  if (n > 0) {
    for (i in 1:nrow(sub)) {
      cat('* ', sub[i,'facility'], paste0('(', sub[i,'district'], ')'),'\n\n')
    }
  } else {
    cat('N/A\n\n')
  }
  sub <- df[!is.na(df$facility) & df$hcp_departure == 1,]
  n <- nrow(sub)
  cat('##', n, 'facility(ies) where a trained provider stopped working\n\n')
  if (n > 0) {
    for (i in 1:nrow(sub)) {
      cat('* ', sub[i,'facility'], paste0('(', sub[i,'district'], ')'),'\n\n')
    }
  } else {
    cat('N/A\n\n')
  }
  cat('## Details for each facility\n\n')
  for (i in 1:nrow(df)) {
    row <- df[i,]
    fid <- row$'fcode'
    if (!is.na(row$facility)) {
      cat('###', sprintf("%s", row$facility),'\n\n')
      cat('Recruitment covers the period from ', paste0('**', as.character(row$recruitment_start), '**'), ' to ' , paste0('**', as.character(row$recruitment_last), '**'),'\n\n')
      cat('* ', paste0('**', ifelse(!is.na(row$screened), row$screened, 0), '**'), 'children screened\n\n')
      cat('* ', paste0('**', ifelse(!is.na(row$children), row$children, 0), '**'), 'children 0-59m enrolled\n\n')
      cat('* ', paste0('**', ifelse(!is.na(row$female), row$female, 0), '**'), 'female children 0-59m enrolled\n\n')
      cat('* ', paste0('**', ifelse(!is.na(row$yg_infant), row$yg_infant, 0), '**'), 'young infants (0-2m) enrolled\n\n')
      cat('* ', paste0('**', ifelse(!is.na(row$yg_female), row$yg_female, 0), '**'), 'female young infants (0-2m) enrolled\n\n')
      res <- all_df[all_df$fcode == fid,]
      if (nrow(res) > 0) {
        cat(paste0('**', nrow(res), '**'), ' weekly assessment(s) available for this facility\n\n')
        cat('The latest assessment covers the period from ', paste0('**', as.character(row$prev_week), '**'), ' to ', paste0('**', as.character(row$date_week), '**'), '\n\n')
        cat('* Pulse oximeter in use for the sick neonate / child consultations: ')
        if (row$pox_in_use == 1) {
          cat('**YES** \n\n')
        } else {
          cat('**NO** \n\n')
        }
        cat('* New provider started working at the facility: ')
        if (row$hcp_arrival == 1) {
          cat('**YES** \n\n')
        } else {
          cat('**NO** \n\n')
        }
        cat('* Trained provider stopped working at this facility: ')
        if (row$hcp_departure == 1) {
          cat('**YES** \n\n')
        } else {
          cat('**NO** \n\n')
        }
      }
    }
  }

}

#' Format weekly facility assessment data for export (TIMCI-specific function)
#'
#' @param df dataframe containing the processed facility assessment data
#' @return This function returns a formatted dataframe for export.
#' @export
#' @import dplyr

format_weekly_fa_data_for_export <- function(df) {

  df$facility <- ifelse(!is.na(df$facility), df$facility, df$facility_name)
  df$district <- ifelse(!is.na(df$district), df$district, df$lvl2)

  # Order columns
  col_order <- c('fcode',
                 'facility',
                 'district',
                 'recruitment_start',
                 'recruitment_last',
                 'screened',
                 'children',
                 'female',
                 'yg_infant',
                 'yg_female',
                 'date_week',
                 'pox_in_use',
                 'hcp_arrival',
                 'hcp_departure',
                 'drug_stockout')
  df <- df[, col_order]
  df %>% dplyr::rename('facility_id' = 'fcode',
                       'last_weekly_report' = 'date_week')

}
