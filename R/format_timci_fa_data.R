#' Process weekly facility assessment data (TIMCI-specific function)
#'
#' @param df dataframe containing the (raw) ODK data collected in the weekly facility assessment
#' @return This function returns a formatted dataframe for future display and analysis.
#' @export

process_weekly_fa_data <- function(df) {

  df$"page1-pox_use" <- as.integer(df$"page1-pox_use")
  df$"page1-new_hcp" <- as.integer(df$"page1-new_hcp")
  df$"page1-hcp_leave" <- as.integer(df$"page1-hcp_leave")
  df$"page1-drug_stock_out" <- as.integer(df$"page1-drug_stock_out")

  if (Sys.getenv('TIMCI_COUNTRY') != "India") {
    df$"page1-children_seen" <- as.integer(df$"page1-children_seen")
  }

  # Match column names with names from dictionary
  df <- match_from_xls_dict(df, "wfa_dict.xlsx")

}

#' Extract last available facility assessment data (TIMCI-specific function)
#'
#' @param df dataframe containing the processed facility assessment data
#' @param stats dataframe containing summary statistics for each facility
#' @return This function returns a dataframe that only contains the last available assessment for each facility.
#' @export
#' @import dplyr magrittr

extract_last_fa_data <- function(df, stats) {

  # Retrieve last assessment
  res <- plyr::ddply(df, 'fcode', head, 1)
  res <- merge(x = res, y = stats, by.x = 'fcode', by.y = 'facility_id', all = TRUE)

  # Retrieve previous assessment
  prev <- plyr::ddply(df, 'fcode', head, 2)
  prev <- plyr::ddply(prev, 'fcode', tail, 1)

  # Add the date of the last assessment
  res <- merge(x = res, y = prev[,c('date','fcode')], by = 'fcode', all = TRUE)
  res %>% dplyr::rename('date' = 'date.x',
                        'prev' = 'date.y')

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
        cat('The latest assessment covers the period from ', paste0('**', as.character(row$prev), '**'), ' to ', paste0('**', as.character(row$date), '**'), '\n\n')
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
#' @import dplyr magrittr

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
                 'date',
                 'pox_in_use',
                 'hcp_arrival',
                 'hcp_departure',
                 'drug_stockout')
  df <- df[, col_order]
  df %>% dplyr::rename('facility_id' = 'fcode',
                       'last_weekly_report' = 'date')

}
