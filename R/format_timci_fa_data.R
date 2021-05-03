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
  res <- merge(x = res, y = stats, by = 'device_id', all.x = TRUE)

  # Retrieve previous assessment
  prev <- plyr::ddply(df, 'fcode', head, 2)
  prev <- plyr::ddply(prev, 'fcode', tail, 1)

  # Add the date of the last assessment
  res <- merge(x = res, y = prev[,c('date','fcode')], by = 'fcode', all.x = TRUE)
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

  sub <- df[df$pox_in_use == 0,]
  n <- nrow(sub)
  cat('##', n, 'facility(ies) where the pulse oximeter is not used\n\n')
  if (n > 0) {
    for (i in 1:nrow(sub)) {
      cat('* ', sub[i,'facility'], paste0('(', sub[i,'district'], ')'),'\n\n')
    }
  } else {
    cat('N/A\n\n')
  }
  sub <- df[df$hcp_arrival == 1,]
  n <- nrow(sub)
  cat('##', n, 'facility(ies) where a new provider started\n\n')
  if (n > 0) {
    for (i in 1:nrow(sub)) {
      cat('* ', sub[i,'facility'], paste0('(', sub[i,'district'], ')'),'\n\n')
    }
  } else {
    cat('N/A\n\n')
  }
  sub <- df[df$hcp_departure == 1,]
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
    cat('###', sprintf("%s", row$facility),'\n\n')
    cat('Recruitment covers the period from ', paste0('**', as.character(row$recruitment_start), '**'), ' to ' , paste0('**', as.character(row$recruitment_last), '**'),'\n\n')
    cat('* ', paste0('**', row$children, '**'), 'children 0-59m enrolled\n\n')
    cat('* ', paste0('**', row$female, '**'), 'female children 0-59m enrolled\n\n')
    cat('* ', paste0('**', row$yg_infant, '**'), 'young infants (0-2m) enrolled\n\n')
    cat('* ', paste0('**', row$yg_female, '**'), 'female young infants (0-2m) enrolled\n\n')
    res <- all_df[all_df$fcode == fid,]
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

#' Format weekly facility assessment data for export (TIMCI-specific function)
#'
#' @param df dataframe containing the processed facility assessment data
#' @return This function returns a formatted dataframe for export.
#' @export
#' @import dplyr magrittr

format_weekly_fa_data_for_export <- function(df) {

  # Order columns
  col_order <- c('date',
                 'device_id',
                 'district',
                 'facility',
                 'recruitment_start',
                 'recruitment_last',
                 'screened',
                 'children',
                 'female',
                 'yg_infant',
                 'yg_female',
                 'pox_in_use',
                 'hcp_arrival',
                 'hcp_departure',
                 'drug_stockout')
  df <- df[, col_order]

}
