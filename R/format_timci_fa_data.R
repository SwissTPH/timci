#' Process weekly facility assessment data (TIMCI-specific function)
#'
#' @param df dataframe containing the (raw) ODK data collected in the weekly facility assessment
#' @return This function returns a formatted dataframe for future display and analysis.
#' @export

process_weekly_fa_data <- function(df) {

  df <- format_odk_metadata(df)

  df$page1_pox_use <- as.integer(df$page1_pox_use)
  df$page1_new_hcp <- as.integer(df$page1_new_hcp)
  df$page1_hcp_leave <- as.integer(df$page1_hcp_leave)
  df$page1_drug_stock_out <- as.integer(df$page1_drug_stock_out)

  df

}

#' Extract the last available facility assessment data (TIMCI-specific function)
#'
#' @param df dataframe containing the processed facility assessment data
#' @return This function returns a dataframe that only contains the last available assessment for each facility.
#' @export
#' @import dplyr magrittr

extract_last_fa_data <- function(df) {

  # Retrieve last assessment
  res <- plyr::ddply(df, 'page1_fcode', head, 1)

  # Retrieve previous assessment
  prev <- plyr::ddply(df, 'page1_fcode', head, 2)
  prev <- plyr::ddply(prev, 'page1_fcode', tail, 1)

  # Add the date of the last assessment
  res <- merge(x = res, y = prev[,c('date','page1_fcode')], by = 'page1_fcode', all.x = TRUE)
  res %>% dplyr::rename('date' = 'date.x',
                        'prev' = 'date.y')

}

#' Display weekly facility assessment data per facility (TIMCI-specific function)
#'
#' @param df dataframe containing the processed ODK data collected in the weekly facility assessment
#' @param all_df dataframe containing the processed ODK data collected in the weekly facility assessment
#' @return This function returns a formatted dataframe for future display and analysis.
#' @export
#' @import dplyr magrittr stringr

display_weekly_fa_data_per_facility <- function(df, all_df) {

  cat('## Facilities where the pulse oximeter is not used\n\n')
  sub <- df[df$page1_pox_use == 0,]
  if (nrow(sub) > 0) {
    for (i in 1:nrow(sub)) {
      cat(sub[i,'page1_fname'], '(', sub[i,'page1_district'], ')','\n\n')
    }
  } else {
    cat('N/A\n\n')
  }
  cat('## Facilities where a new provider started\n\n')
  sub <- df[df$page1_new_hcp == 1,]
  if (nrow(sub) > 0) {
    for (i in 1:nrow(sub)) {
      cat(sub[i,'page1_fname'], '(', sub[i,'page1_district'], ')','\n\n')
    }
  } else {
    cat('N/A\n\n')
  }
  cat('## Facilities where a trained provider stopped working\n\n')
  sub <- df[df$page1_hcp_leave == 1,]
  if (nrow(sub) > 0) {
    for (i in 1:nrow(sub)) {
      cat(sub[i,'page1_fname'], '(', sub[i,'page1_district'], ')','\n\n')
    }
  } else {
    cat('N/A\n\n')
  }
  cat('## Details for each facility\n\n')
  for (i in 1:nrow(df)) {
    row <- df[i,]
    fid <- row$'page1_fcode'
    cat('###', sprintf("%s", row$'page1_fname'),'\n\n')
    res <- all_df[all_df$'page1_fcode' == fid,]
    cat('Number of weekly assessments available for this facility: ', sprintf('<strong>%s</strong>',nrow(res)), '\n\n')
    cat('The latest report covers the period from ', sprintf('<strong>%s</strong>',as.character(row$'prev')), ' to ', sprintf('<strong>%s</strong>',as.character(row$'date')), '\n\n')
    cat('Number of children screened: TBD\n\n')
    cat('Pulse oximeter in use for the sick neonate / child consultations: ')
    if (row$'page1_pox_use' == 1) {
      cat('**YES** \n\n')
    } else {
      cat('**NO** \n\n')
    }
    cat('New provider started working at the facility: ')
    if (row$'page1_new_hcp' == 1) {
      cat('**YES** \n\n')
    } else {
      cat('**NO** \n\n')
    }
    cat('Trained provider stopped working at this facility: ')
    if (row$'page1_hcp_leave' == 1) {
      cat('**YES** \n\n')
    } else {
      cat('**NO** \n\n')
    }
  }

}

#' Process weekly facility assessment data (TIMCI-specific function)
#'
#' @param df dataframe containing the (raw) ODK data collected in the weekly facility assessment
#' @return This function returns a formatted dataframe for future display and analysis.
#' @export
#' @import dplyr magrittr

format_weekly_fa_data_to_export <- function(df) {

  # Order columns
  col_order <- c('date',
                 'deviceid',
                 'page1_district',
                 'page1_fname',
                 'page1_pox_use',
                 'page1_new_hcp',
                 'page1_hcp_leave')
  df <- df[, col_order]
  df %>% dplyr::rename('District' = 'page1_district',
                       'Facility' = 'page1_fname',
                       'POX In Use Flag' = 'page1_pox_use',
                       'New Provider Flag' = 'page1_new_hcp',
                       'Trained Provider Transitioned Flag' = 'page1_hcp_leave')

}
