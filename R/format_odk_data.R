#' Format ODK metadata
#' The function removes ODK note columns (which are systematically empty columns) from the outputted dataframe
#'
#' @param df dataframe containing the non de-identified (raw) ODK data, assuming standard metadata fields (`today`, `start`, `end`) are present.
#' @param start_date start date (optional, by default set to `NULL`)
#' @param end_date end date (optional, by default set to `NULL`)
#' @return This function returns a formatted dataframe for future display and analysis.
#' @import magrittr dplyr
#' @export

format_odk_metadata <- function(df, start_date = NULL, end_date = NULL) {

  if (dim(df)[1] > 0) {
    df$today <- strftime(df$start,"%Y-%m-%d")
    df$duration <- as.integer(round(df$end - df$start, digits = 0))
    df$start_time <- strftime(df$start,"%T")
    df$start <- strftime(df$start,"%Y-%m-%d %T")
    df$end_time <- strftime(df$end,"%T")
    df$end <- strftime(df$end,"%Y-%m-%d %T")
    df <- df %>% dplyr::rename(date = today)

    # Filter by start date and end date
    if (!is.null(start_date)) {
      df <- df %>%
        dplyr::filter(date >= as.Date(start_date, "%Y-%m-%d"))
      print(df)
    }
    if (!is.null(end_date)) {
      df <- df %>%
        dplyr::filter(date <= as.Date(end_date, "%Y-%m-%d"))
    }

    # Remove ODK note columns (which are systematically empty columns and identified by the string generated_note_name) from the outputted dataframe
    cols <- colnames(df)
    drops <- cols[grepl("generated_note_name_", cols)]
    df <- df[, !(names(df) %in% drops)]

    df
  }

}

#' Unzip and extract ODK data from ODK zip
#'
#' @param odk_zip absolute path to the zip file named "`fid`.zip" containing ODK submissions as CSV, plus separate CSVs for any repeating groups, plus any attachments in a subfolder `media`
#' @param csv_name name of the .CSV file
#' @param start_date start date (optional, by default set to `NULL`)
#' @param end_date end date (optional, by default set to `NULL`)
#' @param local_dir local directory (optional, by default set to `tempdir()`)
#' @param col_specs column specifications (optional)
#' @return This function returns a formatted dataframe for future display and analysis.
#' @import readr utils fs
#' @export

extract_data_from_odk_zip <- function(odk_zip, csv_name, start_date = NULL, end_date = NULL, local_dir = tempdir(), col_specs = NULL) {

  utils::unzip(odk_zip, exdir = local_dir)
  #fn <- fs::dir_ls(local_dir, glob=paste0("*", csv_name))
  #print(fn)
  #raw_odk_data <- fn %>% readr::read_csv()
  fs::dir_ls(local_dir)
  if (!is.null(col_specs)) {
    raw_odk_data <- readr::with_edition(1, readr::read_csv(file.path(local_dir, csv_name), col_types = col_specs))
  } else{
    raw_odk_data <- readr::with_edition(1, readr::read_csv(file.path(local_dir, csv_name)))
  }
  timci::format_odk_metadata(raw_odk_data, start_date, end_date)

}

#' Extract data from ODK server to a formatted dataframe.
#' This function relies on ruODK to export submissions.
#'
#' @param cpid integer, ODK project ID
#' @param cpid_forms list of form IDs in `cpid`
#' @param cfid string, ODK form ID
#' @param cpp string, ODK project passphrase (optional, only required if the ODK project is encrypted)
#' @param start_date date, data collection start date (optional)
#' @param end_date date, data collection end date (optional)
#' @param col_specs column specifications (optional)
#' @param verbose boolean, displays more information about the function output
#' @return This function returns a formatted dataframe for future display and analysis.
#' @import ruODK
#' @export

extract_data_from_odk_server <- function(cpid, cpid_forms, cfid, cpp="", start_date = NULL, end_date = NULL, col_specs = NULL, verbose = FALSE) {

  df <- NULL
  cdir <- tempdir()

  if (cfid %in% cpid_forms) {
    odk_zip <- ruODK::submission_export(local_dir = cdir,
                                        pid = cpid,
                                        fid = cfid,
                                        pp = cpp,
                                        media = FALSE)

    df <- timci::extract_data_from_odk_zip(odk_zip = odk_zip,
                                           csv_name = paste0(cfid,".csv"),
                                           start_date = start_date,
                                           end_date = end_date,
                                           local_dir = cdir,
                                           col_specs = col_specs)
  }

  if (verbose == TRUE) {
    if (!is.null(df)) {
      write(paste0("Data successfully downloaded: ", nrow(df), " row(s)."), stderr())
    } else {
      write("Data could not be downloaded.", stderr())
    }
  }

  df

}

#' Unzip and extract additional data from ODK zip
#'
#' @param odk_zip absolute path to the zip file named "`fid`.zip" containing ODK submissions as CSV, plus separate CSVs for any repeating groups, plus any attachments in a subfolder `media`
#' @param csv_name name of the .CSV file
#' @param local_dir local directory (optional, by default set to `tempdir()`)
#' @return This function returns a formatted dataframe for future display and analysis.
#' @import readr utils fs
#' @export

extract_additional_data_from_odk_zip <- function(odk_zip, csv_name, local_dir = tempdir()) {

  utils::unzip(odk_zip, exdir = local_dir)
  fs::dir_ls(local_dir)
  fn <- file.path(local_dir, csv_name)
  df <- NULL
  if ( file.exists(fn) ) {
    df <- raw_odk_data <- readr::with_edition(1, readr::read_csv(fn))
  }
  df

}

#' Extract complex data from ODK server to a a list of dataframes.
#' This function relies on ruODK to export submissions.
#'
#' @param cpid integer, ODK project ID
#' @param cpid_forms list of form IDs in `cpid`
#' @param cfid string, ODK form ID
#' @param cpp string, ODK project passphrase (optional, only required if the ODK project is encrypted)
#' @param start_date date, data collection start date (optional)
#' @param end_date date, data collection end date (optional)
#' @param col_specs column specifications (optional)
#' @param verbose boolean, displays more information about the function output
#' @return This function returns a list of dataframes.
#' @import ruODK
#' @export

extract_complex_data_from_odk_server <- function(cpid, cpid_forms, cfid, cpp="", start_date = NULL, end_date = NULL, col_specs = NULL, verbose = FALSE) {

  out <- NULL
  cdir <- tempdir()

  if (cfid %in% cpid_forms) {
    odk_zip <- ruODK::submission_export(local_dir = cdir,
                                        pid = cpid,
                                        fid = cfid,
                                        pp = cpp,
                                        media = TRUE)
    # Extract ODK submissions
    df <- timci::extract_data_from_odk_zip(odk_zip = odk_zip,
                                           csv_name = paste0(cfid,".csv"),
                                           start_date = start_date,
                                           end_date = end_date,
                                           local_dir = cdir,
                                           col_specs = col_specs)

    # Extract and append other CSV attachments
    out <- list(df)
    files <- list.files(path = cdir,
                        pattern = glob2rx(paste0(cfid,"*.csv")))
    i = 2
    for (f in files){
      if (f != paste0(cfid,".csv")) {
        if (verbose == TRUE) {
          write(f, stderr())
        }
        df_tmp <- timci::extract_additional_data_from_odk_zip(odk_zip = odk_zip,
                                                              csv_name = f,
                                                              local_dir = cdir)
        out[[i]] <- df_tmp
        i <- i + 1
      }
    }

  }

  if (verbose == TRUE) {
    if (!is.null(df)) {
      write(paste0("Data successfully downloaded: ", nrow(df), " row(s)."), stderr())
    } else {
      write("Data could not be downloaded.", stderr())
    }
  }
  out

}

#' Format multiple select answers so as to separate them
#'
#' @param df dataframe containing ODK data
#' @param cols list of column names
#' @param sep separator, e.g. ";" "," etc
#' @return This function returns a dataframe with multiple answers separated by `sep`.
#' @import stringr
#' @export

format_multiselect_asws <- function(df, cols, sep) {

  dfcols <- colnames(df)

  # Replace the space between different answers by `sep` in multiple select questions
  for (c in cols) {
    if (c %in% dfcols) {
      df[[c]] <- stringr::str_replace_all(df[[c]], " ", sep)
    }
  }
  df

}

#' Replace commas by semicolons in text fields for CSV export
#'
#' @param df dataframe containing ODK data
#' @param cols list of column names
#' @return This function returns a dataframe with multiple answers separated by `sep`.
#' @import stringr
#' @export

format_text_fields <- function(df, cols) {

  dfcols <- colnames(df)

  # Replace the space between different answers by `sep` in multiple select questions
  for (c in cols) {
    if (c %in% dfcols) {
      df[[c]] <- stringr::str_replace_all(df[[c]], ",", ";")
      df[[c]] <- stringr::str_replace_all(df[[c]], "\n", " ")
    }
  }
  df

}

#' Combine 2 columns - function in progress (to be tested)
#'
#' @param df Dataframe containing the non de-identified (raw) ODK data collected at the facility level
#' @param cols1 Vector of reference column names
#' @param cols2 Vector of column names to be merged with the reference columns
#' @return This function returns a formatted dataframe for future display and analysis.
#' @export

combine_columns <- function(df, cols1, cols2) {

  dfcols <- colnames(df)

  for (i in 1:length(cols1)) {
    c1 <- cols1[i]
    c2 <- cols2[i]
    if (c1 %in% dfcols) {
      if (c2 %in% dfcols) {
        df[[c1]] <- ifelse(!is.na(df[[c1]]), df[[c1]], df[[c2]])
      }
    }
  }
  df

}
