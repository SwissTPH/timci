#' Write dataframe to an Excel spreadsheet, with a filename consisting of a prefix and a date stamp
#'
#' @param df dataframe.
#' @param dirname directory where the Excel spreadsheet will be created.
#' @param prefix filename prefix
#' @param rnames Row names
#' @return This function returns the creation timestamp of the Excel spreadsheet
#' @export

export_df2xlsx <- function(df, dirname, prefix, rnames = FALSE) {

  t <- NULL
  if ( !is.null(dirname) ) {
    fname <- file.path(dirname, paste0(prefix, ".xlsx"))
    openxlsx::write.xlsx(df,
                         fname,
                         row.names = rnames,
                         overwrite = TRUE,
                         encoding = "UTF-8")
    t <- format(file.info(fname)$ctime, usetz = TRUE)
  }
  t

}

#' Write dataframe to a CSV file format, with a filename consisting of a prefix and a date stamp
#'
#' @param df dataframe.
#' @param dirname directory where the CSV file will be created.
#' @param prefix filename prefix
#' @return This function returns the creation timestamp of the CSV file
#' @export

export_df2csv <- function(df, dirname, prefix) {

  t <- NULL
  if ( !is.null(dirname) ) {
    fname <- file.path(dirname, paste0(prefix, ".csv"))
    write.csv(df,
              file = fname,
              row.names = FALSE,
              quote = FALSE,
              fileEncoding = "UTF-8")
    t <- format(file.info(fname)$ctime, usetz = TRUE)
  }
  t

}

#' Write dataframe to both CSV and Excel spreadsheet formats, with a filename consisting of a prefix and a date stamp
#'
#' @param df dataframe.
#' @param dirname directory where the RDS file will be created.
#' @param prefix filename prefix
#' @return This function return a list that contains the creation timestamps of the Excel and the CSV files
#' @export

export_df2csvxlsx <- function(df, dirname, prefix) {

  t1 <- timci::export_df2xlsx(df,
                              dirname,
                              prefix)
  t2 <- NULL # CSV export disabled for more efficiency during the cleaning phase
  # t2 <- timci::export_df2csv(df,
  #                            dirname,
  #                            prefix)
  t <- list(t1, t2)

}

#' Write dataframe to an SQLite file, with a filename consisting of a prefix and a date stamp
#'
#' @param df dataframe.
#' @param dirname directory where the SQLite file will be created.
#' @param prefix filename prefix
#' @param rnames Row names
#' @return This function returns the creation timestamp of the Excel spreadsheet
#' @export

export_df2sqlite <- function(df, dirname, prefix, rnames = FALSE) {

  t <- NULL
  if ( !is.null(dirname) ) {
    "To be updated"
  }
  t

}

#' Write dataframe to an RDS file format, with a filename consisting of a prefix and a date stamp
#'
#' @param df dataframe.
#' @param dirname directory where the RDS file will be created.
#' @param prefix filename prefix
#' @export

export_df2rds <- function(df, dirname, prefix) {

  #fname <- file.path(dirname, paste(prefix, "_", Sys.Date(), ".rds", sep = ""))
  fname <- file.path(dirname, paste0(prefix, ".rds"))
  saveRDS(df, file = fname)
  fname

}

#' Anonymise a dataframe by hashing selected columns
#'
#' @param df Input dataframe
#' @param cols_to_anon Columns to use for hash
#' @param algo  hash functions algorithms to be used, currently available choices are md5, which is also the default, sha1, crc32, sha256, sha512, xxhash32, xxhash64, murmur32, spookyhash and blake3.
#' @return This function returns a vector
#' @export
#' @import digest

anonymise_dataframe <- function(df, cols_to_anon, algo = "sha256") {

  to_anon <- subset(df, select = cols_to_anon)
  unname(apply(to_anon, 1, digest::digest, algo = algo))

}

#' Extract and match variable names using a dictionary
#'
#' @param df Input dataframe
#' @param dictionary Dataframe containing 2 columns ('old' and 'new') that map the names of the variables in the input dataframe and the names of the variables in the output dataframe
#' @return This function returns a dataframe.
#' @export
#' @import dplyr

match_from_dict <- function(df, dictionary) {

  # Add column if it does not exit
  df[setdiff(dictionary$old, names(df))] <- NA

  # Rename column names based on the dictionary
  names(df)[match(dictionary$old, names(df))] <- dictionary$new
  df %>%
    dplyr::select(dictionary$new)

}

#' Extract and match variable names using an external Excel dictionary
#'
#' @param df Input dataframe
#' @param xls_dict Excel spreadsheet containing 2 columns ('old' and 'new') that map the names of the variables in the input dataframe and the names of the variables in the output dataframe
#' @return This function returns a dataframe.
#' @export
#' @import dplyr

match_from_xls_dict <- function(df,
                                xls_dict) {

  # Import dictionary
  dictionary_pathname <- system.file(file.path('extdata', xls_dict),
                                     package = 'timci')
  dictionary <- readxl::read_excel(dictionary_pathname)

  df %>%
    timci::match_from_dict(dictionary)

}

#' Import country-specific Excel dictionary
#'
#' @param xls_dict Excel spreadsheet containing 2 columns ('old' and 'new') that map the names of the variables in the input dataframe and the names of the variables in the output dataframe
#' @param country Character that contains the name of the TIMCI country to select (default set to "none")
#' @return This function returns a dataframe.
#' @export
#' @import dplyr

import_country_specific_xls_dict <- function(xls_dict,
                                             country = "none") {

  # Import dictionary
  dictionary_pathname <- system.file(file.path('extdata', xls_dict),
                                     package = 'timci')
  dictionary <- readxl::read_excel(dictionary_pathname)

  # Filter dictionary to only keep variables that are relevant for the country of interest
  if (country == 'Tanzania') {
    dictionary <- dictionary %>%
      dplyr::filter(is_tanzania == 1)
  } else if (country == 'India') {
    dictionary <- dictionary %>%
      dplyr::filter(is_india == 1)
  } else if (country == 'Kenya') {
    dictionary <- dictionary %>%
      dplyr::filter(is_kenya == 1)
  } else if (country == 'Senegal') {
    dictionary <- dictionary %>%
      dplyr::filter(is_senegal == 1)
  }

  dictionary

}

#' Extract and match variable names using an external Excel dictionary
#'
#' @param df Input dataframe
#' @param xls_dict Excel spreadsheet containing 2 columns ('old' and 'new') that map the names of the variables in the input dataframe and the names of the variables in the output dataframe
#' @param is_deidentified Boolean, flag to not export personally identifiable variable (default set to FALSE)
#' @param country Character that contains the name of the TIMCI country to select (default set to "none")
#' @return This function returns a dataframe.
#' @export
#' @import dplyr

match_from_filtered_xls_dict <- function(df,
                                         xls_dict,
                                         is_deidentified = FALSE,
                                         country = "none") {

  # Import dictionary
  dictionary <- timci::import_country_specific_xls_dict(xls_dict, country)

  # Filter dictionary to only keep deidentified variables
  if (is_deidentified) {
    dictionary <- dictionary %>%
      dplyr::filter(deidentified == 1)
  }

  # Match column names with names from dictionary
  df %>%
    timci::match_from_dict(dictionary)

}

#' Import column-specifications from country-specific Excel dictionary
#'
#' @param xls_dict Excel spreadsheet containing 2 columns ('old' and 'new') that map the names of the variables in the input dataframe and the names of the variables in the output dataframe
#' @param country Character that contains the name of the TIMCI country to select (default set to "none")
#' @return This function returns a dataframe.
#' @export
#' @import dplyr

import_col_specifications <- function(xls_dict,
                                      country){

  # Import dictionary
  dictionary <- timci::import_country_specific_xls_dict(xls_dict, country)

  col_specs <- list()
  for ( i in 1:nrow(dictionary) ) {

    label <- dictionary[[i, "old"]]
    val <- dictionary[[i, "type"]]

    if (val == "date") {
      col_specs[[label]] <- readr::col_date(format = "%Y-%m-%d")
    } else if (val == "timestamp") {
      col_specs[[label]] <- readr::col_datetime(format = "")
    } else if (val == "nominal") {
      col_specs[[label]] <- readr::col_character()
      # col_specs[[label]] <- readr::col_factor(levels = NULL,
      #                                         ordered = FALSE,
      #                                         include_na = FALSE)
    } else if (val == "ordinal") {
      col_specs[[label]] <- readr::col_factor(levels = NULL,
                                              ordered = TRUE,
                                              include_na = FALSE)
    } else if (val == "integer") {
      col_specs[[label]] <- readr::col_integer()
    } else if (val == "double") {
      col_specs[[label]] <- readr::col_double()
    } else if (val == "character") {
      col_specs[[label]] <- readr::col_character()
    } else{
      col_specs[[label]] <- readr::col_character()
    }

  }

  n <- length(col_specs)
  col_specs[c(1, n - 1)]
}

#' Combine two dataframes in one
#'
#' @param df1 Input dataframe 1
#' @param df2 Input dataframe 2
#' @param verbose If set to true, writes text in stderr()
#' @return This function returns a dataframe.
#' @export
#' @import data.table

combine_dataframes <- function(df1,
                               df2,
                               verbose = FALSE) {

  if (verbose) {
    write(paste0("  o ", nrow(df1), "submissions"), stderr())
    write(paste0("  o ", nrow(df2), "submissions"), stderr())
  }

  df1 <- data.frame(lapply(df1, as.character),
                    stringsAsFactors = FALSE)
  df2 <- data.frame(lapply(df2, as.character),
                    stringsAsFactors = FALSE)

  combined_list <- list(source1 = df1,
                        source2 = df2)
  out <- data.table::rbindlist(combined_list,
                               use.names = TRUE,
                               fill = TRUE,
                               idcol = "origin")
  out <- data.frame(out)
  names(out) <- gsub("\\.", "-", names(out))

  if (verbose) {
    write(paste0("  o ", nrow(out), "submissions"), stderr())
  }

  out

}

#' Pivot values of duplicated rows into new columns
#'
#' @param df Input dataframe
#' @return This function returns a dataframe.
#' @export
#' @import dplyr tidyr

pivot_duplicates_to_columns <- function(df) {

  cols <- colnames(df)
  cols <- cols[!(cols %in% c("child_id"))]

  df %>%
    dplyr::group_by(child_id) %>%
    dplyr::mutate(row_n = row_number()) %>%
    tidyr::pivot_wider(child_id,
                       names_from = row_n,
                       values_from = cols)

}

#' Check that a dataframe is not empty
#'
#' @param df dataframe.
#' @return This function returns a boolean that is equal to TRUE if the dataframe contains at least 1 row and 1 column.
#' @export

is_not_empty <- function(df) {

  df_is_not_empty <- FALSE
  if (!is.null(df)) {
    df_is_not_empty <- (nrow(df) > 0) & (length(df) > 0)
  }
  df_is_not_empty

}

#' Calculate a normalised Levenshtein ratio between two strings
#'
#' @param s1 string
#' @param s2 string
#' @return This function returns a normalised levenshtein ratio.
#' @export
#' @import stringdist

normalised_levenshtein_ratio <- function(s1, s2) {

  n <- max(nchar(s1),nchar(s2))
  d <- stringdist(s1, s2, method = 'lv')
  out <- 100 * (1 - d/n)
  out

}

#' Export datasets
#'
#' @param df quality check dataframe.
#' @param idx directory where the Excel file will be created.
#' @param label filename prefix
#' @param cdir Row names
#' @param description Row names
#' @return creation timestamp of the Excel file
#' @export

dataset_export <- function(df,
                           idx,
                           label,
                           cdir,
                           description) {

  msg <- paste0("**",
                description,
                "** is a NULL object and cannot be exported.")

  if (!is.null(df)) {

    filename <- paste(idx, label, sep = "_")
    timestamps <- timci::export_df2csvxlsx(df, cdir, filename)
    if ( !is.null(timestamps[[2]]) ) {
      msg <- paste0(description,
                    " have been exported to **",
                    filename,
                    ".xslx** (**",
                    timestamps[[1]],
                    "**) and to **",
                    filename,
                    ".csv** (**",
                    timestamps[[2]],
                    "**) in the **",
                    basename(cdir),
                    "** folder.")
    } else{
      msg <- paste0(description,
                    " have been exported to **",
                    filename,
                    ".xslx** (**",
                    timestamps[[1]],
                    "**) in the **",
                    basename(cdir),
                    "** folder.")
    }

  }

  cat(msg)

}
