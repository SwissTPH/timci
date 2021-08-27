#' Write dataframe to an Excel file, with a filename consisting of a prefix and a date stamp
#'
#' @param df dataframe.
#' @param dirname directory where the Excel file will be created.
#' @param prefix filename prefix
#' @export

export_df2xlsx <- function(df, dirname, prefix) {

  #fname <- file.path(dirname, paste(prefix, "_", Sys.Date(), ".xlsx", sep = ""))
  fname <- file.path(dirname, paste0(prefix, ".xlsx"))
  openxlsx::write.xlsx(df, fname, row.names = FALSE, overwrite = TRUE)
  fname

}

#' Write dataframe to a CSV file format, with a filename consisting of a prefix and a date stamp
#'
#' @param df dataframe.
#' @param dirname directory where the RDS file will be created.
#' @param prefix filename prefix
#' @export

export_df2csv <- function(df, dirname, prefix) {

  #fname <- file.path(dirname, paste(prefix, "_", Sys.Date(), ".csv", sep = ""))
  fname <- file.path(dirname, paste0(prefix, ".csv"))
  write.csv(df, file = fname, row.names = FALSE, quote = FALSE)
  fname

}

#' Write dataframe to both CSV and Excel file formats, with a filename consisting of a prefix and a date stamp
#'
#' @param df dataframe.
#' @param dirname directory where the RDS file will be created.
#' @param prefix filename prefix
#' @export

export_df2csvxlsx <- function(df, dirname, prefix) {

  fn <- timci::export_df2xlsx(df,
                              dirname,
                              prefix)
  fn <- timci::export_df2csv(df,
                             dirname,
                             prefix)
  fn

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
#' @import magrittr dplyr

match_from_dict <- function(df, dictionary) {

  # Add column if it does not exit
  df[setdiff(dictionary$old,names(df))] <- NA

  # Rename column names based on the dictionary
  names(df)[match(dictionary$old, names(df))] <- dictionary$new
  df %>%
    dplyr::select(dictionary$new)

}

#' Extract and match variable names using an external Excel dictionary
#'
#' @param df Input dataframe
#' @param xls_dict Excel file containing 2 columns ('old' and 'new') that map the names of the variables in the input dataframe and the names of the variables in the output dataframe
#' @return This function returns a dataframe.
#' @export
#' @import magrittr dplyr

match_from_xls_dict <- function(df, xls_dict) {

  dictionary <- readxl::read_excel(system.file(file.path('extdata', xls_dict), package = 'timci'))
  df <- match_from_dict(df, dictionary)

}
