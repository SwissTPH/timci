% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{export_df2csvxlsx}
\alias{export_df2csvxlsx}
\title{Write dataframe to both CSV and Excel spreadsheet formats, with a filename consisting of a prefix and a date stamp}
\usage{
export_df2csvxlsx(df, dirname, prefix)
}
\arguments{
\item{df}{dataframe.}

\item{dirname}{directory where the RDS file will be created.}

\item{prefix}{filename prefix}
}
\value{
This function return a list that contains the creation timestamps of the Excel and the CSV files
}
\description{
Write dataframe to both CSV and Excel spreadsheet formats, with a filename consisting of a prefix and a date stamp
}
