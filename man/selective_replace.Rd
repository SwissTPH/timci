% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clean_timci_data.R
\name{selective_replace}
\alias{selective_replace}
\title{Replace NA and 0 values in a data frame column with 1 if another corresponding column is 1.}
\usage{
selective_replace(df, col, cols)
}
\arguments{
\item{df}{A data frame.}

\item{col}{A character string indicating the name of the column to replace.}

\item{cols}{A character vector of column names to check for the existence of the `col` column.}
}
\value{
The input data frame with specified column values replaced.
}
\description{
This function replaces NA and 0 values in a specified column of a data frame with 1 if
another corresponding column with the same name and a "1" suffix has a value of 1.
}
