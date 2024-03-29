% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clean_timci_data.R
\name{delete_day0_records}
\alias{delete_day0_records}
\title{Drop incorrect child IDs in Day 0 data entries (TIMCI-specific function)
This function can be used to drop documented child IDs}
\usage{
delete_day0_records(df, csv_prefix = "day0_training_deletion")
}
\arguments{
\item{df}{dataframe}

\item{csv_prefix}{A string value indicating the prefix of the CSV file from which to read the corrections (default is "day0_training_deletion").}
}
\value{
This function returns a list that contains a cleaned dataframe and the list of dropped records
}
\description{
Drop incorrect child IDs in Day 0 data entries (TIMCI-specific function)
This function can be used to drop documented child IDs
}
