% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plots.R
\name{generate_day_bar_plot}
\alias{generate_day_bar_plot}
\title{Create a daily bar plot}
\usage{
generate_day_bar_plot(
  date_vec,
  date_min,
  date_max,
  ylim = NULL,
  ylbl = "Frequencies",
  rref = NULL,
  relative = FALSE,
  date_vec_ref = NULL
)
}
\arguments{
\item{date_vec}{Vector containing dates}

\item{date_min}{Start date of the plot}

\item{date_max}{End date of the plot}

\item{ylim}{Numeric vector of length 2 that contains the min and max values of the y-axis (optional, default = NULL)}

\item{ylbl}{String of the y-axis (optional, default = NULL)}

\item{rref}{reference value (optional, default = NULL)}

\item{relative}{Boolean for plotting relative/absolute plots (optional, default = FALSE)}

\item{date_vec_ref}{Vector containing reference dates (optional, default = NULL)}
}
\value{
This function returns a ggplot object which contains a bar plot of frequencies by dates
}
\description{
generate_day_bar_plot() creates an absolute or a relative bar plot.
}
