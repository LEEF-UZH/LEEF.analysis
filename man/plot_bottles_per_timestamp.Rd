% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_bottles_per_timestamp_per_timestamp.R
\name{plot_bottles_per_timestamp}
\alias{plot_bottles_per_timestamp}
\title{Plot diagnostic plot to check bottles per timestamp}
\usage{
plot_bottles_per_timestamp(
  db = getOption("RRDdb", "LEEF.RRD.sqlite"),
  lastDays = 7
)
}
\arguments{
\item{db}{fully qualified path to the sqlite database. Default, read from option \code{RRDdb}.
If not set, defaults to option \code{RRDdb}; if this is not set, defaults to \code{LEEF.RRD.sqlite}}

\item{lastDays}{last days for which to plot}
}
\value{
\code{ggplot} object of the plot
}
\description{
Plot diagnostic plot to check bottles per timestamp
}
