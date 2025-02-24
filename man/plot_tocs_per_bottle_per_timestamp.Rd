% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_tocs_per_bottle.R
\name{plot_tocs_per_bottle_per_timestamp}
\alias{plot_tocs_per_bottle_per_timestamp}
\title{Plot diagnostic plot to check number of species per timestamp}
\usage{
plot_tocs_per_bottle_per_timestamp(
  db = getOption("RRDdb", "LEEF.RRD.sqlite"),
  type = c("IC", "TC", "TN", "TOC")
)
}
\arguments{
\item{db}{fully qualified path to the sqlite database. Default, read from option \code{RRDdb}.
If not set, defaults to option \code{RRDdb}; if this is not set, defaults to \code{LEEF.RRD.sqlite}}

\item{type}{the type of the measurement which should be displayed. A vector with the types.
Possible values are: "TOC", "TN", "IC", "TN", "".}
}
\value{
\code{ggplot} object of the plot
}
\description{
Plot diagnostic plot to check number of species per timestamp
}
