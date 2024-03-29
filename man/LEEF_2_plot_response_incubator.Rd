% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/LEEF_2_plot_response_incubator.R
\name{LEEF_2_plot_response_incubator}
\alias{LEEF_2_plot_response_incubator}
\title{Plot diagnostic plot to check number of species per timestamp}
\usage{
LEEF_2_plot_response_incubator(
  db = getOption("RRDdb", "LEEF.RRD.sqlite"),
  arrow = FALSE
)
}
\arguments{
\item{db}{fully qualified path to the sqlite database. Default, read from option \code{RRDdb}.
If not set, defaults to option \code{RRDdb}; if this is not set, defaults to \code{LEEF.RRD.sqlite}}

\item{arrow}{if \code{TRUE} read data from arrow instead of sqlite database}
}
\value{
\code{ggplot} object of the plot
}
\description{
Plot diagnostic plot to check number of species per timestamp
}
