% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_view_conductivity.R
\name{make_view_conductivity}
\alias{make_view_conductivity}
\title{Create \code{o2} view wgich contains selected fields from the conductivity measurement}
\usage{
make_view_conductivity(
  db = getOption("RRDdb", "LEEF.RRD.sqlite"),
  start_date = "2021-09-20",
  overwrite = FALSE
)
}
\arguments{
\item{db}{fully qualified path to the sqlite database. Default, read from option \code{RRDdb}.
If not set, defaults to option \code{RRDdb}; if this is not set, defaults to \code{LEEF.RRD.sqlite}}

\item{start_date}{the start date of the experiment}

\item{overwrite}{if \code{TRUE}, overwrite existing view}
}
\value{
the result of the execution of the ecreatiuon of the view.
}
\description{
Create \code{o2} view wgich contains selected fields from the conductivity measurement
}
