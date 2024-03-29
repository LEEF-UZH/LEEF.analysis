% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/LEEF_2_make_view_density.R
\name{LEEF_2_make_view_density}
\alias{LEEF_2_make_view_density}
\title{Create \code{density} view wgich contains all density data from all measurements for LEEF-2}
\usage{
LEEF_2_make_view_density(
  db = getOption("RRDdb", "LEEF.RRD.sqlite"),
  start_date = "2022-11-07",
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
Create \code{density} view wgich contains all density data from all measurements for LEEF-2
}
