% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_view_toc.R
\name{make_view_toc}
\alias{make_view_toc}
\title{Create \code{toc} view wich contains selected fields from the toc__toc measurement}
\usage{
make_view_toc(
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
Create \code{toc} view wich contains selected fields from the toc__toc measurement
}
