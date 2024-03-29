% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/LEEF_2_add_experiment_tables.R
\name{LEEF_2_add_experiment_tables}
\alias{LEEF_2_add_experiment_tables}
\title{Add \code{composition} and \code{experimetal_design} table to db.}
\usage{
LEEF_2_add_experiment_tables(
  db = getOption("RRDdb", "LEEF-2.RRD.sqlite"),
  composition = NULL,
  experimetal_design = NULL,
  stressor_levels = NULL,
  immigration_schedule = NULL,
  overwrite = FALSE
)
}
\arguments{
\item{db}{fully qualified path to the sqlite database. Default, read from option \code{RRDdb}.
If not set, defaults to option \code{RRDdb}; if this is not set, defaults to \code{LEEF.RRD.sqlite}}

\item{composition}{fully qualified path to the \code{csv} file with the composition data}

\item{stressor_levels}{fully qualified path to the \code{csv} file with the stressor levels for each sampling date}

\item{immigration_schedule}{fully qualified path to the \code{csv} file with the immigration event dates}

\item{overwrite}{if \code{TRUE}, the existing tables will be overwritten. There is actually no need for this.}
}
\description{
Add \code{composition} and \code{experimetal_design} table to db.
}
