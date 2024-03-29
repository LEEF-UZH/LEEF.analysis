% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/report_flowcytometer.R
\name{report_flowcytometer}
\alias{report_flowcytometer}
\title{Report for checking pipeline and sampling}
\usage{
report_flowcytometer(
  timestamp = "20230106",
  extracted_base_dir =
    "~/Duck/LEEFSwift3/LEEF_2.archived.data/LEEF/3.archived.data/extracted/",
  leef = "LEEF-2",
  output_dir = ".",
  format = "html",
  browse = TRUE
)
}
\arguments{
\item{timestamp}{one or more timestamp to be plotted. If \code{NULL}, all
timestamps available in \code{extracted_base_dir} will be plotted}

\item{extracted_base_dir}{directory in which the extracted data can be found
with filenames as in the archive}

\item{leef}{LeEEF experiment, either \code{"LEEF-1"} or \code{"LEEF-2"}}

\item{output_dir}{output directory of the fil=nal report}

\item{format}{the format of the report as a character vector of length 1.
Supported are at the moment: \code{html} (the default), \code{pdf} and
\code{word}.}

\item{browse}{if \code{TRUE} (the default) opr=en the report in the
appropriate program.}
}
\value{
the fully qualified file name to the created report.
}
\description{
Report for checking pipeline and sampling
}
