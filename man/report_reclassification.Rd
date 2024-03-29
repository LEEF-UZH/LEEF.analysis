% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/report_reclassification.R
\name{report_reclassification}
\alias{report_reclassification}
\title{Do the reclassification and create a report}
\usage{
report_reclassification(
  reclassification_report = system.file(leef, "Reclassification_20220105.Rmd", package =
    "LEEF.analysis"),
  leef = "LEEF-1",
  output_dir = normalizePath(file.path("~", "Reclassification_20220105")),
  output_name = "Reclassification_20220105",
  format = "html"
)
}
\arguments{
\item{reclassification_report}{The name of the reclassification report. The default points to the first one.
Included reclassification reports are:
\itemize{
\item \code{system.file(Reclassification_2022.01.10.Rmd, package = "LEEF.analysis")}
\item \code{system.file(Reclassification_20220105_20220221_flowcam.Rmd, package = "LEEF.analysis")}
\item \code{system.file(Reclassification_20220506.Rmd, package = "LEEF.analysis")}
}}

\item{leef}{LEEF study - allowed are \verb{"LEEF-1} and \code{"LEEF-2"}}

\item{output_dir}{directory of the output of the reclassification and the report}

\item{output_name}{The name of the final report, without extension.}

\item{format}{the format of the report as a character vector of length 1.
Supported are at the moment: \code{html} (the default), \code{pdf} and \code{word}.}
}
\value{
the fully qualified file name to the created report.
}
\description{
The report needs to adapted to new classifications.
}
