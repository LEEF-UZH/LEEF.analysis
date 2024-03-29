% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sanitychecks.R
\name{sanity_get_timestamp}
\alias{sanity_get_timestamp}
\title{Extract timestamp from \code{sample_metadata.yml} file}
\usage{
sanity_get_timestamp(sample_dir = ".")
}
\arguments{
\item{sample_dir}{root dir in which the folder \code{00.general.parameter} and \code{9.raw,data}
are located}
}
\value{
The extracted timestamp as a string
}
\description{
Extract timestamp from \code{sample_metadata.yml} file
}
