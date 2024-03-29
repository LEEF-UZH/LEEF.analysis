% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/LEEF_1_biomass_bemovi_25_non_cropped.R
\name{LEEF_1_biomass_bemovi_25_non_cropped}
\alias{LEEF_1_biomass_bemovi_25_non_cropped}
\title{LEEF-1 - Add biomass to traits}
\usage{
LEEF_1_biomass_bemovi_25_non_cropped(ciliate_traits_25, ciliate_density_25)
}
\arguments{
\item{ciliate_traits_25}{traits as read from file \code{morph_mvt_TIMESTAMP.rds}}

\item{ciliate_density_25}{density as read from file \code{mean_density_per_ml_TIMESTAMP.rds}}
}
\value{
list containing two objects, \code{traits} containing complete traits file
  as the argument \code{algai_traits} day includinc biomass column and \code{biomasses}
  per timestamp, bottle and species per milliliter.
}
\description{
LEEF-1 - Add biomass to traits
}
