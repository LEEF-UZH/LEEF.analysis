#' Classify algae_traits data.frame
#'
#' @param datadir `character` vector containing the root directory for all files
#' @param algae_traits_name `character` vector containing the name of the algae
#'   traits file without path
#' @param classifier_constant_name `character` vector of name of the classifier
#'   for temperature treatment **constant** including path
#' @param classifier_increasing_name `character` vector of name of the
#'   classifier for temperature treatment **increasing** including path
#' @param timestamp timestamp to be used to stamp the classified data
#' @param species_tracked names of the species tracked as a character vector.
#'   If `NULL` it will be read from the original configuration file in the `datadir`.
#' @param bottle if not 'NULL' (default) only classify this bottle. Needs to be a single bottle!
#'
#' @return `list` containing two objects:
#'       - `algae_traits` including species
#'       - `algae_densities` densities of the different particles identified
#'
#' @importFrom yaml read_yaml
#'
#' @export
#'
#' @md
#' @examples
#'
#'
classify_flowcam_files <- function(
  datadir,
  algae_traits_name = "algae_traits_filtered.rds",
  classifier_constant_name,
  classifier_increasing_name,
  timestamp = "55555555",
  species_tracked = NULL,
  bottle = NULL
){

  p <- yaml::read_yaml(file.path(datadir, "flowcam.yml"))

  if (is.null(species_tracked)) {
    species_tracked <- p$species_tracked
  }


# Filter for bottle -------------------------------------------------------


  if (!is.null(bottle)){
    tmp_algae_traits <- tempfile(
      pattern = paste0("algae_traits_bottle_", bottle, "_"),
      fileext = ".rds"
    )
    dat <- readRDS(file.path(datadir, algae_traits_name))
    saveRDS(
      dat[dat$bottle == bottle,],
      tmp_algae_traits
    )
  } else {
    tmp_algae_traits <- file.path(datadir, algae_traits_name)
  }

# The classification ------------------------------------------------------


  classified <- LEEF.measurement.flowcam::classify(
    algae_traits = readRDS(tmp_algae_traits),
    classifiers_constant = readRDS(classifier_constant_name),
    classifiers_increasing = readRDS(classifier_increasing_name),
    composition = read.csv(file.path(datadir, "compositions.csv")),
    exp_design = read.csv(file.path(datadir, "experimental_design.csv")),
    species_tracked = species_tracked,
    timestamp = timestamp
  )


# Return the classifications --------------------------------------------------

  return(classified)
}
