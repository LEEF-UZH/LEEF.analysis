#' Classify algae_traits data.frame
#'
#' @param datadir `character` vector containing the root directory for all files
#' @param algae_traits_name `character` vector containing the name of the algae
#'   traits file without path
#' @param classifier_name `character` vector of name of the classifier
#'   for flowcam
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
LEEF_2_classify_flowcam_files <- function(
  datadir,
  algae_traits_name = "algae_traits_filtered.rds",
  classifier_name = "svm_flowcam_classifiers.rds",
  timestamp = "55555555",
  species_tracked = NULL,
  bottle = NULL
){

  p <- yaml::read_yaml(file.path(datadir, "flowcam.yml"))

  if (is.null(species_tracked)) {
    species_tracked <- p$species_tracked
  }


# Filter for bottle -------------------------------------------------------


  dat <- readRDS(file.path(datadir, algae_traits_name))
  dat <- data.frame(dat)
  if (!is.null(bottle)){
    # tmp_algae_traits <- tempfile(
    #   pattern = paste0("algae_traits_bottle_", bottle, "_"),
    #   fileext = ".rds"
    # )
    dat <- dat[dat$bottle == bottle, ]
  }

# This is needed for the v1.7.1 of the parameter --------------------------

  names(dat) <- tolower(names(dat))
  names(dat)[which(names(dat) == "date_flowcam")] <- "Date_Flowcam"

# The classification ------------------------------------------------------

  classified <- LEEF.2.measurement.flowcam::classify_LEEF_2(
    algae_traits = dat,
    classifiers = readRDS(classifier_name),
    exp_design = read.csv(file.path(datadir, "experimental_design.csv")),
    species_tracked = species_tracked,
    timestamp = timestamp
  )


# Return the classifications --------------------------------------------------

  return(classified)
}
