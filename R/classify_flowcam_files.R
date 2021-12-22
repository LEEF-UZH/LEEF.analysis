#' Classify algae_traits data.frame
#'
#' @param datadir `character` vector containing the root directory for all files
#' @param algae_traits_name `character` vector containing the name of the algae
#'   traits file without path
#' @param classifier_constant_name `character` vector of name of the classifier
#'   for temperature treatment **constant** including path
#' @param classifier_increasing_name `character` vector of name of the
#'   classifier for temperature treatment **increasing** including path
#' @param directrory for the results. Default is a temporary directory. If
#'   another string, the directory where the results will be saved. If `NULL`
#'   the objects are returned,
#' @return depends on the value of `directory`.
#'    - `dir == NULL`: list` containing two objects:
#'       - `algae_traits` including species
#'       - `algae_densities` densities of the different particles identified
#'    - `dir` is a string or default:
#'       - path where the objects are stored
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
  dir = tempfile(pattern = "extracted.data_")
){

  p <- yaml::read_yaml(file.path(datadir, "flowcam.yml"))


# The classification ------------------------------------------------------


  classified <- LEEF.measurement.flowcam::classify(
    algae_traits = readRDS(file.path(datadir, algae_traits_name)),
    classifiers_constant = readRDS(classifier_constant_name),
    classifiers_increasing = readRDS(classifier_increasing_name),
    composition = read.csv(file.path(datadir, "compositions.csv")),
    exp_design = read.csv(file.path(datadir, "experimental_design.csv")),
    species_tracked = p$species_tracked
  )


# Save to results or return them --------------------------------------------------


  if (is.null(dir)) {
    return(classified)
  } else {
    path <- file.path(dir, "flowcam" )
    dir.create(
      path,
      recursive = TRUE,
      showWarnings = FALSE
    )
    write.csv(
      x = classified$algae_traits,
      file = file.path(path, "algae_traits'csv"),
      row.names = FALSE
    )
    write.csv(
      x = classified$algae_densities,
      file = file.path(path, "algae_density.csv"),
      row.names = FALSE
    )
    return(dir)
  }
}
