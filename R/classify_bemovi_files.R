#' Classify algae_traits data.frame
#'
#' @param datadir `character` vector containing the root directory for all files
#' @param algae_traits_name `character` vector containing the name of the algae
#'   traits file without path
#' @param classifier_constant_name `character` vector of name of the classifier
#'   for temperature treatment **constant** including path
#' @param classifier_increasing_name `character` vector of name of the
#'   classifier for temperature treatment **increasing** including path
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
classify_bemovi_files <- function(
  datadir,
  bemovi_extract_name = "bemovi_extract.yml",
  classifier_constant_name,
  classifier_increasing_name
){

  p <- yaml::read_yaml(file.path(datadir, bemovi_extract_name))


# The classification ------------------------------------------------------

  morph_mvt <- readRDS(file.path(datadir, p$merged.data.folder, p$morph_mvt))
  morph_mvt <- morph_mvt[, grep("_prob|species", names(morph_mvt), invert = TRUE)]

  classified <- LEEF.measurement.bemovi::classify(
    bemovi_extract = file.path(datadir, bemovi_extract_name),
    morph_mvt = morph_mvt,
    trajectory_data = readRDS(file.path(datadir, p$merged.data.folder, p$master)),
    classifiers_constant = readRDS(classifier_constant_name),
    classifiers_increasing = readRDS(classifier_increasing_name),
    video_description_file = as.data.frame(read.table(file.path(datadir, p$video.description.folder, p$video.description.file), sep = "\t", header = TRUE, stringsAsFactors = FALSE)),
    composition = utils::read.csv(file.path(datadir, "compositions.csv"))
  )


# Return the classifications --------------------------------------------------


  return(classified)
}
