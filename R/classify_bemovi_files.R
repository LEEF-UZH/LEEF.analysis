#' Classify algae_traits data.frame
#'
#' @param datadir `character` vector containing the root directory for all files
#' @param bemovi_extract_name `character` vector containing the name of the
#'   bemovi config file including path.
#' @param classifier_constant_name `character` vector of name of the classifier
#'   for temperature treatment **constant** including path
#' @param classifier_increasing_name `character` vector of name of the
#'   classifier for temperature treatment **increasing** including path
#' @param exclude_videos file names of videos to exclude. If \code{NULL}, all will be used.
#'
#' @return `list` containing three objects:
#'       - `mean_density_per_ml`
#'       - `morph_mvt`
#'       - `trajectory_data`
#'
#' @importFrom yaml read_yaml
#' @importFrom utils read.table
#'
#' @export
#'
#' @md
#' @examples
#'
#'
classify_bemovi_files <- function(
  datadir,
  bemovi_extract_name = NULL,
  classifier_constant_name,
  classifier_increasing_name2,
  exclude_videos = NULL
){

  p <- yaml::read_yaml(bemovi_extract_name)


# The classification ------------------------------------------------------

  morph_mvt <- readRDS(file.path(datadir, p$merged.data.folder, p$morph_mvt))
  morph_mvt <- morph_mvt[, grep("_prob|species", names(morph_mvt), invert = TRUE)]

  traj <- readRDS(file.path(datadir, p$merged.data.folder, p$master))

  if (!is.null(exclude_videos)){
    morph_mvt <- morph_mvt[!(morph_mvt$file %in% exclude_videos),]
    traj <- traj[!(morph_mvt$file %in% exclude_videos),]
  }
  classified <- LEEF.measurement.bemovi::classify(
    bemovi_extract = bemovi_extract_name,
    morph_mvt = morph_mvt,
    trajectory_data = traj,
    classifiers_constant = readRDS(classifier_constant_name),
    classifiers_increasing = readRDS(classifier_increasing_name),
    video_description_file = as.data.frame(read.table(file.path(datadir, p$video.description.folder, p$video.description.file), sep = "\t", header = TRUE, stringsAsFactors = FALSE)),
    composition = utils::read.csv(file.path(datadir, "compositions.csv"))
  )


# Return the classifications --------------------------------------------------


  return(classified)
}
