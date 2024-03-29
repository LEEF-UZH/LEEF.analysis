#' Create overlays
#'
#' @param archive_dir
#' @param classification_dir
#' @param magnification the magnification of the bemovi method
#' @param yml_postfix
#' @param timestamp the timestamp of the sampling day
#' @param overlay_folder
#' @param overlay_type
#' @param label column to be used to label the particle. Default \code{ffmpeg}
#' @param ffmpeg command to run ffmpeg. It can include a path. Default
#' @param font_size
#' @param circle_size
#' @param crf integer value between 1 to 51, where 1 means lossless, 17 is
#'   nearly visually lossless, 51 is worst quality. Default value is 23
#' @param gamma increase video dynamic range. Value between 0.1 and 10. Default \code{"pecies"}
#'   2. see \url{https://ffmpeg.org/ffmpeg-filters.html#eq} for further info
#' @param mc_cores number of cores to use. Default \code{1}
#'
#' @return
#'
#' @export
#'
#' @examples
overlays_from_reclassification <- function(
  archive_dir = "/Users/rainerkrug/Duck/LEEFSwift3",
  classification_dir = "~/RRD.Reclassification_1",
  magnification = 16,
  yml_postfix = NULL,
  timestamp,
  overlay_folder = NULL,
  overlay_type = "both",
  label = "species",
  ffmpeg = "ffmpeg",
  font_size = 24,
  circle_size = 120,
  crf = 23,
  gamma = 2,
  mc_cores = 1
){

  avi_file_dir <- file.path(
    archive_dir,
    "LEEF.archived.data/LEEF/3.archived.data/pre_processed",
    paste0("LEEF.bemovi.mag.", as.character(magnification), ".bemovi.", as.character(timestamp))
  )

  if (is.null(yml_postfix)) {
    bemovi_extract_name <- file.path(
      avi_file_dir,
      paste0("bemovi_extract.mag.", as.character(magnification), ".yml")
    )
    p <- yaml::read_yaml(bemovi_extract_name)
    traj_data <- file.path(
      classification_dir,
      paste0("bemovi_mag_", as.character(magnification)),
      "trajectories",
      paste0(gsub("\\.rds$", "", p$trajectory), ".mean_density_per_ml.", as.character(timestamp), ".rds")
    )
  } else {
    bemovi_extract_name <- file.path(
      archive_dir,
      paste0("bemovi_extract.mag.", as.character(magnification), ".", yml_postfix, ".yml")
    )
    p <- yaml::read_yaml(bemovi_extract_name)
    traj_data <- file.path(
      classification_dir,
      paste0("bemovi_mag_", as.character(magnification)),
      "trajectories",
      paste0(gsub("\\.rds$", "", p$trajectory), ".mean_density_per_ml_", yml_postfix, ".", as.character(timestamp), ".rds")
    )
  }


  if (is.null(overlay_folder)){
    overlay_folder <- file.path(
      classification_dir,
      paste0("bemovi_mag_", as.character(magnification)),
      "overlays",
      gsub("\\.rds$", "", basename(traj_data))
    )
  } else {
    overlay_folder <- file.path(
      overlay_folder,
      gsub("\\.rds$", "", basename(traj_data))
    )
  }

  bemovi.LEEF::create_overlays_subtitle_directory(
    traj_data = readRDS(traj_data),
    avi_file_dir = avi_file_dir,
    crop = p$crop,
    temp_overlay_folder = tempfile(),
    overlay_folder = overlay_folder,
    overlay_type = overlay_type,
    label = label,
    ffmpeg = ffmpeg,
    font_size = font_size,
    circle_size = circle_size,
    crf = crf,
    gamma = gamma,
    mc_cores = mc_cores
  )
}
