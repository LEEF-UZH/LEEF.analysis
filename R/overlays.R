#' Create
#'
#' @param timestamp the timestamp of the sampling day
#' @param magnification the magnification of the bemovi method
#' @param cropped if \code{TRUE}, the cropped measurement should be used
#' @param label column to be used to label the particle. Default
#'   \code{"pecies"}.
#' @param overlay.type Overlays can either be shown as "label", "circle" or
#'   "both". Default: \code{"both"}
#' @param crf integer value between 1 to 51, where 1 means lossless, 17 is nearly visually lossless,
#'    51 is worst quality. Default value is 23
#' @param gamma increase video dynamic range. Value between 0.1 and 10. Default 2. see \url{https://ffmpeg.org/ffmpeg-filters.html#eq} for further info
#' @param ffmpeg command to run ffmpeg. It can include a path. Default
#'   \code{ffmpeg}
#' @param mc.cores number of cores to be used for parallel execution. Defaults
#'   to 1
#' @param from_current_to_archive_dir path from the current directory to the
#'   archived data can be found and the folders \code{extracted} and
#'   \code{pre-processed}
#'
#' @return
#' @export
#'
#' @examples
overlays <- function(
  timestamp = 20210920,
  magnification = 25,
  cropped = FALSE,
  label = "species",
  overlay.type = "both",
  crf = 17,
  gamma = 2,
  ffmpeg = "ffmpeg",
  from_current_to_archive_dir = "./../../../../../Duck/LEEFSwift3/LEEF.archived.data/LEEF/3.archived.data",
  mc.cores = 7
){
  output.dir <- "."
  mdf <- file.path(
    from_current_to_archive_dir, "/extracted",
    paste0("LEEF.bemovi.mag.", magnification, ".bemovi.", timestamp),
    "5 - merged data",
    "."
  )
  mf <- ifelse(
    cropped,
    "Master_cropped.rds",
    "Master.rds"
  )
  avivf <- file.path(
    from_current_to_archive_dir, "/pre_processed",
    paste0("LEEF.bemovi.mag.", magnification, ".bemovi.", timestamp),
    "."
  )
  fbn <- paste(
    "LEEF.bemovi.mag",
    magnification,
    ifelse(
      cropped,
      "cropped.bemovi",
      "bemovi"
    ),
    timestamp,
    sep = "."
  )

  conf <- paste(
    "bemovi_extract",
    "mag",
    magnification,
    ifelse(
      cropped,
      "cropped.yml",
      "yml"
    ),
    sep = "."
  )

  bemovi.LEEF::load_parameter(file.path(avivf, conf))

  bemovi.LEEF::create_overlays_subtitle(
    to.data = output.dir,
    merged.data.folder = mdf,
    raw.video.folder = avivf,
    temp.overlay.folder = paste(fbn, "overlay.tmp", sep = "."),
    overlay.folder = paste(fbn, "overlay", sep = "."),
    label = label,
    ffmpeg = ffmpeg,
    master = mf,
    overlay.type = overlay.type,
    crf = crf,
    gamma = gamma,
    mc.cores = mc.cores
  )
}
