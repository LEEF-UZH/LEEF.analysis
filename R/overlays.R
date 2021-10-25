#' Create
#'
#' @param timestamp the timestamp of the sampling day
#' @param magnification the magnification of the bemovi method
#' @param cropped if \code{TRUE}, the cropped measurement should be used
#' @param label column to be used to label the particle. Default
#'   \code{"pecies"}.
#' @param overlay.type Overlays can either be shown as "label", "circle" or
#'   "both". Default: \code{"both"}
#' @param hq if \code{TRUE}, overlays are in same quality as the original video,
#'   if \code{FALSE} they will be downsampled. Default \code{FALSE}
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
  hq = FALSE,
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
    hq = hq,
    mc.cores = mc.cores
  )
}
