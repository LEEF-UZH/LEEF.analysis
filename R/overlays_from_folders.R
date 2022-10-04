#' Create overlays
#'
#'
#' This is a wrapper around the function `bemovi.LEEF::create_overlays_subtitle_directory()`
#' providing default values for LEEF.
#' Overlays will be created from a folder containing the video files and the bemovi config file
#' and the trajectory data file name.
#' @param traj_data_file file name of the file containing the trajectory data (usually from the Master file)
#' @param avi_url url containing the input `.avi` files
#' @param bemovi_extract_yml_file name of the `bemovi_extract.yml` config file
#' @param temp_overlay_folder directory where the subtitle files will be saved
#' @param overlay_folder directory where the overlay video will be saved
#' #' @param overlay_type option for the overlays. Overlays can either be shown as
#'   \code{"label"}, \code{"circle"} or \code{"both"}
#' @param label column to be used to label the particle. Default is
#'   \code{"trajectory"}, other useful might be \code{"species"}
#' @param ffmpeg command to run ffmpeg. The default is \code{par_ffmpeg()}. It
#'   can include a path.
#' @param font_size size of the font for the labels. Default: 24
#' @param circle_size size of the circle. Default: 120
#' @param crf integer value between 1 to 51, where 1 means lossless, 17 is
#'   nearly visually lossless, 51 is worst quality. Default value is 23
#' @param gamma  gamma correction. Value between 0.1 and 10. Default 2. see
#'   \url{https://ffmpeg.org/ffmpeg-filters.html#eq} for further info
#' @param mc_cores number of cores toi be used for parallel execution.
#'   Defaults to \code{par_mc.cores()}
#' @param overwrite if `TRUE`, existing overlays will be ov erwritten. Default is `FALSE`
#'
#' @return
#'
#' @md
#'
#' @importFrom pbapply  pblapply
#' @export
#'
#' @examples
overlays_from_folders <- function(
    traj_data_file,
    avi_url,
    bemovi_extract_yml_file,
    temp_overlay_folder,
    overlay_folder,
    overlay_type = "both",
    label = "species",
    ffmpeg = "ffmpeg",
    font_size = 24,
    circle_size = 120,
    crf = 23,
    gamma = 2,
    mc_cores = 1,
    overwrite = FALSE
){
  if (substr(avi_url, nchar(avi_url), nchar(avi_url)) != "/") {
    avi_url <- paste0(avi_url, "/")
  }

  traj_data <- readRDS(traj_data_file)
  avi_files <- unique(traj_data$file)


  if (overwrite){
    unlink(overlay_folder, recursive = TRUE)
    dir.create(overlay_folder)
    unlink(temp_overlay_folder, recursive = TRUE)
    dir.create(temp_overlay_folder)
  } else {
    ex_ov <- list.files(overlay_folder)
    if (length(ex_ov) > 0){
      ex_ov <- tools::file_path_sans_ext(ex_ov)
      avi_files <- avi_files[!(avi_files %in% ex_ov)]
    }
  }

  if (length(avi_files > 0)){
    avi_files <- paste0(avi_url, avi_files, ".avi")

    tmpdir <- tempfile()
    dir.create(tmpdir)
    on.exit(
      unlink(tmpdir, recursive = TRUE)
    )

    message("Downloading ", length(avi_files), " avi files...")
    pbapply::pblapply(
      avi_files,
      function(avi_file){
        download.file(
          url = avi_file,
          destfile = file.path(tmpdir, basename(avi_file)),
          quiet = TRUE
        )
      }
    )

    result <- pbmcapply::pbmclapply(
      basename(avi_files),
      function(avi_file) {
        message("Generating Overlay ", avi_file, "...")
        result <- -999
        try(
          {
            result <- suppressMessages(
              bemovi.LEEF::create_overlays_subtitle_single(
                traj_data = traj_data,
                avi_file = file.path(tmpdir, avi_file),
                crop = yaml::read_yaml(bemovi_extract_yml_file)$crop,
                temp_overlay_folder = temp_overlay_folder,
                overlay_folder = overlay_folder,
                overlay_type = overlay_type,
                label = label,
                ffmpeg = ffmpeg,
                font_size = font_size,
                circle_size = circle_size,
                crf = crf,
                gamma = gamma
              )
            )
          }
        )
        return(result)
      },
      mc.allow.recursive = FALSE,
      mc.cores = mc_cores
    )
    names(result) <- avi_files
  } else {
    result <- NULL
  }
  return(result)
}
