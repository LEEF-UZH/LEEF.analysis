#' Classify bemovi data by using the archived data
#'
#' @param directory with extracted data
#' @param magnification the magnification or the bemovi videos to be reclassified
#' @param bemovi_extract_name the name of the `.yml` containing the parameter for
#'   the analysis in the directory of the data. The directory depends on the
#'   `root_dir`, `magnification` and `timestamp`
#' @param timestamps `character` vector containing the timestamps to be classified
#' @param classifier classifier for bemovi 16 including path
#' @param output path to which the classified data will be saved as `rds`
#' @param exclude_videos file names of videos to exclude. If \code{NULL}, all will be used.
#' @param mc.cores number of cores to be used. Defaults to 1
#'
#' @return invisible `NULL`
#'
#' @importFrom  pbmcapply  pbmclapply
#' @importFrom yaml read_yaml write_yaml
#' @export
#'
#' @md
#' @examples
#'
#'
LEEF_2_classify_bemovi_archive <- function(
  extracted_dir = NULL,
  magnification = 16,
  bemovi_extract_name = NULL,
  timestamps,
  classifier = NULL,
  output,
  exclude_videos = NULL,
  mc.cores = 1
){
  dir.create(
    output,
    showWarnings = FALSE,
    recursive = TRUE
  )

  dir <- tempfile(pattern = "extracted.data_")
  dir.create(dir, recursive = TRUE, showWarnings = TRUE)

  # do the stuff -------------------------------------------------------

  return(
    pbmcapply::pbmclapply(
    # parallel::mclapply(
      timestamps,
      function(timestamp){
        datadir <- file.path(
          extracted_dir,
          paste0("LEEF.bemovi.mag.", as.character(magnification), ".bemovi.", as.character(timestamp))
        )
        message("###############################################")
        message("Classifying timestamp ", timestamp, "...")

        # HACK I do not trust this part...
        # if (!is.null(species_tracked)) {
        #   p <- yaml::read_yaml(beyml)
        #   p$species_tracked <- species_tracked

        #   beyml <- file.path(dir, bemovi_extract_name)
        #   yaml::write_yaml(p, beyml)
        # }

        suppressMessages(
          {
            classified <- NULL
            try(
              expr = {
                classified <- LEEF_2_classify_bemovi_files(
                  datadir = datadir,
                  bemovi_extract_name = bemovi_extract_name,
                  classifier = classifier,
                  exclude_videos
                )
              }
            )
          }
        )

        if (!is.null(classified)) {
          message("Saving timestamp ", timestamp, "...")

          classified$mean_density_per_ml <- cbind(timestamp = timestamp, classified$mean_density_per_ml)

          p <- yaml::read_yaml(bemovi_extract_name)

          trajectory_path <- file.path(output, "trajectories")

          dir.create(
            output,
            recursive = TRUE,
            showWarnings = FALSE
          )
          dir.create(
            trajectory_path,
            recursive = TRUE,
            showWarnings = FALSE
          )

          saveRDS(
            classified$morph_mvt,
            file = file.path(
              output,
              tolower(gsub("\\.rds$", paste0(".", timestamp, ".rds"), p$morph_mvt))
              )
          )

          saveRDS(
            classified$mean_density_per_ml,
            file = file.path(
              output,
              tolower(gsub("\\.rds$", paste0(".", timestamp, ".rds"), p$mean_density))
            )
          )

          saveRDS(
            classified$trajectory_data,
            file = file.path(
              trajectory_path,
              tolower(gsub("\\.rds$", paste0(".", timestamp, ".rds"), p$master))
            )
          )

        } else {
          message("ERROR in classifying timestamp ", timestamp)
        }


        message("Done")
        message("###############################################")
        invisible(NULL)
      },
    mc.preschedule = FALSE,
    mc.cores = mc.cores
    )
  )
}
