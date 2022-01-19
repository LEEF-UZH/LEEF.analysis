#' Classify bemovi data by using the archived data
#'
#' @param root_dir directory containing the archive. It contains the following directories:
#' - LEEF.archived.data
#' - LEEF.archived.data_segments
#' - LEEF.backend.data
#' - LEEF.backend.data_segments
#' @param magnification the magnification or the bemovi videos to be reclassified
#' @param bemovi_extract_name the name of the `.yml` containing the parameter for the nalysis
#' @param timestamps `character` vector containing the timestamps to be classified
#' @param classifier_constant_name the classifier for temperature treatment **constant**
#' @param classifier_increasing_name the classifier for temperature treatment **increasing**
#' @param output path to which the classified data will be saved as `rds`
#' @param mc.cores number of cores to be used. Defaults to 1
#' @return invisible `NULL`
#'
#' @importFrom  parallel mclapply
#' @export
#'
#' @md
#' @examples
#'
#'
classify_bemovi_archive <- function(
  archive_dir = "/Users/rainerkrug/MountainDuck/LEEFSwift3",
  magnification = 16,
  bemovi_extract_name = "bemovi_extract.yml",
  timestamps,
  classifier_constant_name,
  classifier_increasing_name,
  output,
  mc.cores = 1
){
  dir.create(
    output,
    showWarnings = FALSE,
    recursive = TRUE
  )

  dir <- tempfile(pattern = "extracted.data_")

  # do the stuff -------------------------------------------------------

  return(
    parallel::mclapply(
      timestamps,
      function(timestamp){
        datadir <- file.path(
          archive_dir,
          "LEEF.archived.data/LEEF/3.archived.data/extracted",
          paste0("LEEF.bemovi.mag.", as.character(magnification), ".bemovi.", as.character(timestamp))
        )
        message("###############################################")
        message("Classifying timestamp ", timestamp, "...")
        suppressMessages(
          {
            classified <- NULL
            try(
              expr = {
                classified <- classify_bemovi_files(
                  datadir = datadir,
                  bemovi_extract_name = bemovi_extract_name,
                  classifier_constant_name = classifier_constant_name,
                  classifier_increasing_name = classifier_increasing_name
                )
              }
            )
          }
        )

        if (!is.null(classified)) {
          message("Saving timestamp ", timestamp, "...")

          classified$morph_mvt <- cbind(timestamp = timestamp, classified$morph_mvt)
          classified$mean_density_per_ml <- cbind(timestamp = timestamp, classified$mean_density_per_ml)

          p <- yaml::read_yaml(file.path(datadir, bemovi_extract_name))

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
              tolower(
                gsub("\\.rds$", paste0(".", timestamp, ".rds"), p$mean_density))
            )
          )

          saveRDS(
            classified$trajectory_data,
            file = file.path(
              trajectory_path,
              tolower(
                paste0("trajectory.", gsub("\\.rds$", paste0(".", timestamp, ".rds"), p$mean_density))
              )
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
