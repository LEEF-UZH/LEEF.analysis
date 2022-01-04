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
#' @param db_path if a valid path for an existing or new database `LEEF.RRD.sqlite` to which the
#'   classified data will be added.
#' @param trajectory_path if `NULL`, the trajectory files will be discarded. Otherwise, they
#'   will be saved in the path specified in a subdirectory called `trajectories` and the files
#'   named `trajectory.mag_MAGNIFICATION.BEMOVI_EXTRACT_NAME.TIMESTAMP.rds`
#' @return invisible `NULL`
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
  db_path,
  trajectory_path = NULL
){

  dir.create(
    db_path,
    showWarnings = FALSE,
    recursive = TRUE
  )
  if (!is.null(trajectory_path)) {
    trajectory_path <- file.path(trajectory_path, "trajectories")
    dir.create(
      trajectory_path,
      showWarnings = FALSE,
      recursive = TRUE
    )
  }

  dir <- tempfile(pattern = "extracted.data_")

  # do the stuff -------------------------------------------------------

  return(
    lapply(
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
            classified <- classify_bemovi_files(
              datadir = datadir,
              bemovi_extract_name = bemovi_extract_name,
              classifier_constant_name = classifier_constant_name,
              classifier_increasing_name = classifier_increasing_name
            )
          }
        )

        message("Saving timestamp ", timestamp, "...")

        p <- yaml::read_yaml(file.path(datadir, bemovi_extract_name))

        path <- file.path(dir, "bemovi")

        dir.create(
          path,
          recursive = TRUE,
          showWarnings = FALSE
        )
        dir.create(
          file.path(path, p$merged.data.folder),
          recursive = TRUE,
          showWarnings = FALSE
        )

        write.csv(
          x = classified$morph_mvt,
          file = file.path(path, gsub("\\.rds$", ".csv", p$morph_mvt)),
          row.names = FALSE
        )

        tfn <- NULL
        if (!is.null(trajectory_path)) {
          tfn <- paste0(
            "trajectory.mag_", magnification, ".",
            gsub("\\.yml$", "", bemovi_extract_name), ".",
            timestamp, ".",
            "rds"
          )
          tfn <- file.path(
            trajectory_path,
            tfn
          )
          saveRDS(
            classified$trajectory_data,
            file = tfn
          )
        }

        write.csv(
          x = classified$mean_density_per_ml,
          file = file.path(path, gsub("\\.rds$", ".csv", p$mean_density)),
          row.names = FALSE
        )

        message("Adding timestamp ", timestamp, " to db...")
        LEEF.backend.sqlite::additor_sqlite_multiple_db(
          input = file.path(dir),
          output = db_path
        )

        message("Deleting temporary data ", timestamp, "...")
        unlink(path, recursive = TRUE, force = TRUE)

        message("Done")
        message("###############################################")
        return(tfn)
      }
    )
  )
}
