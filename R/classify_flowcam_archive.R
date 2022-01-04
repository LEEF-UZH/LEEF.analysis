#' Classify algae_traits data.frame
#'
#' @param root_dir directory containes the archive. It contains the following directories:
#' - LEEF.archived.data
#' - LEEF.archived.data_segments
#' - LEEF.backend.data
#' - LEEF.backend.data_segments
#' @param timestamps `character` vector containing the timestamps to be classified
#' @param classifier_constant_name the classifier for temperature treatment **constant**
#' @param classifier_increasing_name the classifier for temperature treatment **increasing**
#' @param db_path if a valid path for an existing or new database `LEEF.RRD.sqlite` to which the
#'   classified data will be added.
#'
#' @return invisible `NULL`
#' @export
#'
#' @md
#' @examples
#'
#'
classify_flowcam_archive <- function(
  archive_dir = "/Users/rainerkrug/MountainDuck/LEEFSwift3",
  timestamps,
  algae_traits_name = "algae_traits_filtered.rds",
  classifier_constant_name,
  classifier_increasing_name,
  db_path
){

  dir.create( db_path, showWarnings = FALSE, recursive = TRUE)

  dir <- tempfile(pattern = "extracted.data_")

  # do the stuff -------------------------------------------------------

  return(
    lapply(
      timestamps,
      function(timestamp){
        datadir <- file.path(
          archive_dir,
          "LEEF.archived.data/LEEF/3.archived.data/extracted",
          paste0("LEEF.fast.flowcam.", as.character(timestamp))
        )
        message("###############################################")
        message("Classifying timestamp ", timestamp, "...")
        suppressMessages(
          {
            classified <- classify_flowcam_files(
              datadir = datadir,
              algae_traits_name = algae_traits_name,
              classifier_constant_name = classifier_constant_name,
              classifier_increasing_name = classifier_increasing_name,
              timestamp = timestamp
            )
          }
        )

        message("Saving timestamp ", timestamp, "...")
        path <- file.path(dir, "flowcam")
        dir.create(
          path,
          recursive = TRUE,
          showWarnings = FALSE)

        write.csv(
          x = classified$algae_traits,
          file = file.path(path, "algae_traits.csv")
        )
        write.csv(
          x = classified$algae_density,
          file = file.path(path, "algae_densities.csv")
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
        return(NULL)
      }
    )
  )
}
