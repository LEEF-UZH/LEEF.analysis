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
#'   classified data will be added. If `NULL` the data will not be added to a database.
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
  db_path = NULL
){

  dir.create( db_path, showWarnings = FALSE, recursive = TRUE)

  dir <- tempfile(pattern = "extracted.data_")

  # do the stuff -------------------------------------------------------

  return(
    lapply(
      timestamps,
      function(timestamp){
        algae_traits_path <- file.path(
          archive_dir,
          "LEEF.archived.data/LEEF/3.archived.data/extracted",
          paste0("LEEF.fast.flowcam.", as.character(timestamp)),
          algae_traits_name
        )
        message("###############################################")
        message("Classifying timestamp ", timestamp, "...")
        suppressMessages(
          classified <- classify_flowcam_files(
            datadir = "~/Desktop/2.extracted.data/flowcam/",
            algae_traits_name = algae_traits_name,
            classifier_constant_name = "~/Desktop/1.pre-processed.data/flowcam/svm_flowcam_classifiers_18c_december2021.rds",
            classifier_increasing_name = "~/Desktop/1.pre-processed.data/flowcam/svm_flowcam_classifiers_increasing_trained_at_18c_december2021.rds",
            dir = NULL
          )
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
        return(TRUE)
      }
    )
  )
}
