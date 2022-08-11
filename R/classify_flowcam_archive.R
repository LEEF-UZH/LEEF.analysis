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
#' @param species_tracked names of the species tracked as a character vector.
#'   If `NULL` it will be read from the original configuration file in the `datadir`.
#' @param output path to which the classified data will be saved as `rds`
#' @param mc.cores number of cores to be used. Defaults to 1
#'
#' @return invisible `NULL`
#' @export
#'
#' @md
#'
classify_flowcam_archive <- function(
  archive_dir = "/Users/rainerkrug/MountainDuck/LEEFSwift3",
  timestamps,
  algae_traits_name = "algae_traits_filtered.rds",
  classifier_constant_name,
  classifier_increasing_name,
  species_tracked = NULL,
  output,
  mc.cores = 1

){

  dir.create( output, showWarnings = FALSE, recursive = TRUE)

  dir <- tempfile(pattern = "extracted.data_")

  # do the stuff -------------------------------------------------------

  return(
    parallel::mclapply(
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
            classified <- NULL
            try(
              expr = {
                classified <- classify_flowcam_files(
                  datadir = datadir,
                  algae_traits_name = algae_traits_name,
                  classifier_constant_name = classifier_constant_name,
                  classifier_increasing_name = classifier_increasing_name,
                  timestamp = timestamp,
                  species_tracked = species_tracked
                )
              }
            )
          }
        )

        if (!is.null(classified)) {
          message("Saving timestamp ", timestamp, "...")

          saveRDS(
            classified$algae_traits,
            file = file.path(output, paste0("algae_traits.", timestamp, ".rds"))
          )
          saveRDS(
            classified$algae_density,
            file = file.path(output, paste0("algae_density.", timestamp, ".rds"))
          )
        }

        message("Done")
        message("###############################################")
        return(NULL)
      },
      mc.preschedule = FALSE,
      mc.cores = mc.cores
    )
  )
}
