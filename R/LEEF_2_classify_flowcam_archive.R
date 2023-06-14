#' Classify algae_traits data.frame
#'
#' @param extracted_dir path to the extracted data
#' @param timestamps `character` vector containing the timestamps to be classified
#' @param algae_traits_name
#' @param classifier classifier for flowcytometer
#' @param species_tracked names of the species tracked as a character vector. If `NULL` it will be read from the original configuration file in the `datadir`.
#' @param output path to which the classified data will be saved as `rds`
#' @param mc.cores number of cores to be used. Defaults to 1
#' @param bottle if not 'NULL' (default) only classify this bottle. Needs to be a single bottle!
#'
#' @return invisible `NULL`
#'
#' @importFrom pbmcapply pbmclapply
#' @export
#'
#' @md
#' @examples
#'

LEEF_2_classify_flowcam_archive <- function(
  extracted_dir = NULL,
  timestamps,
  algae_traits_name = "algae_traits_filtered.rds",
  classifier = NULL,
  species_tracked = NULL,
  output,
  mc.cores = 1,
  bottle = NULL
){

  dir.create( output, showWarnings = FALSE, recursive = TRUE)

  dir <- tempfile(pattern = "extracted.data_")

  # do the stuff -------------------------------------------------------

  return(
    pbmcapply::pbmclapply(
    # lapply(
      timestamps,
      function(timestamp){
        datadir <- file.path(
          extracted_dir,
          paste0("LEEF.flowcam.flowcam.", as.character(timestamp))
        )
        message("###############################################")
        message("Classifying timestamp ", timestamp, "...")
        # suppressMessages(
        #   {
            classified <- NULL
            try(
              expr = {
                classified <- LEEF_2_classify_flowcam_files(
                  datadir = datadir,
                  algae_traits_name = algae_traits_name,
                  classifier = classifier,
                  timestamp = timestamp,
                  species_tracked = species_tracked,
                  bottle = bottle
                )
              }
            )
        #   }
        # )

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
