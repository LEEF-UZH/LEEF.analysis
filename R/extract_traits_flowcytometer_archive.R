#' Extract traits from flowcytometer data by using the archived data
#'
#' @param extracted_dir
#' @param timestamps `character` vector containing the timestamps to be classified
#' @param output path to which the classified data will be saved as `rds`
#' @param mc.cores number of cores to be used. Defaults to 1
#'
#' @return invisible `NULL`
#'
#' @importFrom  parallel mclapply
#' @importFrom yaml read_yaml write_yaml
#' @export
#'
#' @md
#' @examples
#'
#'
#'


TODO!!!!!

extract_traits_flowcytometer_archive <- function(
  extracted_dir = "/Volumes/LEEF-1_archive/LEEF.archived.data/LEEF/3.archived.data/extracted/",
  particles = "bacteria",
  timestamps,
  output,
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
      timestamps,
      function(timestamp){
        datadir <- file.path(
          extracted_dir,
          paste0("LEEF.flowcytometer.flowcytometer.", as.character(timestamp))
        )
        message("###############################################")
        message("Extracting timestamp ", timestamp, "...")

        suppressMessages(
          {
            densities <- NULL
            try(
              expr = {
                traits <- LEEF.measurement.flowcytometer::extract_traits(
                  input = datadir,
                  particles = particles
                )
              }
            )
          }
        )

        if (!is.null(densities)) {
          message("Saving timestamp ", timestamp, "...")

          dir.create(file.path(output))
          saveRDS(
            object = traits$batceria,
            file = file.path(output, paste0("flowcytometer_density.", timestamp, ".rds"))
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
