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
extract_traits_flowcytometer_archive <- function(
  extracted_dir = "/Volumes/LEEF-1_archive/LEEF.archived.data/LEEF/3.archived.data/extracted/",
  gates_coordinates,
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
          paste0("LEEF.fast.flowcytometer.", as.character(timestamp))
        )
        message("###############################################")
        message("Gating timestamp ", timestamp, "...")

        suppressMessages(
          {
            densities <- NULL
            try(
              expr = {
                densities <- LEEF.measurement.flowcytometer::gating(
                  gates_coordinates = gates_coordinates,
                  fsa = readRDS(file.path(file.path(datadir, "flowcytometer_fsa_ungated.rds"))),
                  flow.data = read.csv(file.path(file.path(datadir, "flowcytometer_ungated.csv")))
                )
              }
            )
          }
        )

        if (!is.null(densities)) {
          message("Saving timestamp ", timestamp, "...")

          dir.create(file.path(output))
          saveRDS(
            object = densities$flow.data,
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
