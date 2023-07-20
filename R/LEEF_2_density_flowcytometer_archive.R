#' Gate and extract densities from flowcytometer data by using the archived data
#'
#' @param timestamps `character` vector containing the timestamps to be classified
#' @param extracted_dir srchive directory of the extracted data
#' @param output path to which the classified data will be saved as `rds`
#' @param use_H if \code{TRUE}, gating will be done using \code{height}, otherwie \code{area}
#' @param min_FSC.A numeric. If \code{!NULL}, \code{FSA.A <= min_FSC.A} will be fitered out by using
#'   a rectangular filter
#'   \code{flowCore::rectangleGate(filterId="filter_out_0", "FSC-A" = c(min_FSC.A, +Inf))}
#' @param particles particle class to extract. Mainly \code{bacteria} or
#'   \code{algae}.
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
LEEF_2_density_flowcytometer_archive <- function(
  extracted_dir,
  timestamps,
  output,
  use_H,
  min_FSC.A = NULL,
  particles = NULL,
  mc.cores = 1
){
  dir.create(
    output,
    showWarnings = FALSE,
    recursive = TRUE
  )

  tmpinput <- tempfile()
  dir.create(
    tmpinput,
    recursive = TRUE, 
    showWarnings = FALSE
  )

  on.exit(unlink(tmpinput))


  # do the stuff -------------------------------------------------------

  return(
    # pbmcapply::pbmclapply(
    lapply(
      timestamps,
      function(timestamp){
        datadir <- file.path(
          extracted_dir,
          paste0("LEEF.flowcytometer.flowcytometer.", as.character(timestamp))
        )

        dir.create(file.path(tmpinput, "flowcytometer"), recursive = TRUE, showWarnings = FALSE)
        file.copy(
            list.files(datadir, full.names = TRUE),
            file.path(tmpinput, "flowcytometer"),
            recursive = TRUE
        )

        message("###############################################")
        message("Gating timestamp ", timestamp, "...")

        suppressMessages(
          {
            densities <- NULL
            try(
              expr = {
                densities <- LEEF.2.measurement.flowcytometer::extractor_flowcytometer_density(
                  input = tmpinput,
                  output = tmpinput,
                  use_H = use_H,
                  min_FSC.A = min_FSC.A,
                  dens_back = TRUE
                )
              }
            )
          }
        )

        if (!is.null(densities)) {
          if (!is.null(particles)){
            densities <- densities[densities$species %in% particles,]
          }

          message("Saving timestamp ", timestamp, "...")

          dir.create(file.path(output))
          saveRDS(
            object = densities,
            file = file.path(output, paste0("flowcytometer_density.", timestamp, ".rds"))
          )
          unlink(file.path(tmpinput, "flowcytometer"))

        } else {
          message("ERROR in extracting density in timestamp ", timestamp)
        }


        message("Done")
        message("###############################################")
        invisible(NULL)
      }#,
      #mc.preschedule = FALSE,
      #mc.cores = mc.cores
    )
  )
}
