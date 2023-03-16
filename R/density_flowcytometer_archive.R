#' Gate and extract densities from flowcytometer data by using the archived data
#'
#' @param timestamps `character` vector containing the timestamps to be classified
#' @param output path to which the classified data will be saved as `rds`
#' @param extracted_dir srchive directory of the extracted data
#' @param gates_coordinates the \code{gates_coordinates}
#' @param use_H if \code{TRUE}, gating will be done using \code{height}, otherwie \code{area}
#' @param excl_FSCA_0 boolean. If \code{TRUE}, \code{FSA.A <= 0} will be filtered out by using
#'   a rectangular filter
#' @param log10_all if \code{TRUE}, all data not yet log10 transformed will be log10 transformed
#'   ("FL2-A", "FL1-H", "FL2-H", "FL3-H", "FL4-H", "FSC-H", "SSC-H") in the same way as in the pipeline.
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
density_flowcytometer_archive <- function(
  extracted_dir = "/Volumes/LEEF-1_archive/LEEF.archived.data/LEEF/3.archived.data/extracted/",
  gates_coordinates,
  timestamps,
  output,
  use_H,
  excl_FSCA_0,
  log10_all = FALSE,
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
                # log transform everything which had not been transformed in the pipeline
                fsa <- readRDS(file.path(file.path(datadir, "flowcytometer_fsa_ungated.rds")))
                if (log10fsa) {
                  fsa <- flowCore::transform(
                    fsa,
                    flowCore::transformList(
                      c("FL2-A", "FL1-H", "FL2-H", "FL3-H", "FL4-H", "FSC-H", "SSC-H"),
                      flowCore::truncateTransform("truncate at 1")
                    )
                  )
                  fsa <- flowCore::transform(
                    fsa,
                    flowCore::transformList(
                      c("FL2-A", "FL1-H", "FL2-H", "FL3-H", "FL4-H", "FSC-H", "SSC-H"),
                      "log10"
                    )
                  )
                }
                densities <- flowcytometer_density <- LEEF.measurement.flowcytometer::extractor_flowcytometer_density(
                  gates_coordinates = gates_coordinates,
                  fsa = fsa,
                  flow.data = read.csv(file.path(file.path(datadir, "flowcytometer_ungated.csv"))),
                  use_H = use_H,
                  excl_FSCA_0 = excl_FSCA_0,
                  dens_back = TRUE
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
          message("ERROR in extracting density in timestamp ", timestamp)
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
