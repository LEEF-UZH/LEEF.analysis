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
#' @param particles the particles, as defined in the gates file, to be extracted. Allowed are one or multiple of
#'   \code{bacteria, LNA, MNA, HNA, algae}
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
traits_flowcytometer_archive <- function(
  extracted_dir = "/Volumes/LEEF-1_archive/LEEF.archived.data/LEEF/3.archived.data/extracted/",
  gates_coordinates,
  timestamps,
  output,
  use_H,
  excl_FSCA_0,
  log10_all = FALSE,
  particles = c("bacteria", "algae"),
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
                traits <- LEEF.measurement.flowcytometer::extract_traits(
                  particles = particles,
                  metadata_flowcytometer = utils::read.csv(
                    file.path(
                      params$extracted_dir,
                      paste0("LEEF.fast.flowcytometer.", as.character(params$timestamp)),
                      "metadata_flowcytometer.csv")
                  ),
                  use_H = use_H,
                  excl_FSCA_0 = excl_FSCA_0,
                  timestamp = timestamp,
                  gates_coordinates = gates_coordinates,
                  fsa = fsa
                )
              }
            )
          }
        )

        if (!is.null(traits)) {
          message("Saving timestamp ", timestamp, "...")

          dir.create(file.path(output))
          for (pn in names(traits)){
            saveRDS(
              object = traits[[pn]],
              file = file.path(output, paste0("flowcytometer_traits_", pn, ".", timestamp, ".rds"))
            )
          }
        } else {
          message("ERROR in extracting traits in timestamp ", timestamp)
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
