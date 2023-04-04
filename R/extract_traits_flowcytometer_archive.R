#' Extract traits from flowcytometer data by using the archived data
#'
#' @param extracted_dir
#' @param gates_coordinates the \code{gates_coordinates}
#' @param particles particle class to extract. Mainly \code{bacteria} or
#'   \code{algae}, See \code{LEEF.measurement.flowcytometer::extract_traits()}
#'   for details.
#' @param timestamps `character` vector containing the timestamps to be
#'   classified
#' @param output path to which the classified data will be saved as `rds`
#' @param length_slope slope of the linear regression of FSC.A and size ( lm(mean_FSC.A ~ diameter_micrometer )
#' @param length_intercept intercept of the linear regression of FSC.A and size ( lm(mean_FSC.A ~ diameter_micrometer )
#' @param log10_all if \code{TRUE}, all data not yet log10 transformed will be log10 transformed
#'   ("FL2-A", "FL1-H", "FL2-H", "FL3-H", "FL4-H", "FSC-H", "SSC-H") in the same way as in the pipeline.
#' @param use_H if \code{TRUE}, gating will be done using \code{height}, otherwie \code{area}
#' @param excl_FSCA_0 boolean. If \code{TRUE}, \code{FSA.A <= 0} will be filtered out by using
#'   a rectangular filter
#' @param wellid_keyword the kwyword which is used to identify the well ID. Usually "$WELLID" (default), but for the EAWAG Flowcytometer it is "$SMNO".
#' @param mc.cores number of cores to be used. Defaults to 1
#'
#' @return invisible `NULL`
#'
#' @importFrom pbmcapply pbmclapply
#' @importFrom yaml read_yaml write_yaml
#' @importFrom magrittr %>%
#' @importFrom dplyr
#'
#' @export
#'
#' @md
#' @examples
#'

extract_traits_flowcytometer_archive <- function(
  extracted_dir = "~/Desktop/flowcytometer.FIXED/LEEF.FIXED.archived.data/LEEF/3.archived.data/extracted/", # "/Volumes/LEEF-1_archive/LEEF.archived.data/LEEF/3.archived.data/extracted/",
  gates_coordinates,
  particles = "bacteria",
  timestamps,
  output,
  length_slope = 4.933454e-06, # ,201615
  length_intercept = 2.215799e-01, #-39861.52,
  use_H,
  excl_FSCA_0,
  log10_all = FALSE,
  mc.cores = 1,
  wellid_keyword = "$WELLID"
){
  if (length(particles) > 1){
    stop("Argument particles has to be a character vector of length 1!")
  }

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
    # parallel::mclapply(
        timestamps,
      function(timestamp){
        datadir <- file.path(
          extracted_dir,
          paste0("LEEF.fast.flowcytometer.", as.character(timestamp))
        )
        message("###############################################")
        message("Extracting traits from ", timestamp, "...")

        suppressMessages(
          {
            traits <- NULL
            try(
              expr = {
                fsa <- readRDS(file.path(file.path(datadir, "flowcytometer_fsa_ungated.rds")))
                if (log10_all) {
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
                  input = datadir,
                  particles = particles,
                  metadata_flowcytometer = read.csv(file.path(datadir, "metadata_flowcytometer.csv")),
                  fsa = fsa,
                  gates_coordinates = gates_coordinates,
                  excl_FSCA_0 = excl_FSCA_0,
                  use_H = use_H,
                  wellid_keyword = wellid_keyword
                )
              }
            )
          }
        )

        if (!is.null(traits)) {
          # stop("There is something still wrong here!!!")
          traits[[particles]]$length <- traits[[particles]]$FSC.A * length_slope + length_intercept
          traits[[particles]]$length[traits[[particles]]$length <= 0] <- NA
          traits[[particles]]$volume <- 4/9 * pi * traits[[particles]]$length^3 # assumes second radius is equals length/3

          # traits_sum <- traits[[particles]] %>%
          #   dplyr::group_by(bottle) %>%
          #   dplyr::summarise(n = n(), mean = mean(length), sd = sd(length), median = median(length))

          message("Saving timestamp ", timestamp, "...")


          dir.create(file.path(output), showWarnings = FALSE)
          saveRDS(
            object = traits[[particles]],
            file = file.path(output, paste0("flowcytometer_traits_", particles, ".", timestamp, ".rds"))
          )

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
