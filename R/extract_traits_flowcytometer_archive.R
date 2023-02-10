#' Extract traits from flowcytometer data by using the archived data
#'
#' @param extracted_dir
#' @param particles particle class to extract. Mainly \code{bacteria} or
#'   \code{algae}, See \code{LEEF.measurement.flowcytometer::extract_traits()}
#'   for details.
#' @param timestamps `character` vector containing the timestamps to be
#'   classified
#' @param output path to which the classified data will be saved as `rds`
#' @param length_slope slope of the linear regression of FSC.A and size ( lm(mean_FSC.A ~ diameter_micrometer )
#' @param length_intercept intercept of the linear regression of FSC.A and size ( lm(mean_FSC.A ~ diameter_micrometer )
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
  extracted_dir = "/Volumes/LEEF-1_archive/LEEF.archived.data/LEEF/3.archived.data/extracted/",
  particles = "bacteria",
  timestamps,
  output,
  length_slope = 201615 , # 4.933e-06,
  length_intercept = -39861.52, #0.2216,
  mc.cores = 1
){
  stop("There is something still wrong here - calculation of langth and volume!!!!!!!")

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
    # pbmcapply::pbmclapply(
    parallel::mclapply(
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
                traits <- LEEF.measurement.flowcytometer::extract_traits(
                  input = datadir,
                  particles = particles,
                  metadata_flowcytometer = read.csv(file.path(datadir, "metadata_flowcytometer.csv"))
                )
              }
            )
          }
        )

        if (!is.null(traits)) {
          message("Saving timestamp ", timestamp, "...")

          stop("There is something still wrong here!!!")
          traits[[particles]]$length <- (10^(traits[[particles]]$FSC.A)) / length_slope - length_intercept/length_slope
          traits[[particles]]$volume <-4/9 * pi * traits[[particles]]$length^3

          traits_sum <- traits[[particles]] %>% dplyr::group_by(bottle) %>%
            dplyr::summarise(n = n(), mean = mean(length), sd = sd(length), median = median(length))


          dir.create(file.path(output))
          saveRDS(
            object = traits$bacteria,
            file = file.path(output, paste0("flowcytometer_traits_bacteria.", timestamp, ".rds"))
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
