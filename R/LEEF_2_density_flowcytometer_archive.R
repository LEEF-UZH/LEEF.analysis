#' Gate and extract densities from flowcytometer data by using the archived data
#'
#' @param timestamps `character` vector containing the timestamps to be classified
#' @param extracted_dir srchive directory of the extracted data
#' @param output path to which the classified data will be saved as `rds`
#' @param use_H if \code{TRUE}, gating will be done using \code{height}, otherwie \code{area}
#' @param gates_coordinates the \code{gates_coordinates}
#' @param min_FSC.A numeric. If \code{!NULL}, \code{FSA.A <= min_FSC.A} will be fitered out by using
#'   a rectangular filter
#'   \code{flowCore::rectangleGate(filterId="filter_out_0", "FSC-A" = c(min_FSC.A, +Inf))}
#' @param particles particle class to extract. Mainly \code{bacteria} or
#'   \code{algae}.
#'
#' @return invisible `NULL`
#'
#' @importFrom  parallel mclapply
#' @importFrom yaml read_yaml write_yaml
#' @importFrom pbapply pblapply
#' @importFrom pbmcapply pbmclapply
#' @export
#'
#' @md
#' @examples
LEEF_2_density_flowcytometer_archive <- function(
    extracted_dir,
    timestamps,
    output,
    use_H,
    gates_coordinates,
    min_FSC.A = NULL,
    particles = NULL,
    mc.cores = 1) {
  dir.create(
    output,
    showWarnings = FALSE,
    recursive = TRUE
  )

  # do the stuff -------------------------------------------------------

  return(
    # lapply(
    # pbapply::pblapply(
    pbmcapply::pbmclapply(

      ## do for each timestamp
      timestamps,
      function(timestamp) {

        ## before September 2024 the densities were calculated here by reading in the extracted (level1)
        ## flowcytometer data.
        ## Now the per bottle density and the per bottle biomass are calculated from the trait data
        ## that has already been extracted and saved.

        ## first we need to read in some metadata about the flowcytometer samples
        flow.data.p1 <- readRDS(file.path(extracted_dir,
                                          paste0("LEEF.flowcytometer.flowcytometer.",
                                                 timestamp),
                                          "flowcytometer_ungated.p_1.rds")) |>
          select(timestamp, plate, bottle, sample, volume, dilution_factor, date)
        flow.data.p2 <- readRDS(file.path(extracted_dir,
                                          paste0("LEEF.flowcytometer.flowcytometer.",
                                                 timestamp),
                                          "flowcytometer_ungated.p_2.rds")) |>
          select(timestamp, plate, bottle, sample, volume, dilution_factor, date)
        flow.data <- bind_rows(flow.data.p1, flow.data.p2)


        ## Then read in the trait data
        bacteria_traits <- readRDS(
          file.path(
            output,
            paste0("flowcytometer_traits_bacteria.", timestamp, ".rds")
          )
        )
        algae_traits <- readRDS(
          file.path(
            output,
            paste0("flowcytometer_traits_algae.", timestamp, ".rds")
          )
        )

        ## Then calculate the per sample count and biomass
        bacteria_summary <- bacteria_traits |>
          group_by(timestamp, plate, bottle, sample) |>
          summarise(
            count = n(),
            biomass_per_sample = sum(biomass, na.rm = TRUE)
          ) |>
          mutate(species = "bacteria")

        algae_summary <- algae_traits |>
          group_by(timestamp, plate, bottle, sample) |>
          summarise(
            count = n(),
            biomass_per_sample = NA
          ) |>
          mutate(species = "algae")

        density_summary <- bind_rows(bacteria_summary, algae_summary)

        ## merge in the metadata
        density_summary <-
          left_join(density_summary, flow.data, by = c("timestamp", "plate", "bottle", "sample"))

        ## calcuate the densities of algae and bacteria, in individuals and in grams (biomass)
        density_summary$density <- density_summary$count * 1000000 /
          density_summary$volume * density_summary$dilution_factor
        density_summary$biomass <- density_summary$biomass_per_sample * 1000000 /
          density_summary$volume * density_summary$dilution_factor
        density_summary$sample_letter <- substr(x = density_summary$sample, start = 1, stop = 1)
        density_summary$sample_number <- as.numeric(substr(x = density_summary$sample, start = 2, stop = 3))
        density_summary$date <- format(as.Date(density_summary$date, "%d-%b-%Y"), "%Y-%m-%d")
        density_summary <- density_summary[order(density_summary$date,
                                                 density_summary$sample_letter,
                                                 density_summary$sample_number), ]

        ## save the density data
        saveRDS(
        object = density_summary,
        file = file.path(output, paste0("flowcytometer_density.x.", timestamp, ".rds"))
        )

#
#         ## old code below
#
#         datadir <- file.path(
#           extracted_dir,
#           paste0("LEEF.flowcytometer.flowcytometer.", as.character(timestamp))
#         )
#
#         tmpinput <- tempfile()
#
#         dir.create(file.path(tmpinput, "flowcytometer"), recursive = TRUE, showWarnings = FALSE)
#         file.copy(
#           list.files(datadir, full.names = TRUE),
#           file.path(tmpinput, "flowcytometer"),
#           recursive = TRUE
#         )
#
#         unlink(file.path(tmpinput, "flowcytometer", "gates_coordinates.csv"))
#         write.csv(gates_coordinates, file.path(tmpinput, "flowcytometer", "gates_coordinates.csv"))
#         unlink(file.path(tmpinput, "flowcytometer", "flowcytometer_gates.p_1.rds"))
#         LEEF.2.measurement.flowcytometer::calculate_gates(gates_coordinates = gates_coordinates) |>
#           saveRDS(file = file.path(tmpinput, "flowcytometer", "flowcytometer_gates.p_1.rds"))
#         unlink(file.path(tmpinput, "flowcytometer", "flowcytometer_gates.p_2.rds"))
#         LEEF.2.measurement.flowcytometer::calculate_gates(gates_coordinates = gates_coordinates) |>
#           saveRDS(file = file.path(tmpinput, "flowcytometer", "flowcytometer_gates.p_2.rds"))
#
#         message("\n###############################################")
#         message("Gating timestamp ", timestamp, "...")
#
#         suppressMessages({
#           densities <- NULL
#           try(
#             expr = {
#               densities <- LEEF.2.measurement.flowcytometer::extractor_flowcytometer_density(
#                 input = tmpinput,
#                 output = tmpinput,
#                 use_H = use_H,
#                 gates_coordinates = gates_coordinates,
#                 min_FSC.A = min_FSC.A,
#                 dens_back = TRUE
#               )
#             }
#           )
#         })
#
#         dir.create(
#           fsa_dir <- file.path(output, "fsa_gated_density", paste0("LEEF.flowcytometer.flowcytometer.", timestamp)),
#           recursive = TRUE,
#           showWarnings = FALSE
#         )
#         file.copy(
#           list.files(file.path(tmpinput, "flowcytometer"), "flowcytometer_fsa_ungated.", full.names = TRUE),
#           fsa_dir,
#           recursive = FALSE
#         )
#         file.copy(
#           list.files(file.path(tmpinput, "flowcytometer"), "flowcytometer_gates.", full.names = TRUE),
#           fsa_dir,
#           recursive = FALSE
#         )
#
#         unlink(tmpinput, recursive = TRUE, force = TRUE)
#         if (dir.exists(tmpinput)) {
#           stop("Temporary directory ", tmpinput, " still exists!")
#         }
#
#
#         if (!is.null(densities)) {
#           if (!is.null(particles)) {
#             densities <- densities[densities$species %in% particles, ]
#           }
#
#           message("Saving timestamp ", timestamp, "...")
#
#           saveRDS(
#             object = densities,
#             file = file.path(output, paste0("flowcytometer_density.", timestamp, ".rds"))
#           )
#         } else {
#           message("ERROR in extracting density in timestamp ", timestamp)
#         }


        message("Done")
        message("###############################################")
        invisible(NULL)
      },
      mc.preschedule = FALSE,
      mc.cores = mc.cores
    )
  )
}
