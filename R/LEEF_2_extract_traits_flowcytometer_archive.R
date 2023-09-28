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
#' @param min_FSC.A numeric. If \code{!NULL}, \code{FSA.A <= min_FSC.A} will be fitered out by using
#'   a rectangular filter
#'   \code{flowCore::rectangleGate(filterId="filter_out_0", "FSC-A" = c(min_FSC.A, +Inf))}
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
LEEF_2_extract_traits_flowcytometer_archive <- function(
    extracted_dir = "~/Desktop/flowcytometer.FIXED/LEEF.FIXED.archived.data/LEEF/3.archived.data/extracted/", # "/Volumes/LEEF-1_archive/LEEF.archived.data/LEEF/3.archived.data/extracted/",
    gates_coordinates,
    particles = "bacteria",
    timestamps,
    output,
    length_slope, # = 4.933454e-06, # ,201615
    length_intercept, # = 2.215799e-01, #-39861.52,
    use_H,
    min_FSC.A,
    log10_all = FALSE,
    wellid_keyword = "$WELLID",
    mc.cores = 1) {
    dir.create(
        output,
        showWarnings = FALSE,
        recursive = TRUE
    )


    # do the stuff -------------------------------------------------------

    # biomass_per_bottle <- pbapply::pblapply(
    biomass_per_bottle <- pbmcapply::pbmclapply(
        timestamps,
        function(timestamp) {
            biomass_per_bottle <- dplyr::tibble()
            datadir <- file.path(
                extracted_dir,
                paste0("LEEF.flowcytometer.flowcytometer.", as.character(timestamp))
            )
            message("\n###############################################")
            message("Extracting traits from ", timestamp, "...")

            suppressMessages({
                traits <- NULL
                try(
                    expr = {
                        fsa_p1 <- readRDS(file.path(file.path(datadir, "flowcytometer_fsa_ungated.p_1.rds")))
                        if (log10_all) {
                            fsa_p1 <- flowCore::transform(
                                fsa_p1,
                                flowCore::transformList(
                                    c("FL2-A", "FL1-H", "FL2-H", "FL3-H", "FL4-H", "FSC-H", "SSC-H"),
                                    flowCore::truncateTransform("truncate at 1")
                                )
                            )
                            fsa_p1 <- flowCore::transform(
                                fsa_p1,
                                flowCore::transformList(
                                    c("FL2-A", "FL1-H", "FL2-H", "FL3-H", "FL4-H", "FSC-H", "SSC-H"),
                                    "log10"
                                )
                            )
                        }


                        fsa_p2 <- readRDS(file.path(file.path(datadir, "flowcytometer_fsa_ungated.p_2.rds")))
                        if (log10_all) {
                            fsa_p2 <- flowCore::transform(
                                fsa_p1,
                                flowCore::transformList(
                                    c("FL2-A", "FL1-H", "FL2-H", "FL3-H", "FL4-H", "FSC-H", "SSC-H"),
                                    flowCore::truncateTransform("truncate at 1")
                                )
                            )
                            fsa_p2 <- flowCore::transform(
                                fsa_p2,
                                flowCore::transformList(
                                    c("FL2-A", "FL1-H", "FL2-H", "FL3-H", "FL4-H", "FSC-H", "SSC-H"),
                                    "log10"
                                )
                            )
                        }


                        if (is.null(gates_coordinates)) {
                            gates_coordinates <- utils::read.csv(
                                file.path(datadir, "gates_coordinates.csv")
                            )
                        }


                        traits <- LEEF.2.measurement.flowcytometer::extract_traits(
                            input = datadir,
                            particles = particles,
                            metadata_flowcytometer = read.csv(file.path(datadir, "metadata_flowcytometer.csv")),
                            fsa_p1 = fsa_p1,
                            fsa_p2 = fsa_p2,
                            gates_coordinates = gates_coordinates,
                            min_FSC.A = min_FSC.A,
                            use_H = use_H,
                            wellid_keyword = wellid_keyword
                        )
                    }
                )
            })

            if (!is.null(traits)) {
                dir.create(file.path(output), showWarnings = FALSE)
                biomass_per_bottle <- dplyr::tibble()
                dens_fn <- file.path(output, paste0("flowcytometer_density.", timestamp, ".rds"))
                dens <- readRDS(dens_fn)

                for (p in particles) {
                    message("\nExtracting ", p, "...")

                    traits[[p]]$species <- p
                    traits[[p]]$length <- as.numeric(NA)
                    traits[[p]]$volume <- as.numeric(NA)
                    traits[[p]]$biomass <- as.numeric(NA)

                    if (p == "bacteria") {
                        # stop("There is something still wrong here!!!")

                        traits[[p]]$length <- traits[[p]]$FSC.A * length_slope + length_intercept

                        # traits[[p]]$length[traits[[p]]$length <= 0] <- as.numeric(NA)
                        ##
                        ## We assume the bacteria to have the shape of an ellipsoid of revolution
                        ## See e.g. https://en.wikipedia.org/wiki/Ellipsoid#Volume
                        ## The volume can be calculated as follows
                        ## V = 4/3 * pi * a * b * c ## we assume, that :
                        ## a = length / 2 ## b = c = a/3 = length / 6
                        ## therefore we have:
                        ## V = 4/3 * pi * (l/2) * (l/6) * (l/6)
                        ## V = 4/3 * pi * l^3 / 72
                        ##

                        traits[[p]]$volume <- 4 / 3 * pi * traits[[p]]$length^3 / 72
                        traits[[p]]$biomass <- traits[[p]]$volume / 10^12
                    }

                    saveRDS(
                        object = traits[[p]],
                        file = file.path(output, paste0("flowcytometer_traits_", p, ".", timestamp, ".rds"))
                    )


                    bm <- traits[[p]] |>
                        group_by(bottle, sample, plate, species) |>
                        summarise(biomass = sum(biomass)) |>
                        full_join(
                            x = dens,
                            by = dplyr::join_by(bottle, sample, plate, species)
                        ) |>
                        mutate(biomass = biomass * 1000000 / volume * dilution_factor) |>
                        ungroup()

                    biomass_per_bottle <- rbind(
                        biomass_per_bottle,
                        bm
                    )

                    rm(bm)
                    message("Done\n")
                }

                unlink(dens_fn)
                saveRDS(
                    object = biomass_per_bottle,
                    file = dens_fn
                )
            } else {
                message("ERROR in extracting traits in timestamp ", timestamp)
            }

            message("Saving Densities")

            message("Done")
            message("###############################################")


            return(biomass_per_bottle)
        },
        mc.preschedule = FALSE,
        mc.cores = mc.cores
    )
    names(biomass_per_bottle) <- timestamps
    invisible(biomass_per_bottle)
}
